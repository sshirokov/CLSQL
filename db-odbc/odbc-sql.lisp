;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          odbc-sql.cl
;;;; Purpose:       Low-level interface for CLSQL ODBC backend
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  Feb 2002
;;;;
;;;; $Id: odbc-sql.lisp 8983 2004-04-12 21:16:48Z kevin $
;;;;
;;;; This file, part of CLSQL, is Copyright (c) 2002 by Kevin M. Rosenberg
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(defpackage #:clsql-odbc
    (:use #:common-lisp #:clsql-base-sys)
    (:export #:odbc-database)
    (:documentation "This is the CLSQL interface to ODBC."))

(in-package #:clsql-odbc)

;; ODBC interface

(defclass odbc-database (database)
  ((odbc-conn :accessor database-odbc-conn :initarg :odbc-conn)))

(defmethod database-name-from-spec (connection-spec
				    (database-type (eql :odbc)))
  (check-connection-spec connection-spec database-type (dsn user password))
  (destructuring-bind (dsn user password) connection-spec
    (declare (ignore password))
    (concatenate 'string dsn "/" user)))

(defmethod database-connect (connection-spec (database-type (eql :odbc)))
  (check-connection-spec connection-spec database-type (dsn user password))
  (destructuring-bind (dsn user password) connection-spec
    (handler-case
	(make-instance 'odbc-database
	  :name (database-name-from-spec connection-spec :odbc)
	  :odbc-conn
	  (odbc-dbi:connect :user user
			:password password
			:data-source-name dsn))
      (error () 	;; Init or Connect failed
	(error 'clsql-connect-error
	       :database-type database-type
	       :connection-spec connection-spec
	       :errno nil
	       :error "Connection failed")))))

#+nil
(defun store-type-of-connected-database (db)
  (let* ((odbc-db (odbc-db db))
	 (server-name (get-odbc-info odbc-db odbc::$SQL_SERVER_NAME))
	 (dbms-name (get-odbc-info odbc-db odbc::$SQL_DBMS_NAME))
	 (type
	  ;; need SERVER-NAME and DBMS-NAME because many drivers mix this up
	  (cond 
	   ((or (search "postgresql" server-name :test #'char-equal)
		(search "postgresql" dbms-name :test #'char-equal))
	    :postgresql)
	   ((or (search "mysql" server-name :test #'char-equal)
		(search "mysql" dbms-name :test #'char-equal))
	    :mysql)
	   ((or (search "oracle" server-name :test #'char-equal)
		(search "oracle" dbms-name :test #'char-equal))
	    :oracle))))
    (setf (database-type db) type)))
  

(defmethod database-disconnect ((database odbc-database))
  (odbc-dbi:disconnect (database-odbc-conn database))
  (setf (database-odbc-conn database) nil)
  t)

(defmethod database-query (query-expression (database odbc-database) result-types) 
  (handler-case
      (odbc-dbi:sql query-expression :db (database-odbc-conn database)
	       :types result-types)
    (error ()
      (error 'clsql-sql-error
	     :database database
	     :expression query-expression
	     :errno nil
	     :error "Query failed"))))

(defmethod database-execute-command (sql-expression 
				     (database odbc-database))
  (handler-case
      (odbc-dbi:sql sql-expression (database-odbc-conn database))
    (error ()
      (error 'clsql-sql-error
	     :database database
	     :expression sql-expression
	     :errno nil
	     :error "Execute command failed"))))

(defstruct odbc-result-set
  (query nil)
  (types nil :type cons)
  (full-set nil :type boolean))

(defmethod database-query-result-set ((query-expression string)
				      (database odbc-database) 
				      &key full-set result-types)
  (handler-case 
      (multiple-value-bind (query column-names)
	  (odbc-dbi:sql query-expression 
		   :db (database-odbc-conn database) 
		   :row-count nil
		   :column-names t
		   :query t
		   :result-types result-types
		   )
	(values
	 (make-odbc-result-set :query query :full-set full-set 
				:types result-types)
	 (length column-names)
	 nil ;; not able to return number of rows with odbc
	 ))
    (error ()
      (error 'clsql-sql-error
	     :database database
	     :expression query-expression
	     :errno nil
	     :error "Query result set failed"))))

(defmethod database-dump-result-set (result-set (database odbc-database))
  (odbc-dbi:close-query (odbc-result-set-query result-set))
  t)

(defmethod database-store-next-row (result-set
				    (database odbc-database)
				    list)
  (let ((row (odbc-dbi:fetch-row (odbc-result-set-query result-set) nil 'eof)))
    (if (eq row 'eof)
	nil
      (progn
	(loop for elem in row
	    for rest on list
	    do
	      (setf (car rest) elem))
	list))))

;;; Sequence functions

(defun %sequence-name-to-table (sequence-name)
  (concatenate 'string "_clsql_seq_" (sql-escape sequence-name)))

(defun %table-name-to-sequence-name (table-name)
  (and (>= (length table-name) 11)
       (string= (subseq table-name 0 11) "_clsql_seq_")
       (subseq table-name 11)))

(defmethod database-create-sequence (sequence-name
				     (database odbc-database))
  (let ((table-name (%sequence-name-to-table sequence-name)))
    (database-execute-command
     (concatenate 'string "CREATE TABLE " table-name
		  " (id int NOT NULL PRIMARY KEY AUTO_INCREMENT)")
     database)
    (database-execute-command 
     (concatenate 'string "INSERT INTO " table-name
		  " VALUES (0)")
     database)))

(defmethod database-drop-sequence (sequence-name
				   (database odbc-database))
  (database-execute-command
   (concatenate 'string "DROP TABLE " (%sequence-name-to-table sequence-name)) 
   database))

(defmethod database-list-sequences ((database odbc-database)
                                    &key (owner nil))
  (declare (ignore owner))
  (mapcar #'(lambda (s) (%table-name-to-sequence-name (car s)))
          (database-query "SHOW TABLES LIKE '%clsql_seq%'" 
                          database nil)))

(defmethod database-set-sequence-position (sequence-name
                                           (position integer)
                                           (database odbc-database))
  (database-execute-command
   (format nil "UPDATE ~A SET id=~A" (%sequence-name-to-table sequence-name)
           position)
   database)
  position)

(defmethod database-sequence-next (sequence-name (database odbc-database))
  (warn "Not implemented."))

(defmethod database-sequence-last (sequence-name (database odbc-database))
  (declare (ignore sequence-name)))

(defmethod database-create (connection-spec (type (eql :odbc)))
  (warn "Not implemented."))

(defmethod database-destroy (connection-spec (type (eql :odbc)))
  (warn "Not implemented."))

(defmethod database-probe (connection-spec (type (eql :odbc)))
  (warn "Not implemented."))

#+ignore		       
(when (clsql-base-sys:database-type-library-loaded :odbc)
  (clsql-base-sys:initialize-database-type :database-type :odbc))
