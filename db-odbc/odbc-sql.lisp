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
  ((odbc-conn :accessor database-odbc-conn :initarg :odbc-conn)
   (odbc-db-type :accessor database-odbc-db-type)))

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
	(let ((db
	       (make-instance 'odbc-database
		 :name (database-name-from-spec connection-spec :odbc)
		 :database-type :odbc
		 :odbc-conn
		 (odbc-dbi:connect :user user
				   :password password
				   :data-source-name dsn))))
	  (store-type-of-connected-database db)
	  db)
    (clsql-error (e)
      (error e))
    (error () 	;; Init or Connect failed
      (error 'clsql-connect-error
	     :database-type database-type
	     :connection-spec connection-spec
	     :errno nil
	     :error "Connection failed")))))

(defun store-type-of-connected-database (db)
  (let* ((odbc-conn (database-odbc-conn db))
	 (server-name (odbc-dbi::get-odbc-info odbc-conn odbc::$SQL_SERVER_NAME))
	 (dbms-name (odbc-dbi::get-odbc-info odbc-conn odbc::$SQL_DBMS_NAME))
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
    (setf (database-odbc-db-type db) type)))
  
(defmethod database-disconnect ((database odbc-database))
  (odbc-dbi:disconnect (database-odbc-conn database))
  (setf (database-odbc-conn database) nil)
  t)

(defmethod database-query (query-expression (database odbc-database) 
			   result-types) 
  (handler-case
      (odbc-dbi:sql query-expression :db (database-odbc-conn database)
		    :result-types result-types)
    (clsql-error (e)
      (error e))
    #+ignore
    (error ()
      (error 'clsql-sql-error
	     :database database
	     :expression query-expression
	     :errno nil
	     :error "Query failed"))))

(defmethod database-execute-command (sql-expression 
				     (database odbc-database))
  (handler-case
      (odbc-dbi:sql sql-expression :db (database-odbc-conn database))
    (clsql-error (e)
      (error e))
    (error ()
      (error 'clsql-sql-error
	     :database database
	     :expression sql-expression
	     :errno nil
	     :error "Execute command failed"))))

(defstruct odbc-result-set
  (query nil)
  (types nil)
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
		   :result-types result-types)
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
		  " (last_value int NOT NULL PRIMARY KEY, increment_by int, min_value int, is_called char(1))")
     database)
    (database-execute-command 
     (concatenate 'string "INSERT INTO " table-name
		  " VALUES (1,1,1,'f')")
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

(defmethod database-list-tables ((database odbc-database)
				 &key (owner nil))
  (declare (ignore owner))
    (multiple-value-bind (rows col-names)
      (odbc-dbi:list-all-database-tables :db (database-odbc-conn database))
    (let ((pos (position "TABLE_NAME" col-names :test #'string-equal)))
      (when pos
	(loop for row in rows
	    collect (nth pos row))))))

(defmethod database-list-attributes ((table string) (database odbc-database)
                                     &key (owner nil))
  (declare (ignore owner))
  (multiple-value-bind (rows col-names)
      (odbc-dbi:list-all-table-columns table :db (database-odbc-conn database))
    (let ((pos (position "COLUMN_NAME" col-names :test #'string-equal)))
      (when pos
	(loop for row in rows
	    collect (nth pos row))))))

(defmethod database-attribute-type ((attribute string) (table string) (database odbc-database)
                                     &key (owner nil))
  (declare (ignore owner))
  (multiple-value-bind (rows col-names)
      (odbc-dbi:list-all-table-columns table :db (database-odbc-conn database))
    (let ((pos (position "TYPE_NAME" col-names :test #'string-equal)))
      (when pos
	(loop for row in rows
	    collect (nth pos row))))))

(defmethod database-set-sequence-position (sequence-name
                                           (position integer)
                                           (database odbc-database))
  (database-execute-command
   (format nil "UPDATE ~A SET last_value=~A,is_called='t'" 
	   (%sequence-name-to-table sequence-name)
           position)
   database)
  position)

(defmethod database-sequence-next (sequence-name (database odbc-database))
  (without-interrupts
   (let* ((table-name (%sequence-name-to-table sequence-name))
	  (tuple
	   (car (database-query 
		 (concatenate 'string "SELECT last_value,is_called FROM " 
			      table-name)
		 database
		 :auto))))
     (cond
       ((char-equal (schar (second tuple) 0) #\f)
	(database-execute-command
	 (format nil "UPDATE ~A SET is_called='t'" table-name)
	 database)
	(car tuple))
       (t
	(let ((new-pos (1+ (car tuple))))
	 (database-execute-command
	  (format nil "UPDATE ~A SET last_value=~D" table-name new-pos)
	  database)
	 new-pos))))))
	     
(defmethod database-sequence-last (sequence-name (database odbc-database))
  (without-interrupts
   (caar (database-query 
	  (concatenate 'string "SELECT last_value FROM " 
		       (%sequence-name-to-table sequence-name))
	  database
	  :auto))))

(defmethod database-create (connection-spec (type (eql :odbc)))
  (warn "Not implemented."))

(defmethod database-destroy (connection-spec (type (eql :odbc)))
  (warn "Not implemented."))

(defmethod database-probe (connection-spec (type (eql :odbc)))
  (warn "Not implemented."))

#+ignore		       
(when (clsql-base-sys:database-type-library-loaded :odbc)
  (clsql-base-sys:initialize-database-type :database-type :odbc))
