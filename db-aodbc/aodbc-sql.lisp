;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          aodbc-sql.cl
;;;; Purpose:       Low-level interface for CLSQL AODBC backend
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  Feb 2002
;;;;
;;;; $Id$
;;;;
;;;; This file, part of CLSQL, is Copyright (c) 2002 by Kevin M. Rosenberg
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(in-package #:clsql-aodbc)

;; interface foreign library loading routines
(defmethod clsql-sys:database-type-library-loaded ((database-type (eql :aodbc)))
  "T if foreign library was able to be loaded successfully. "
  (when (find-package :dbi) ;; finds Allegro's DBI (AODBC) package
    t))

(defmethod clsql-sys:database-type-load-foreign ((databae-type (eql :aodbc)))
  t)

(when (find-package :dbi)
  (clsql-sys:database-type-load-foreign :aodbc)) 

(defmethod database-initialize-database-type ((database-type (eql :aodbc)))
  t)


;; AODBC interface

(defclass aodbc-database (database)
  ((aodbc-conn :accessor database-aodbc-conn :initarg :aodbc-conn)
   (aodbc-db-type :accessor database-aodbc-db-type :initform :unknown)))

(defmethod database-name-from-spec (connection-spec
				    (database-type (eql :aodbc)))
  (check-connection-spec connection-spec database-type (dsn user password))
  (destructuring-bind (dsn user password) connection-spec
    (declare (ignore password))
    (concatenate 'string dsn "/" user)))

(defmethod database-connect (connection-spec (database-type (eql :aodbc)))
  (check-connection-spec connection-spec database-type (dsn user password))
  #+aodbc-v2
  (destructuring-bind (dsn user password) connection-spec
    (handler-case
	(make-instance 'aodbc-database
	  :name (database-name-from-spec connection-spec :aodbc)
	  :database-type :aodbc
	  :aodbc-conn
	  (dbi:connect :user user
		       :password password
		       :data-source-name dsn))
      (clsql-error (e)
	(error e))
      (error () 	;; Init or Connect failed
	(error 'clsql-connect-error
	       :database-type database-type
	       :connection-spec connection-spec
	       :errno nil
	       :error "Connection failed")))))

(defmethod database-disconnect ((database aodbc-database))
  #+aodbc-v2
  (dbi:disconnect (database-aodbc-conn database))
  (setf (database-aodbc-conn database) nil)
  t)

(defmethod database-query (query-expression (database aodbc-database) result-types field-names) 
  #+aodbc-v2
  (handler-case
      (dbi:sql query-expression :db (database-aodbc-conn database)
	       :types result-types
               :column-names field-names)
      (clsql-error (e)
	(error e))
    (error ()
      (error 'clsql-sql-error
	     :database database
	     :expression query-expression
	     :errno nil
	     :error "Query failed"))))

(defmethod database-execute-command (sql-expression 
				     (database aodbc-database))
  #+aodbc-v2
  (handler-case
      (dbi:sql sql-expression :db (database-aodbc-conn database))
      (clsql-error (e)
	(error e))
    (error ()
      (error 'clsql-sql-error
	     :database database
	     :expression sql-expression
	     :errno nil
	     :error "Execute command failed"))))

(defstruct aodbc-result-set
  (query nil)
  (types nil :type cons)
  (full-set nil :type boolean))

(defmethod database-query-result-set ((query-expression string)
				      (database aodbc-database) 
				      &key full-set result-types)
  #+aodbc-v2
  (handler-case 
      (multiple-value-bind (query column-names)
	  (dbi:sql query-expression 
		   :db (database-aodbc-conn database) 
		   :row-count nil
		   :column-names t
		   :query t
		   :types result-types
		   )
	(values
	 (make-aodbc-result-set :query query :full-set full-set 
				:types result-types)
	 (length column-names)
	 nil ;; not able to return number of rows with aodbc
	 ))
      (clsql-error (e)
	(error e))
    (error ()
      (error 'clsql-sql-error
	     :database database
	     :expression query-expression
	     :errno nil
	     :error "Query result set failed"))))

(defmethod database-dump-result-set (result-set (database aodbc-database))
  #+aodbc-v2
  (dbi:close-query (aodbc-result-set-query result-set))
  t)

(defmethod database-store-next-row (result-set
				    (database aodbc-database)
				    list)
  #+aodbc-v2
  (let ((row (dbi:fetch-row (aodbc-result-set-query result-set) nil 'eof)))
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
  (concatenate 'string "_CLSQL_SEQ_" (sql-escape sequence-name)))

(defun %table-name-to-sequence-name (table-name)
  (and (>= (length table-name) 11)
       (string-equal (subseq table-name 0 11) "_CLSQL_SEQ_")
       (subseq table-name 11)))

(defmethod database-create-sequence (sequence-name
				     (database aodbc-database))
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
				   (database aodbc-database))
  (database-execute-command
   (concatenate 'string "DROP TABLE " (%sequence-name-to-table sequence-name)) 
   database))

(defmethod database-list-sequences ((database aodbc-database)
                                    &key (owner nil))
  (declare (ignore owner))
  (warn "database-list-sequences not implemented for AODBC.")
  nil)

(defmethod database-list-tables ((database aodbc-database)
				 &key (owner nil))
  (declare (ignore owner))
  #+aodbc-v2
  (multiple-value-bind (rows col-names)
      (dbi:list-all-database-tables :db (database-aodbc-conn database))
    (declare (ignore col-names))
      ;; TABLE_SCHEM is hard-coded in second column by ODBC Driver Manager
      ;; TABLE_NAME in third column, TABLE_TYPE in fourth column
      (loop for row in rows
	  when (and (not (string-equal "information_schema" (nth 1 row)))
		    (string-equal "TABLE" (nth 3 row)))
	  collect (nth 2 row))))

(defmethod database-list-views ((database aodbc-database)
				 &key (owner nil))
  (declare (ignore owner))
  #+aodbc-v2
  (multiple-value-bind (rows col-names)
      (dbi:list-all-database-tables :db (database-aodbc-conn database))
    (declare (ignore col-names))
    ;; TABLE_SCHEM is hard-coded in second column by ODBC Driver Manager
    ;; TABLE_NAME in third column, TABLE_TYPE in fourth column
    (loop for row in rows
	when (and (not (string-equal "information_schema" (nth 1 row)))
		  (string-equal "VIEW" (nth 3 row)))
	collect (nth 2 row))))

(defmethod database-list-attributes ((table string) (database aodbc-database)
                                     &key (owner nil))
  (declare (ignore owner))
  #+aodbc-v2
  (multiple-value-bind (rows col-names)
      (dbi:list-all-table-columns table :db (database-aodbc-conn database))
    (let ((pos (position "COLUMN_NAME" col-names :test #'string-equal)))
      (when pos
	(loop for row in rows
	    collect (nth pos row))))))

(defmethod database-attribute-type ((attribute string) (table string) (database aodbc-database)
                                     &key (owner nil))
  (declare (ignore owner))
  #+aodbc-v2
  (multiple-value-bind (rows col-names)
      (dbi:list-all-table-columns table :db (database-aodbc-conn database))
    (let ((pos (position "TYPE_NAME" col-names :test #'string-equal)))
      (when pos
	(loop for row in rows
	    collect (nth pos row))))))

(defmethod database-list-indexes ((database aodbc-database)
				 &key (owner nil))
  (warn "database-list-indexes not implemented for AODBC.")
  nil)

(defmethod database-set-sequence-position (sequence-name
                                           (position integer)
                                           (database aodbc-database))
  (database-execute-command
   (format nil "UPDATE ~A SET last_value=~A,is_called='t'" 
	   (%sequence-name-to-table sequence-name)
           position)
   database)
  position)

(defmethod database-sequence-next (sequence-name (database aodbc-database))
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
	     
(defmethod database-sequence-last (sequence-name (database aodbc-database))
  (without-interrupts
   (caar (database-query 
	  (concatenate 'string "SELECT last_value FROM " 
		       (%sequence-name-to-table sequence-name))
	  database
	  :auto))))

(defmethod database-create (connection-spec (type (eql :aodbc)))
  (warn "Not implemented."))

(defmethod database-destroy (connection-spec (type (eql :aodbc)))
  (warn "Not implemented."))

(defmethod database-probe (connection-spec (type (eql :aodbc)))
  (warn "Not implemented."))

;;; Backend capabilities

(defmethod database-underlying-type ((database aodbc-database))
  (database-aodbc-db-type database))

(defmethod db-backend-has-create/destroy-db? ((db-type (eql :aodbc)))
  nil)

(defmethod database-initialize-database-type ((database-type (eql :aodbc)))
  t)

(when (clsql-sys:database-type-library-loaded :aodbc)
  (clsql-sys:initialize-database-type :database-type :aodbc))
