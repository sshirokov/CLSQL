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
    (:use #:common-lisp #:clsql-sys)
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
      #+ignore
      (sql-condition (e)
	(error e))
      (error () 	;; Init or Connect failed
	(error 'sql-connection-error
	       :database-type database-type
	       :connection-spec connection-spec
	       :message "Connection failed")))))

(defmethod database-underlying-type ((database odbc-database))
  (database-odbc-db-type database))

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
			   result-types field-names) 
  (handler-case
      (odbc-dbi:sql query-expression :db (database-odbc-conn database)
		    :result-types result-types
                    :column-names field-names)
    #+ignore
    (sql-error (e)
      (error e))
    (error ()
      (error 'sql-database-data-error
	     :database database
	     :expression query-expression
	     :message "Query failed"))))

(defmethod database-execute-command (sql-expression 
				     (database odbc-database))
  (handler-case
      (odbc-dbi:sql sql-expression :db (database-odbc-conn database))
    #+ignore
    (sql-error (e)
      (error e))
    (error ()
      (error 'sql-database-data-error
	     :database database
	     :expression sql-expression
	     :message "Execute command failed"))))

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
      (error 'sql-database-data-error
	     :database database
	     :expression query-expression
	     :message "Query result set failed"))))

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
  (concatenate 'string "_CLSQL_SEQ_" (sql-escape sequence-name)))

(defun %table-name-to-sequence-name (table-name)
  (and (>= (length table-name) 11)
       (string-equal (subseq table-name 0 11) "_CLSQL_SEQ_")
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
  ;; FIXME: Underlying database backend stuff should come from that backend
  
  (case (database-odbc-db-type database)
    (:mysql
     (mapcan #'(lambda (s)
		 (let ((sn (%table-name-to-sequence-name (car s))))
		   (and sn (list sn))))
	     (database-query "SHOW TABLES" database nil nil)))
    ((:postgresql :postgresql-socket)
     (mapcar #'(lambda (s) (%table-name-to-sequence-name (car s)))
	    (database-query "SELECT RELNAME FROM pg_class WHERE RELNAME LIKE '%clsql_seq%'" 
			    database nil nil)))))

(defmethod database-list-tables ((database odbc-database)
				 &key (owner nil))
  (declare (ignore owner))
    (multiple-value-bind (rows col-names)
	(odbc-dbi:list-all-database-tables :db (database-odbc-conn database))
      (declare (ignore col-names))
      ;; TABLE_SCHEM is hard-coded in second column by ODBC Driver Manager
      ;; TABLE_NAME in third column, TABLE_TYPE in fourth column
      (loop for row in rows
	  when (and (not (string-equal "information_schema" (nth 1 row)))
		    (string-equal "TABLE" (nth 3 row)))
	  collect (nth 2 row))))

(defmethod database-list-views ((database odbc-database)
				 &key (owner nil))
  (declare (ignore owner))
    (multiple-value-bind (rows col-names)
	(odbc-dbi:list-all-database-tables :db (database-odbc-conn database))
      (declare (ignore col-names))
      ;; TABLE_SCHEM is hard-coded in second column by ODBC Driver Manager
      ;; TABLE_NAME in third column, TABLE_TYPE in fourth column
      (loop for row in rows
	  when (and (not (string-equal "information_schema" (nth 1 row)))
		    (string-equal "VIEW" (nth 3 row)))
	  collect (nth 2 row))))

(defmethod database-list-attributes ((table string) (database odbc-database)
                                     &key (owner nil))
  (declare (ignore owner))
  (multiple-value-bind (rows col-names)
      (odbc-dbi:list-all-table-columns table :db (database-odbc-conn database))
    (declare (ignore col-names))
    ;; COLUMN_NAME is hard-coded by odbc spec as fourth position
    (loop for row in rows
	collect (fourth row))))

(defmethod database-attribute-type ((attribute string) (table string) (database odbc-database)
                                     &key (owner nil))
  (declare (ignore owner))
  (multiple-value-bind (rows col-names)
      (odbc-dbi:list-all-table-columns table :db (database-odbc-conn database))
    (declare (ignore col-names))
    ;; COLUMN_NAME is hard-coded by odbc spec as fourth position
    ;; TYPE_NAME is the sixth column
    ;; PRECISION/COLUMN_SIZE is the seventh column
    ;; SCALE/DECIMAL_DIGITS is the ninth column
    ;; NULLABLE is the eleventh column
    (loop for row in rows
	when (string-equal attribute (fourth row))
	do
	(let ((size (seventh row))
	      (precision (ninth row))
	      (scale (nth 10 row)))
	  (return (values (ensure-keyword (sixth row))
			  (when size (parse-integer size))
			  (when precision (parse-integer precision))
			  (when scale (parse-integer scale))))))))

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
		 database :auto nil))))
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
	  database :auto nil))))

(defmethod database-create (connection-spec (type (eql :odbc)))
  (declare (ignore connection-spec))
  (warn "Not implemented."))

(defmethod database-destroy (connection-spec (type (eql :odbc)))
  (declare (ignore connection-spec))
  (warn "Not implemented."))

(defmethod database-probe (connection-spec (type (eql :odbc)))
  (when (find (car connection-spec) (database-list connection-spec type)
	      :test #'string-equal)
    t))

(defmethod database-list (connection-spec (type (eql :odbc)))
  (declare (ignore connection-spec))
  (odbc-dbi:list-all-data-sources))

(defmethod database-list-indexes ((database odbc-database)
                                  &key (owner nil))
  (let ((result '()))
    (dolist (table (database-list-tables database :owner owner) result)
      (setq result
	(append (database-list-table-indexes table database :owner owner)
		result)))))

(defmethod database-list-table-indexes (table (database odbc-database)
					&key (owner nil))
  (declare (ignore owner))
  (odbc-list-table-indexes table database))

(defun odbc-list-table-indexes (table database)
  (multiple-value-bind (rows col-names)
      (odbc-dbi:list-table-indexes 
       table
       :db (database-odbc-conn database))
    (declare (ignore col-names))
    ;; INDEX_NAME is hard-coded in sixth position by ODBC driver
    ;; FIXME: ??? is hard-coded in the fourth position
    (do ((results nil)
	 (loop-rows rows (cdr loop-rows)))
	((null loop-rows) (nreverse results))
      (let* ((row (car loop-rows))
	     (col (nth 5 row)))
	(unless (find col results :test #'string-equal)
	  (push col results))))))

;;; Database capabilities

(defmethod db-backend-has-create/destroy-db? ((db-type (eql :odbc)))
  nil)


(defmethod database-initialize-database-type ((database-type (eql :odbc)))
  ;; nothing to do
  t)

(when (clsql-sys:database-type-library-loaded :odbc)
  (clsql-sys:initialize-database-type :database-type :odbc))
