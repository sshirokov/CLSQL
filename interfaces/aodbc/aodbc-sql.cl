;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          aodbc-sql.cl
;;;; Purpose:       Low-level interface for CLSQL AODBC backend
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  Feb 2002
;;;;
;;;; $Id: aodbc-sql.cl,v 1.7 2002/03/29 08:23:38 kevin Exp $
;;;;
;;;; This file, part of CLSQL, is Copyright (c) 2002 by Kevin M. Rosenberg
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(declaim (optimize (debug 3) (speed 3) (safety 1) (compilation-speed 0)))
(in-package :clsql-aodbc)


(defmethod database-type-library-loaded ((database-type (eql :aodbc)))
  "T if foreign library was able to be loaded successfully. "
  (when (find-package :dbi) ;; finds Allegro's DBI (AODBC) package
    t))

(defmethod clsql-sys:database-type-load-foreign ((databae-type (eql :aodbc)))
  t)

(when (find-package :dbi)
  (clsql-sys:database-type-load-foreign :aodbc)) ;; doesn't do anything

(defmethod database-initialize-database-type ((database-type (eql :aodbc)))
  t)

(defclass aodbc-database (database)
  ((aodbc-conn :accessor database-aodbc-conn :initarg :aodbc-conn)))

(defmethod database-name-from-spec (connection-spec
				    (database-type (eql :aodbc)))
  (check-connection-spec connection-spec database-type (dsn user password))
  (destructuring-bind (dsn user password) connection-spec
    (declare (ignore password))
    (concatenate 'string dsn "/" user)))

(defmethod database-connect (connection-spec (database-type (eql :aodbc)))
  (check-connection-spec connection-spec database-type (dsn user password))
  (destructuring-bind (dsn user password) connection-spec
    (handler-case
	(make-instance 'aodbc-database
	  :name (database-name-from-spec connection-spec :aodbc)
	  :aodbc-conn
	  (dbi:connect :user user
		       :password password
		       :data-source-name dsn))
      (error () 	;; Init or Connect failed
	(error 'clsql-connect-error
	       :database-type database-type
	       :connection-spec connection-spec
	       :errno nil
	       :error "Connection failed")))))

(defmethod database-disconnect ((database aodbc-database))
  (dbi:disconnect (database-aodbc-conn database))
  (setf (database-aodbc-conn database) nil)
  t)

(defmethod database-query (query-expression (database aodbc-database) types) 
  (handler-case
      (dbi:sql query-expression :db (database-aodbc-conn database)
	       :types types)
    (error ()
      (error 'clsql-sql-error
	     :database database
	     :expression query-expression
	     :errno nil
	     :error "Query failed"))))

(defmethod database-execute-command (sql-expression 
				     (database aodbc-database))
  (handler-case
      (dbi:sql sql-expression :db (database-aodbc-conn database))
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

(defmethod database-query-result-set (query-expression (database aodbc-database) 
				      &key full-set types)
  (handler-case 
      (multiple-value-bind (query column-names)
	  (dbi:sql query-expression 
		   :db (database-aodbc-conn database) 
		   :row-count nil
		   :column-names t
		   :query t
		   :types types
		   )
	(values
	 (make-aodbc-result-set :query query :full-set full-set 
				:types types)
	 (length column-names)
	 nil ;; not able to return number of rows with aodbc
	 ))
    (error ()
      (error 'clsql-sql-error
	     :database database
	     :expression query-expression
	     :errno nil
	     :error "Query result set failed"))))

(defmethod database-dump-result-set (result-set (database aodbc-database))
  (dbi:close-query (aodbc-result-set-query result-set))
  t)

(defmethod database-store-next-row (result-set
				    (database aodbc-database)
				    list)
  (let ((row (dbi:fetch-row (aodbc-result-set-query result-set) nil 'eof)))
    (if (eq row 'eof)
	nil
      (progn
	(loop for elem in row
	    for rest on list
	    do
	      (setf (car rest) elem))
	list))))

		       
