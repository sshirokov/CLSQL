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


;; AODBC interface

(defclass aodbc-database (generic-odbc-database)
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
	  :dbi-package (find-package '#:dbi)
	  :aodbc-conn
	  (dbi:connect :user user
		       :password password
		       :data-source-name dsn))
      (clsql-error (e)
	(error e))
      (error () 	;; Init or Connect failed
	(error 'sql-connection-error
	       :database-type database-type
	       :connection-spec connection-spec
	       :message "Connection failed")))))



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
	     
(defmethod database-sequence-last (sequence-name (database aodbc-database))
  (without-interrupts
   (caar (database-query 
	  (concatenate 'string "SELECT last_value FROM " 
		       (%sequence-name-to-table sequence-name))
	  database :auto nil))))

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
