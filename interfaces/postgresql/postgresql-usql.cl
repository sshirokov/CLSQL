;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          postgresql-usql.sql
;;;; Purpose:       PostgreSQL interface for USQL routines
;;;; Programmers:   Kevin M. Rosenberg and onShore Development Inc
;;;; Date Started:  Mar 2002
;;;;
;;;; $Id: postgresql-usql.cl,v 1.4 2002/05/15 17:10:28 kevin Exp $
;;;;
;;;; This file, part of CLSQL, is Copyright (c) 2002 by Kevin M. Rosenberg
;;;; and by onShore Development Inc.
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(declaim (optimize (debug 3) (speed 3) (safety 1) (compilation-speed 0)))
(in-package :clsql-postgresql)

(defmethod database-list-tables ((database postgresql-database)
                                 &key (system-tables nil))
  (let ((res (mapcar #'car (database-query
			    "SELECT tablename FROM pg_tables"
			    database nil))))
    (if (not system-tables)
        (remove-if #'(lambda (table)
                       (equal (subseq table 0 3)
                              "pg_")) res)
      res)))



(defmethod database-list-attributes ((table string)
				     (database postgresql-database))
  (let* ((result
	  (mapcar #'car
		  (database-query
		   (format nil
			   "SELECT attname FROM pg_class,pg_attribute WHERE pg_class.oid=attrelid AND relname='~A'" table)
		   database nil))))
    (if result
	(reverse
         (remove-if #'(lambda (it) (member it '("cmin"
                                                "cmax"
                                                "xmax"
                                                "xmin"
						"oid"
                                                "ctid"
						;; kmr -- added tableoid
						"tableoid") :test #'equal)) 
		    result)))))

(defmethod database-attribute-type (attribute (table string)
				    (database postgresql-database))
  (let ((result
	  (mapcar #'car
		  (database-query
		   (format nil
			   "SELECT pg_type.typname FROM pg_type,pg_class,pg_attribute WHERE pg_class.oid=pg_attribute.attrelid AND pg_class.relname='~A' AND pg_attribute.attname='~A' AND pg_attribute.atttypid=pg_type.oid"
			   table attribute)
		   database nil))))
    (if result
	(intern (string-upcase (car result)) :keyword) nil)))


(defmethod database-create-sequence (sequence-name
				     (database postgresql-database))
  (database-execute-command
   (concatenate 'string "CREATE SEQUENCE " (sql-escape sequence-name)) database))

(defmethod database-drop-sequence (sequence-name
				   (database postgresql-database))
  (database-execute-command
   (concatenate 'string "DROP SEQUENCE " (sql-escape sequence-name)) database))

(defmethod database-sequence-next (sequence-name 
				   (database postgresql-database))
  (parse-integer
   (caar
    (database-query
     (concatenate 'string "SELECT NEXTVAL ('" (sql-escape sequence-name) "')")
     database nil))))

;; Functions depending upon high-level USQL classes/functions

#|
(defmethod database-output-sql ((expr clsql-sys::sql-typecast-exp) 
				(database postgresql-database))
  (with-slots (clsql-sys::modifier clsql-sys::components)
    expr
    (if clsql-sys::modifier
        (progn
          (clsql-sys::output-sql clsql-sys::components database)
          (write-char #\: clsql-sys::*sql-stream*)
          (write-char #\: clsql-sys::*sql-stream*)
          (write-string (symbol-name clsql-sys::modifier) 
			clsql-sys::*sql-stream*)))))

(defmethod database-output-sql-as-type ((type (eql 'integer)) val
					(database postgresql-database))
  ;; typecast it so it uses the indexes
  (when val
    (make-instance 'clsql-sys::sql-typecast-exp
                   :modifier 'int8
                   :components val)))
|#
