;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          mysql-usql.cl
;;;; Purpose:       MySQL interface functions to support UncommonSQL
;;;; Programmers:   Kevin M. Rosenberg and onShore Development Inc
;;;; Date Started:  Mar 2002
;;;;
;;;; $Id: mysql-usql.cl,v 1.1 2002/09/18 07:43:40 kevin Exp $
;;;;
;;;; This file, part of CLSQL, is Copyright (c) 2002 by Kevin M. Rosenberg
;;;; and by onShore Development Inc.
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(declaim (optimize (debug 3) (speed 3) (safety 1) (compilation-speed 0)))
(in-package :clsql-mysql)

;; Table and attribute introspection

(defmethod database-list-tables ((database mysql-database)
				 &key (system-tables nil))
  (declare (ignore system-tables))
  (mapcar #'car (database-query "show tables" database :auto)))
    

(defmethod database-list-attributes ((table string) (database mysql-database))
  (mapcar #'car
	  (database-query
	   (format nil "SHOW COLUMNS FROM ~A" table)
	   database nil)))

(defmethod database-attribute-type (attribute (table string)
				    (database mysql-database))
  (let ((result
	  (mapcar #'cadr
		  (database-query
		   (format nil
			   "SHOW COLUMNS FROM ~A LIKE '~A'" table attribute)
		   database nil))))
    (let* ((str (car result))
	   (end-str (position #\( str))
	   (substr (subseq str 0 end-str)))
      (if substr
      (intern (string-upcase substr) :keyword) nil))))

;;; Sequence functions

(defun %sequence-name-to-table (sequence-name)
  (concatenate 'string "_usql_seq_" (sql-escape sequence-name)))

(defmethod database-create-sequence (sequence-name
				     (database mysql-database))
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
				   (database mysql-database))
  (database-execute-command
   (concatenate 'string "DROP TABLE " (%sequence-name-to-table sequence-name)) 
   database))

(defmethod database-sequence-next (sequence-name (database mysql-database))
  (database-execute-command 
   (concatenate 'string "UPDATE " (%sequence-name-to-table sequence-name)
		" SET id=LAST_INSERT_ID(id+1)")
   database)
  (mysql:mysql-insert-id (clsql-mysql::database-mysql-ptr database)))

;; Misc USQL functions

#|
#+ignore
(defmethod database-output-sql ((expr clsql-sys::sql-typecast-exp) 
				(database mysql-database))
  (with-slots (clsql-sys::modifier clsql-sys::components)
    expr
    (if clsql-sys::modifier
        (progn
          (clsql-sys::output-sql clsql-sys::components database)
          (write-char #\: sql-sys::*sql-stream*)
          (write-char #\: sql-sys::*sql-stream*)
          (write-string (symbol-name clsql-sys::modifier) 
			clsql-sys::*sql-stream*)))))

#+ignore
(defmethod database-output-sql-as-type ((type (eql 'integer)) val
					(database mysql-database))
  ;; typecast it so it uses the indexes
  (when val
    (make-instance 'clsql-sys::sql-typecast-exp
                   :modifier 'int8
                   :components val)))
|#
