;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          mysql-usql.cl
;;;; Purpose:       MySQL interface functions to support UncommonSQL
;;;; Programmers:   Kevin M. Rosenberg and onShore Development Inc
;;;; Date Started:  Mar 2002
;;;;
;;;; $Id$
;;;;
;;;; This file, part of CLSQL, is Copyright (c) 2002 by Kevin M. Rosenberg
;;;; and by onShore Development Inc.
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(in-package #:clsql-mysql)

;; Table and attribute introspection

(defmethod database-list-tables ((database mysql-database) &key (owner nil))
  (declare (ignore owner))
  (remove-if #'(lambda (s)
                 (and (>= (length s) 10)
                      (string= (subseq s 0 10) "_usql_seq_")))
             (mapcar #'car (database-query "SHOW TABLES" database nil))))
    
;; MySQL 4.1 does not support views 
(defmethod database-list-views ((database mysql-database)
                                &key (owner nil))
  (declare (ignore owner database))
  nil)

(defmethod database-list-indexes ((database mysql-database)
                                  &key (owner nil))
  (let ((result '()))
    (dolist (table (database-list-tables database :owner owner) result)
      (mapc #'(lambda (index) (push (nth 2 index) result))
            (database-query 
             (format nil "SHOW INDEX FROM ~A" (string-upcase table))
             database nil)))))
  
(defmethod database-list-attributes ((table string) (database mysql-database)
                                     &key (owner nil))
  (declare (ignore owner))
  (mapcar #'car
	  (database-query
	   (format nil "SHOW COLUMNS FROM ~A" table)
	   database nil)))

(defmethod database-attribute-type (attribute (table string)
				    (database mysql-database)
                                    &key (owner nil))
  (declare (ignore owner))
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

(defun %table-name-to-sequence-name (table-name)
  (and (>= (length table-name) 10)
       (string= (subseq table-name 0 10) "_usql_seq_")
       (subseq table-name 10)))

(defmethod database-create-sequence (sequence-name
				     (database mysql-database))
  (let ((table-name (%sequence-name-to-table sequence-name)))
    (database-execute-command
     (concatenate 'string "CREATE TABLE " table-name
		  " (id int NOT NULL PRIMARY KEY AUTO_INCREMENT)")
     database)
    (database-execute-command 
     (concatenate 'string "INSERT INTO " table-name
		  " VALUES (-1)")
     database)))

(defmethod database-drop-sequence (sequence-name
				   (database mysql-database))
  (database-execute-command
   (concatenate 'string "DROP TABLE " (%sequence-name-to-table sequence-name)) 
   database))

(defmethod database-list-sequences ((database mysql-database)
                                    &key (owner nil))
  (declare (ignore owner))
  (mapcar #'(lambda (s) (%table-name-to-sequence-name (car s)))
          (database-query "SHOW TABLES LIKE '%usql_seq%'" 
                          database nil)))

(defmethod database-set-sequence-position (sequence-name
                                           (position integer)
                                           (database mysql-database))
  (database-execute-command
   (format nil "UPDATE ~A SET id=~A" (%sequence-name-to-table sequence-name)
           position)
   database)
  (mysql:mysql-insert-id (clsql-mysql::database-mysql-ptr database)))

(defmethod database-sequence-next (sequence-name (database mysql-database))
  (database-execute-command 
   (concatenate 'string "UPDATE " (%sequence-name-to-table sequence-name)
		" SET id=LAST_INSERT_ID(id+1)")
   database)
  (mysql:mysql-insert-id (clsql-mysql::database-mysql-ptr database)))

(defmethod database-sequence-last (sequence-name (database mysql-database))
  (declare (ignore sequence-name database)))

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
