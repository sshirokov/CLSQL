;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          postgresql-socket-usql.sql
;;;; Purpose:       PostgreSQL interface for USQL routines
;;;; Programmers:   Kevin M. Rosenberg and onShore Development Inc
;;;; Date Started:  Mar 2002
;;;;
;;;; $Id: postgresql-socket-usql.lisp 7061 2003-09-07 06:34:45Z kevin $
;;;;
;;;; This file, part of CLSQL, is Copyright (c) 2002 by Kevin M. Rosenberg
;;;; and by onShore Development Inc.
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(in-package #:clsql-postgresql-socket)


(defmethod database-list-objects-of-type ((database postgresql-socket-database)
                                          type owner)
  (let ((owner-clause
         (cond ((stringp owner)
                (format nil " AND (relowner=(SELECT usesysid FROM pg_user WHERE (usename='~A')))" owner))
               ((null owner)
                (format nil " AND (NOT (relowner=1))"))
               (t ""))))
    (mapcar #'car
            (database-query
             (format nil
                     "SELECT relname FROM pg_class WHERE (relkind = '~A')~A"
                     type
                     owner-clause)
             database nil))))
    
(defmethod database-list-tables ((database postgresql-socket-database)
                                 &key (owner nil))
  (database-list-objects-of-type database "r" owner))
  
(defmethod database-list-views ((database postgresql-socket-database)
                                &key (owner nil))
  (database-list-objects-of-type database "v" owner))
  
(defmethod database-list-indexes ((database postgresql-socket-database)
                                  &key (owner nil))
  (database-list-objects-of-type database "i" owner))
  
(defmethod database-list-attributes ((table string)
				     (database postgresql-socket-database)
                                     &key (owner nil))
  (let* ((owner-clause
          (cond ((stringp owner)
                 (format nil " AND (relowner=(SELECT usesysid FROM pg_user WHERE usename='~A'))" owner))
                ((null owner) " AND (not (relowner=1))")
                (t "")))
         (result
	  (mapcar #'car
		  (database-query
		   (format nil "SELECT attname FROM pg_class,pg_attribute WHERE pg_class.oid=attrelid AND relname='~A'~A"
                           (string-downcase table)
                           owner-clause)
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
				    (database postgresql-socket-database)
                                    &key (owner nil))
  (let* ((owner-clause
          (cond ((stringp owner)
                 (format nil " AND (relowner=(SELECT usesysid FROM pg_user WHERE usename='~A'))" owner))
                ((null owner) " AND (not (relowner=1))")
                (t "")))
         (result
	  (mapcar #'car
		  (database-query
		   (format nil "SELECT pg_type.typname FROM pg_type,pg_class,pg_attribute WHERE pg_class.oid=pg_attribute.attrelid AND pg_class.relname='~A' AND pg_attribute.attname='~A' AND pg_attribute.atttypid=pg_type.oid~A"
			   (string-downcase table)
                           (string-downcase attribute)
                           owner-clause)
		   database nil))))
    (when result
      (intern (string-upcase (car result)) :keyword))))

(defmethod database-create-sequence (sequence-name
				     (database postgresql-socket-database))
  (database-execute-command
   (concatenate 'string "CREATE SEQUENCE " (sql-escape sequence-name))
   database))

(defmethod database-drop-sequence (sequence-name
				   (database postgresql-socket-database))
  (database-execute-command
   (concatenate 'string "DROP SEQUENCE " (sql-escape sequence-name)) database))

(defmethod database-list-sequences ((database postgresql-socket-database)
                                    &key (owner nil))
  (database-list-objects-of-type database "S" owner))

(defmethod database-set-sequence-position (name (position integer)
                                          (database postgresql-socket-database))
  (values
   (parse-integer
    (caar
     (database-query
      (format nil "SELECT SETVAL ('~A', ~A)" name position)
      database nil)))))

(defmethod database-sequence-next (sequence-name 
				   (database postgresql-socket-database))
  (values
   (parse-integer
    (caar
     (database-query
      (concatenate 'string "SELECT NEXTVAL ('" (sql-escape sequence-name) "')")
      database nil)))))

(defmethod database-sequence-last (sequence-name (database postgresql-socket-database))
  (values
   (parse-integer
    (caar
     (database-query
      (concatenate 'string "SELECT LAST_VALUE ('" sequence-name "')")
      database nil)))))
  

;; Functions depending upon high-level USQL classes/functions

#|
(defmethod database-output-sql ((expr clsql-sys::sql-typecast-exp) 
				(database postgresql-socket-database))
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
					(database postgresql-socket-database))
  (when val   ;; typecast it so it uses the indexes
    (make-instance 'clsql-sys::sql-typecast-exp
                   :modifier 'int8
                   :components val)))
|#
