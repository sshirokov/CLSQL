;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:     postgresql-socket-sql.sql
;;;; Purpose:  High-level PostgreSQL interface using socket
;;;; Authors:  Kevin M. Rosenberg based on original code by Pierre R. Mai 
;;;; Created:  Feb 2002
;;;;
;;;; $Id$
;;;;
;;;; This file, part of CLSQL, is Copyright (c) 2002-2004 by Kevin M. Rosenberg
;;;; and Copyright (c) 1999-2001 by Pierre R. Mai
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(in-package #:cl-user)

(defpackage :clsql-postgresql-socket
    (:use #:common-lisp #:clsql-base-sys #:postgresql-socket)
    (:export #:postgresql-socket-database)
    (:documentation "This is the CLSQL socket interface to PostgreSQL."))

(in-package #:clsql-postgresql-socket)

;; interface foreign library loading routines


(clsql-base-sys:database-type-load-foreign :postgresql-socket)


;; Field type conversion

(defun make-type-list-for-auto (cursor)
  (let* ((fields (postgresql-cursor-fields cursor))
	 (num-fields (length fields))
	 (new-types '()))
    (dotimes (i num-fields)
      (declare (fixnum i))
      (push (canonical-field-type fields i) new-types))
    (nreverse new-types)))

(defun canonical-field-type (fields index)
  "Extracts canonical field type from fields list"
  (let ((oid (cadr (nth index fields))))
    (case oid
      ((#.pgsql-ftype#bytea
	#.pgsql-ftype#int2
	#.pgsql-ftype#int4)
       :int32)
      (#.pgsql-ftype#int8
       :int64)
      ((#.pgsql-ftype#float4
	#.pgsql-ftype#float8)
       :double)
      (otherwise
       t))))

(defun canonicalize-types (types cursor)
  (if (null types)
      nil
      (let ((auto-list (make-type-list-for-auto cursor)))
	(cond
	  ((listp types)
	   (canonicalize-type-list types auto-list))
	  ((eq types :auto)
	   auto-list)
	  (t
	   nil)))))

(defun canonicalize-type-list (types auto-list)
  "Ensure a field type list meets expectations.
Duplicated from clsql-uffi package so that this interface
doesn't depend on UFFI."
  (let ((length-types (length types))
	(new-types '()))
    (loop for i from 0 below (length auto-list)
	  do
	  (if (>= i length-types)
	      (push t new-types) ;; types is shorted than num-fields
	      (push
	       (case (nth i types)
		 (:int
		  (case (nth i auto-list)
		    (:int32
		     :int32)
		    (:int64
		     :int64)
		    (t
		     t)))
		 (:double
		  (case (nth i auto-list)
		    (:double
		     :double)
		    (t
		     t)))
		 (t
		  t))
	       new-types)))
    (nreverse new-types)))


(defun convert-to-clsql-warning (database condition)
  (warn 'clsql-database-warning :database database
	:message (postgresql-condition-message condition)))

(defun convert-to-clsql-error (database expression condition)
  (error 'clsql-sql-error :database database
	 :expression expression
	 :errno (type-of condition)
	 :error (postgresql-condition-message condition)))

(defmacro with-postgresql-handlers
    ((database &optional expression)
     &body body)
  (let ((database-var (gensym))
	(expression-var (gensym)))
    `(let ((,database-var ,database)
	   (,expression-var ,expression))
       (handler-bind ((postgresql-warning
		       (lambda (c)
			 (convert-to-clsql-warning ,database-var c)))
		      (postgresql-error
		       (lambda (c)
			 (convert-to-clsql-error
			  ,database-var ,expression-var c))))
	 ;; KMR - removed double @@
	 ,@body))))

(defmethod database-initialize-database-type ((database-type
					       (eql :postgresql-socket)))
  t)

(defclass postgresql-socket-database (database)
  ((connection :accessor database-connection :initarg :connection
	       :type postgresql-connection)))

(defmethod database-type ((database postgresql-socket-database))
  :postgresql-socket)

(defmethod database-name-from-spec (connection-spec
				    (database-type (eql :postgresql-socket)))
  (check-connection-spec connection-spec database-type
			 (host db user password &optional port options tty))
  (destructuring-bind (host db user password &optional port options tty)
      connection-spec
    (declare (ignore password options tty))
    (concatenate 'string 
      (etypecase host
	(null
	 "localhost")
	(pathname (namestring host))
	(string host))
      (when port 
	(concatenate 'string
		     ":"
		     (etypecase port
		       (integer (write-to-string port))
		       (string port))))
      "/" db "/" user)))

(defmethod database-connect (connection-spec 
			     (database-type (eql :postgresql-socket)))
  (check-connection-spec connection-spec database-type
			 (host db user password &optional port options tty))
  (destructuring-bind (host db user password &optional
			    (port +postgresql-server-default-port+)
			    (options "") (tty ""))
      connection-spec
    (handler-case
	(handler-bind ((postgresql-warning
			(lambda (c)
			  (warn 'clsql-simple-warning
				:format-control "~A"
				:format-arguments
				(list (princ-to-string c))))))
	  (open-postgresql-connection :host host :port port
				      :options options :tty tty
				      :database db :user user
				      :password password))
      (postgresql-error (c)
	;; Connect failed
	(error 'clsql-connect-error
	       :database-type database-type
	       :connection-spec connection-spec
	       :errno (type-of c)
	       :error (postgresql-condition-message c)))
      (:no-error (connection)
		 ;; Success, make instance
		 (make-instance 'postgresql-socket-database
				:name (database-name-from-spec connection-spec
							       database-type)
				:database-type :postgresql-socket
				:connection-spec connection-spec
				:connection connection)))))

(defmethod database-disconnect ((database postgresql-socket-database))
  (close-postgresql-connection (database-connection database))
  t)

(defmethod database-query (expression (database postgresql-socket-database) result-types field-names)
  (let ((connection (database-connection database)))
    (with-postgresql-handlers (database expression)
      (start-query-execution connection expression)
      (multiple-value-bind (status cursor)
	  (wait-for-query-results connection)
	(unless (eq status :cursor)
	  (close-postgresql-connection connection)
	  (error 'clsql-sql-error
		 :database database
		 :expression expression
		 :errno 'missing-result
		 :error "Didn't receive result cursor for query."))
	(setq result-types (canonicalize-types result-types cursor))
        (values
         (loop for row = (read-cursor-row cursor result-types)
               while row
               collect row
               finally
               (unless (null (wait-for-query-results connection))
                 (close-postgresql-connection connection)
                 (error 'clsql-sql-error
                        :database database
                        :expression expression
                        :errno 'multiple-results
                        :error "Received multiple results for query.")))
         (when field-names
           (result-field-names cursor)))))))

(defun result-field-names (cursor)
  "Return list of result field names."
  ;; FIXME -- implement
  nil)

(defmethod database-execute-command
    (expression (database postgresql-socket-database))
  (let ((connection (database-connection database)))
    (with-postgresql-handlers (database expression)
      (start-query-execution connection expression)
      (multiple-value-bind (status result)
	  (wait-for-query-results connection)
	(when (eq status :cursor)
	  (loop
	      (multiple-value-bind (row stuff)
		  (skip-cursor-row result)
		(unless row
		  (setq status :completed result stuff)
		  (return)))))
	(cond
	  ((null status)
	   t)
	  ((eq status :completed)
	   (unless (null (wait-for-query-results connection))
	     (close-postgresql-connection connection)
	     (error 'clsql-sql-error
		    :database database
		    :expression expression
		    :errno 'multiple-results
		    :error "Received multiple results for command."))
	   result)
	  (t
	   (close-postgresql-connection connection)
	   (error 'clsql-sql-error
		  :database database
		  :expression expression
		  :errno 'missing-result
		  :error "Didn't receive completion for command.")))))))

(defstruct postgresql-socket-result-set
  (done nil)
  (cursor nil)
  (types nil))

(defmethod database-query-result-set ((expression string)
				      (database postgresql-socket-database) 
				      &key full-set result-types)
  (declare (ignore full-set))
  (let ((connection (database-connection database)))
    (with-postgresql-handlers (database expression)
      (start-query-execution connection expression)
      (multiple-value-bind (status cursor)
	  (wait-for-query-results connection)
	(unless (eq status :cursor)
	  (close-postgresql-connection connection)
	  (error 'clsql-sql-error
		 :database database
		 :expression expression
		 :errno 'missing-result
		 :error "Didn't receive result cursor for query."))
	(values (make-postgresql-socket-result-set
		 :done nil 
		 :cursor cursor
		 :types (canonicalize-types result-types cursor))
		(length (postgresql-cursor-fields cursor)))))))

(defmethod database-dump-result-set (result-set
				     (database postgresql-socket-database))
  (if (postgresql-socket-result-set-done result-set)
      t
      (with-postgresql-handlers (database)
	(loop while (skip-cursor-row 
		     (postgresql-socket-result-set-cursor result-set))
	  finally (setf (postgresql-socket-result-set-done result-set) t)))))

(defmethod database-store-next-row (result-set
				    (database postgresql-socket-database)
				    list)
  (let ((cursor (postgresql-socket-result-set-cursor result-set)))
    (with-postgresql-handlers (database)
      (if (copy-cursor-row cursor 
			   list
			   (postgresql-socket-result-set-types
			    result-set))
	  t
	  (prog1 nil
	    (setf (postgresql-socket-result-set-done result-set) t)
	    (wait-for-query-results (database-connection database)))))))

;;; Object listing

(defun owner-clause (owner)
  (cond 
   ((stringp owner)
    (format
     nil
     " AND (relowner=(SELECT usesysid FROM pg_user WHERE (usename='~A')))" 
     owner))
   ((null owner)
    (format nil " AND (NOT (relowner=1))"))
   (t "")))

(defun database-list-objects-of-type (database type owner)
  (mapcar #'car
	  (database-query
	   (format nil
		   "SELECT relname FROM pg_class WHERE (relkind = '~A')~A"
		   type
		   (owner-clause owner))
	   database nil)))

(defmethod database-list-tables ((database postgresql-socket-database)
                                 &key (owner nil))
  (database-list-objects-of-type database "r" owner))
  
(defmethod database-list-views ((database postgresql-socket-database)
                                &key (owner nil))
  (database-list-objects-of-type database "v" owner))
  
(defmethod database-list-indexes ((database postgresql-socket-database)
                                  &key (owner nil))
  (database-list-objects-of-type database "i" owner))

(defmethod database-list-table-indexes (table
					(database postgresql-socket-database)
					&key (owner nil))
  (let ((indexrelids
	 (database-query
	  (format 
	   nil
	   "select indexrelid from pg_index where indrelid=(select relfilenode from pg_class where relname='~A'~A)"
	   (string-downcase table)
	   (owner-clause owner))
	  database :auto))
	(result nil))
    (dolist (indexrelid indexrelids (nreverse result))
      (push 
       (caar (database-query
	      (format nil "select relname from pg_class where relfilenode='~A'"
		      (car indexrelid))
	      database
	      nil))
       result))))

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
  

(defmethod database-create (connection-spec (type (eql :postgresql-socket)))
  (destructuring-bind (host name user password) connection-spec
    (let ((database (database-connect (list host "template1" user password)
				      type)))
      (unwind-protect
	   (execute-command (format nil "create database ~A" name))
	(database-disconnect database)))))

(defmethod database-destroy (connection-spec (type (eql :postgresql-socket)))
  (destructuring-bind (host name user password) connection-spec
    (let ((database (database-connect (list host "template1" user password)
				      type)))
      (unwind-protect
	  (execute-command (format nil "drop database ~A" name))
	(database-disconnect database)))))


(defmethod database-probe (connection-spec (type (eql :postgresql-socket)))
  (when (find (second connection-spec) (database-list connection-spec type)
	      :key #'car :test #'string-equal)
    t))

(defmethod database-list (connection-spec (type (eql :postgresql-socket)))
  (destructuring-bind (host name user password) connection-spec
    (declare (ignore name))
    (let ((database (database-connect (list host "template1" user password)
				      type)))
      (unwind-protect
	   (progn
	     (setf (slot-value database 'clsql-base-sys::state) :open)
	     (mapcar #'car (database-query "select datname from pg_database" 
					   database :auto)))
	(progn
	  (database-disconnect database)
	  (setf (slot-value database 'clsql-base-sys::state) :closed))))))

(defmethod database-describe-table ((database postgresql-socket-database) 
				    table)
  (database-query
   (format nil "select a.attname, t.typname
                               from pg_class c, pg_attribute a, pg_type t
                               where c.relname = '~a'
                                   and a.attnum > 0
                                   and a.attrelid = c.oid
                                   and a.atttypid = t.oid"
           (sql-escape (string-downcase table)))
   database :auto))


;; Database capabilities

(defmethod db-backend-has-create/destroy-db? ((db-type (eql :postgresql-socket)))
  nil)

(defmethod db-type-has-fancy-math? ((db-type (eql :postgresql-socket)))
  t)

(defmethod db-type-default-case ((db-type (eql :postgresql)))
  :lower)

(when (clsql-base-sys:database-type-library-loaded :postgresql-socket)
  (clsql-base-sys:initialize-database-type :database-type :postgresql-socket))
