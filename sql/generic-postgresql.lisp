;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;;
;;;; $Id$
;;;;
;;;; Generic postgresql layer, used by db-postgresql and db-postgresql-socket
;;;;
;;;; This file is part of CLSQL.
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(in-package #:clsql-sys)

(defclass generic-postgresql-database (database)
  ()
  (:documentation "Encapsulate same behavior across postgresql and postgresql-socket backends."))



;; Object functions

(defmethod database-get-type-specifier (type args database
					(db-type (eql :postgresql)))
  (declare (ignore type args database))
  "VARCHAR")

(defmethod database-get-type-specifier ((type (eql 'string)) args database
					(db-type (eql :postgresql)))
  (declare (ignore database))
  (if args
      (format nil "CHAR(~A)" (car args))
      "VARCHAR"))

(defmethod database-get-type-specifier ((type (eql 'wall-time)) args database
					(db-type (eql :postgresql)))
  (declare (ignore args database))
  "TIMESTAMP WITHOUT TIME ZONE")


;;; Backend functions

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
	   database nil nil)))

(defmethod database-list-tables ((database generic-postgresql-database)
                                 &key (owner nil))
  (database-list-objects-of-type database "r" owner))
  
(defmethod database-list-views ((database generic-postgresql-database)
                                &key (owner nil))
  (database-list-objects-of-type database "v" owner))
  
(defmethod database-list-indexes ((database generic-postgresql-database)
                                  &key (owner nil))
  (database-list-objects-of-type database "i" owner))


(defmethod database-list-table-indexes (table (database generic-postgresql-database)
					&key (owner nil))
  (let ((indexrelids
	 (database-query
	  (format 
	   nil
	   "select indexrelid from pg_index where indrelid=(select relfilenode from pg_class where relname='~A'~A)"
	   (string-downcase table)
	   (owner-clause owner))
	  database :auto nil))
	(result nil))
    (dolist (indexrelid indexrelids (nreverse result))
      (push 
       (caar (database-query
	      (format nil "select relname from pg_class where relfilenode='~A'"
		      (car indexrelid))
	      database nil nil))
       result))))

(defmethod database-list-attributes ((table string)
				     (database generic-postgresql-database)
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
                   database nil nil))))
    (if result
        (remove-if #'(lambda (it) (member it '("cmin"
                                               "cmax"
                                               "xmax"
                                               "xmin"
                                               "oid"
                                               "ctid"
                                               ;; kmr -- added tableoid
                                               "tableoid") :test #'equal)) 
                   result))))

(defmethod database-attribute-type (attribute (table string)
				    (database generic-postgresql-database)
                                    &key (owner nil))
  (let ((row (car (database-query
		   (format nil "SELECT pg_type.typname,pg_attribute.attlen,pg_attribute.atttypmod,pg_attribute.attnotnull FROM pg_type,pg_class,pg_attribute WHERE pg_class.oid=pg_attribute.attrelid AND pg_class.relname='~A' AND pg_attribute.attname='~A' AND pg_attribute.atttypid=pg_type.oid~A"
			   (string-downcase table)
			   (string-downcase attribute)
			   (owner-clause owner))
		   database nil nil))))
    (when row
      (values
       (ensure-keyword (first row))
       (if (string= "-1" (second row))
	   (- (parse-integer (third row) :junk-allowed t) 4)
	 (parse-integer (second row)))
       nil
       (if (string-equal "f" (fourth row))
	   1
	 0)))))

(defmethod database-create-sequence (sequence-name
				     (database generic-postgresql-database))
  (database-execute-command
   (concatenate 'string "CREATE SEQUENCE " (sql-escape sequence-name))
   database))

(defmethod database-drop-sequence (sequence-name
				   (database generic-postgresql-database))
  (database-execute-command
   (concatenate 'string "DROP SEQUENCE " (sql-escape sequence-name)) database))

(defmethod database-list-sequences ((database generic-postgresql-database)
                                    &key (owner nil))
  (database-list-objects-of-type database "S" owner))

(defmethod database-set-sequence-position (name (position integer)
                                                (database generic-postgresql-database))
  (values
   (parse-integer
    (caar
     (database-query
      (format nil "SELECT SETVAL ('~A', ~A)" name position)
      database nil nil)))))

(defmethod database-sequence-next (sequence-name 
				   (database generic-postgresql-database))
  (values
   (parse-integer
    (caar
     (database-query
      (concatenate 'string "SELECT NEXTVAL ('" (sql-escape sequence-name) "')")
      database nil nil)))))

(defmethod database-sequence-last (sequence-name (database generic-postgresql-database))
  (values
   (parse-integer
    (caar
     (database-query
      (concatenate 'string "SELECT LAST_VALUE ('" sequence-name "')")
      database nil nil)))))

(defun postgresql-database-list (connection-spec type)
  (destructuring-bind (host name user password) connection-spec
    (declare (ignore name))
    (let ((database (database-connect (list host "template1" user password)
				      type)))
      (unwind-protect
	   (progn
	     (setf (slot-value database 'clsql-sys::state) :open)
	     (mapcar #'car (database-query "select datname from pg_database" 
					   database nil nil)))
	(progn
	  (database-disconnect database)
	  (setf (slot-value database 'clsql-sys::state) :closed))))))

(defmethod database-list (connection-spec (type (eql :postgresql)))
  (postgresql-database-list connection-spec type))

(defmethod database-list (connection-spec (type (eql :postgresql-socket)))
  (postgresql-database-list connection-spec type))


(defmethod database-describe-table ((database generic-postgresql-database) table)
  (database-query 
   (format nil "select a.attname, t.typname
                               from pg_class c, pg_attribute a, pg_type t
                               where c.relname = '~a'
                                   and a.attnum > 0
                                   and a.attrelid = c.oid
                                   and a.atttypid = t.oid"
           (sql-escape (string-downcase table)))
   database :auto nil))

;;; Prepared statements

(defvar *next-prepared-id-num* 0)
(defun next-prepared-id ()
  (let ((num (incf *next-prepared-id-num*)))
    (format nil "CLSQL_PS_~D" num)))

(defclass postgresql-stmt ()
  ((database :initarg :database :reader database)
   (id :initarg :id :reader id)
   (bindings :initarg :bindings :reader bindings)
   (field-names :initarg :field-names :accessor stmt-field-names)
   (result-types :initarg :result-types :reader result-types)))

(defun clsql-type->postgresql-type (type)
  (cond
    ((in type :int :integer) "INT4")
    ((in type :short) "INT2")
    ((in type :bigint) "INT8")
    ((in type :float :double :number) "NUMERIC")
    ((and (consp type) (in (car type) :char :varchar)) "VARCHAR")
    (t
     (error 'sql-user-error 
	    :message 
	    (format nil "Unknown clsql type ~A." type)))))

(defun prepared-sql-to-postgresql-sql (sql)
  ;; FIXME: Convert #\? to "$n". Don't convert within strings
  (declare (simple-string sql))
  (with-output-to-string (out)
    (do ((len (length sql))
	 (param 0)
	 (in-str nil)
	 (pos 0 (1+ pos)))
	((= len pos))
      (declare (fixnum len param pos))
      (let ((c (schar sql pos)))
	(declare (character c))
	(cond
	 ((or (char= c #\") (char= c #\'))
	  (setq in-str (not in-str))
	  (write-char c out))
	 ((and (char= c #\?) (not in-str))
	  (write-char #\$ out) 
	  (write-string (write-to-string (incf param)) out))
	 (t
	  (write-char c out)))))))

(defmethod database-prepare (sql-stmt types (database generic-postgresql-database) result-types field-names)
  (let ((id (next-prepared-id)))
    (database-execute-command
     (format nil "PREPARE ~A (~{~A~^,~}) AS ~A"
	     id
	     (mapcar #'clsql-type->postgresql-type types)
	     (prepared-sql-to-postgresql-sql sql-stmt))
     database)
    (make-instance 'postgresql-stmt
		   :id id
		   :database database
		   :result-types result-types
		   :field-names field-names
		   :bindings (make-list (length types)))))

(defmethod database-bind-parameter ((stmt postgresql-stmt) position value)
  (setf (nth (1- position) (bindings stmt)) value)) 

(defun binding-to-param (binding)
  (typecase binding
    (string
     (concatenate 'string "'" (sql-escape-quotes binding) "'"))
    (t
     binding)))

(defmethod database-run-prepared ((stmt postgresql-stmt))
  (with-slots (database id bindings field-names result-types) stmt
    (let ((query (format nil "EXECUTE ~A (~{~A~^,~})"
			 id (mapcar #'binding-to-param bindings))))
      (cond
       ((and field-names (not (consp field-names)))
	(multiple-value-bind (res names)
	    (database-query query database result-types field-names)
	  (setf field-names names)
	  (values res names)))
       (field-names
	(values (nth-value 0 (database-query query database result-types nil))
		field-names))
       (t
	(database-query query database result-types field-names))))))

;;; Capabilities

(defmethod db-type-has-fancy-math? ((db-type (eql :postgresql)))
  t)

(defmethod db-type-default-case ((db-type (eql :postgresql)))
  :lower)

(defmethod db-type-has-prepared-stmt? ((db-type (eql :postgresql)))
  t)

(defmethod db-type-has-prepared-stmt? ((db-type (eql :postgresql-socket)))
  t)

