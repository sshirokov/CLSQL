;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          postgresql-sql.lisp
;;;; Purpose:       High-level PostgreSQL interface using UFFI
;;;; Date Started:  Feb 2002
;;;;
;;;; $Id$
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(in-package #:cl-user)

(defpackage #:clsql-postgresql
    (:use #:common-lisp #:clsql-base-sys #:postgresql #:clsql-uffi)
    (:export #:postgresql-database)
    (:documentation "This is the CLSQL interface to PostgreSQL."))

(in-package #:clsql-postgresql)

;;; Field conversion functions

(defun make-type-list-for-auto (num-fields res-ptr)
  (let ((new-types '()))
    (dotimes (i num-fields)
      (declare (fixnum i))
      (let* ((type (PQftype res-ptr i)))
	(push
	 (case type
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
	    t))
	 new-types)))
      (nreverse new-types)))

(defun canonicalize-types (types num-fields res-ptr)
  (if (null types)
      nil
      (let ((auto-list (make-type-list-for-auto num-fields res-ptr)))
	(cond
	  ((listp types)
	   (canonicalize-type-list types auto-list))
	  ((eq types :auto)
	   auto-list)
	  (t
	   nil)))))

(defun tidy-error-message (message)
  (unless (stringp message)
    (setq message (uffi:convert-from-foreign-string message)))
  (let ((message (string-right-trim '(#\Return #\Newline) message)))
    (cond
      ((< (length message) (length "ERROR:"))
       message)
      ((string= message "ERROR:" :end1 6)
       (string-left-trim '(#\Space) (subseq message 6)))
      (t
       message))))

(defmethod database-initialize-database-type ((database-type
					       (eql :postgresql)))
  t)

(uffi:def-type pgsql-conn-def pgsql-conn)
(uffi:def-type pgsql-result-def pgsql-result)


(defclass postgresql-database (database)
  ((conn-ptr :accessor database-conn-ptr :initarg :conn-ptr
	     :type pgsql-conn-def)))

(defmethod database-type ((database postgresql-database))
  :postgresql)

(defmethod database-name-from-spec (connection-spec (database-type
						     (eql :postgresql)))
  (check-connection-spec connection-spec database-type
			 (host db user password &optional port options tty))
  (destructuring-bind (host db user password &optional port options tty)
      connection-spec
    (declare (ignore password options tty))
    (concatenate 'string 
      (etypecase host
	(null "localhost")
	(pathname (namestring host))
	(string host))
      (when port 
	(concatenate 'string
		     ":"
		     (etypecase port
		       (integer (write-to-string port))
		       (string port))))
      "/" db "/" user)))


(defmethod database-connect (connection-spec (database-type (eql :postgresql)))
  (check-connection-spec connection-spec database-type
			 (host db user password &optional port options tty))
  (destructuring-bind (host db user password &optional port options tty)
      connection-spec
    (uffi:with-cstrings ((host-native host)
			 (user-native user)
			 (password-native password)
			 (db-native db)
			 (port-native port)
			 (options-native options)
			 (tty-native tty))
      (let ((connection (PQsetdbLogin host-native port-native
				      options-native tty-native
				      db-native user-native
				      password-native)))
	(declare (type pgsql-conn-def connection))
	(when (not (eq (PQstatus connection) 
		       pgsql-conn-status-type#connection-ok))
	  (error 'clsql-connect-error
		 :database-type database-type
		 :connection-spec connection-spec
		 :errno (PQstatus connection)
		 :error (tidy-error-message 
			 (PQerrorMessage connection))))
	(make-instance 'postgresql-database
		       :name (database-name-from-spec connection-spec
						      database-type)
		       :connection-spec connection-spec
		       :conn-ptr connection)))))


(defmethod database-disconnect ((database postgresql-database))
  (PQfinish (database-conn-ptr database))
  (setf (database-conn-ptr database) nil)
  t)

(defmethod database-query (query-expression (database postgresql-database) result-types)
  (let ((conn-ptr (database-conn-ptr database)))
    (declare (type pgsql-conn-def conn-ptr))
    (uffi:with-cstring (query-native query-expression)
      (let ((result (PQexec conn-ptr query-native)))
        (when (uffi:null-pointer-p result)
          (error 'clsql-sql-error
                 :database database
                 :expression query-expression
                 :errno nil
                 :error (tidy-error-message (PQerrorMessage conn-ptr))))
        (unwind-protect
            (case (PQresultStatus result)
              (#.pgsql-exec-status-type#empty-query
               nil)
              (#.pgsql-exec-status-type#tuples-ok
	       (let ((num-fields (PQnfields result)))
		 (setq result-types
		   (canonicalize-types result-types num-fields
					     result))
		 (loop for tuple-index from 0 below (PQntuples result)
		       collect
		       (loop for i from 0 below num-fields
			     collect
			     (if (zerop (PQgetisnull result tuple-index i))
				 (convert-raw-field
				  (PQgetvalue result tuple-index i)
				  result-types i)
				 nil)))))
              (t
               (error 'clsql-sql-error
                      :database database
                      :expression query-expression
                      :errno (PQresultStatus result)
                      :error (tidy-error-message
                              (PQresultErrorMessage result)))))
          (PQclear result))))))

(defmethod database-execute-command (sql-expression
                                     (database postgresql-database))
  (let ((conn-ptr (database-conn-ptr database)))
    (declare (type pgsql-conn-def conn-ptr))
    (uffi:with-cstring (sql-native sql-expression)
      (let ((result (PQexec conn-ptr sql-native)))
        (when (uffi:null-pointer-p result)
          (error 'clsql-sql-error
                 :database database
                 :expression sql-expression
                 :errno nil
                 :error (tidy-error-message (PQerrorMessage conn-ptr))))
        (unwind-protect
            (case (PQresultStatus result)
              (#.pgsql-exec-status-type#command-ok
               t)
              ((#.pgsql-exec-status-type#empty-query
                #.pgsql-exec-status-type#tuples-ok)
               (warn "Strange result...")
               t)
              (t
               (error 'clsql-sql-error
                      :database database
                      :expression sql-expression
                      :errno (PQresultStatus result)
                      :error (tidy-error-message
                              (PQresultErrorMessage result)))))
          (PQclear result))))))

(defstruct postgresql-result-set
  (res-ptr (uffi:make-null-pointer 'pgsql-result) 
	   :type pgsql-result-def)
  (types nil) 
  (num-tuples 0 :type integer)
  (num-fields 0 :type integer)
  (tuple-index 0 :type integer))

(defmethod database-query-result-set ((query-expression string)
				      (database postgresql-database) 
                                      &key full-set result-types)
  (let ((conn-ptr (database-conn-ptr database)))
    (declare (type pgsql-conn-def conn-ptr))
    (uffi:with-cstring (query-native query-expression)
      (let ((result (PQexec conn-ptr query-native)))
        (when (uffi:null-pointer-p result)
          (error 'clsql-sql-error
                 :database database
                 :expression query-expression
                 :errno nil
                 :error (tidy-error-message (PQerrorMessage conn-ptr))))
        (case (PQresultStatus result)
          ((#.pgsql-exec-status-type#empty-query
            #.pgsql-exec-status-type#tuples-ok)
	   (let ((result-set (make-postgresql-result-set
                        :res-ptr result
                        :num-fields (PQnfields result)
                        :num-tuples (PQntuples result)
			:types (canonicalize-types 
				      result-types
				      (PQnfields result)
				      result))))
	     (if full-set
		 (values result-set
			 (PQnfields result)
			 (PQntuples result))
		 (values result-set
			 (PQnfields result)))))
	  (t
	   (unwind-protect
               (error 'clsql-sql-error
                      :database database
                      :expression query-expression
                      :errno (PQresultStatus result)
                      :error (tidy-error-message
                              (PQresultErrorMessage result)))
             (PQclear result))))))))
  
(defmethod database-dump-result-set (result-set (database postgresql-database))
  (let ((res-ptr (postgresql-result-set-res-ptr result-set))) 
    (declare (type pgsql-result-def res-ptr))
    (PQclear res-ptr)
    t))

(defmethod database-store-next-row (result-set (database postgresql-database) 
                                    list)
  (let ((result (postgresql-result-set-res-ptr result-set))
	(types (postgresql-result-set-types result-set)))
    (declare (type pgsql-result-def result))
    (if (>= (postgresql-result-set-tuple-index result-set)
	    (postgresql-result-set-num-tuples result-set))
	nil
      (loop with tuple-index = (postgresql-result-set-tuple-index result-set)
          for i from 0 below (postgresql-result-set-num-fields result-set)
          for rest on list
          do
            (setf (car rest)
              (if (zerop (PQgetisnull result tuple-index i))
                  (convert-raw-field
                   (PQgetvalue result tuple-index i)
		   types i)
                nil))
          finally
            (incf (postgresql-result-set-tuple-index result-set))
            (return list)))))

;;; Large objects support (Marc B)

(defmethod database-create-large-object ((database postgresql-database))
  (lo-create (database-conn-ptr database)
	     (logior postgresql::+INV_WRITE+ postgresql::+INV_READ+)))


#+mb-original
(defmethod database-write-large-object (object-id (data string) (database postgresql-database))
  (let ((ptr (database-conn-ptr database))
	(length (length data))
	(result nil)
	(fd nil))
    (with-transaction (:database database)
       (unwind-protect
	  (progn 
	    (setf fd (lo-open ptr object-id postgresql::+INV_WRITE+))
	    (when (>= fd 0)
	      (when (= (lo-write ptr fd data length) length)
		(setf result t))))
	 (progn
	   (when (and fd (>= fd 0))
	     (lo-close ptr fd))
	   )))
    result))

(defmethod database-write-large-object (object-id (data string) (database postgresql-database))
  (let ((ptr (database-conn-ptr database))
	(length (length data))
	(result nil)
	(fd nil))
    (database-execute-command "begin" database)
    (unwind-protect
	(progn 
	  (setf fd (lo-open ptr object-id postgresql::+INV_WRITE+))
	  (when (>= fd 0)
	    (when (= (lo-write ptr fd data length) length)
	      (setf result t))))
      (progn
	(when (and fd (>= fd 0))
	  (lo-close ptr fd))
	(database-execute-command (if result "commit" "rollback") database)))
    result))

;; (MB) the begin/commit/rollback stuff will be removed when with-transaction wil be implemented
;; (KMR) Can't use with-transaction since that function is in high-level code
(defmethod database-read-large-object (object-id (database postgresql-database))
  (let ((ptr (database-conn-ptr database))
	(buffer nil)
	(result nil)
	(length 0)
	(fd nil))
    (unwind-protect
       (progn
	 (database-execute-command "begin" database)
	 (setf fd (lo-open ptr object-id postgresql::+INV_READ+))
	 (when (>= fd 0)
	   (setf length (lo-lseek ptr fd 0 2))
	   (lo-lseek ptr fd 0 0)
	   (when (> length 0)
	     (setf buffer (uffi:allocate-foreign-string 
			   length :unsigned t))
	     (when (= (lo-read ptr fd buffer length) length)
	       (setf result (uffi:convert-from-foreign-string
			     buffer :length length :null-terminated-p nil))))))
      (progn
	(when buffer (uffi:free-foreign-object buffer))
	(when (and fd (>= fd 0)) (lo-close ptr fd))
	(database-execute-command (if result "commit" "rollback") database)))
    result))

(defmethod database-delete-large-object (object-id (database postgresql-database))
  (lo-unlink (database-conn-ptr database) object-id))


;;; Object listing

(defmethod database-list-objects-of-type ((database postgresql-database)
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
    
(defmethod database-list-tables ((database postgresql-database)
                                 &key (owner nil))
  (database-list-objects-of-type database "r" owner))
  
(defmethod database-list-views ((database postgresql-database)
                                &key (owner nil))
  (database-list-objects-of-type database "v" owner))
  
(defmethod database-list-indexes ((database postgresql-database)
                                  &key (owner nil))
  (database-list-objects-of-type database "i" owner))
  
(defmethod database-list-attributes ((table string)
				     (database postgresql-database)
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
				    (database postgresql-database)
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
				     (database postgresql-database))
  (database-execute-command
   (concatenate 'string "CREATE SEQUENCE " (sql-escape sequence-name))
   database))

(defmethod database-drop-sequence (sequence-name
				   (database postgresql-database))
  (database-execute-command
   (concatenate 'string "DROP SEQUENCE " (sql-escape sequence-name)) database))

(defmethod database-list-sequences ((database postgresql-database)
                                    &key (owner nil))
  (database-list-objects-of-type database "S" owner))

(defmethod database-set-sequence-position (name (position integer)
                                                (database postgresql-database))
  (values
   (parse-integer
    (caar
     (database-query
      (format nil "SELECT SETVAL ('~A', ~A)" name position)
      database nil)))))

(defmethod database-sequence-next (sequence-name 
				   (database postgresql-database))
  (values
   (parse-integer
    (caar
     (database-query
      (concatenate 'string "SELECT NEXTVAL ('" (sql-escape sequence-name) "')")
      database nil)))))

(defmethod database-sequence-last (sequence-name (database postgresql-database))
  (values
   (parse-integer
    (caar
     (database-query
      (concatenate 'string "SELECT LAST_VALUE ('" sequence-name "')")
      database nil)))))
  
(defmethod database-create (connection-spec (type (eql :postgresql)))
  (destructuring-bind (host name user password) connection-spec
    (declare (ignore user password))
    (multiple-value-bind (output status)
	(clsql-base-sys:command-output "createdb -h~A ~A"
				       (if host host "localhost")
				       name)
      (if (or (not (zerop status))
	      (search "database creation failed: ERROR:" output))
	  (error 'clsql-access-error
		 :connection-spec connection-spec
		 :database-type type
		 :error 
		 (format nil "database-create failed: ~A" 
			 output))
	t))))

(defmethod database-destroy (connection-spec (type (eql :postgresql)))
  (destructuring-bind (host name user password) connection-spec
    (declare (ignore user password))
    (multiple-value-bind (output status)
	(clsql-base-sys:command-output "dropdb -h~A ~A"
				       (if host host "localhost")
				       name)
      (if (or (not (zerop status))
	      (search "database removal failed: ERROR:" output))
	  (error 'clsql-access-error
		 :connection-spec connection-spec
		 :database-type type
		 :error 
		 (format nil "database-destory failed: ~A" 
			 output))
	t))))


(defmethod database-probe (connection-spec (type (eql :postgresql)))
  (destructuring-bind (host name user password) connection-spec
    (let ((database (database-connect (list host "template1" user password)
				      type)))
      (unwind-protect
	  (when
	      (find name (database-query "select datname from pg_database" 
					 database :auto)
		    :key #'car :test #'string-equal)
	    t)
	(database-disconnect database)))))


(when (clsql-base-sys:database-type-library-loaded :postgresql)
  (clsql-base-sys:initialize-database-type :database-type :postgresql))
