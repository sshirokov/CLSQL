;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          mysql-sql.lisp
;;;; Purpose:       High-level MySQL interface using UFFI
;;;; Date Started:  Feb 2002
;;;;
;;;; $Id$
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(defpackage #:clsql-mysql
    (:use #:common-lisp #:clsql-base-sys #:mysql #:clsql-uffi)
    (:export #:mysql-database)
    (:documentation "This is the CLSQL interface to MySQL."))

(in-package #:clsql-mysql)

;;; Field conversion functions

(defun make-type-list-for-auto (num-fields res-ptr)
  (declare (fixnum num-fields))
  (let ((new-types '())
	#+ignore (field-vec (mysql-fetch-fields res-ptr)))
    (dotimes (i num-fields)
      (declare (fixnum i))
      (let* ( (field (mysql-fetch-field-direct res-ptr i))
	     #+ignore (field (uffi:deref-array field-vec '(:array mysql-field) i))
	      (type (uffi:get-slot-value field 'mysql-field 'type)))
	(push
	 (case type
	   ((#.mysql-field-types#tiny 
	     #.mysql-field-types#short
	     #.mysql-field-types#int24
	     #.mysql-field-types#long)
	    :int32)
	   (#.mysql-field-types#longlong
	    :int64)
	   ((#.mysql-field-types#double
	     #.mysql-field-types#float
	     #.mysql-field-types#decimal)
	    :double)
	   (otherwise
	    t))
	 new-types)))
    (nreverse new-types)))

(defun canonicalize-types (types num-fields res-ptr)
  (when types
    (let ((auto-list (make-type-list-for-auto num-fields res-ptr)))
      (cond
	((listp types)
	 (canonicalize-type-list types auto-list))
	((eq types :auto)
	 auto-list)
	(t
	 nil)))))

(defmethod database-initialize-database-type ((database-type (eql :mysql)))
  t)

(uffi:def-type mysql-mysql-ptr-def (* mysql-mysql))
(uffi:def-type mysql-row-def mysql-row)
(uffi:def-type mysql-mysql-res-ptr-def (* mysql-mysql-res))

(defclass mysql-database (database)
  ((mysql-ptr :accessor database-mysql-ptr :initarg :mysql-ptr
	      :type mysql-mysql-ptr-def)))

(defmethod database-type ((database mysql-database))
  :mysql)

(defmethod database-name-from-spec (connection-spec (database-type (eql :mysql)))
  (check-connection-spec connection-spec database-type (host db user password))
  (destructuring-bind (host db user password) connection-spec
    (declare (ignore password))
    (concatenate 'string 
		 (if host host "localhost")
		 "/" db "/" user)))

(defmethod database-connect (connection-spec (database-type (eql :mysql)))
  (check-connection-spec connection-spec database-type (host db user password))
  (destructuring-bind (host db user password) connection-spec
    (let ((mysql-ptr (mysql-init (uffi:make-null-pointer 'mysql-mysql)))
	  (socket nil))
      (if (uffi:null-pointer-p mysql-ptr)
	  (error 'clsql-connect-error
		 :database-type database-type
		 :connection-spec connection-spec
		 :errno (mysql-errno mysql-ptr)
		 :error (mysql-error-string mysql-ptr))
	(uffi:with-cstrings ((host-native host)
			    (user-native user)
			    (password-native password)
			    (db-native db)
			    (socket-native socket))
	  (let ((error-occurred nil))
	    (unwind-protect
		(if (uffi:null-pointer-p 
		     (mysql-real-connect 
		      mysql-ptr host-native user-native password-native
		      db-native 0 socket-native 0))
		    (progn
		      (setq error-occurred t)
		      (error 'clsql-connect-error
			     :database-type database-type
			     :connection-spec connection-spec
			     :errno (mysql-errno mysql-ptr)
			     :error (mysql-error-string mysql-ptr)))
		  (make-instance 'mysql-database
		    :name (database-name-from-spec connection-spec
						   database-type)
		    :database-type :mysql
		    :connection-spec connection-spec
		    :mysql-ptr mysql-ptr))
	      (when error-occurred (mysql-close mysql-ptr)))))))))


(defmethod database-disconnect ((database mysql-database))
  (mysql-close (database-mysql-ptr database))
  (setf (database-mysql-ptr database) nil)
  t)


(defmethod database-query (query-expression (database mysql-database) 
			   result-types)
  (declare (optimize (speed 3) (safety 0) (debug 0) (space 0)))
  (let ((mysql-ptr (database-mysql-ptr database)))
    (uffi:with-cstring (query-native query-expression)
      (if (zerop (mysql-real-query mysql-ptr query-native 
                                   (length query-expression)))
	  (let ((res-ptr (mysql-use-result mysql-ptr)))
	    (if res-ptr
		(unwind-protect
		     (let ((num-fields (mysql-num-fields res-ptr)))
		       (declare (fixnum num-fields))
		       (setq result-types (canonicalize-types 
				    result-types num-fields
				    res-ptr))
		       (loop for row = (mysql-fetch-row res-ptr)
                             for lengths = (mysql-fetch-lengths res-ptr)
			     until (uffi:null-pointer-p row)
			   collect
			     (do* ((rlist (make-list num-fields))
				   (i 0 (1+ i))
				   (pos rlist (cdr pos)))
				 ((= i num-fields) rlist)
			       (declare (fixnum i))
			       (setf (car pos)  
				 (convert-raw-field
				  (uffi:deref-array row '(:array
							  (* :unsigned-char))
						    i)
				  result-types i
                                  (uffi:deref-array lengths '(:array :unsigned-long)
						    i))))))
		  (mysql-free-result res-ptr))
		(error 'clsql-sql-error
		       :database database
		       :expression query-expression
		       :errno (mysql-errno mysql-ptr)
		       :error (mysql-error-string mysql-ptr))))
	  (error 'clsql-sql-error
		 :database database
		 :expression query-expression
		 :errno (mysql-errno mysql-ptr)
		 :error (mysql-error-string mysql-ptr))))))

(defmethod database-execute-command (sql-expression (database mysql-database))
  (uffi:with-cstring (sql-native sql-expression)
    (let ((mysql-ptr (database-mysql-ptr database)))
      (declare (type mysql-mysql-ptr-def mysql-ptr))
      (if (zerop (mysql-real-query mysql-ptr sql-native 
                                   (length sql-expression)))
	  t
	(error 'clsql-sql-error
	       :database database
	       :expression sql-expression
	       :errno (mysql-errno mysql-ptr)
	       :error (mysql-error-string mysql-ptr))))))


(defstruct mysql-result-set 
  (res-ptr (uffi:make-null-pointer 'mysql-mysql-res) :type mysql-mysql-res-ptr-def)
  (types nil :type list)
  (num-fields 0 :type fixnum)
  (full-set nil :type boolean))


(defmethod database-query-result-set ((query-expression string)
				      (database mysql-database)
				      &key full-set result-types)
  (uffi:with-cstring (query-native query-expression)
    (let ((mysql-ptr (database-mysql-ptr database)))
     (declare (type mysql-mysql-ptr-def mysql-ptr))
      (if (zerop (mysql-real-query mysql-ptr query-native
                                   (length query-expression)))
	  (let ((res-ptr (if full-set
			     (mysql-store-result mysql-ptr)
			   (mysql-use-result mysql-ptr))))
	    (declare (type mysql-mysql-res-ptr-def res-ptr))
	    (if (not (uffi:null-pointer-p res-ptr))
		(let* ((num-fields (mysql-num-fields res-ptr))
		       (result-set (make-mysql-result-set
				    :res-ptr res-ptr
				    :num-fields num-fields
				    :full-set full-set
				    :types
				    (canonicalize-types 
				     result-types num-fields
				     res-ptr)))) 
		  (if full-set
		      (values result-set
			      num-fields
			      (mysql-num-rows res-ptr))
		      (values result-set
			      num-fields)))
		(error 'clsql-sql-error
		     :database database
		     :expression query-expression
		     :errno (mysql-errno mysql-ptr)
		     :error (mysql-error-string mysql-ptr))))
	(error 'clsql-sql-error
	       :database database
	       :expression query-expression
	       :errno (mysql-errno mysql-ptr)
	       :error (mysql-error-string mysql-ptr))))))

(defmethod database-dump-result-set (result-set (database mysql-database))
  (mysql-free-result (mysql-result-set-res-ptr result-set))
  t)


(defmethod database-store-next-row (result-set (database mysql-database) list)
  (let* ((res-ptr (mysql-result-set-res-ptr result-set))
	 (row (mysql-fetch-row res-ptr))
         (lengths (mysql-fetch-lengths res-ptr))
	 (types (mysql-result-set-types result-set)))
    (declare (type mysql-mysql-res-ptr-def res-ptr)
	     (type mysql-row-def row))
    (unless (uffi:null-pointer-p row)
      (loop for i from 0 below (mysql-result-set-num-fields result-set)
	    for rest on list
	    do
	    (setf (car rest) 
		  (convert-raw-field
		   (uffi:deref-array row '(:array (* :unsigned-char)) i)
		   types
		   i
                   (uffi:deref-array lengths '(:array :unsigned-long) i))))
      list)))


;; Table and attribute introspection

(defmethod database-list-tables ((database mysql-database) &key (owner nil))
  (declare (ignore owner))
  (remove-if #'(lambda (s)
                 (and (>= (length s) 11)
                      (string= (subseq s 0 11) "_clsql_seq_")))
             (mapcar #'car (database-query "SHOW TABLES" database nil))))
    
;; MySQL 4.1 does not support views 
(defmethod database-list-views ((database mysql-database)
                                &key (owner nil))
  (declare (ignore owner))
  nil)

(defmethod database-list-indexes ((database mysql-database)
                                  &key (owner nil))
  (let ((result '()))
    (dolist (table (database-list-tables database :owner owner) result)
      (setq result
	(append (database-list-table-indexes table database :owner owner)
		result)))))

(defmethod database-list-table-indexes (table (database mysql-database)
					&key (owner nil))
  (declare (ignore owner))
  (do ((results nil)
       (rows (database-query 
	      (format nil "SHOW INDEX FROM ~A" (string-upcase table))
	      database nil)
	     (cdr rows)))
      ((null rows) (nreverse results))
    (let ((col (nth 2 (car rows))))
      (unless (find col results :test #'string-equal)
	(push col results)))))
  
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
  (concatenate 'string "_clsql_seq_" (sql-escape sequence-name)))

(defun %table-name-to-sequence-name (table-name)
  (and (>= (length table-name) 11)
       (string= (subseq table-name 0 11) "_clsql_seq_")
       (subseq table-name 11)))

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
          (database-query "SHOW TABLES LIKE '%clsql_seq%'" 
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
  (without-interrupts
   (database-execute-command 
    (concatenate 'string "UPDATE " (%sequence-name-to-table sequence-name)
		 " SET id=LAST_INSERT_ID(id+1)")
    database)
   (mysql:mysql-insert-id (clsql-mysql::database-mysql-ptr database))))

(defmethod database-sequence-last (sequence-name (database mysql-database))
  (declare (ignore sequence-name)))

(defmethod database-create (connection-spec (type (eql :mysql)))
  (destructuring-bind (host name user password) connection-spec
    (multiple-value-bind (output status)
	(clsql-base-sys:command-output "mysqladmin create -u~A -p~A -h~A ~A"
				       user password 
				       (if host host "localhost")
				       name)
      (if (or (not (eql 0 status))
	      (and (search "failed" output) (search "error" output)))
	  (error 'clsql-access-error
		 :connection-spec connection-spec
		 :database-type type
		 :error 
		 (format nil "database-create failed: ~A" output))
	  t))))

(defmethod database-destroy (connection-spec (type (eql :mysql)))
  (destructuring-bind (host name user password) connection-spec
    (multiple-value-bind (output status)
	(clsql-base-sys:command-output "mysqladmin drop -f -u~A -p~A -h~A ~A"
				       user password 
				       (if host host "localhost")
				       name)
      (if (or (not (eql 0 status))
	      (and (search "failed" output) (search "error" output)))
	  (error 'clsql-access-error
		 :connection-spec connection-spec
		 :database-type type
		 :error 
		 (format nil "database-destroy failed: ~A" output))
	t))))

(defmethod database-probe (connection-spec (type (eql :mysql)))
  (when (find (second connection-spec) (database-list connection-spec type)
	      :key #'car :test #'string-equal)
    t))

(defmethod database-list (connection-spec (type (eql :mysql)))
  (destructuring-bind (host name user password) connection-spec
    (declare (ignore name))
    (let ((database (database-connect (list host "mysql" user password) type)))
      (unwind-protect
	   (progn
	     (setf (slot-value database 'clsql-base-sys::state) :open)
	     (mapcar #'car (database-query "show databases" database :auto)))
	(progn
	  (database-disconnect database)
	  (setf (slot-value database 'clsql-base-sys::state) :closed))))))


(when (clsql-base-sys:database-type-library-loaded :mysql)
  (clsql-base-sys:initialize-database-type :database-type :mysql))
