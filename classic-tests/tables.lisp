;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          tables.cl
;;;; Purpose:       Table creation tests in CLSQL
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  Mar 2002
;;;;
;;;; $Id$
;;;;
;;;; This file, part of CLSQL, is Copyright (c) 2002 by Kevin M. Rosenberg
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

;;; This test suite looks for a configuration file named ".clsql-test.config"
;;; located in the users home directory.
;;;
;;; This file contains a single a-list that specifies the connection
;;; specs for each database type to be tested. For example, to test all
;;; platforms, a sample "test.config" may look like:
;;;
;;; ((:mysql ("localhost" "a-mysql-db" "user1" "secret"))
;;;  (:aodbc ("my-dsn" "a-user" "pass"))
;;;  (:postgresql ("localhost" "another-db" "user2" "dont-tell"))
;;;  (:postgresql-socket ("pg-server" "a-db-name" "user" "secret-password")))

(in-package :clsql-tests)

(defvar *config-pathname*
  (make-pathname :default (user-homedir-pathname)
		 :name ".clsql-test"
		 :type ".config"))

(defclass conn-specs ()
  ((aodbc-spec :accessor aodbc-spec)
   (mysql-spec :accessor mysql-spec)
   (pgsql-spec :accessor pgsql-spec)
   (pgsql-socket-spec :accessor pgsql-socket-spec))
  (:documentation "Test fixture for CLSQL testing"))


(defun read-specs (&optional (path *config-pathname*))
  (if (probe-file path)
      (with-open-file (stream path :direction :input)
	(let ((config (read stream))
	      (specs (make-instance 'conn-specs)))
	  (setf (aodbc-spec specs) (cadr (assoc :aodbc config)))
	  (setf (mysql-spec specs) (cadr (assoc :mysql config)))
	  (setf (pgsql-spec specs) (cadr (assoc :postgresql config)))
	  (setf (pgsql-socket-spec specs) 
		(cadr (assoc :postgresql-socket config)))
	  specs))
      (warn "CLSQL tester config file ~S not found" path)))

(defvar *conn-specs* (read-specs))

(defmethod mysql-table-test ((test conn-specs))
  (test-table (mysql-spec test) :mysql))

(defmethod aodbc-table-test ((test conn-specs))
  (test-table (aodbc-spec test) :aodbc))

(defmethod pgsql-table-test ((test conn-specs))
  (test-table (pgsql-spec test) :postgresql))

(defmethod pgsql-socket-table-test ((test conn-specs))
  (test-table (pgsql-socket-spec test) :postgresql-socket))

(defmethod test-table (spec type)
  (when spec
    (let ((db (clsql:connect spec :database-type type :if-exists :new)))
      (unwind-protect
	   (progn
	     (create-test-table db)
	     (dolist (row (query "select * from test_clsql" :database db :types :auto))
	       (test-table-row row :auto type))
	     (dolist (row (query "select * from test_clsql" :database db :types nil))
	       (test-table-row row nil type))
	     (loop for row across (map-query 'vector #'list "select * from test_clsql" 
					     :database db :types :auto)
		   do (test-table-row row :auto type))
	     (loop for row across (map-query 'vector #'list "select * from test_clsql" 
					     :database db :types nil)
		   do (test-table-row row nil type))
	     (loop for row in (map-query 'list #'list "select * from test_clsql" 
					 :database db :types nil)
		   do (test-table-row row nil type))
	     (loop for row in (map-query 'list #'list "select * from test_clsql" 
					 :database db :types :auto)
		 do (test-table-row row :auto type))
	     (test (map-query nil #'list "select * from test_clsql" 
			      :database db :types :auto)
		   nil
		   :fail-info "Expected NIL result from map-query nil")
	     (do-query ((int float bigint str) "select * from test_clsql")
	       (test-table-row (list int float bigint str) nil type))
	     (do-query ((int float bigint str) "select * from test_clsql" :types :auto)
	       (test-table-row (list int float bigint str) :auto type))
	     (drop-test-table db)
	     )
	(disconnect :database db)))))


;;;; Testing functions

(defun transform-float-1 (i)
  (coerce (* i (abs (/ i 2)) (expt 10 (* 2 i))) 'double-float))

(defun transform-bigint-1 (i)
  (* i (expt 10 (* 3 (abs i)))))

(defun create-test-table (db)
  (ignore-errors
    (clsql:execute-command 
     "DROP TABLE test_clsql" :database db))
  (clsql:execute-command 
   "CREATE TABLE test_clsql (t_int integer, t_float float, t_bigint BIGINT, t_str CHAR(30))" 
   :database db)
  (dotimes (i 11)
    (let* ((test-int (- i 5))
	   (test-flt (transform-float-1 test-int)))
      (clsql:execute-command
       (format nil "INSERT INTO test_clsql VALUES (~a,~a,~a,'~a')"
	       test-int
	       (number-to-sql-string test-flt)
	       (transform-bigint-1 test-int)
	       (number-to-sql-string test-flt)
	       )
       :database db))))

(defun parse-double (num-str)
  (let ((*read-default-float-format* 'double-float))
    (coerce (read-from-string num-str) 'double-float)))

(defun test-table-row (row types db-type)
  (test (and (listp row)
	     (= 4 (length row)))
	t
	:fail-info 
	(format nil "Row ~S is incorrect format" row))
  (destructuring-bind (int float bigint str) row
    (cond
      ((eq types :auto)
       (test (and (integerp int)
		  (typep float 'double-float)
		  (or (eq db-type :aodbc) ;; aodbc doesn't handle bigint conversions
		      (integerp bigint)) 
		  (stringp str))
	     t
	     :fail-info 
	     (format nil "Incorrect field type for row ~S (types :auto)" row)))
       ((null types)
	(test (and (stringp int)
		     (stringp float)
		     (stringp bigint)
		     (stringp str))
	      t
	      :fail-info 
	      (format nil "Incorrect field type for row ~S (types nil)" row))
	(setq int (parse-integer int))
	(setq bigint (parse-integer bigint))
	(setq float (parse-double float)))
       ((listp types)
	(error "NYI")
	)
       (t 
	(test t nil
	      :fail-info
	      (format nil "Invalid types field (~S) passed to test-table-row" types))))
    (test (transform-float-1 int)
	  float
	  :test #'eql
	  :fail-info 
	  (format nil "Wrong float value ~A for int ~A (row ~S)" float int row))
    (test float
	  (parse-double str)
	  :test #'double-float-equal
	  :fail-info (format nil "Wrong string value ~A for double ~A~%Row: ~S"
			     str float row))))


(defun double-float-equal (a b)
  (if (zerop a)
      (if (zerop b)
	  t
	  nil)
      (let ((diff (abs (/ (- a b) a))))
	(if (> diff (* 10 double-float-epsilon))
	    nil
	    t))))
	 
(defun drop-test-table (db)
  (clsql:execute-command "DROP TABLE test_clsql" :database db))


(deftest lowlevel.mysql.table.1
  (let ((spec (mysql-spec *conn-specs*))
	(result))
    (when spec
      (let ((db (clsql-mysql::database-connect spec :mysql)))
	(clsql-mysql::database-execute-command "DROP TABLE IF EXISTS test_clsql" db)
	(clsql-mysql::database-execute-command 
	 "CREATE TABLE test_clsql (i integer, sqrt double, sqrt_str CHAR(20))" db)
	(dotimes (i 10)
	  (clsql-mysql::database-execute-command
	   (format nil "INSERT INTO test_clsql VALUES (~d,~d,'~a')"
		   i (clsql:number-to-sql-string (sqrt i))
		   (clsql:number-to-sql-string (sqrt i)))
	   db))
	(let ((res (clsql-mysql::database-query-result-set "select * from test_clsql" db :full-set t :types nil)))
	  (setq result (mysql:mysql-num-rows
			(clsql-mysql::mysql-result-set-res-ptr res)))
	  (clsql-mysql::database-dump-result-set res db))
	(clsql-mysql::database-execute-command "DROP TABLE test_clsql" db)
	(clsql-mysql::database-disconnect db))))
  10)

;(mysql-table-test specs)
;(pgsql-table-test specs)
;(pgsql-socket-table-test specs)
;(aodbc-table-test specs)



(defmacro def-test-table (name spec type)
  (deftest ,name
      (when ,spec
	(let ((db (clsql:connect ,spec :database-type ,type :if-exists :new)))
	  (unwind-protect
	   (progn
	     (create-test-table db)
	     (dolist (row (query "select * from test_clsql" :database db :types :auto))
	       (test-table-row row :auto type))
	     (dolist (row (query "select * from test_clsql" :database db :types nil))
	       (test-table-row row nil type))
	     (loop for row across (map-query 'vector #'list "select * from test_clsql" 
					     :database db :types :auto)
		   do (test-table-row row :auto type))
	     (loop for row across (map-query 'vector #'list "select * from test_clsql" 
					     :database db :types nil)
		   do (test-table-row row nil type))
	     (loop for row in (map-query 'list #'list "select * from test_clsql" 
					 :database db :types nil)
		   do (test-table-row row nil type))
	     (loop for row in (map-query 'list #'list "select * from test_clsql" 
					 :database db :types :auto)
		 do (test-table-row row :auto type))
	     (test (map-query nil #'list "select * from test_clsql" 
			      :database db :types :auto)
		   nil
		   :fail-info "Expected NIL result from map-query nil")
	     (do-query ((int float bigint str) "select * from test_clsql")
	       (test-table-row (list int float bigint str) nil type))
	     (do-query ((int float bigint str) "select * from test_clsql" :types :auto)
	       (test-table-row (list int float bigint str) :auto type))
	     (drop-test-table db)
	     )
	(disconnect :database db)))))
