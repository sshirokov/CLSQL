;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          xptest-clsql.cl
;;;; Purpose:       Test of CLSQL using XPTest package
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  Mar 2002
;;;;
;;;; $Id: xptest-clsql.lisp,v 1.1 2002/09/30 10:19:24 kevin Exp $
;;;;
;;;; The XPTest package can be downloaded from
;;;; http://alpha.onshored.com/lisp-software/
;;;;
;;;; This file, part of CLSQL, is Copyright (c) 2002 by Kevin M. Rosenberg
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************


;;; This test suite looks for a configuration file named "test.config"
;;; This file contains a single a-list that specifies the connection
;;; specs for each database type to be tested. For example, to test all
;;; platforms, a sample "test.config" may look like:
;;;
;;; ((:mysql ("localhost" "a-mysql-db" "user1" "secret"))
;;;  (:aodbc ("my-dsn" "a-user" "pass"))
;;;  (:paostgresql ("localhost" "another-db" "user2" "dont-tell"))
;;;  (:postgresql-socket ("pg-server" "a-db-name" "user" "secret-password")))

(declaim (optimize (debug 3) (speed 3) (safety 1) (compilation-speed 0)))
(in-package :cl-user)
(mk:load-system "XPTest")

(in-package :clsql-user)
(use-package :xptest)

(def-test-fixture clsql-fixture ()
  ((aodbc-spec :accessor aodbc-spec)
   (mysql-spec :accessor mysql-spec)
   (pgsql-spec :accessor pgsql-spec)
   (pgsql-socket-spec :accessor pgsql-socket-spec))
  (:documentation "Test fixture for CLSQL testing"))

(defvar *config-pathname* (make-pathname :name "test"
					 :type "config"
					 :defaults *load-truename*))
(defmethod setup ((fix clsql-fixture))
  (if (probe-file *config-pathname*)
      (let (config)
	(with-open-file (stream *config-pathname* :direction :input)
	  (setq config (read stream)))
	(setf (aodbc-spec fix) (cadr (assoc :aodbc config)))
	(setf (mysql-spec fix) (cadr (assoc :mysql config)))
	(setf (pgsql-spec fix) (cadr (assoc :postgresql config)))
	(setf (pgsql-socket-spec fix) 
	      (cadr (assoc :postgresql-socket config))))
      (error "XPTest Config file ~S not found" *config-pathname*)))

(defmethod teardown ((fix clsql-fixture))
  t)

(defmethod mysql-table-test ((test clsql-fixture))
  (test-table (mysql-spec test) :mysql))

(defmethod aodbc-table-test ((test clsql-fixture))
  (test-table (aodbc-spec test) :aodbc))

(defmethod pgsql-table-test ((test clsql-fixture))
  (test-table (pgsql-spec test) :postgresql))

(defmethod pgsql-socket-table-test ((test clsql-fixture))
  (test-table (pgsql-socket-spec test) :postgresql-socket))


(defmethod test-table (spec type)
  (when spec
    (let ((db (clsql:connect spec :database-type type :if-exists :new)))
      (unwind-protect
	   (progn
	     (create-test-table db)
	     (dolist (row (query "select * from test_clsql" :database db :types :auto))
	       (test-table-row row :auto))
	     (dolist (row (query "select * from test_clsql" :database db :types nil))
	       (test-table-row row nil))
	     (loop for row across (map-query 'vector #'list "select * from test_clsql" 
					     :database db :types :auto)
		   do (test-table-row row :auto))
	     (loop for row across (map-query 'vector #'list "select * from test_clsql" 
					     :database db :types nil)
		   do (test-table-row row nil))
	     (loop for row in (map-query 'list #'list "select * from test_clsql" 
					 :database db :types nil)
		   do (test-table-row row nil))
	     (loop for row in (map-query 'list #'list "select * from test_clsql" 
					 :database db :types :auto)
		   do (test-table-row row :auto))
	     (when (map-query nil #'list "select * from test_clsql" 
					 :database db :types :auto)
	       (failure "Expected NIL result from map-query nil"))
	     (do-query ((int float bigint str) "select * from test_clsql")
	       (test-table-row (list int float bigint str) nil))
	     (do-query ((int float bigint str) "select * from test_clsql" :types :auto)
	       (test-table-row (list int float bigint str) :auto))
	     (drop-test-table db)
	     )
	(disconnect :database db)))))


(defmethod mysql-low-level ((test clsql-fixture))
  (let ((spec (mysql-spec test)))
    (when spec
      (let ((db (clsql-mysql::database-connect spec :mysql)))
	(clsql-mysql::database-execute-command "DROP TABLE IF EXISTS test_clsql" db)
	(clsql-mysql::database-execute-command 
	 "CREATE TABLE test_clsql (i integer, sqrt double, sqrt_str CHAR(20))" db)
	(dotimes (i 10)
	  (clsql-mysql::database-execute-command
	   (format nil "INSERT INTO test_clsql VALUES (~d,~d,'~a')"
		   i (number-to-sql-string (sqrt i))
		   (number-to-sql-string (sqrt i)))
	   db))
	(let ((res (clsql-mysql::database-query-result-set "select * from test_clsql" db :full-set t :types nil)))
	  (unless (= 10 (mysql:mysql-num-rows (clsql-mysql::mysql-result-set-res-ptr res)))
	    (failure "Error calling mysql-num-rows"))
	  (clsql-mysql::database-dump-result-set res db))
	(clsql-mysql::database-execute-command "DROP TABLE test_clsql" db)
	(clsql-mysql::database-disconnect db)))))

(defparameter clsql-test-suite 
    (make-test-suite
     "CLSQL Test Suite"
     "Basic test suite for database operations."
     ("MySQL Low Level Interface" 'clsql-fixture
		   :test-thunk 'mysql-low-level
		   :description "A test of MySQL low-level interface")
     ("MySQL Table" 'clsql-fixture
		   :test-thunk 'mysql-table-test
		   :description "A test of MySQL")
     ("PostgreSQL Table" 'clsql-fixture
		   :test-thunk 'pgsql-table-test
		   :description "A test of PostgreSQL tables")     
     ("PostgreSQL Socket Table" 'clsql-fixture
		   :test-thunk 'pgsql-socket-table-test
		   :description "A test of PostgreSQL Socket tables")
  ))

#+allegro 
(add-test (make-test-case "AODBC table test" 'clsql-fixture
			  :test-thunk 'aodbc-table-test
			  :description "Test AODBC table")
	  clsql-test-suite)

;;;; Testing functions

(defun transform-float-1 (i)
  (* i (abs (/ i 2)) (expt 10 (* 2 i))))

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

(defun test-table-row (row types)
  (unless (and (listp row)
	       (= 4 (length row)))
    (failure "Row ~S is incorrect format" row))
  (destructuring-bind (int float bigint str) row
    (cond
      ((eq types :auto)
       (unless (and (integerp int)
		    (typep float 'double-float)
		    (integerp bigint)
		    (stringp str))
	 (failure "Incorrect field type for row ~S" row)))
       ((null types)
	(unless (and (stringp int)
		     (stringp float)
		     (stringp bigint)
		     (stringp str))
	  (failure "Incorrect field type for row ~S" row))
	(setq int (parse-integer int))
	(setq bigint (parse-integer bigint))
	(setq float (parse-double float)))
       ((listp types)
	(error "NYI")
	)
       (t 
	(failure "Invalid types field (~S) passed to test-table-row" types)))
    (unless (= float (transform-float-1 int))
      (failure "Wrong float value ~A for int ~A (row ~S)" float int row))
    (unless (= float (parse-double str))
      (failure "Wrong string value ~A" str))))


(defun drop-test-table (db)
  (clsql:execute-command "DROP TABLE test_clsql"))

(report-result (run-test clsql-test-suite :handle-errors nil) :verbose t)


