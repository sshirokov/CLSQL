;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          classic-tests.lisp
;;;; Purpose:       Automated test of CLSQL using ACL's tester
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
;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; File:    tests.lisp
;;;; Author: Kevin Rosenberg
;;;; $Id$
;;;;
;;;; This file is part of CLSQL.
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

;;; You need a file named "~/.clsql-tests.config"

;;; This file contains a single a-list that specifies the connection
;;; specs for each database type to be tested. For example, to test all
;;; platforms, a sample "test.config" may look like:
;;;
;;; ((:mysql ("localhost" "a-mysql-db" "user1" "secret"))
;;;  (:aodbc ("my-dsn" "a-user" "pass"))
;;;  (:postgresql ("localhost" "another-db" "user2" "dont-tell"))
;;;  (:postgresql-socket ("pg-server" "a-db-name" "user" "secret-password"))
;;;  (:sqlite ("path-to-sqlite-db")))

(in-package #:clsql-classic-tests)

(defvar *config-pathname*
  (make-pathname :defaults (user-homedir-pathname)
		 :name ".clsql-test"
		 :type "config"))

(defvar +all-db-types+
  #-clisp '(:postgresql :postgresql-socket :sqlite :aodbc :mysql)
  #+clisp '(:sqlite))

(defclass conn-specs ()
  ((aodbc-spec :accessor aodbc-spec :initform nil)
   (mysql-spec :accessor mysql-spec :initform nil)
   (pgsql-spec :accessor postgresql-spec :initform nil)
   (pgsql-socket-spec :accessor postgresql-socket-spec :initform nil)
   (sqlite-spec :accessor sqlite-spec :initform nil))
  (:documentation "Connection specs for CLSQL testing"))


(defun read-specs (&optional (path *config-pathname*))
  (if (probe-file path)
      (with-open-file (stream path :direction :input)
	(let ((config (read stream))
	      (specs (make-instance 'conn-specs)))
	  (setf (aodbc-spec specs) (cadr (assoc :aodbc config)))
	  (setf (mysql-spec specs) (cadr (assoc :mysql config)))
	  (setf (postgresql-spec specs) (cadr (assoc :postgresql config)))
	  (setf (postgresql-socket-spec specs) 
		(cadr (assoc :postgresql-socket config)))
	  (setf (sqlite-spec specs) (cadr (assoc :sqlite config)))
	  specs))
      (progn
	(warn "CLSQL test config file ~S not found" path)
	nil)))

(defgeneric test-table (spec type))

(defmethod test-table (spec type)
  (when spec
    (let ((db (clsql:connect spec :database-type type :if-exists :new)))
      (unwind-protect
	   (progn
	     (create-test-table db)
	     (dolist (row (query "select * from test_clsql" :database db :result-types :auto))
	       (test-table-row row :auto type))
	     (dolist (row (query "select * from test_clsql" :database db :result-types nil))
	       (test-table-row row nil type))
	     (loop for row across (map-query 'vector #'list "select * from test_clsql" 
					     :database db :result-types :auto)
		   do (test-table-row row :auto type))
	     (loop for row across (map-query 'vector #'list "select * from test_clsql" 
					     :database db :result-types nil)
		   do (test-table-row row nil type))
	     (loop for row in (map-query 'list #'list "select * from test_clsql" 
					 :database db :result-types nil)
		   do (test-table-row row nil type))
	     (loop for row in (map-query 'list #'list "select * from test_clsql" 
					 :database db :result-types :auto)
		 do (test-table-row row :auto type))
	     (test (map-query nil #'list "select * from test_clsql" 
			      :database db :result-types :auto)
		   nil
		   :fail-info "Expected NIL result from map-query nil")
	     (do-query ((int float bigint str) "select * from test_clsql")
	       (test-table-row (list int float bigint str) nil type))
	     (do-query ((int float bigint str) "select * from test_clsql" :result-types :auto)
	       (test-table-row (list int float bigint str) :auto type))
	     (drop-test-table db)
	     )
	(disconnect :database db)))))

;;;
;;; SQLite is typeless: execute untyped tests only.
;;;
(defmethod test-table (spec (type (eql :sqlite)))
  (when spec
    (let ((db (clsql:connect spec :database-type type :if-exists :new)))
      (unwind-protect
	   (progn
	     (create-test-table db)
	     (dolist (row (query "select * from test_clsql" :database db :result-types nil))
	       (test-table-row row nil type))
	     (loop for row across (map-query 'vector #'list "select * from test_clsql" 
					     :database db :result-types nil)
		   do (test-table-row row nil type))
	     (loop for row in (map-query 'list #'list "select * from test_clsql" 
					 :database db :result-types nil)
		   do (test-table-row row nil type))

	     (do-query ((int float bigint str) "select * from test_clsql")
	       (test-table-row (list int float bigint str) nil type))
	     (drop-test-table db)
	     )
	(disconnect :database db)))))

(defun mysql-low-level (specs)
  #-clisp
  (let ((spec (mysql-spec specs)))
    (when spec
      (let ((db (clsql-mysql::database-connect spec :mysql)))
	(clsql-mysql::database-execute-command "DROP TABLE IF EXISTS test_clsql" db)
	(clsql-mysql::database-execute-command 
	 "CREATE TABLE test_clsql (i integer, sqrt double, sqrt_str CHAR(20))" db)
	(dotimes (i 10)
	  (clsql-mysql::database-execute-command
	   (format nil "INSERT INTO test_clsql VALUES (~d,~d,'~a')"
		   i (clsql-base:number-to-sql-string (sqrt i))
		   (clsql-base:number-to-sql-string (sqrt i)))
	   db))
	(let ((res (clsql-mysql::database-query-result-set "select * from test_clsql" db :full-set t :result-types nil)))
	  (test (mysql:mysql-num-rows
		 (clsql-mysql::mysql-result-set-res-ptr res))
		10
		:test #'eql
		:fail-info "Error calling mysql-num-rows")
	  (clsql-mysql::database-dump-result-set res db))
	(clsql-mysql::database-execute-command "DROP TABLE test_clsql" db)
	(clsql-mysql::database-disconnect db)))))



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
	       (clsql-base:number-to-sql-string test-flt)
	       (transform-bigint-1 test-int)
	       (clsql-base:number-to-sql-string test-flt)
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
    (unless (eq db-type :sqlite)		; SQLite is typeless.
      (test (transform-float-1 int)
	    float
	    :test #'eql
	    :fail-info 
	    (format nil "Wrong float value ~A for int ~A (row ~S)" float int row)))
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

(defun db-type-spec (db-type specs)
  (let ((accessor (intern (concatenate 'string (symbol-name db-type)
				       (symbol-name '#:-spec))
			  (find-package '#:clsql-classic-tests))))
    (funcall accessor specs)))

(defun db-type-ensure-system (db-type)
  (unless (find-package (symbol-name db-type))
    (asdf:operate 'asdf:load-op
		  (intern (concatenate 'string
				       (symbol-name '#:clsql-)
				       (symbol-name db-type))))))

(defun run-tests ()
  (let ((specs (read-specs)))
    (unless specs
      (warn "Not running test because test configuration file is missing")
      (return-from run-tests :skipped))
    (mysql-low-level specs)
    (with-tests (:name "CLSQL")
      (dolist (db-type +all-db-types+)
	(let ((spec (db-type-spec db-type specs)))
	  (when spec
	    (db-type-ensure-system db-type)
	    (ignore-errors (destroy-database spec db-type))
	    (ignore-errors (create-database spec db-type))
	    (test-table spec db-type))))))
  t)
