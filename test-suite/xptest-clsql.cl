;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          xptest-clsql.cl
;;;; Purpose:       Test of CLSQL using XPTest package
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  Mar 2002
;;;;
;;;; $Id: xptest-clsql.cl,v 1.1 2002/03/27 09:00:15 kevin Exp $
;;;;
;;;; This file, part of CLSQL, is Copyright (c) 2002 by Kevin M. Rosenberg
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(declaim (optimize (debug 3) (speed 3) (safety 1) (compilation-speed 0)))
(in-package :cl-user)
(mk:load-system "XPTest")

(in-package :clsql-user)
(use-package :xptest)

(def-test-fixture clsql-fixture ()
  ((aodbc-spec :accessor aodbc-spec)
   (mysql-spec :accessor mysql-spec)
   (postgresql-spec :accessor postgresql-spec)
   (postgresql-socket-spec :accessor postgresql-socket-spec))
  (:documentation "Test fixture for CLSQL testing"))

(defvar *config-pathname* (make-pathname :name "test-clsql"
					 :type "config"
					 :defaults *load-truename*))
(defmethod setup ((fix clsql-fixture))
  (if (probe-file *config-pathname*)
      (let (config)
	(with-open-file (stream *config-pathname* :direction :input)
	  (setq config (read stream)))
	(setf (aodbc-spec fix) (cadr (assoc :aodbc config)))
	(setf (mysql-spec fix) (cadr (assoc :mysql config)))
	(setf (postgresql-spec fix) (cadr (assoc :postgresql config)))
	(setf (postgresql-socket-spec fix) 
	      (cadr (assoc :postgresql-socket config))))
      (error "XPTest Config file ~S not found" *config-pathname*)))

(defmethod teardown ((fix clsql-fixture))
  t)

(defmethod mysql-test ((test clsql-fixture))
  (let ((spec (mysql-spec test)))
    (when spec
      (let ((db (clsql:connect spec :database-type :mysql 
			       :if-exists :new)))
	(unwind-protect
	     (progn
	       (create-test-table db)
	       (pprint (query "select * from test_clsql" 
				    :database db
				    :types :auto))
	       (pprint (map-query 'vector #'list "select * from test_clsql" 
					:database db
					:types :auto))	
	       (drop-test-table db)
	       )
	  (disconnect :database db))))))

(defparameter clsql-test-suite 
    (make-test-suite
     "CLSQL Test Suite"
     "Basic test suite for database operations."
     ("MySQL Test" 'clsql-fixture
		   :test-thunk 'mysql-test
		   :description "A test of MySQL")))

(report-result (run-test clsql-test-suite) :verbose t)


;;;; Testing functions

(defun create-test-table (db)
  (ignore-errors
    (clsql:execute-command 
     "DROP TABLE test_clsql" :database db))
  (clsql:execute-command 
   "CREATE TABLE test_clsql (n integer, n_pi float, n_pi_str CHAR(20))" 
   :database db)
  (dotimes (i 11)
    (let ((n (- i 5)))
      (clsql:execute-command
       (format nil "INSERT INTO test_clsql VALUES (~a,~a,'~a')"
	       n (clsql:float-to-sql-string (* pi n))
	       (clsql:float-to-sql-string (* pi n)))
       :database db))))

(defun drop-test-table (db)
  (clsql:execute-command "DROP TABLE test_clsql"))
