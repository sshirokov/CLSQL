;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:    test-basic.lisp
;;;; Purpose: Tests for clsql-base and result types
;;;; Author:  Kevin M. Rosenberg
;;;; Created: Mar 2002
;;;;
;;;; $Id: tests.lisp 8926 2004-04-10 21:12:52Z kevin $
;;;;
;;;; This file, part of CLSQL, is Copyright (c) 2002-2004 by Kevin M. Rosenberg
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(in-package #:clsql-tests)

(defun test-basic-initialize ()
  (ignore-errors
   (clsql:execute-command "DROP TABLE TYPE_TABLE"))
  (clsql:execute-command 
   "CREATE TABLE TYPE_TABLE (t_int integer, t_float double precision, t_bigint BIGINT, t_str VARCHAR(30))")
  (dotimes (i 11)
    (let* ((test-int (- i 5))
	   (test-flt (transform-float-1 test-int)))
      (clsql:execute-command
       (format nil "INSERT INTO TYPE_TABLE VALUES (~a,~a,~a,'~a')"
	       test-int
	       (clsql-base:number-to-sql-string test-flt)
	       (transform-bigint-1 test-int)
	       (clsql-base:number-to-sql-string test-flt)
	       )))))

(defun test-basic-forms ()
  nil)

(defun test-basic-forms-untyped ()
  nil)


(defun %test-basic-forms ()
  (dolist (row (query "select * from TYPE_TABLE" :result-types :auto))
    (test-table-row row :auto))
  (dolist (row (query "select * from TYPE_TABLE" :result-types nil))
    (test-table-row row nil))
  (loop for row across (map-query 'vector #'list "select * from TYPE_TABLE" 
				  :result-types :auto)
	do (test-table-row row :auto))
  (loop for row across (map-query 'vector #'list "select * from TYPE_TABLE" 
				  :result-types nil)
	do (test-table-row row nil))
  (loop for row in (map-query 'list #'list "select * from TYPE_TABLE" 
			      :result-types nil)
	do (test-table-row row nil))
  (loop for row in (map-query 'list #'list "select * from TYPE_TABLE" 
			      :result-types :auto)
	do (test-table-row row :auto))
  (test (map-query nil #'list "select * from TYPE_TABLE" 
		   :result-types :auto)
	nil
	:fail-info "Expected NIL result from map-query nil")
  (do-query ((int float bigint str) "select * from TYPE_TABLE")
    (test-table-row (list int float bigint str) nil))
  (do-query ((int float bigint str) "select * from TYPE_TABLE" :result-types :auto)
    (test-table-row (list int float bigint str) :auto)))


(defun %test-basic-forms-untyped ()
  (dolist (row (query "select * from TYPE_TABLE" :result-types nil))
    (test-table-row row nil))
  (loop for row across (map-query 'vector #'list "select * from TYPE_TABLE" 
				  :result-types nil)
	do (test-table-row row nil))
  (loop for row in (map-query 'list #'list "select * from TYPE_TABLE" 
			      :result-types nil)
	do (test-table-row row nil))
  
  (do-query ((int float bigint str) "select * from TYPE_TABLE")
    (test-table-row (list int float bigint str) nil)))


;;;; Testing functions

(defun transform-float-1 (i)
  (coerce (* i (abs (/ i 2)) (expt 10 (* 2 i))) 'double-float))

(defun transform-bigint-1 (i)
  (* i (expt 10 (* 3 (abs i)))))



(defun parse-double (num-str)
  (let ((*read-default-float-format* 'double-float))
    (coerce (read-from-string num-str) 'double-float)))

(defun test-table-row (row types)
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
		  (or (member *test-database-type* 
			      '(:odbc :aodbc))  ;; aodbc considers bigints as strings
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
	(when (stringp int)
	  (setq int (parse-integer int)))
	(setq bigint (parse-integer bigint))
	(when (stringp float)
	  (setq float (parse-double float))))
       ((listp types)
	(error "NYI")
	)
       (t 
	(test t nil
	      :fail-info
	      (format nil "Invalid types field (~S) passed to test-table-row" types))))
    (unless (eq *test-database-type* :sqlite)		; SQLite is typeless.
      (test (transform-float-1 int)
	    float
	    :test #'double-float-equal
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
