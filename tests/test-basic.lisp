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
  (append
   (test-basic-forms-untyped)
   '(
     (deftest BASIC/TYPE/1
	(let ((results '()))
	  (dolist (row (query "select * from TYPE_TABLE" :result-types :auto)
		    results)
	    (destructuring-bind (int float bigint str) row
	      (push (list (integerp int)
			  (typep float 'double-float)
			  (integerp bigint)
			  (stringp str))
		    results))))
      ((t t t t) (t t t t) (t t t t) (t t t t) (t t t t) (t t t t) (t t t t) (t t t t) (t t t t) (t t t t) (t t t t)))
    

     (deftest BASIC/TYPE/2
	 (let ((results '()))
	   (dolist (row (query "select * from TYPE_TABLE" :result-types :auto)
		     results)
	     (destructuring-bind (int float bigint str) row
	       (push (list (double-float-equal 
			    (transform-float-1 int)
			    float)
			   (double-float-equal
			    (parse-double str)
			    float))
		     results))))
       ((t t) (t t) (t t) (t t) (t t) (t t) (t t) (t t) (t t) (t t) (t t)))
     )))

(defun test-basic-forms-untyped ()
  '((deftest BASIC/SELECT/1
	(let ((rows (query "select * from TYPE_TABLE" :result-types :auto)))
	  (values 
	   (length rows)
	   (length (car rows))))
      11 4)
    
    (deftest BASIC/SELECT/2
	(let ((results '()))
	  (dolist (row (query "select * from TYPE_TABLE" :result-types nil)
		    results)
	    (destructuring-bind (int float bigint str) row
	      (push (list (stringp int)
			  (stringp float)
			  (stringp bigint)
			  (stringp str))
		    results))))
      ((t t t t) (t t t t) (t t t t) (t t t t) (t t t t) (t t t t) (t t t t) (t t t t) (t t t t) (t t t t) (t t t t)))
    
    (deftest BASIC/SELECT/3
	(let ((results '()))
	  (dolist (row (query "select * from TYPE_TABLE" :result-types nil)
		    results)
	    (destructuring-bind (int float bigint str) row
	      (push (list (double-float-equal 
			   (transform-float-1 (parse-integer int))
			   (parse-double float))
			  (double-float-equal
			   (parse-double str)
			   (parse-double float)))
		    results))))
      ((t t) (t t) (t t) (t t) (t t) (t t) (t t) (t t) (t t) (t t) (t t)))

    (deftest BASIC/MAP/1
	(let ((results '())
	      (rows (map-query 'vector #'list "select * from TYPE_TABLE" 
			       :result-types nil)))
	  (dotimes (i (length rows) results)
	    (push
	     (list
	      (listp (aref rows i))
	      (length (aref rows i))
	      (eql (- i 5)
		   (parse-integer (first (aref rows i)) 
				  :junk-allowed nil))
	      (double-float-equal
	       (transform-float-1 (parse-integer (first (aref rows i))))
	       (parse-double (second (aref rows i)))))
	     results)))
      ((t 4 t t) (t 4 t t) (t 4 t t) (t 4 t t) (t 4 t t) (t 4 t t) (t 4 t t) (t 4 t t) (t 4 t t) (t 4 t t) (t 4 t t)))
    
    (deftest BASIC/MAP/2
	(let ((results '())
	      (rows (map-query 'list #'list "select * from TYPE_TABLE" 
			       :result-types nil)))
	  (dotimes (i (length rows) results)
	    (push
	     (list
	      (listp (nth i rows))
	      (length (nth i rows))
	      (eql (- i 5)
		   (parse-integer (first (nth i rows)) 
				  :junk-allowed nil))
	      (double-float-equal
	       (transform-float-1 (parse-integer (first (nth i rows))))
	       (parse-double (second (nth i rows)))))
	     results)))
      ((t 4 t t) (t 4 t t) (t 4 t t) (t 4 t t) (t 4 t t) (t 4 t t) (t 4 t t) (t 4 t t) (t 4 t t) (t 4 t t) (t 4 t t)))

    (deftest BASIC/DO/1
	(let ((results '()))
	  (do-query ((int float bigint str) "select * from TYPE_TABLE")
	    (push (list (double-float-equal 
			 (transform-float-1 (parse-integer int))
			 (parse-double float))
			(double-float-equal
			 (parse-double str)
			 (parse-double float)))
		  results))
	  results)
      ((t t) (t t) (t t) (t t) (t t) (t t) (t t) (t t) (t t) (t t) (t t)))
    ))


;;;; Testing functions

(defun transform-float-1 (i)
  (coerce (* i (abs (/ i 2)) (expt 10 (* 2 i))) 'double-float))

(defun transform-bigint-1 (i)
  (* i (expt 10 (* 3 (abs i)))))

(defun parse-double (num-str)
  (let ((*read-default-float-format* 'double-float))
    (coerce (read-from-string num-str) 'double-float)))

(defun double-float-equal (a b)
  (if (zerop a)
      (if (zerop b)
	  t
	  nil)
      (let ((diff (abs (/ (- a b) a))))
	(if (> diff (* 10 double-float-epsilon))
	    nil
	    t))))
