;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name: oracle-objects.lisp
;;;;
;;;; $Id$
;;;;
;;;; This file is part of CLSQL.
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(in-package #:clsql-oracle)

(defparameter *oracle-default-varchar2-length* "512")

(defmethod database-get-type-specifier (type args database (db-type (eql :oracle)))
  (declare (ignore type args database))
  (concatenate 'string "VARCHAR2(" *oracle-default-varchar2-length* ")"))

(defmethod database-get-type-specifier ((type (eql 'integer)) args 
					database (db-type (eql :oracle)))
  (declare (ignore database))
  (if args
      (format nil "NUMBER(~A,~A)"
	      (or (first args) 38) (or (second args) 0))
    "INTEGER"))

(defmethod database-get-type-specifier ((type (eql 'bigint)) args
					database (db-type (eql :oracle)))
  (declare (ignore database)) 
  (if args
      (format nil "NUMBER(~A,~A)"
	      (or (first args) 38) (or (second args) 0))
    "NUMBER(38,0)"))

(defmethod database-get-type-specifier ((type (eql 'simple-base-string)) args
					database (db-type (eql :oracle)))
  (declare (ignore database)) 
  (if args
      (format nil "VARCHAR2(~A)" (car args))
    (concatenate 'string "VARCHAR2(" *oracle-default-varchar2-length* ")")))

(defmethod database-get-type-specifier ((type (eql 'simple-string)) args
					database (db-type (eql :oracle)))
  (declare (ignore database)) 
  (if args
      (format nil "VARCHAR2(~A)" (car args))
    (concatenate 'string "VARCHAR2(" *oracle-default-varchar2-length* ")")))

(defmethod database-get-type-specifier ((type (eql 'string)) args
					database (db-type (eql :oracle)))
  (declare (ignore database)) 
  (if args
      (format nil "VARCHAR2(~A)" (car args))
    (concatenate 'string "VARCHAR2(" *oracle-default-varchar2-length* ")")))

(defmethod database-get-type-specifier ((type (eql 'raw-string)) args
					database (db-type (eql :oracle)))
  (declare (ignore database)) 
  (if args
      (format nil "VARCHAR2(~A)" (car args))
    (concatenate 'string "VARCHAR2(" *oracle-default-varchar2-length* ")")))

(defmethod database-get-type-specifier ((type (eql 'float)) args
					database (db-type (eql :oracle)))
  (declare (ignore database)) 
  (if args
      (format nil "NUMBER(~A,~A)" (or (first args) 38) (or (second args) 38))
    "double precision"))

(defmethod database-get-type-specifier ((type (eql 'long-float)) args
					database (db-type (eql :oracle)))
  (declare (ignore database)) 
  (if args
      (format nil "NUMBER(~A,~A)"
	      (or (first args) 38) (or (second args) 38))
    "double precision"))

(defmethod database-get-type-specifier ((type (eql 'boolean)) args
					database (db-type (eql :oracle)))
  (declare (ignore args database))
  "CHAR(1)")

(defmethod read-sql-value (val type
			   database (db-type (eql :oracle)))
  ;;(format t "value is \"~A\" of type ~A~%" val (type-of val))
  (declare (ignore type database))
  (etypecase val
    (string
     (read-from-string val))
    (symbol
     nil)))

(defmethod read-sql-value (val (type (eql 'integer))
			   database (db-type (eql :oracle)))
  (declare (ignore database))
  val)

(defmethod read-sql-value (val (type (eql 'float))
			   database (db-type (eql :oracle)))
  (declare (ignore database))
  val)

(defmethod read-sql-value (val (type (eql 'boolean))
			   database (db-type (eql :oracle)))
  (declare (ignore database))
  (when (char-equal #\t (schar val 0))
    t))

(defmethod database-get-type-specifier ((type (eql 'wall-time)) args
					database (db-type (eql :oracle)))
  (declare (ignore args database))
  "DATE")

(defmethod database-get-type-specifier ((type (eql 'duration)) args
					database (db-type (eql :oracle)))
  (declare (ignore args database))
  "NUMBER(38)")
