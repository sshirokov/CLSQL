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

(defmethod database-get-type-specifier
  (type args (database oracle-database))
  (declare (ignore type args))
  (concatenate 'string "VARCHAR2(" *oracle-default-varchar2-length* ")"))

(defmethod database-get-type-specifier
  ((type (eql 'integer)) args (database oracle-database))
  (if args
      (format nil "NUMBER(~A,~A)"
	      (or (first args) 38) (or (second args) 0))
    "NUMBER(38,0)"))

(defmethod database-get-type-specifier
  ((type (eql 'simple-base-string)) args (database oracle-database))
  (if args
      (format nil "VARCHAR2(~A)" (car args))
    (concatenate 'string "VARCHAR2(" *oracle-default-varchar2-length* ")")))

(defmethod database-get-type-specifier
  ((type (eql 'simple-string)) args (database oracle-database))
  (if args
      (format nil "VARCHAR2(~A)" (car args))
    (concatenate 'string "VARCHAR2(" *oracle-default-varchar2-length* ")")))

(defmethod database-get-type-specifier
  ((type (eql 'string)) args (database oracle-database))
  (if args
      (format nil "VARCHAR2(~A)" (car args))
    (concatenate 'string "VARCHAR2(" *oracle-default-varchar2-length* ")"))
  "VARCHAR2(512)")

(defmethod database-get-type-specifier
  ((type (eql 'raw-string)) args (database oracle-database))
  (if args
      (format nil "VARCHAR2(~A)" (car args))
    (concatenate 'string "VARCHAR2(" *oracle-default-varchar2-length* ")"))
  "VARCHAR2(256)")

(defmethod database-get-type-specifier
  ((type (eql 'float)) args (database oracle-database))
  (if args
      (format nil "NUMBER(~A,~A)"
	      (or (first args) 38) (or (second args) 38))
    "NUMBER"))

(defmethod database-get-type-specifier
  ((type (eql 'long-float)) args (database oracle-database))
  (if args
      (format nil "NUMBER(~A,~A)"
	      (or (first args) 38) (or (second args) 38))
    "NUMBER"))

(defmethod database-get-type-specifier
    ((type (eql 'boolean)) args (database oracle-database))
  (declare (ignore args))
  "CHAR(1)")

(defmethod read-sql-value (val type (database oracle-database))
  ;;(format t "value is \"~A\" of type ~A~%" val (type-of val))
  (declare (ignore type))
  (etypecase val
    (string
     (read-from-string val))
    (symbol
     nil)))

(defmethod read-sql-value (val (type (eql 'string)) database)
  (declare (ignore database))
  val)

(defmethod read-sql-value
  (val (type (eql 'integer)) (database oracle-database))
  val)

(defmethod read-sql-value (val (type (eql 'float)) (database oracle-database))
  val)

(defmethod read-sql-value (val (type (eql 'boolean)) (database oracle-database))
  (when (char-equal #\t (schar val 0))
    t))

(defmethod database-get-type-specifier
  ((type (eql 'wall-time)) args (database oracle-database))
  (declare (ignore args))
  "DATE")

(defmethod database-get-type-specifier
  ((type (eql 'duration))
   args
   (database oracle-database))
  (declare (ignore args))
  "NUMBER(38)")
