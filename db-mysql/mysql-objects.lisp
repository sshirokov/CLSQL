;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:     mysql-objects.lisp
;;;; Purpose:  CLSQL Object layer for MySQL
;;;; Created:  May 2004
;;;;
;;;; $Id: mysql-sql.lisp 9403 2004-05-19 23:46:45Z kevin $
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(in-package #:clsql-mysql)

(defmethod database-get-type-specifier ((type (eql 'wall-time)) args (database mysql-database))
  (declare (ignore args))
  "DATETIME")

(defmethod database-output-sql-as-type ((type (eql 'boolean)) val (database mysql-database))
  (if val 1 0))

(defmethod read-sql-value (val (type (eql 'boolean)) (database mysql-database))
  (etypecase val
    (string (if (string= "0" val) nil t))
    (integer (if (zerop val) nil t))))
