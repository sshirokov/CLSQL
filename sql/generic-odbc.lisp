;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;;
;;;; $Id$
;;;;
;;;; Generic ODBC layer, used by db-odbc and db-aodbc backends
;;;;
;;;; This file is part of CLSQL.
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(in-package #:clsql-sys)

(defclass generic-odbc-database (database)
  ()
  (:documentation "Encapsulate same behavior across odbc and aodbc backends."))

(defmethod read-sql-value (val (type (eql 'boolean))
			   (database generic-odbc-database)
			   (db-type (eql :postgresql)))
  (if (string= "0" val) nil t))

