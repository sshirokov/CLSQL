;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:    odbc-dbi.cl
;;;; Purpose: Mid-level (DBI) interface for CLSQL ODBC backend
;;;; Author:  Kevin M. Rosenberg
;;;; Create:  April 2004
;;;;
;;;; $Id: odbc-sql.lisp 8983 2004-04-12 21:16:48Z kevin $
;;;;
;;;; This file, part of CLSQL, is Copyright (c) 2002 by Kevin M. Rosenberg
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(in-package #:cl-user)

(defpackage #:odbc-dbi
  (:use #:cl #:odbc)
  (:export
   #:bind-parameter
   #:close-query
   #:connect
   #:db-external-format
   #:db-hstmt
   #:db-width
   #:disconnect
   #:end-transaction
   #:fetch-row
   #:list-all-database-tables
   #:list-all-table-columns
   #:loop-over-results
   #:prepare-sql
   #:rr-sql
   #:run-prepared-sql
   #:set-autocommit
   #:sql
   
   #:*auto-trim-strings*
   #:*default-database*
   #:*default-odbc-external-format*
   #:*null-value*
   )
  (:documentation "This is the mid-level interface ODBC.")))

(in-package #:odbc-dbi)

(defun connect (&key user password data-source-name)
  (warn "Not implemented.")
  nil)

(defun disconnect (conn)
  (warn "Not implemented."))  

(defun query (expr &key db result-types row-count column-names)
  (warn "Not implemented."))

(defun execute (expr conn)
  (warn "Not implemented."))

(defun close-query (result-set)
  (warn "Not implemented."))

(defun fetch-row (result-set error-eof eof-value)
  (warn "Not implemented."))
