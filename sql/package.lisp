;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          package.lisp
;;;; Purpose:       Package definition for CLSQL (high-level) interface
;;;; Programmers:   Kevin M. Rosenberg based on
;;;;                Original code by Pierre R. Mai 
;;;; Date Started:  Feb 2002
;;;;
;;;; $Id$
;;;;
;;;; This file, part of CLSQL, is Copyright (c) 2002-2004 by Kevin M. Rosenberg
;;;; and Copyright (c) 1999-2001 by Pierre R. Mai
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(in-package #:cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defpackage #:clsql-sys
    (:nicknames #:clsql)
    (:use #:cl #:clsql-base-sys)
    (:import-from 
     #:clsql-base
     .
     #1=(
	 #:clsql-condition
	 #:clsql-error
	 #:clsql-simple-error
	 #:clsql-warning
	 #:clsql-simple-warning
	 #:clsql-invalid-spec-error
	 #:clsql-invalid-spec-error-connection-spec
	 #:clsql-invalid-spec-error-database-type
	 #:clsql-invalid-spec-error-template
	 #:clsql-connect-error
	 #:clsql-connect-error-database-type
	 #:clsql-connect-error-connection-spec
	 #:clsql-connect-error-errno
	 #:clsql-connect-error-error
	 #:clsql-sql-error
	 #:clsql-sql-error-database
	 #:clsql-sql-error-expression
	 #:clsql-sql-error-errno
	 #:clsql-sql-error-error
	 #:clsql-database-warning
	 #:clsql-database-warning-database
	 #:clsql-database-warning-message
	 #:clsql-exists-condition
	 #:clsql-exists-condition-new-db
	 #:clsql-exists-condition-old-db
	 #:clsql-exists-warning
	 #:clsql-exists-error
	 #:clsql-closed-error
	 #:clsql-closed-error-database
	 
	 #:*loaded-database-types*
	 #:reload-database-types
	 #:*default-database-type*
	 #:*initialized-database-types*
	 #:initialize-database-type
	 
	 #:database
	 #:database-name
	 #:closed-database
	 #:database-name-from-spec
	 
	 ;; utils.cl
	 #:number-to-sql-string
	 #:float-to-sql-string
	 #:sql-escape-quotes
	 ))
    (:export
     ;; sql.cl
     #:map-query
     #:do-query
     #:for-each-row
     
     ;; functional.cl
     #:insert-records
     #:delete-records
     #:update-records
     #:with-database
     
     ;; Large objects (Marc B)
     #:create-large-object
     #:write-large-object
     #:read-large-object
     #:delete-large-object

     .
     #1#
     )
    (:documentation "This is the INTERNAL SQL-Interface package of CLSQL."))
  
  )					;eval-when

(defpackage #:clsql-user
  (:use #:common-lisp #:clsql)
  (:documentation "This is the user package for experimenting with CLSQL."))
