;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          package.lisp
;;;; Purpose:       Package definition for CLSQL-CLASSIC high-level interface
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
  (defpackage #:clsql-classic-sys
    (:nicknames #:clsql-classic)
    (:use #:cl #:clsql-base)
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
	 #:clsql-access-error
	 #:clsql-access-error-database-type
	 #:clsql-access-error-connection-spec
	 #:clsql-access-error-error
	 #:clsql-connect-error
	 #:clsql-connect-error-errno
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
	 #:database-type
	 #:is-database-open
	 #:database-name-from-spec
	 
	 ;; utils.lisp
	 #:number-to-sql-string
	 #:float-to-sql-string
	 #:sql-escape-quotes

	 ;; database.lisp -- Connection
	 #:*default-database-type*	          ; clsql-base xx
	 #:*default-database*	          ; classes    xx
	 #:connect			          ; database   xx
	 #:*connect-if-exists*	          ; database   xx
	 #:connected-databases	          ; database   xx
	 #:database		          ; database   xx
	 #:database-name                     ; database   xx
	 #:disconnect		          ; database   xx
	 #:reconnect                         ; database
	 #:find-database                     ; database   xx
	 #:status                            ; database   xx
	 #:with-database
	 #:with-default-database
	 #:create-database
	 #:destroy-database
	 #:probe-database

	 ;; basic-sql.lisp
	 #:query
	 #:execute-command
	 #:write-large-object
	 #:read-large-object
	 #:delete-large-object
	 #:do-query
	 #:map-query

	 ;; Transactions
	 #:with-transaction
	 #:commit-transaction
	 #:rollback-transaction
	 #:add-transaction-commit-hook
	 #:add-transaction-rollback-hook
	 #:commit                            ; transact   xx
	 #:rollback			  ; transact   xx
	 #:with-transaction		  ; transact   xx		.
	 #:start-transaction                 ; transact   xx
	 #:in-transaction-p                  ; transact   xx
	 #:database-start-transaction
	 #:database-abort-transaction
	 #:database-commit-transaction
	 #:transaction-level
	 #:transaction
	 #:disconnect-pooled
	 ))
    (:export
     ;; sql.cl
     #:for-each-row
     
     ;; Large objects (Marc B)
     #:create-large-object
     #:write-large-object
     #:read-large-object
     #:delete-large-object

     ;; functional.lisp
     ;; These are no longer export since different functions are
     ;; exported by the CLSQL package
     ;; #:insert-records
     ;; #:delete-records
     ;; #:update-records
     
     .
     #1#
     )
    (:documentation "This is the INTERNAL SQL-Interface package of CLSQL-CLASSIC."))
  
  )					;eval-when

(defpackage #:clsql-classic-user
  (:use #:common-lisp #:clsql-classic)
  (:documentation "This is the user package for experimenting with CLSQL-CLASSIC."))
