;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          package.cl
;;;; Purpose:       Package definition for high-level SQL interface
;;;; Programmers:   Kevin M. Rosenberg based on
;;;;                Original code by Pierre R. Mai 
;;;; Date Started:  Feb 2002
;;;;
;;;; $Id: package.cl,v 1.6 2002/04/07 15:23:10 kevin Exp $
;;;;
;;;; This file, part of CLSQL, is Copyright (c) 2002 by Kevin M. Rosenberg
;;;; and Copyright (c) 1999-2001 by Pierre R. Mai
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(declaim (optimize (debug 3) (speed 3) (safety 1) (compilation-speed 0)))
(in-package :cl-user)

;;;; This file makes the required package definitions for CLSQL's
;;;; core packages.
;;;; 

(eval-when (:compile-toplevel :load-toplevel :execute)
(defpackage :clsql-sys
  (:use :common-lisp)
  (:export
     ;; "Private" exports for use by interface packages
     #:check-connection-spec
     #:database-type-load-foreign
     #:database-type-library-loaded ;; KMR - Tests if foreign library okay
     #:database-initialize-database-type
     #:database-connect
     #:database-disconnect
     #:database-query
     #:database-execute-command
     #:database-query-result-set
     #:database-dump-result-set
     #:database-store-next-row
     
     ;; For UncommonSQL support
     #:database-list-tables
     #:database-list-attributes
     #:database-attribute-type
     #:database-create-sequence 
     #:database-drop-sequence
     #:database-sequence-next
    
     #:sql-escape
     ;; Shared exports for re-export by CLSQL
     .
     #1=(#:clsql-condition
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
	 #:*connect-if-exists*
	 #:*default-database*
	 #:connected-databases
	 #:database
	 #:database-name
	 #:closed-database
	 #:find-database
	 #:database-name-from-spec
	 #:connect
	 #:disconnect
	 #:query
	 #:execute-command
	 #:map-query
	 #:do-query

	 ;; functional.cl

	 #:insert-records
	 #:delete-records
	 #:update-records
	 #:with-database
	 
	 ;; utils.cl
	 #:number-to-sql-string
	 #:float-to-sql-string
	 #:sql-escape-quotes
	 
	 ;; For UncommonSQL support
	 #:sql-ident
	 #:list-tables
	 #:list-attributes
	 #:attribute-type
	 #:create-sequence 
	 #:drop-sequence
	 #:sequence-next
	 
	 ))
    (:documentation "This is the INTERNAL SQL-Interface package of CLSQL."))

(defpackage #:clsql
    (:import-from #:clsql-sys . #1#)
    (:export . #1#)
    (:documentation "This is the SQL-Interface package of CLSQL."))
);eval-when

(defpackage #:clsql-user
    (:use #:common-lisp #:clsql)
    (:documentation "This is the user package for experimenting with CLSQL."))
