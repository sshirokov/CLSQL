;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          package.lisp
;;;; Purpose:       Package definition for base (low-level) SQL interface
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

;;;; This file makes the required package definitions for CLSQL's
;;;; core packages.

(eval-when (:compile-toplevel :load-toplevel :execute)
(defpackage #:clsql-base-sys
  (:use #:cl)
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
     #:database-sequence-last
     #:database-set-sequence-position
     #:database-list-attributes
     #:database-list-sequences
     #:database-list-indexes
     #:database-list-views
     
     ;; Support for pooled connections
     #:database-type
     
     ;; Large objects (Marc B)
     #:database-create-large-object
     #:database-write-large-object
     #:database-read-large-object
     #:database-delete-large-object
     
     ;; Shared exports for re-export by CLSQL-BASE
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
         #:clsql-sql-syntax-error
         #:clsql-type-error
         
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

	 ;; accessors for database class
	 #:name
	 #:connection-spec
	 #:transaction
	 #:transaction-level
	 #:conn-pool

	 ;; utils.lisp
	 #:number-to-sql-string
	 #:float-to-sql-string
	 #:sql-escape-quotes
	 
	 ;; time.lisp
	 #:bad-component
	 #:current-day
	 #:current-month
	 #:current-year
	 #:day-duration
	 #:db-timestring
	 #:decode-duration
	 #:decode-time
	 #:duration
	 #:duration+
	 #:duration<
	 #:duration<=
	 #:duration=
	 #:duration>
	 #:duration>=
	 #:duration-day
	 #:duration-hour
	 #:duration-minute
	 #:duration-month
	 #:duration-second
	 #:duration-year
	 #:duration-reduce                
	 #:format-duration
	 #:format-time
	 #:get-time
	 #:interval-clear
	 #:interval-contained
	 #:interval-data
	 #:interval-edit
	 #:interval-end
	 #:interval-match
	 #:interval-push
	 #:interval-relation
	 #:interval-start
	 #:interval-type
	 #:make-duration
	 #:make-interval
	 #:make-time
	 #:merged-time
	 #:midnight
	 #:month-name
	 #:parse-date-time
	 #:parse-timestring
	 #:print-date
	 #:roll
	 #:roll-to
	 #:time
	 #:time+
	 #:time-
	 #:time-by-adding-duration
	 #:time-compare
	 #:time-difference
	 #:time-dow
	 #:time-element
	 #:time-max
	 #:time-min
	 #:time-mjd
	 #:time-msec
	 #:time-p
	 #:time-sec
	 #:time-well-formed
	 #:time-ymd
	 #:time<
	 #:time<=
	 #:time=
	 #:time>
	 #:time>=
	 #:timezone
	 #:universal-time
	 #:wall-time
	 #:wall-timestring
	 #:week-containing

	 ;; recording.lisp -- SQL I/O Recording 
	 #:record-sql-comand
	 #:record-sql-result
	 #:add-sql-stream                 ; recording  xx
	 #:delete-sql-stream	          ; recording  xx
	 #:list-sql-streams	          ; recording  xx
	 #:sql-recording-p	          ; recording  xx
	 #:sql-stream			  ; recording  xx
	 #:start-sql-recording		  ; recording  xx
	 #:stop-sql-recording		  ; recording  xx

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
	 #:disconnect-pooled

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

	 ))
    (:documentation "This is the INTERNAL SQL-Interface package of CLSQL-BASE."))

(defpackage #:clsql-base
    (:import-from #:clsql-base-sys . #1#)
    (:export . #1#)
    (:documentation "This is the SQL-Interface package of CLSQL-BASE."))
);eval-when


