;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:     clsql.asd
;;;; Purpose:  System definition for CLSQL-CLASSIC
;;;; Authors:  Marcus Pearce and Kevin M. Rosenberg
;;;; Created:  March 2004
;;;;
;;;; $Id$
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(in-package #:cl-user)
(defpackage #:clsql-system (:use #:asdf #:cl))
(in-package #:clsql-system)

(defsystem #:clsql
    :name "CLSQL"
    :author "Kevin Rosenberg <kevin@rosenberg.net>"
    :maintainer "Kevin M. Rosenberg <kmr@debian.org>"
    :licence "Lessor Lisp General Public License"
    :description "Common Lisp SQL Interface library"
    :long-description "A Common Lisp interface to SQL RDBMS based on
the Xanalys CommonSQL interface for Lispworks. It depends on the
low-level database interfaces as well as a functional and an object
oriented interface."
    :components
    ((:module sql
	      :components
	      ((:module :base
			:pathname ""
			:components
			((:file "cmucl-compat")
			 (:file "package")
			 (:file "utils" :depends-on ("package" "db-interface"))
			 (:file "base-classes" :depends-on ("package"))
			 (:file "conditions" :depends-on ("base-classes"))
			 (:file "db-interface" :depends-on ("conditions"))
			 (:file "initialize" :depends-on ("db-interface" "utils"))
			 (:file "loop-extension" :depends-on ("db-interface"))
			 (:file "time" :depends-on ("package"))
			 (:file "database" :depends-on ("initialize"))
			 (:file "recording" :depends-on ("time" "database"))
			 (:file "basic-sql" :depends-on ("database" "cmucl-compat"))
			 (:file "pool" :depends-on ("basic-sql"))
			 (:file "transaction" :depends-on ("basic-sql"))
			 (:file "kmr-mop" :depends-on ("package"))))
	       (:module :core
			:pathname ""
			:components ((:file "generics")
				     (:file "classes" :depends-on ("generics"))
				     (:file "operations" :depends-on ("classes"))
				     (:file "syntax" :depends-on ("operations")))
			:depends-on (:base))
	       (:module :functional
			:pathname ""
			:components ((:file "sql")
				     (:file "table" :depends-on ("sql")))
			:depends-on (:core))
	       (:module :object
			:pathname ""
		       :components ((:file "metaclasses")
				    (:file "objects" :depends-on ("metaclasses")))
		       :depends-on (:functional))
	       (:module :generic
			:pathname ""
		       :components ((:file "generic-postgresql")
				    (:file "generic-odbc"))
		       :depends-on (:functional))))))
     

(defmethod perform ((o test-op) (c (eql (find-system 'clsql))))
  (operate 'load-op 'clsql-tests)
  (operate 'test-op 'clsql-tests :force t))
