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
    :author ""
    :maintainer ""
    :version ""
    :licence ""
    :description "A high level Common Lisp interface to SQL RDBMS."
    :long-description "A high level Common Lisp interface to SQL RDBMS
based on the Xanalys CommonSQL interface for Lispworks. It depends on
the low-level database interfaces provided by CLSQL and includes both
a functional and an object oriented interface."
    :depends-on (clsql-base)
    :components
    ((:module sql
	      :components
	      ((:module :package
			:pathname ""
			:components ((:file "package")
				     (:file "kmr-mop" :depends-on ("package"))))
	       (:module :core
			:pathname ""
			:components ((:file "classes")
				     (:file "operations" :depends-on ("classes"))
				     (:file "syntax"))
			:depends-on (:package))
	       (:module :functional
			:pathname ""
			:components ((:file "sql")
				     (:file "table"))
			:depends-on (:core))
	       (:module :object
			:pathname ""
		       :components ((:file "metaclasses")
				    (:file "objects" :depends-on ("metaclasses")))
		       :depends-on (:functional))))))
     

(defmethod perform ((o test-op) (c (eql (find-system 'clsql))))
  (operate 'load-op 'clsql-tests)
  (operate 'test-op 'clsql-tests))
