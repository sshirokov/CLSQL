;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; ======================================================================
;;;; File:    clsql-usql-tests.asd
;;;; Author:  Marcus Pearce <m.t.pearce@city.ac.uk>
;;;; Created: 30/03/2004
;;;; Updated: <04/04/2004 12:34:41 marcusp>
;;;; ======================================================================
;;;;
;;;; Description ==========================================================
;;;; ======================================================================
;;;;
;;;; ASDF system definition for CLSQL-USQL test suite.
;;;;
;;;; ======================================================================

(in-package #:cl-user)

(asdf:defsystem :clsql-usql-tests
    :name "CLSQL-USQL Tests"
    :author ""
    :maintainer ""
    :version ""
    :licence ""
    :description "A regression test suite for CLSQL-USQL."
    :components 
    ((:module usql-tests
	      :serial t
	      :components ((:file "package")
			   (:file "test-init")
			   (:file "test-connection")
			   (:file "test-fddl")
			   (:file "test-fdml")
			   (:file "test-ooddl")
			   (:file "test-oodml")
			   (:file "test-syntax"))))
    :depends-on (:clsql-usql :rt))
