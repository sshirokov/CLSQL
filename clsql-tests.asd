;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; File:    clsql-tests.asd
;;;; Author:  Marcus Pearce <m.t.pearce@city.ac.uk>
;;;; Created: 30/03/2004
;;;; Updated: <04/04/2004 12:34:41 marcusp>
;;;;
;;;; $Id: clsql-classic.asd 8847 2004-04-07 14:38:14Z kevin $
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************
;;;; ======================================================================

(in-package #:cl-user)

(asdf:defsystem clsql-tests
    :name "CLSQL Tests"
    :author ""
    :maintainer ""
    :version ""
    :licence ""
    :description "A regression test suite for CLSQL-USQL."
    :components 
    ((:module tests
	      :serial t
	      :components ((:file "package")
			   (:file "test-init")
			   (:file "test-connection")
			   (:file "test-fddl")
			   (:file "test-fdml")
			   (:file "test-ooddl")
			   (:file "test-oodml")
			   (:file "test-syntax"))))
    :depends-on (:clsql :rt))
