;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; File:     clsql-tests.asd
;;;; Authors:  Marcus Pearce <m.t.pearce@city.ac.uk> and Kevin Rosenberg 
;;;; Created:  30/03/2004
;;;; Updated:  $Id$
;;;;
;;;; $Id$
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************
;;;; ======================================================================

(in-package #:cl-user)
(defpackage #:clsql-classic-tests-system (:use #:asdf #:cl))
(in-package #:clsql-classic-tests-system)

(defsystem clsql-tests
    :name "CLSQL Tests"
    :author ""
    :maintainer ""
    :version ""
    :licence ""
    :description "A regression test suite for CLSQL."
    :depends-on (clsql rt)
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
			   (:file "test-syntax")))))

(defmethod perform ((o test-op) (c (eql (find-system 'clsql-tests))))
  (error "Automated performing of test-op is not yet supported.")
  #+ignore
  (unless (funcall (intern (symbol-name '#:run-tests)
			   (find-package '#:clsql-tests)))
    (error "test-op failed")))
