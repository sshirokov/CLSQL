;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          clsql-classic-tests.asd
;;;; Purpose:       ASDF system definitionf for clsql testing package
;;;; Author:        Kevin M. Rosenberg
;;;; Date Started:  Apr 2003
;;;;
;;;; $Id$
;;;; *************************************************************************

(in-package #:cl-user)
(defpackage #:clsql-classic-tests-system (:use #:asdf #:cl))
(in-package #:clsql-classic-tests-system)

(defsystem clsql-classic-tests
  :name "clsql-classic-tests"
  :author "Kevin Rosenberg <kevin@rosenberg.net>"
  :maintainer "Kevin M. Rosenberg <kmr@debian.org>"
  :licence "Lessor Lisp General Public License"
  :description "Testing suite for CLSQL"

  :depends-on (clsql ptester #-clisp clsql-mysql)
  :components
  ((:module :classic-tests
	    :components
	    ((:file "package")
;;	     (:file "tables" :depends-on ("package")))
	     (:file "tests" :depends-on ("package")))
	    )))

(defmethod perform ((o test-op) (c (eql (find-system 'clsql-classic-tests))))
  (unless (funcall (intern (symbol-name '#:run-tests)
			   (find-package '#:clsql-classic-tests)))
    (error "test-op failed")))

