;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          clsql-tests.asd
;;;; Purpose:       ASDF system definitionf for clsql testing package
;;;; Author:        Kevin M. Rosenberg
;;;; Date Started:  Apr 2003
;;;;
;;;; $Id: clsql-tests.asd,v 1.2 2003/05/07 02:45:08 kevin Exp $
;;;; *************************************************************************

(defpackage #:clsql-tests-system (:use #:asdf #:cl))
(in-package #:clsql-tests-system)

(defsystem clsql-tests
  :name "clsql-tests"
  :author "Kevin Rosenberg <kevin@rosenberg.net>"
  :maintainer "Kevin M. Rosenberg <kmr@debian.org>"
  :licence "Lessor Lisp General Public License"
  :description "Testing suite for CLSQL"

  :depends-on (:clsql :clsql-mysql :clsql-postgresql :clsql-postgresql-socket
		      #+allegro :clsql-aodbc)
  :components
  ((:module tests
	    :components
	    ((:file "rt")
	     (:file "acl-compat-tester")
	     (:file "package" :depends-on ("rt"))
;;	     (:file "tables" :depends-on ("package")))
	     (:file "tests" :depends-on ("package" "acl-compat-tester")))
	    )))

(defmethod perform ((o test-op) (c (eql (find-system :clsql-tests))))
  (or (funcall (intern (symbol-name '#:do-tests)
		       (find-package '#:regression-test)))
      (error "test-op failed")))

