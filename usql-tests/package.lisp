;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; ======================================================================
;;;; File:    package.lisp
;;;; Author:  Marcus Pearce <m.t.pearce@city.ac.uk>
;;;; Created: 30/03/2004
;;;; Updated: <04/04/2004 12:00:14 marcusp>
;;;; ======================================================================
;;;;
;;;; Description ==========================================================
;;;; ======================================================================
;;;;
;;;; Package definition for CLSQL-USQL test suite.
;;;;
;;;; ======================================================================


(in-package #:cl-user)

(defpackage #:clsql-usql-tests
  (:nicknames #:usql-tests)
  (:use #:clsql-usql #:common-lisp #:rtest)
  (:export #:test-usql #:test-initialise-database #:test-connect-to-database)
  (:documentation "Regression tests for CLSQL-USQL."))
