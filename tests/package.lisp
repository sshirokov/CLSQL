;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; ======================================================================
;;;; File:    package.lisp
;;;; Author:  Marcus Pearce <m.t.pearce@city.ac.uk>
;;;; Created: 30/03/2004
;;;; Updated: $Id$
;;;; ======================================================================
;;;;
;;;; Description ==========================================================
;;;; ======================================================================
;;;;
;;;; Package definition for CLSQL test suite.
;;;;
;;;; ======================================================================


(in-package #:cl-user)

(defpackage #:clsql-tests
  (:use #:clsql #:common-lisp #:rtest #:ptester)
  (:export #:run-tests #:test-initialise-database #:test-connect-to-database)
  (:documentation "Regression tests for CLSQL."))
