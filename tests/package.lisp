;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          package.lisp
;;;; Purpose:       Package file clsql testing suite
;;;; Author:        Kevin M. Rosenberg
;;;; Date Started:  Apr 2003
;;;;
;;;; $Id: package.lisp,v 1.1 2003/05/02 03:08:58 kevin Exp $
;;;; *************************************************************************

(defpackage #:clsql-tests
  (:use #:asdf #:cl #:clsql #:rtest #:util.test))

(in-package #:clsql-tests)

(setf *catch-errors* nil)

(rem-all-tests)
