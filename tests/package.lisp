;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          package.lisp
;;;; Purpose:       Package file clsql testing suite
;;;; Author:        Kevin M. Rosenberg
;;;; Date Started:  Apr 2003
;;;;
;;;; $Id$
;;;; *************************************************************************

(in-package #:cl-user)

(defpackage #:clsql-tests
  (:use #:asdf #:cl #:clsql #:rtest #:ptester))


