;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          package.lisp
;;;; Purpose:       Package file clsql testing suite
;;;; Author:        Kevin M. Rosenberg
;;;; Date Started:  Apr 2003
;;;;
;;;; $Id: package.lisp,v 1.4 2003/07/20 18:31:22 kevin Exp $
;;;; *************************************************************************

(in-package #:cl-user)

(defpackage #:clsql-tests
  (:use #:asdf #:cl #:clsql #:rtest #:ptester))


