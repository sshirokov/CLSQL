;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          clsql-base.asd
;;;; Purpose:       ASDF definition file for Base CLSQL
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  Feb 2002
;;;;
;;;; $Id$
;;;;
;;;; This file, part of CLSQL, is Copyright (c) 2002 by Kevin M. Rosenberg
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(eval-when (:compile-toplevel)
  (declaim (optimize (debug 3) (speed 3) (safety 1) (compilation-speed 0))))

(defpackage #:clsql-base-system (:use #:asdf #:cl))
(in-package #:clsql-base-system)

#+(or allegro lispworks cmu sbcl openmcl mcl scl)
(defsystem clsql-base
  :name "cl-sql-base"
  :author "Kevin Rosenberg <kevin@rosenberg.net>"
  :maintainer "Kevin M. Rosenberg <kmr@debian.org>"
  :licence "Lessor Lisp General Public License"
  :description "Common Lisp SQL Base Package"
  :long-description "cl-sql-base package provides the low-level interface for the database drivers."

  :components
  ((:module :base
	    :components
	    ((:file "cmucl-compat")
	     (:file "package")
	     (:file "utils" :depends-on ("package"))
	     (:file "classes" :depends-on ("package"))
	     (:file "conditions" :depends-on ("classes"))
	     (:file "db-interface" :depends-on ("conditions"))
	     (:file "initialize" :depends-on ("db-interface"))
	     (:file "loop-extension" :depends-on ("db-interface"))
	     (:file "time" :depends-on ("package"))
	     (:file "database" :depends-on ("initialize"))
	     (:file "recording" :depends-on ("time" "database"))
	     (:file "basic-sql" :depends-on ("database"))
	     (:file "pool" :depends-on ("basic-sql"))
	     (:file "transaction" :depends-on ("basic-sql"))
	     ))))

