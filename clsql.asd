;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          clsql.system
;;;; Purpose:       Defsystem-3/4 for CLSQL
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

(defpackage #:clsql-system (:use #:asdf #:cl))
(in-package #:clsql-system)

#+(or allegro lispworks cmu sbcl openmcl mcl scl)
(defsystem clsql
  :name "clsql"
  :author "Kevin Rosenberg <kevin@rosenberg.net>"
  :maintainer "Kevin M. Rosenberg <kmr@debian.org>"
  :version "1.5.x"
  :licence "Lessor Lisp General Public License"
  :description "Common Lisp SQL Interface Library"
  :long-description "cl-sql package provides the high-level interface for the CLSQL system."
  
  :components
  ((:module :sql
	    :components
	    ((:file "package")
	     (:file "pool" :depends-on ("package"))
	     (:file "loop-extension")
	     (:file "sql" :depends-on ("pool"))
	     (:file "transactions" :depends-on ("sql"))
	     (:file "functional" :depends-on ("sql"))
	     (:file "usql" :depends-on ("sql")))))
  :depends-on (:clsql-base)
  )

#+(or allegro lispworks cmu sbcl openmcl mcl scl)
(defmethod perform ((o test-op) (c (eql (find-system :clsql))))
  (oos 'load-op 'clsql-tests)
  (oos 'test-op 'clsql-tests))
