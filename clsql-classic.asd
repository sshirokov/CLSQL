;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          clsql-classic.asd
;;;; Purpose:       System definition for CLSQL-CLASSIC
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

(defpackage #:clsql-classic-system (:use #:asdf #:cl))
(in-package #:clsql-classic-system)

#+(or allegro lispworks cmu sbcl openmcl mcl scl)
(defsystem clsql-classic
  :name "clsql-classic"
  :author "Kevin Rosenberg <kevin@rosenberg.net>"
  :maintainer "Kevin M. Rosenberg <kmr@debian.org>"
  :version "2.1.x"
  :licence "Lessor Lisp General Public License"
  :description "Common Lisp SQL Interface Library"
  :long-description "cl-sql package provides the high-level interface for the CLSQL system."
  
  :depends-on (clsql-base)
  :components
  ((:module :classic
	    :components
	    ((:file "package")
	     (:file "sql" :depends-on ("package"))
	     (:file "functional" :depends-on ("sql"))))))

#+(or allegro lispworks cmu sbcl openmcl mcl scl)
(defmethod perform ((o test-op) (c (eql (find-system 'clsql-classic))))
  (warn "Testing is provided by the CLSQL-TESTS system"))
