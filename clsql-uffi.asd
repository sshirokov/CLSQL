;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          clsql-uffi.asd
;;;; Purpose:       ASDF definition file for CLSQL UFFI Helper package
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  Aug 2002
;;;;
;;;; $Id: clsql-uffi.asd,v 1.12 2002/09/30 10:19:23 kevin Exp $
;;;;
;;;; This file, part of CLSQL, is Copyright (c) 2002 by Kevin M. Rosenberg
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(declaim (optimize (debug 3) (speed 3) (safety 1) (compilation-speed 0)))

(in-package :asdf)

;;; System definition

#+(or allegro lispworks cmu openmcl mcl)
(defsystem :clsql-uffi
  :name "cl-sql-base"
  :author "Kevin M. Rosenberg <kmr@debian.org>"
  :version "0.9.2"
  :maintainer "Kevin M. Rosenberg <kmr@debian.org>"
  :licence "Lessor Lisp General Public License"
  :description "Common UFFI Helper functions for Common Lisp SQL Interface Library"
  :long-description "cl-sql-uffi package provides common helper functions using the UFFI for the CLSQL package."

  :components
  ((:module :uffi
	    :components
	    ((:file "clsql-uffi-package")
	     (:file "clsql-uffi-loader" :depends-on ("clsql-uffi-package"))
	     (:file "clsql-uffi" :depends-on ("clsql-uffi-loader")))))
  :depends-on (:uffi :clsql-base))
