;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          clsql.system
;;;; Purpose:       Defsystem-3/4 for CLSQL
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  Feb 2002
;;;;
;;;; $Id: clsql.asd,v 1.14 2002/10/14 04:09:02 kevin Exp $
;;;;
;;;; This file, part of CLSQL, is Copyright (c) 2002 by Kevin M. Rosenberg
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(in-package :asdf)

#+(or allegro lispworks cmu sbcl openmcl mcl)
(defsystem :clsql
  :name "cl-sql"
  :author "Kevin M. Rosenberg <kmr@debian.org>"
  :version "0.9.2"
  :maintainer "Kevin M. Rosenberg <kmr@debian.org>"
  :licence "Lessor Lisp General Public License"
  :description "Common Lisp SQL Interface Library"
  :long-description "cl-sql package provides the high-level interface for the CLSQL system."
  
  :perform (load-op :after (op clsql)
		    (pushnew :clsql cl:*features*))
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

#+(or allegro lispworks cmu sbcl openmcl mcl)
(when (ignore-errors (find-class 'load-compiled-op))
  (defmethod perform :after ((op load-compiled-op) (c (eql (find-system :clsql))))
    (pushnew :clsql cl:*features*)))

