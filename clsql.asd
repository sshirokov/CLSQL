;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          clsql.system
;;;; Purpose:       Defsystem-3/4 for CLSQL
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  Feb 2002
;;;;
;;;; $Id: clsql.asd,v 1.10 2002/09/20 01:40:54 kevin Exp $
;;;;
;;;; This file, part of CLSQL, is Copyright (c) 2002 by Kevin M. Rosenberg
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(in-package :asdf)

(defsystem clsql
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

(defmethod source-file-type  ((c cl-source-file)
			      (s (eql (find-system :clsql)))) 
   "cl")
