;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          clsql-base.asd
;;;; Purpose:       ASDF definition file for Base CLSQL
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  Feb 2002
;;;;
;;;; $Id: clsql-base.asd,v 1.11 2002/09/18 07:50:01 kevin Exp $
;;;;
;;;; This file, part of CLSQL, is Copyright (c) 2002 by Kevin M. Rosenberg
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(in-package :asdf)

(defsystem clsql-base
    :perform (load-op :after (op clsql-base)
		      (pushnew :clsql-base cl:*features*))
    :components
    ((:module :base
	      :components
	      ((:file "cmucl-compat")
	       (:file "package")
	       (:file "utils" :depends-on ("package"))
	       (:file "classes" :depends-on ("package"))
	       (:file "conditions" :depends-on ("classes"))
	       (:file "db-interface" :depends-on ("conditions"))
	       (:file "initialize" :depends-on ("db-interface"))))))

(defmethod source-file-type  ((c cl-source-file)
			      (s (eql (find-system :clsql-base)))) 
   "cl")

