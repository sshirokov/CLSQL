;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          clsql.system
;;;; Purpose:       Defsystem-3/4 for CLSQL
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  Feb 2002
;;;;
;;;; $Id: clsql.asd,v 1.3 2002/08/23 19:39:56 kevin Exp $
;;;;
;;;; This file, part of CLSQL, is Copyright (c) 2002 by Kevin M. Rosenberg
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(declaim (optimize (debug 3) (speed 3) (safety 1) (compilation-speed 0)))

(in-package :asdf)

#-clsql-base
(let ((path (make-pathname :name "clsql-base" :type "system"
				      :defaults *load-truename*)))
  (when (probe-file path)
    (load path)))

;;; System definitions

(unless (ignore-errors (find-class 'clsql-cl-source-file))
  (defclass clsql-cl-source-file (cl-source-file) ())
  (defmethod source-file-type ((c clsql-cl-source-file) (s module)) 
    "cl"))

(defsystem clsql
  :default-component-class clsql-cl-source-file
  :pathname #.(format nil "~A:clsql;" +clsql-logical-host+)
  :perform (load-op :after (op clsql)
		    (pushnew :clsql cl:*features*))
  :components ((:file "package")
	       (:file "pool" :depends-on ("package"))
	       (:file "loop-extension")
	       (:file "sql" :depends-on ("pool"))
		 (:file "transactions" :depends-on ("sql"))
		 (:file "functional" :depends-on ("sql"))
		 (:file "usql" :depends-on ("sql")))
  :depends-on (:clsql-base)
  )
