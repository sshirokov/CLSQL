;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          clsql-base.asd
;;;; Purpose:       ASDF definition file for Base CLSQL
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  Feb 2002
;;;;
;;;; $Id: clsql-base.asd,v 1.2 2002/08/18 04:08:56 kevin Exp $
;;;;
;;;; This file, part of CLSQL, is Copyright (c) 2002 by Kevin M. Rosenberg
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(declaim (optimize (debug 3) (speed 3) (safety 1) (compilation-speed 0)))
(in-package :asdf)

;; For use with non-Debian installations
(let ((helper-pathname (make-pathname :name "set-cl-library" :type "cl"
				      :defaults *load-truename*)))
  (when (probe-file helper-pathname)
      (load helper-pathname)))

;;; System definitions

(defsystem clsql-base
  :default-component-class clsql-cl-source-file
  :pathname "cl-library:clsql-base;"
  :perform (load-op :after (op clsql-base)
		    (pushnew :clsql cl:*features*))
  :components ((:file "cmucl-compat")
	       (:file "package")
	       (:file "utils" :depends-on ("package"))
	       (:file "classes" :depends-on ("package"))
	       (:file "conditions" :depends-on ("classes"))
	       (:file "db-interface" :depends-on ("conditions"))
	       (:file "initialize" :depends-on ("db-interface")))
  )
