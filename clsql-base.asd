;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          clsql-base.asd
;;;; Purpose:       ASDF definition file for Base CLSQL
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  Feb 2002
;;;;
;;;; $Id: clsql-base.asd,v 1.4 2002/08/23 19:39:56 kevin Exp $
;;;;
;;;; This file, part of CLSQL, is Copyright (c) 2002 by Kevin M. Rosenberg
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(declaim (optimize (debug 3) (speed 3) (safety 1) (compilation-speed 0)))
(in-package :asdf)

;; For use with non-Common Lisp Controller installations
#-common-lisp-controller
(let ((path (make-pathname :name "set-logical" :type "cl"
				      :defaults *load-truename*)))
  (when (probe-file path)
    (load path)
    (set-logical-host-for-pathname 
     "clsql" 
     (make-pathname :host (pathname-host *load-truename*)
		    :device (pathname-device *load-truename*)
		    :directory (pathname-directory *load-truename*)))))

(defconstant +clsql-logical-host+
  #+common-lisp-controller "cl-library"
  #-common-lisp-controller "clsql"
  "Logical hostname for loading system")

(unless (ignore-errors (find-class 'clsql-cl-source-file))
  (defclass clsql-cl-source-file (cl-source-file) ())
  (defmethod source-file-type ((c clsql-cl-source-file) (s module)) 
    "cl"))

 ;;; System definitions

(defsystem clsql-base
  :default-component-class clsql-cl-source-file
  :pathname #.(format nil "~A:clsql-base;" +clsql-logical-host+)
  :perform (load-op :after (op clsql-base)
		    (pushnew :clsql-base cl:*features*))
  :components ((:file "cmucl-compat")
	       (:file "package")
	       (:file "utils" :depends-on ("package"))
	       (:file "classes" :depends-on ("package"))
	       (:file "conditions" :depends-on ("classes"))
	       (:file "db-interface" :depends-on ("conditions"))
	       (:file "initialize" :depends-on ("db-interface")))
  )
