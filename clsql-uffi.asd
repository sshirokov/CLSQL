;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          clsql-uffi.asd
;;;; Purpose:       ASDF definition file for CLSQL UFFI Helper package
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  Aug 2002
;;;;
;;;; $Id: clsql-uffi.asd,v 1.1 2002/08/18 02:57:50 kevin Exp $
;;;;
;;;; This file, part of CLSQL, is Copyright (c) 2002 by Kevin M. Rosenberg
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(declaim (optimize (debug 3) (speed 3) (safety 1) (compilation-speed 0)))

(in-package :asdf)

(unless (find-class 'clsql-cl-source-file)
  (defclass clsql-cl-source-file (cl-source-file) ())
  (defmethod source-file-type ((c clsql-cl-source-file) (s module)) 
    "cl"))

;;; System definition

(defsystem clsql-uffi
    :default-component-class clsql-cl-source-file
    :pathname "cl-library:clsql-uffi;"
    :components ((:file "clsql-uffi-package")
		 (:file "clsql-uffi-loader" :depends-on ("clsql-uffi-package"))
		 (:file "clsql-uffi" :depends-on ("clsql-uffi-loader")))
    :depends-on (:uffi))

