;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          oracle-loader.lisp
;;;; Purpose:       Foreign library loader for CLSQL Oracle interface
;;;;
;;;; $Id$
;;;;
;;;; This file is part of CLSQL.
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(in-package #:clsql-oracle)

(defparameter *oracle-home*
  (let ((oracle-home (getenv "ORACLE_HOME")))
    (when oracle-home
      (parse-namestring (concatenate 'string oracle-home "/"))))
  "Pathname of ORACLE_HOME as set in user environment.")

(defparameter *oracle-client-library-path* 
    (uffi:find-foreign-library
     '("libclntsh" "oci")
     `(,@(when *load-truename*
	   (list (make-pathname
		  :directory (pathname-directory *load-truename*))))
	 ,@(when *oracle-home*
	     (list
	      (make-pathname :defaults *oracle-home*
			     :directory 
			     (append (pathname-directory *oracle-home*)
				     (list "lib")))
	      (make-pathname :defaults *oracle-home*
			     :directory 
			     (append (pathname-directory *oracle-home*)
				     (list "bin")))))
	 "/usr/lib/oracle/10.1.0.2/client/lib/")
     :drive-letters '("C")))

(defvar *oracle-supporting-libraries* '("c")
  "Used only by CMU. List of library flags needed to be passed to ld to
load the Oracle client library succesfully.  If this differs at your site,
set to the right path before compiling or loading the system.")

(defvar *oracle-library-loaded* nil
  "T if foreign library was able to be loaded successfully")

(defmethod clsql-sys:database-type-library-loaded ((database-type (eql :oracle)))
  *oracle-library-loaded*)

(defmethod clsql-sys:database-type-load-foreign ((database-type (eql :oracle)))
  (if (pathnamep *oracle-client-library-path*) 
      (progn
	(uffi:load-foreign-library *oracle-client-library-path*
				   :module "clsql-oracle"
				   :supporting-libraries 
				   *oracle-supporting-libraries*)
	(setq *oracle-library-loaded* t))
      (warn "Unable to load oracle client library.")))

(clsql-sys:database-type-load-foreign :oracle)


