;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          db2-loader.lisp
;;;; Purpose:       Foreign library loader for CLSQL Db2 interface
;;;;
;;;; $Id$
;;;;
;;;; This file is part of CLSQL.
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(in-package #:clsql-db2)

(defparameter *db2-lib-path*
  (let ((db2-home (getenv "DB2_HOME")))
    (when db2-home
      (make-pathname :directory 
		     (append 
		      (pathname-directory
		       (parse-namestring (concatenate 'string db2-home "/")))
			(list "lib"))))))

(defparameter *db2-client-library-path* 
    (uffi:find-foreign-library
     "libdb2"
     `(,@(when *load-truename* (list (make-pathname :directory (pathname-directory *load-truename*))))
       ,@(when *db2-lib-path* (list *db2-lib-path*))
       #+64bit "/opt/IBM/db2/V8.1/lib64/"
       "/opt/IBM/db2/V8.1/lib/")
     :drive-letters '("C")))

(defvar *db2-supporting-libraries* '("c")
  "Used only by CMU. List of library flags needed to be passed to ld to
load the Db2 client library succesfully.  If this differs at your site,
set to the right path before compiling or loading the system.")

(defvar *db2-library-loaded* nil
  "T if foreign library was able to be loaded successfully")

(defmethod clsql-sys:database-type-library-loaded ((database-type (eql :db2)))
  *db2-library-loaded*)

(defmethod clsql-sys:database-type-load-foreign ((database-type (eql :db2)))
  (if (pathnamep *db2-client-library-path*) 
      (progn
	(uffi:load-foreign-library *db2-client-library-path*
				   :module "clsql-db2"
				   :supporting-libraries 
				   *db2-supporting-libraries*)
	(setq *db2-library-loaded* t))
      (warn "Unable to load db2 client library.")))

(clsql-sys:database-type-load-foreign :db2)


