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

(defparameter *oracle-lib-path*
    (let ((oracle-home (getenv "ORACLE_HOME")))
      (when oracle-home
	(make-pathname :directory 
		       (append 
			(pathname-directory
			 (parse-namestring (concatenate 'string oracle-home "/")))
			(list "lib"))))))

(defparameter *clsql-oracle-library-path* 
    (uffi:find-foreign-library
     '("libclntsh" "oracle") 
     `(,@(when *load-truename* (list (make-pathname :directory (pathname-directory *load-truename*))))
       ,@(when *oracle-lib-path* (list *oracle-lib-path*))
       "/9i/lib/"
       "/usr/lib/clsql/"
       "/sw/lib/clsql/"
       "/home/kevin/debian/src/clsql/db-oracle/")
     :drive-letters '("C")))

(defvar *oracle-library-candidate-drive-letters* '("C" "D" "E"))

(defvar *oracle-supporting-libraries* '("c")
  "Used only by CMU. List of library flags needed to be passed to ld to
load the Oracle client library succesfully.  If this differs at your site,
set to the right path before compiling or loading the system.")

(defvar *oracle-library-loaded* nil
  "T if foreign library was able to be loaded successfully")

(defmethod clsql-sys:database-type-library-loaded ((database-type (eql :oracle)))
  *oracle-library-loaded*)

(setf *oracle-lib-path* #p"/usr/lib/oracle/10.1.0.2/client/lib/")

(defmethod clsql-sys:database-type-load-foreign ((database-type (eql :oracle)))
  #+ignore
  (uffi:load-foreign-library
   (make-pathname :defaults *oracle-lib-path* :name "libclntsh" :type "so"))
  (uffi:load-foreign-library *clsql-oracle-library-path* 
			     :module "clsql-oracle" 
			     :supporting-libraries *oracle-supporting-libraries*)
  (setq *oracle-library-loaded* t))


(clsql-sys:database-type-load-foreign :oracle)


