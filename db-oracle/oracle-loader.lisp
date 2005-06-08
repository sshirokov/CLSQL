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

(defparameter *oracle-client-library-filenames*
  (list* "libclntsh" "oci"
         (when *oracle-home*
           (loop for dir-name in '("lib" "bin")
                 nconc (loop for lib-name in '("libclntsh" "oci")
                             collect (make-pathname :defaults lib-name
                                                    :directory (append (pathname-directory *oracle-home*)
                                                                       (list dir-name))))))))

(defvar *oracle-supporting-libraries* '("c")
  "Used only by CMU. List of library flags needed to be passed to ld to
load the Oracle client library succesfully.  If this differs at your site,
set to the right path before compiling or loading the system.")

(defvar *oracle-library-loaded* nil
  "T if foreign library was able to be loaded successfully")

(defmethod clsql-sys:database-type-library-loaded ((database-type (eql :oracle)))
  *oracle-library-loaded*)

(defmethod clsql-sys:database-type-load-foreign ((database-type (eql :oracle)))
  (clsql-uffi:find-and-load-foreign-library *oracle-client-library-filenames*
                                            :module "clsql-oracle"
                                            :supporting-libraries *oracle-supporting-libraries*)
  (setq *oracle-library-loaded* t))

(clsql-sys:database-type-load-foreign :oracle)


