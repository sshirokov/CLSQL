;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          odbc-loader.sql
;;;; Purpose:       ODBC library loader using UFFI
;;;; Programmers:   Kevin M. Rosenberg
;;;; Date Started:  April 2004
;;;;
;;;; $Id: odbc-loader.lisp 8270 2003-11-25 06:37:14Z kevin $
;;;;
;;;; This file, part of CLSQL, is Copyright (c) 2004 by Kevin M. Rosenberg
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(in-package #:odbc)

(defparameter *odbc-library-path* 
  (uffi:find-foreign-library
   '("odbc32" "libodbc" "libiodbc")
   `(,(make-pathname :directory (pathname-directory *load-truename*))
      "/usr/lib/"
      "/sw/lib/"
      "/usr/local/lib/"
      "/home/kevin/debian/src/clsql/db-odbc/"
      "/odbc/lib/opt/")
   :drive-letters '("C")))
  
(defvar *odbc-supporting-libraries* '("c")
  "Used only by CMU. List of library flags needed to be passed to ld to
load the Odbc client library succesfully.  If this differs at your site,
set to the right path before compiling or loading the system.")

(defvar *odbc-library-loaded* nil
  "T if foreign library was able to be loaded successfully")

(defmethod clsql-base-sys:database-type-library-loaded ((database-type (eql :odbc)))
  *odbc-library-loaded*)
				      
(defmethod clsql-base-sys:database-type-load-foreign ((database-type (eql :odbc)))
  (uffi:load-foreign-library *odbc-library-path*) 
  (setq *odbc-library-loaded* t))

(clsql-base-sys:database-type-load-foreign :odbc)



