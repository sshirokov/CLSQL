;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:     mysql-loader.sql
;;;; Purpose:  MySQL library loader using UFFI
;;;; Author:   Kevin M. Rosenberg
;;;; Created:  Feb 2002
;;;;
;;;; $Id$
;;;;
;;;; This file, part of CLSQL, is Copyright (c) 2002-2004 by Kevin M. Rosenberg
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(in-package #:mysql)

(defparameter *clsql-mysql-library-path* 
  (uffi:find-foreign-library
   "mysql"
   `(,clsql-mysql-system::*library-file-dir*
     "/usr/lib/clsql/"
     "/sw/lib/clsql/")
   :drive-letters '("C")))

(defvar *mysql-library-candidate-names*
    '("libmysqlclient" "libmysql"))

(defparameter *mysql-library-candidate-directories*
    `(,(pathname-directory *load-pathname*)
      "/opt/mysql/lib/mysql/" "/usr/local/lib/"
      #+64bit "/usr/lib64/"
      "/usr/lib/" "/usr/local/lib/mysql/" "/usr/lib/mysql/" "/mysql/lib/opt/" "/sw/lib/mysql/"))

(defvar *mysql-library-candidate-drive-letters* '("C" "D" "E"))

(defvar *mysql-supporting-libraries* '("c")
  "Used only by CMU. List of library flags needed to be passed to ld to
load the MySQL client library succesfully.  If this differs at your site,
set to the right path before compiling or loading the system.")

(defvar *mysql-library-loaded* nil
  "T if foreign library was able to be loaded successfully")

(defmethod clsql-sys:database-type-library-loaded ((database-type (eql :mysql)))
  *mysql-library-loaded*)
				      
(defmethod clsql-sys:database-type-load-foreign ((database-type (eql :mysql)))
  (let ((mysql-path
	 (uffi:find-foreign-library *mysql-library-candidate-names*
				    *mysql-library-candidate-directories*
				    :drive-letters
				    *mysql-library-candidate-drive-letters*)))
    (unless (probe-file mysql-path)
      (error "Can't find mysql client library to load"))
    (uffi:load-foreign-library mysql-path
			       :module "mysql" 
			       :supporting-libraries 
			       *mysql-supporting-libraries*)
    (uffi:load-foreign-library *clsql-mysql-library-path* 
			       :module "clsql-mysql" 
			       :supporting-libraries 
			       (append *mysql-supporting-libraries*)))
  (setq *mysql-library-loaded* t))


(clsql-sys:database-type-load-foreign :mysql)

