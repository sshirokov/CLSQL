;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          clsql-uffi-loader.sql
;;;; Purpose:       library loader using CLSQL UFFI helper library
;;;; Programmers:   Kevin M. Rosenberg
;;;; Date Started:  Mar 2002
;;;;
;;;; $Id: clsql-uffi-loader.cl,v 1.2 2002/09/26 18:18:39 kevin Exp $
;;;;
;;;; This file, part of CLSQL, is Copyright (c) 2002 by Kevin M. Rosenberg
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(declaim (optimize (debug 3) (speed 3) (safety 1) (compilation-speed 0)))
(in-package :clsql-uffi)

(defvar *clsql-uffi-library-filename* 
  (uffi:find-foreign-library
   "clsql-uffi"
   `(,(directory-namestring
       (translate-logical-pathname 
	"cl-library:clsql-uffi;"))
     "/usr/lib/clsql/"
     "/opt/lisp/clsql/uffi/"
     "/home/kevin/debian/src/clsql/uffi/")
   :drive-letters '("C" "D" "E" "F" "G")))

(defvar *clsql-uffi-supporting-libraries* '("c")
  "Used only by CMU. List of library flags needed to be passed to ld to
load the MySQL client library succesfully.  If this differs at your site,
set to the right path before compiling or loading the system.")

(defvar *uffi-library-loaded* nil
  "T if foreign library was able to be loaded successfully")

(defun load-uffi-foreign-library ()
  (if (uffi:load-foreign-library *clsql-uffi-library-filename* 
				 :module "clsql-uffi" 
				 :supporting-libraries 
				 *clsql-uffi-supporting-libraries*)
      (setq *uffi-library-loaded* t)
    (warn "Unable to load helper library ~A" *clsql-uffi-library-filename*)))

(load-uffi-foreign-library)



