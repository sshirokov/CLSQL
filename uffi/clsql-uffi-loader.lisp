;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:     clsql-uffi-loader.sql
;;;; Purpose:  Library loader using CLSQL UFFI helper library
;;;; Author:   Kevin M. Rosenberg
;;;; Created:  Mar 2002
;;;;
;;;; $Id$
;;;;
;;;; This file, part of CLSQL, is Copyright (c) 2002-2004 by Kevin M. Rosenberg
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(in-package #:clsql-uffi)

(defun find-and-load-foreign-library (filenames &key module supporting-libraries (errorp t))
  (setq filenames (if (listp filenames) filenames (list filenames))
        filenames
          (append
           (loop for search-path in clsql:*foreign-library-search-paths*
                 nconc (loop for filename in filenames
                             collect (merge-pathnames filename search-path)))
           filenames))
  (or (loop for type in (uffi:foreign-library-types)
            for suffix = (make-pathname :type type)
            thereis (loop for filename in filenames
                          thereis (handler-case
                                    (uffi:load-foreign-library (merge-pathnames filename suffix)
                                                               :module module
                                                               :supporting-libraries supporting-libraries)
                                    (error (c)
                                      (warn "~A" c)
                                      nil))))
      (when errorp
        (error "Couldn't load foreign librar~@P ~{~S~^, ~}."
               (length filenames) filenames))))

(defvar *clsql-uffi-library-filenames*
    (list #+(or 64bit x86-64) "clsql_uffi64"
          #+(or 64bit x86-64) (make-pathname :name "clsql_uffi64"
                                             :directory clsql-uffi-system::*library-file-dir*)
          "clsql_uffi"
          (make-pathname :name "clsql_uffi"
                         :directory clsql-uffi-system::*library-file-dir*)))

(defvar *clsql-uffi-supporting-libraries* '("c")
  "Used only by CMU. List of library flags needed to be passed to ld to
load the MySQL client library succesfully.  If this differs at your site,
set to the right path before compiling or loading the system.")

(defvar *uffi-library-loaded* nil
  "T if foreign library was able to be loaded successfully")

(defun load-uffi-foreign-library ()
  (find-and-load-foreign-library *clsql-uffi-library-filenames*
                                 :module "clsql-uffi"
                                 :supporting-libraries
                                 *clsql-uffi-supporting-libraries*)
  (setq *uffi-library-loaded* t))

(load-uffi-foreign-library)

