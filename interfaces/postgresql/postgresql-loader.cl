;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          postgresql-loader.sql
;;;; Purpose:       PostgreSQL library loader using UFFI
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  Feb 2002
;;;;
;;;; $Id: postgresql-loader.cl,v 1.7 2002/05/13 22:05:21 kevin Exp $
;;;;
;;;; This file, part of CLSQL, is Copyright (c) 2002 by Kevin M. Rosenberg
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(declaim (optimize (debug 3) (speed 3) (safety 1) (compilation-speed 0)))
(in-package :postgresql)


(defvar *postgresql-supporting-libraries* '("crypt" "c")
  "Used only by CMU. List of library flags needed to be passed to ld to
load the PostgresSQL client library succesfully.  If this differs at your site,
set to the right path before compiling or loading the system.")

(defvar *postgresql-library-loaded* nil
  "T if foreign library was able to be loaded successfully")

(defmethod clsql-sys:database-type-library-loaded ((database-type
						    (eql :postgresql)))
  *postgresql-library-loaded*)
				      
(defmethod clsql-sys:database-type-load-foreign ((database-type
						  (eql :postgresql)))
  (when
      (uffi:load-foreign-library 
       (uffi:find-foreign-library "libpq"
				  '("/opt/postgresql/lib/" "/usr/local/lib/" 
				    "/usr/lib/" "/postgresql/lib/"
				    "/usr/local/pgsql/lib/" "/usr/lib/pgsql/"
				    "/opt/pgsql/lib/pgsql")
				  :drive-letters '("C" "D" "E"))
       
       :module "postgresql"
       :supporting-libraries 
       *postgresql-supporting-libraries*)
    (setq *postgresql-library-loaded* t)))

(clsql-sys:database-type-load-foreign :postgresql)
(when (clsql-sys:database-type-library-loaded :postgresql)
  (clsql-sys:initialize-database-type :database-type :postgresql)
  (pushnew :postgresql cl:*features*))
