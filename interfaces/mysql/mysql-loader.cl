;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          mysql-loader.sql
;;;; Purpose:       MySQL library loader using UFFI
;;;; Programmers:   Kevin M. Rosenberg
;;;; Date Started:  Feb 2002
;;;;
;;;; $Id: mysql-loader.cl,v 1.1 2002/03/23 14:04:52 kevin Exp $
;;;;
;;;; This file, part of CLSQL, is Copyright (c) 2002 by Kevin M. Rosenberg
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(declaim (optimize (debug 3) (speed 3) (safety 1) (compilation-speed 0)))
(in-package :mysql)

;;;; Modified by Kevin Rosenberg 
;;;;  - probe potential directories to find library
;;;;  - Changed from CMUCL functions to UFFI to
;;;;      -- prevent library from being loaded multiple times
;;;;      -- support Allegro CL and Lispworks

(defvar *clsql-mysql-library-filename* 
    (translate-logical-pathname 
     #+(or linux unix) "CLSQL:interfaces;mysql;clsql-mysql.so"
     #+(or mswindows win32) "CLSQL:interfaces;mysql;clsql-mysql.dll"
     ))

(defvar *mysql-library-filename* 
    (cond
     ((probe-file "/opt/mysql/lib/mysql/libmysqlclient.so")
      "/opt/mysql/lib/mysql/libmysqlclient.so")
     ((probe-file "/usr/local/lib/libmysqlclient.so")
      "/usr/local/lib/libmysqlclient.so")
     ((probe-file "/usr/lib/libmysqlclient.so")
      "/usr/lib/libmysqlclient.so")
     #+(or win32 mswindows) 
     ((probe-file "c:/mysql/lib/opt/libmysql.dll")
      "c:/mysql/lib/opt/libmysql.dll")
     (t
      (warn "Can't find MySQL client library to load.")))
  "Location where the MySQL client library is to be found.")

(defvar *mysql-supporting-libraries* '("c")
  "Used only by CMU. List of library flags needed to be passed to ld to
load the MySQL client library succesfully.  If this differs at your site,
set to the right path before compiling or loading the system.")



(defmethod database-type-load-foreign ((database-type (eql :mysql)))
  (uffi:load-foreign-library *mysql-library-filename* 
			     :module "mysql" 
                             :supporting-libraries 
			     *mysql-supporting-libraries*)
  (uffi:load-foreign-library *clsql-mysql-library-filename* 
			     :module "clsql-mysql" 
                             :supporting-libraries 
			     (append *mysql-supporting-libraries*)))


(database-type-load-foreign :mysql)


