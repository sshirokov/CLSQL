;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          clsql-mysql.asd
;;;; Purpose:       ASDF definition file for CLSQL MySQL backend
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  Aug 2002
;;;;
;;;; $Id: clsql-mysql.asd,v 1.3 2002/09/01 09:00:14 kevin Exp $
;;;;
;;;; This file, part of CLSQL, is Copyright (c) 2002 by Kevin M. Rosenberg
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(declaim (optimize (debug 3) (speed 3) (safety 1) (compilation-speed 0)))

(in-package :asdf)

;;; System definition

(defmethod source-file-type  ((c cl-source-file)
			      (s (eql (find-system 'clsql-mysql)))) 
   "cl")

(defsystem clsql-mysql
  :pathname #.(format nil "~A:clsql-mysql;" +clsql-logical-host+)
  :components ((:file "mysql-package")
	       (:file "mysql-loader" :depends-on ("mysql-package"))
	       (:file "mysql-api" :depends-on ("mysql-loader"))
	       (:file "mysql-sql" :depends-on ("mysql-api"))
		 (:file "mysql-usql" :depends-on ("mysql-sql")))
  :depends-on (:uffi :clsql-base :clsql-uffi))





