;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          clsql-mysql.asd
;;;; Purpose:       ASDF definition file for CLSQL MySQL backend
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  Aug 2002
;;;;
;;;; $Id: clsql-mysql.asd,v 1.14 2003/05/07 02:45:08 kevin Exp $
;;;;
;;;; This file, part of CLSQL, is Copyright (c) 2002 by Kevin M. Rosenberg
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(defpackage #:clsql-mysql-system (:use #:asdf #:cl))
(in-package #:clsql-mysql-system)

;;; System definition

#+(or allegro lispworks cmu sbcl openmcl mcl scl)
(defsystem :clsql-mysql
  :name "cl-sql-mysql"
  :author "Kevin M. Rosenberg <kmr@debian.org>"
  :maintainer "Kevin M. Rosenberg <kmr@debian.org>"
  :licence "Lessor Lisp General Public License"
  :description "Common Lisp SQL MySQL Driver"
  :long-description "cl-sql-mysql package provides a database driver to the MySQL database system."

  :components
  ((:module :db-mysql
	    :components
	    ((:file "mysql-package")
	     (:file "mysql-loader" :depends-on ("mysql-package"))
	     (:file "mysql-api" :depends-on ("mysql-loader"))
	     (:file "mysql-sql" :depends-on ("mysql-api"))
	     (:file "mysql-usql" :depends-on ("mysql-sql")))))
  :depends-on (:uffi :clsql-base :clsql-uffi))
