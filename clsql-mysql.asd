;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          clsql-mysql.asd
;;;; Purpose:       ASDF definition file for CLSQL MySQL backend
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  Aug 2002
;;;;
;;;; $Id$
;;;;
;;;; This file, part of CLSQL, is Copyright (c) 2002 by Kevin M. Rosenberg
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(defpackage #:clsql-mysql-system (:use #:asdf #:cl))
(in-package #:clsql-mysql-system)

(defvar *library-file-dir* (append (pathname-directory *load-truename*)
				   (list "db-mysql")))

(defclass clsql-mysql-source-file (c-source-file)
  ())

(defmethod output-files ((o compile-op) (c clsql-mysql-source-file))
  (let ((found (some #'(lambda (dir)
			    (probe-file (make-pathname :directory dir
						       :name (component-name c)
						       :type "so")))
			'((:absolute "usr" "lib" "clsql"))))) 
    (list (if found
	      found
	      (make-pathname :name (component-name c)
			     :type "so"
			     :directory *library-file-dir*)))))

(defmethod perform ((o load-op) (c clsql-mysql-source-file))
  nil) ;;; library will be loaded by a loader file

(defmethod perform ((o compile-op) (c clsql-mysql-source-file))
  (unless (zerop (run-shell-command
		  "cd ~A; make"
		  (namestring (make-pathname :name nil
					     :type nil
					     :directory *library-file-dir*))))
    (error 'operation-error :component c :operation o)))

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
	    ((:clsql-mysql-source-file "mysql")
	     (:file "mysql-package")
	     (:file "mysql-loader" :depends-on ("mysql-package" "mysql"))
	     (:file "mysql-api" :depends-on ("mysql-loader"))
	     (:file "mysql-sql" :depends-on ("mysql-api"))
	     (:file "mysql-usql" :depends-on ("mysql-sql")))))
  :depends-on (:uffi :clsql-base :clsql-uffi))
