;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          clsql-postgresql.asd
;;;; Purpose:       ASDF file for CLSQL SQLite backend
;;;; Programmer:    Aurelio Bignoli
;;;; Date Started:  Aug 2003
;;;;
;;;; $Id: clsql-sqlite.asd,v 1.5 2004/03/09 20:55:11 aurelio Exp $
;;;;
;;;; This file, part of CLSQL, is Copyright (c) 2003 by Aurelio Bignoli
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************
(defpackage #:clsql-sqlite-system (:use #:asdf #:cl))
(in-package #:clsql-sqlite-system)

(defsystem clsql-sqlite
  :name "cl-sql-sqlite"
  :author "Aurelio Bignoli <aurelio@bignoli.it>"
  :maintainer "Aurelio Bignoli"
  :licence "Lessor Lisp General Public License"
  :description "Common Lisp SQLite Driver"
  :long-description "cl-sql-sqlite package provides a database driver to SQLite database library."

  :components
    ((:module :db-sqlite
	      :components
	      ((:file "sqlite-package")
	       (:file "sqlite-loader" :depends-on ("sqlite-package"))
	       (:file #+clisp "sqlite-api-clisp"
		      #-clisp "sqlite-api-uffi"
                      :depends-on ("sqlite-loader"))
	       (:file "sqlite-sql" :depends-on (#+clisp "sqlite-api-clisp"
						#-clisp "sqlite-api-uffi"))
	       (:file "sqlite-usql" :depends-on ("sqlite-sql")))))
    :depends-on (#-clisp :uffi
			 :clsql-base))
