;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          clsql-aodbc.asd
;;;; Purpose:       ASDF definition file for CLSQL AODBC backend
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  Aug 2002
;;;;
;;;; $Id: clsql-aodbc.asd,v 1.11 2002/09/25 12:44:59 kevin Exp $
;;;;
;;;; This file, part of CLSQL, is Copyright (c) 2002 by Kevin M. Rosenberg
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(in-package :asdf)

#+(and allegro (not allegro-cl-trial))
(defsystem :clsql-aodbc
  :name "cl-sql-aodbc"
  :author "Kevin M. Rosenberg <kmr@debian.org>"
  :version "0.9.2"
  :maintainer "Kevin M. Rosenberg <kmr@debian.org>"
  :licence "Lessor Lisp General Public License"
  :description "Common Lisp SQL AODBC Driver"
  :long-description "cl-sql-aodbc package provides a database driver to AllegroCL's AODBC database interface."

  :components
    ((:module :db-aodbc
	      :components
	      ((:file "aodbc-package")
	       (:file "aodbc-sql" :depends-on ("aodbc-package")))))
    :depends-on (:clsql-base))

#+(and allegro (not allegro-cl-trial))
(defmethod source-file-type  ((c cl-source-file)
			      (s (eql (find-system :clsql-aodbc)))) 
   "cl")

