;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          clsql-postgresql.asd
;;;; Purpose:       ASDF file for CLSQL PostgresSQL socket backend
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  Aug 2002
;;;;
;;;; $Id: clsql-postgresql-socket.asd,v 1.13 2002/10/16 11:51:04 kevin Exp $
;;;;
;;;; This file, part of CLSQL, is Copyright (c) 2002 by Kevin M. Rosenberg
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(in-package :asdf)

;;; System definition

#+(or allegro lispworks cmu sbcl openmcl mcl scl)
(defsystem :clsql-postgresql-socket
  :name "cl-sql-postgresql-socket"
  :author "Kevin M. Rosenberg <kmr@debian.org>"
  :version "0.9.2"
  :maintainer "Kevin M. Rosenberg <kmr@debian.org>"
  :licence "Lessor Lisp General Public License"
  :description "Common Lisp SQL PostgreSQL Socket Driver"
  :long-description "cl-sql-postgresql-socket package provides a database driver to the PostgreSQL database via a socket interface."

  :components
  ((:module :db-postgresql-socket
	    :components
	    ((:file "postgresql-socket-package")
	     (:file "postgresql-socket-api"
		    :depends-on ("postgresql-socket-package"))
	     (:file "postgresql-socket-sql"
		    :depends-on ("postgresql-socket-api")))))
  :depends-on (:clsql-base :uffi))
