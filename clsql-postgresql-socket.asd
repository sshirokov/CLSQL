;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          clsql-postgresql.asd
;;;; Purpose:       ASDF file for CLSQL PostgresSQL socket backend
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  Aug 2002
;;;;
;;;; $Id: clsql-postgresql-socket.asd,v 1.3 2002/09/01 09:00:15 kevin Exp $
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
			      (s (eql (find-system 'clsql-postgresql-socket)))) 
   "cl")

(defsystem clsql-postgresql-socket
  :pathname #.(format nil "~A:clsql-postgresql-socket;" +clsql-logical-host+)
  :components ((:file "postgresql-socket-package")
	       (:file "postgresql-socket-api"
		      :depends-on ("postgresql-socket-package"))
	       (:file "postgresql-socket-sql"
		      :depends-on ("postgresql-socket-api")))
  :depends-on (:clsql-base :uffi))
