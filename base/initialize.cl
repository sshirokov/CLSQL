;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          initialize.cl
;;;; Purpose:       Initializion routines for backend
;;;; Programmers:   Kevin M. Rosenberg 
;;;; Date Started:  May 2002
;;;;
;;;; $Id: initialize.cl,v 1.1 2002/05/14 16:23:37 kevin Exp $
;;;;
;;;; This file, part of CLSQL, is Copyright (c) 2002 by Kevin M. Rosenberg
;;;; and Copyright (c) 1999-2001 by Pierre R. Mai
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(declaim (optimize (debug 3) (speed 3) (safety 1) (compilation-speed 0)))
(in-package :clsql-sys)

(defvar *loaded-database-types* nil
  "Contains a list of database types which have been defined/loaded.")

(defmethod database-type-load-foreign :after (database-type)
  (when (database-type-library-loaded database-type)
     (pushnew database-type *loaded-database-types*)))

(defun reload-database-types ()
  "Reloads any foreign code for the loaded database types after a dump."
  (mapc #'database-type-load-foreign *loaded-database-types*))

(defvar *default-database-type* nil
  "Specifies the default type of database.  Currently only :mysql is
supported.")

(defvar *initialized-database-types* nil
  "Contains a list of database types which have been initialized by calls
to initialize-database-type.")

(defun initialize-database-type (&key (database-type *default-database-type*))
  "Initialize the given database-type, if it is not already
initialized, as indicated by `*initialized-database-types*'."
  (if (member database-type *initialized-database-types*)
      t
      (when (database-initialize-database-type database-type)
	(push database-type *initialized-database-types*)
	t)))


