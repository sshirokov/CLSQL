;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          usql.cl
;;;; Purpose:       High-level interface to SQL driver routines needed for
;;;;                UncommonSQL
;;;; Programmers:   Kevin M. Rosenberg and onShore Development Inc
;;;; Date Started:  Mar 2002
;;;;
;;;; $Id: usql.lisp,v 1.1 2002/09/30 10:19:23 kevin Exp $
;;;;
;;;; This file, part of CLSQL, is Copyright (c) 2002 by Kevin M. Rosenberg
;;;; and onShore Development Inc
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************


;;; Minimal high-level routines to enable low-level interface for USQL

(declaim (optimize (debug 3) (speed 3) (safety 1) (compilation-speed 0)))
(in-package :clsql-sys)

(defun list-tables (&key (database *default-database*)
                         (system-tables nil))
  "List all tables in *default-database*, or if the :database keyword arg
is given, the specified database.  If the keyword arg :system-tables
is true, then it will not filter out non-user tables.  Table names are
given back as a list of strings."
  (database-list-tables database :system-tables system-tables))


(defun list-attributes (table &key (database *default-database*))
  "List the attributes of TABLE in *default-database, or if the
:database keyword is given, the specified database.  Attributes are
returned as a list of strings."
  (database-list-attributes table database))

(defun attribute-type (attribute table &key (database *default-database*))
  "Return the field type of the ATTRIBUTE in TABLE.  The optional
keyword argument :database specifies the database to query, defaulting
to *default-database*."
  (database-attribute-type attribute table database))

(defun create-sequence (name &key (database *default-database*))
  (database-create-sequence name database))

(defun drop-sequence (name &key (database *default-database*))
  (database-drop-sequence name database))

(defun sequence-next (name &key (database *default-database*))
  (database-sequence-next name database))


