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
;;;; $Id: usql.cl,v 1.7 2002/05/19 16:05:23 kevin Exp $
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

(defun add-attribute (table attribute &key (database *default-database*))
  "Add the ATTRIBUTE to TABLE.  The ATTRIBUTE sepcification must
include a type argument.  The optional keyword argument :database
specifies the database to operation on, defaulting to
*default-database*."
  (database-add-attribute table attribute database))

(defun rename-attribute (table oldatt newname
			       &key (database *default-database*))
  (error "(rename-attribute ~a ~a ~a ~a) is not implemented" table oldatt newname database))


(defclass %sql-expression ()
    ())

;; For SQL Identifiers of generic type
(defclass sql-ident (%sql-expression)
  ((name
    :initarg :name
    :initform "NULL"))
  (:documentation "An SQL identifer."))

(defmethod make-load-form ((sql sql-ident) &optional environment)
  (declare (ignore environment))
  (with-slots (name)
    sql
    `(make-instance 'sql-ident :name ',name)))


(defun create-sequence (name &key (database *default-database*))
  (database-create-sequence name database))

(defun drop-sequence (name &key (database *default-database*))
  (database-drop-sequence name database))

(defun sequence-next (name &key (database *default-database*))
  (database-sequence-next name database))

(defclass sql-value-exp (%sql-expression)
  ((modifier
    :initarg :modifier
    :initform nil)
   (components
    :initarg :components
    :initform nil))
  (:documentation
   "An SQL value expression.")
  )

(defclass sql-typecast-exp (sql-value-exp)
  ()
  (:documentation
   "An SQL typecast expression.")
  )
(defvar +null-string+ "NULL")

(defvar *sql-stream* nil
  "stream which accumulates SQL output")

(defmethod output-sql ((expr %sql-expression) &optional
                       (database *default-database*))
  (declare (ignore database))
  (write-string +null-string+ *sql-stream*))

(defmethod print-object ((self %sql-expression) stream)
  (print-unreadable-object
   (self stream :type t)
   (write-string (sql-output self) stream)))


;; Methods for translating high-level table classes to low-level functions

(defmethod database-list-attributes ((table sql-ident) database)
  (database-list-attributes (string-downcase
			     (symbol-name (slot-value table 'name)))
			    database)
  )

(defmethod database-attribute-type (attribute (table sql-ident) database)
  (database-attribute-type attribute (string-downcase
				      (symbol-name (slot-value table 'name)))
			   database))
