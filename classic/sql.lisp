;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:         sql.lisp
;;;; Purpose:      High-level SQL interface
;;;; Authors:      Kevin M. Rosenberg based on code by Pierre R. Mai 
;;;; Date Started: Feb 2002
;;;;
;;;; $Id$
;;;;
;;;; This file, part of CLSQL, is Copyright (c) 2002-2004 by Kevin M. Rosenberg
;;;; and Copyright (c) 1999-2001 by Pierre R. Mai
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(in-package #:clsql-classic-sys)


;;; Row processing macro

(defmacro for-each-row (((&rest fields) &key from order-by where distinct limit) &body body)
  (let ((d (gensym "DISTINCT-"))
	(bind-fields (loop for f in fields collect (car f)))
	(w (gensym "WHERE-"))
	(o (gensym "ORDER-BY-"))
	(frm (gensym "FROM-"))
	(l (gensym "LIMIT-"))
	(q (gensym "QUERY-")))
    `(let ((,frm ,from)
	   (,w ,where)
	   (,d ,distinct)
	   (,l ,limit)
	   (,o ,order-by))
      (let ((,q (query-string ',fields ,frm ,w ,d ,o ,l)))
	(loop for tuple in (query ,q)
	      collect (destructuring-bind ,bind-fields tuple
		   ,@body))))))

(defun query-string (fields from where distinct order-by limit)
  (concatenate
   'string
   (format nil "select ~A~{~A~^,~} from ~{~A~^ and ~}" 
	   (if distinct "distinct " "") (field-names fields)
	   (from-names from))
   (if where (format nil " where ~{~A~^ ~}"
		     (where-strings where)) "")
   (if order-by (format nil " order by ~{~A~^, ~}"
			(order-by-strings order-by)))
   (if limit (format nil " limit ~D" limit) "")))

(defun lisp->sql-name (field)
  (typecase field
    (string field)
    (symbol (string-upcase (symbol-name field)))
    (cons (cadr field))
    (t (format nil "~A" field))))

(defun field-names (field-forms)
  "Return a list of field name strings from a fields form"
  (loop for field-form in field-forms
	collect
	(lisp->sql-name
	 (if (cadr field-form)
	     (cadr field-form)
	     (car field-form)))))

(defun from-names (from)
  "Return a list of field name strings from a fields form"
  (loop for table in (if (atom from) (list from) from)
	collect (lisp->sql-name table)))


(defun where-strings (where)
  (loop for w in (if (atom (car where)) (list where) where)
	collect
	(if (consp w)
	    (format nil "~A ~A ~A" (second w) (first w) (third w))
	    (format nil "~A" w))))

(defun order-by-strings (order-by)
  (loop for o in order-by
	collect
	(if (atom o)
	    (lisp->sql-name o)
	    (format nil "~A ~A" (lisp->sql-name (car o))
		    (lisp->sql-name (cadr o))))))


;;; Marc Battyani : Large objects support

(defun create-large-object (&key (database *default-database*))
  "Creates a new large object in the database and returns the object identifier"
  (database-create-large-object database))

(defun write-large-object (object-id data &key (database *default-database*))
  "Writes data to the large object"
  (database-write-large-object object-id data database))

(defun read-large-object (object-id &key (database *default-database*))
  "Reads the large object content"
  (database-read-large-object object-id database))

(defun delete-large-object (object-id &key (database *default-database*))
  "Deletes the large object in the database"
  (database-delete-large-object object-id database))


;;; These functions are not exported. If you application depends on these
;;; functions consider using the clsql package using has further support.

(defun list-tables (&key (database *default-database*))
  "List all tables in *default-database*, or if the :database keyword arg
is given, the specified database.  If the keyword arg :system-tables
is true, then it will not filter out non-user tables.  Table names are
given back as a list of strings."
  (database-list-tables database))


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


