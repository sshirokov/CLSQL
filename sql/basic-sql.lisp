;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;;
;;;; $Id$
;;;;
;;;; Base SQL functions
;;;;
;;;; This file is part of CLSQL.
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(in-package #:clsql-sys)

;;; Query

(defgeneric query (query-expression &key database result-types flatp field-names)
  (:documentation
   "Executes the SQL query expression QUERY-EXPRESSION, which may
be an SQL expression or a string, on the supplied DATABASE which
defaults to *DEFAULT-DATABASE*. RESULT-TYPES is a list of symbols
which specifies the lisp type for each field returned by
QUERY-EXPRESSION. If RESULT-TYPES is nil all results are returned
as strings whereas the default value of :auto means that the lisp
types are automatically computed for each field. FIELD-NAMES is t
by default which means that the second value returned is a list
of strings representing the columns selected by
QUERY-EXPRESSION. If FIELD-NAMES is nil, the list of column names
is not returned as a second value. FLATP has a default value of
nil which means that the results are returned as a list of
lists. If FLATP is t and only one result is returned for each
record selected by QUERY-EXPRESSION, the results are returned as
elements of a list."))

(defmethod query ((query-expression string) &key (database *default-database*)
                  (result-types :auto) (flatp nil) (field-names t))
  (record-sql-command query-expression database)
  (multiple-value-bind (rows names) 
      (database-query query-expression database result-types field-names)
    (let ((result (if (and flatp (= 1 (length (car rows))))
                      (mapcar #'car rows)
                    rows)))
      (record-sql-result result database)
      (if field-names
	  (values result names)
	result))))

;;; Execute

(defgeneric execute-command (expression &key database)
  (:documentation
   "Executes the SQL command EXPRESSION, which may be an SQL
expression or a string representing any SQL statement apart from
a query, on the supplied DATABASE which defaults to
*DEFAULT-DATABASE*."))

(defmethod execute-command ((sql-expression string)
                            &key (database *default-database*))
  (record-sql-command sql-expression database)
  (let ((res (database-execute-command sql-expression database)))
    (record-sql-result res database))
  (values))

;;; Large objects support

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

