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
   "Execute the SQL query expression QUERY-EXPRESSION on the given
DATABASE which defaults to *default-database*. RESULT-TYPES is a list
of symbols such as :string and :integer, one for each field in the
query, which are used to specify the types to return. The FLATP
argument, which has a default value of nil, specifies if full
bracketed results should be returned for each matched entry. If FLATP
is nil, the results are returned as a list of lists. If FLATP is t,
the results are returned as elements of a list, only if there is only
one result per row. Returns a list of lists of values of the result of
that expression and a list of field names selected in sql-exp."))

(defmethod query ((query-expression string) &key (database *default-database*)
                  (result-types :auto) (flatp nil) (field-names t))
  (record-sql-command query-expression database)
  (multiple-value-bind (rows names) (database-query query-expression database result-types
                                                    field-names)
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
   "Executes the SQL command specified by EXPRESSION for the database
specified by DATABASE, which has a default value of
*DEFAULT-DATABASE*. The argument EXPRESSION may be any SQL statement
other than a query. To run a stored procedure, pass an appropriate
string. The call to the procedure needs to be wrapped in a BEGIN END
pair."))

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

