;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          db-interface.cl
;;;; Purpose:       Generic function definitions for DB interfaces
;;;; Programmers:   Kevin M. Rosenberg based on
;;;;                Original code by Pierre R. Mai. Additions from
;;;;                onShoreD to support UncommonSQL front-end 
;;;; Date Started:  Feb 2002
;;;;
;;;; $Id: db-interface.cl,v 1.5 2002/09/27 14:55:37 kevin Exp $
;;;;
;;;; This file, part of CLSQL, is Copyright (c) 2002 by Kevin M. Rosenberg
;;;; and Copyright (c) 1999-2001 by Pierre R. Mai, and onShoreD
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(declaim (optimize (debug 3) (speed 3) (safety 1) (compilation-speed 0)))
(in-package :clsql-base-sys)

(defgeneric database-type-load-foreign (database-type)
  (:documentation
   "The internal generic implementation of reload-database-types."))

(defgeneric database-type-library-loaded (database-type)
  (:documentation
   "The internal generic implementation for checking if
database type library loaded successfully."))

(defgeneric database-type (database)
  (:documentation
   "Returns database type")
  (:method (database)
	   (signal-nodb-error database)))


(defgeneric database-initialize-database-type (database-type)
  (:documentation
   "The internal generic implementation of initialize-database-type."))

(defgeneric database-name-from-spec (connection-spec database-type)
  (:documentation
   "Returns the name of the database that would be created if connect
was called with the connection-spec."))

(defgeneric database-connect (connection-spec database-type)
  (:documentation "Internal generic implementation of connect."))

(defgeneric database-disconnect (database)
  (:method ((database closed-database))
	   (signal-closed-database-error database))
  (:method ((database t))
	   (signal-nodb-error database))
  (:documentation "Internal generic implementation of disconnect."))

(defgeneric database-query (query-expression database types)
  (:method (query-expression (database closed-database) types)
	   (declare (ignore query-expression types))
	   (signal-closed-database-error database))  
  (:method (query-expression (database t) types)
	   (declare (ignore query-expression types))
	   (signal-nodb-error database))
  (:documentation "Internal generic implementation of query."))


(defgeneric database-execute-command (sql-expression database)
  (:method (sql-expression (database closed-database))
	   (declare (ignore sql-expression))
	   (signal-closed-database-error database))
  (:method (sql-expression (database t))
	   (declare (ignore sql-expression))
	   (signal-nodb-error database))
  (:documentation "Internal generic implementation of execute-command."))

;;; Mapping and iteration
(defgeneric database-query-result-set
    (query-expression database &key full-set types)
  (:method (query-expression (database closed-database) &key full-set types)
	   (declare (ignore query-expression full-set types))
	   (signal-closed-database-error database)
	   (values nil nil nil))
  (:method (query-expression (database t) &key full-set types)
	   (declare (ignore query-expression full-set types))
	   (signal-nodb-error database)
	   (values nil nil nil))
  (:documentation
   "Internal generic implementation of query mapping.  Starts the
query specified by query-expression on the given database and returns
a result-set to be used with database-store-next-row and
database-dump-result-set to access the returned data.  The second
value is the number of columns in the result-set, if there are any.
If full-set is true, the number of rows in the result-set is returned
as a third value, if this is possible (otherwise nil is returned for
the third value).  This might have memory and resource usage
implications, since many databases will require the query to be
executed in full to answer this question.  If the query produced no
results then nil is returned for all values that would have been
returned otherwise.  If an error occurs during query execution, the
function should signal a clsql-sql-error."))

(defgeneric database-dump-result-set (result-set database)
  (:method (result-set (database closed-database))
	   (declare (ignore result-set))
	   (signal-closed-database-error database))
  (:method (result-set (database t))
	   (declare (ignore result-set))
	   (signal-nodb-error database))
  (:documentation "Dumps the received result-set."))

(defgeneric database-store-next-row (result-set database list)
  (:method (result-set (database closed-database) list)
	   (declare (ignore result-set list))
	   (signal-closed-database-error database))
  (:method (result-set (database t) list)
	   (declare (ignore result-set list))
	   (signal-nodb-error database))
  (:documentation
   "Returns t and stores the next row in the result set in list or
returns nil when result-set is finished."))


;; Interfaces to support UncommonSQL

(defgeneric database-create-sequence (name database)
  (:documentation "Create a sequence in DATABASE."))

(defgeneric database-drop-sequence (name database)
  (:documentation "Drop a sequence from DATABASE."))

(defgeneric database-sequence-next (name database)
  (:documentation "Increment a sequence in DATABASE."))

(defgeneric database-start-transaction (database)
  (:documentation "Start a transaction in DATABASE."))

(defgeneric database-commit-transaction (database)
  (:documentation "Commit current transaction in DATABASE."))

(defgeneric database-abort-transaction (database)
  (:documentation "Abort current transaction in DATABASE."))

(defgeneric database-get-type-specifier (type args database)
  (:documentation "Return the type SQL type specifier as a string, for
the given lisp type and parameters."))

(defgeneric database-list-tables (database &key system-tables)
  (:documentation "List all tables in the given database"))

(defgeneric database-list-attributes (table database)
  (:documentation "List all attributes in TABLE."))

(defgeneric database-attribute-type (attribute table database)
  (:documentation "Return the type of ATTRIBUTE in TABLE."))

(defgeneric database-add-attribute (table attribute database)
  (:documentation "Add the attribute to the table."))

(defgeneric database-rename-attribute (table oldatt newname database)
  (:documentation "Rename the attribute in the table to NEWNAME."))

(defgeneric oid (object)
  (:documentation "Return the unique ID of a database object."))

 
;;; Large objects support (Marc Battyani)

(defgeneric database-create-large-object (database)
  (:documentation "Creates a new large object in the database and returns the object identifier"))

(defgeneric database-write-large-object (object-id (data string) database)
  (:documentation "Writes data to the large object"))

(defgeneric database-read-large-object (object-id database)
  (:documentation "Reads the large object content"))

(defgeneric database-delete-large-object (object-id database)
  (:documentation "Deletes the large object in the database"))
