;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:     generics.lisp
;;;; Purpose:  Generic function definitions for DB interfaces
;;;; Author:   Kevin M. Rosenberg based on
;;;; Created:  Apr 2004
;;;;
;;;; $Id: db-interface.lisp 9123 2004-04-21 20:34:42Z kevin $
;;;;
;;;; This file, part of CLSQL, is Copyright (c) 2002-2004 by Kevin M. Rosenberg
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(in-package #:clsql)

(defgeneric select (&rest args) 
  (:documentation
   "The function SELECT selects data from DATABASE, which has a
default value of *DEFAULT-DATABASE*, given the constraints
specified by the rest of the ARGS. It returns a list of objects
as specified by SELECTIONS. By default, the objects will each be
represented as lists of attribute values. The argument SELECTIONS
consists either of database identifiers, type-modified database
identifiers or literal strings. A type-modifed database
identifier is an expression such as [foo :string] which means
that the values in column foo are returned as Lisp strings.  The
FLATP argument, which has a default value of nil, specifies if
full bracketed results should be returned for each matched
entry. If FLATP is nil, the results are returned as a list of
lists. If FLATP is t, the results are returned as elements of a
list, only if there is only one result per row. The arguments
ALL, SET-OPERATION, DISTINCT, FROM, WHERE, GROUP-BY, HAVING and
ORDER-by have the same function as the equivalent SQL expression.
The SELECT function is common across both the functional and
object-oriented SQL interfaces. If selections refers to View
Classes then the select operation becomes object-oriented. This
means that SELECT returns a list of View Class instances, and
SLOT-VALUE becomes a valid SQL operator for use within the where
clause. In the View Class case, a second equivalent select call
will return the same View Class instance objects. If REFRESH is
true, then existing instances are updated if necessary, and in
this case you might need to extend the hook INSTANCE-REFRESHED.
The default value of REFRESH is nil. SQL expressions used in the
SELECT function are specified using the square bracket syntax,
once this syntax has been enabled using
ENABLE-SQL-READER-SYNTAX."))

(defgeneric update-record-from-slot (object slot &key database)
  (:documentation
   "The generic function UPDATE-RECORD-FROM-SLOT updates an individual
data item in the column represented by SLOT. The DATABASE is only used
if OBJECT is not yet associated with any database, in which case a
record is created in DATABASE. Only SLOT is initialized in this case;
other columns in the underlying database receive default values. The
argument SLOT is the CLOS slot name; the corresponding column names
are derived from the View Class definition."))

(defgeneric update-record-from-slots (object slots &key database)
  (:documentation 
   "The generic function UPDATE-RECORD-FROM-SLOTS updates data in the
columns represented by SLOTS. The DATABASE is only used if OBJECT is
not yet associated with any database, in which case a record is
created in DATABASE. Only slots are initialized in this case; other
columns in the underlying database receive default values. The
argument SLOTS contains the CLOS slot names; the corresponding column
names are derived from the view class definition."))

(defgeneric update-records-from-instance (object &key database)
  (:documentation
   "Using an instance of a view class, OBJECT, update the database
table that stores its instance data. If OBJECT is already associated
with a database, that database is used, and DATABASE is ignored. If
OBJECT is not yet associated with a database, a record is created for
instance in the appropriate table of DATABASE and the instance becomes
associated with that database."))

(defgeneric delete-instance-records (instance)
  (:documentation
   "Deletes the records represented by INSTANCE from the database
associated with it. If INSTANCE has no associated database, an error
is signalled."))

(defgeneric update-instance-from-records (instance &key database)
  (:documentation
   "The generic function UPDATE-INSTANCE-FROM-RECORDS updates the
values in the slots of the View Class instance INSTANCE using the data
in the database DATABASE which defaults to the DATABASE that instance
is associated with, or the value of *DEFAULT-DATABASE*. If INSTANCE is
associated with a database, then DATABASE must be that same
database. The update is not recursive on joins. Join slots (that is,
slots with :db-kind :join ) are updated, but the joined objects are
not updated."))

(defgeneric update-slot-from-record (instance slot &key database)
  (:documentation
   "Updates the value in the slot SLOT of the View Class instance
INSTANCE using the data in the database DATABASE which defaults to the
database that INSTANCE is associated with, or the value of
*DEFAULT-DATABASE*. The argument SLOT is the CLOS slot name, the
corresponding column names are derived from the View Class
definition. The update is not recursive on joins. Join slots (that is,
slots with :db-kind :join) are updated, but the joined objects are not
updated."))

(defgeneric instance-refreshed (instance) 
  (:documentation 
   "The function INSTANCE-REFRESHED is called inside SELECT when its
REFRESH argument is true and the instance INSTANCE has just been
updated. The supplied method on STANDARD-DB-OBJECT does nothing. If
your application needs to take action when a View Class instance has
been updated by (select ... :refresh t) then add an INSTANCE-REFRESH
method specializing on your subclass of STANDARD-DB-OBJECT."))

(defgeneric update-slot-with-null (instance slotname slotdef)
  (:documentation "Called to update a slot when its column has a NULL
value.  If nulls are allowed for the column, the slot's value will be
nil, otherwise its value will be set to the result of calling
DATABASE-NULL-VALUE on the type of the slot."))

(defgeneric output-sql (expr database)
  )

(defgeneric output-sql-hash-key (arg database)
  )

(defgeneric collect-table-refs (sql)
  )
(defgeneric database-output-sql (arg database)
  )
(defgeneric database-constraint-description  (constraint database)
  )
(defgeneric database-pkey-constraint  (class database)
  )
(defgeneric database-constraint-statement  (constraints database)
  )
(defgeneric %install-class  (class database)
  )
(defgeneric database-generate-column-definition  (class slotdef database)
  )
(defgeneric update-slot-from-db  (instance slotdef val)
  )
(defgeneric key-value-from-db  (slotdef value database)
  )
(defgeneric get-slot-values-from-view  (obj slotdeflist values)
  )
(defgeneric database-output-sql-as-type  (type val database)
  )
(defgeneric read-sql-value  (val type database)
  )
(defgeneric add-to-relation  (target slot-name value)
  )
(defgeneric remove-from-relation  (target slot-name value)
  )

