;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;;
;;;; $Id$
;;;;
;;;; The CLSQL Functional Data Definition Language (FDDL)
;;;; including functions for schema manipulation. Currently supported
;;;; SQL objects include tables, views, indexes, attributes and
;;;; sequences.
;;;;
;;;; This file is part of CLSQL.
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(in-package #:clsql-sys)


;; Utilities

(defun database-identifier (name)
  (sql-escape (etypecase name
                (string
                 (string-upcase name))
                (sql-ident
                 (sql-output name))
                (symbol
                 (sql-output name)))))


;; Tables 

(defun create-table (name description &key (database *default-database*)
                          (constraints nil))
  "Create a table called NAME, in DATABASE which defaults to
*DEFAULT-DATABASE*, containing the attributes in DESCRIPTION which is
a list containing lists of attribute-name and type information pairs."
  (let* ((table-name (etypecase name 
                       (symbol (sql-expression :attribute name))
                       (string (sql-expression :attribute (make-symbol name)))
                       (sql-ident name)))
         (stmt (make-instance 'sql-create-table
                              :name table-name
                              :columns description
                              :modifiers constraints)))
    (execute-command stmt :database database)))

(defun drop-table (name &key (if-does-not-exist :error)
                        (database *default-database*))
  "Drops table NAME from DATABASE which defaults to
*DEFAULT-DATABASE*. If the table does not exist and IF-DOES-NOT-EXIST
is :ignore then DROP-TABLE returns nil whereas an error is signalled
if IF-DOES-NOT-EXIST is :error."
  (let ((table-name (database-identifier name)))
    (ecase if-does-not-exist
      (:ignore
       (unless (table-exists-p table-name :database database)
         (return-from drop-table nil)))
      (:error
       t))
    (let ((expr (concatenate 'string "DROP TABLE " table-name)))
      (execute-command expr :database database))))

(defun list-tables (&key (owner nil) (database *default-database*))
  "List all tables in DATABASE which defaults to
*DEFAULT-DATABASE*. If OWNER is nil, only user-owned tables are
considered. This is the default. If OWNER is :all , all tables are
considered. If OWNER is a string, this denotes a username and only
tables owned by OWNER are considered. Table names are returned as a
list of strings."
  (database-list-tables database :owner owner))

(defun table-exists-p (name &key (owner nil) (database *default-database*))
  "Test for existence of an SQL table called NAME in DATABASE which
defaults to *DEFAULT-DATABASE*. If OWNER is nil, only user-owned
tables are considered. This is the default. If OWNER is :all , all
tables are considered. If OWNER is a string, this denotes a username
and only tables owned by OWNER are considered. Table names are
returned as a list of strings."
  (when (member (database-identifier name)
                (list-tables :owner owner :database database)
                :test #'string-equal)
    t))


;; Views 

(defun create-view (name &key as column-list (with-check-option nil)
                         (database *default-database*))
  "Creates a view called NAME using the AS query and the optional
COLUMN-LIST and WITH-CHECK-OPTION. The COLUMN-LIST argument is a list
of columns to add to the view. The WITH-CHECK-OPTION adds 'WITH CHECK
OPTION' to the resulting SQL. The default value of WITH-CHECK-OPTION
is NIL. The default value of DATABASE is *DEFAULT-DATABASE*."
  (let* ((view-name (etypecase name 
                      (symbol (sql-expression :attribute name))
                      (string (sql-expression :attribute (make-symbol name)))
                      (sql-ident name)))
         (stmt (make-instance 'sql-create-view
                              :name view-name
                              :column-list column-list
                              :query as
                              :with-check-option with-check-option)))
    (execute-command stmt :database database)))

(defun drop-view (name &key (if-does-not-exist :error)
                       (database *default-database*))
  "Deletes view NAME from DATABASE which defaults to
*DEFAULT-DATABASE*. If the view does not exist and IF-DOES-NOT-EXIST
is :ignore then DROP-VIEW returns nil whereas an error is signalled if
IF-DOES-NOT-EXIST is :error."
  (let ((view-name (database-identifier name)))
    (ecase if-does-not-exist
      (:ignore
       (unless (view-exists-p view-name :database database)
         (return-from drop-view)))
      (:error
       t))
    (let ((expr (concatenate 'string "DROP VIEW " view-name)))
      (execute-command expr :database database))))

(defun list-views (&key (owner nil) (database *default-database*))
  "List all views in DATABASE which defaults to *DEFAULT-DATABASE*. If
OWNER is nil, only user-owned views are considered. This is the
default. If OWNER is :all , all views are considered. If OWNER is a
string, this denotes a username and only views owned by OWNER are
considered. View names are returned as a list of strings."
  (database-list-views database :owner owner))

(defun view-exists-p (name &key (owner nil) (database *default-database*))
  "Test for existence of an SQL view called NAME in DATABASE which
defaults to *DEFAULT-DATABASE*. If OWNER is nil, only user-owned views
are considered. This is the default. If OWNER is :all , all views are
considered. If OWNER is a string, this denotes a username and only
views owned by OWNER are considered. View names are returned as a list
of strings."
  (when (member (database-identifier name)
                (list-views :owner owner :database database)
                :test #'string-equal)
    t))


;; Indexes 

(defun create-index (name &key on (unique nil) attributes
                          (database *default-database*))
  "Creates an index called NAME on the table specified by ON. The
attributes of the table to index are given by ATTRIBUTES. Setting
UNIQUE to T includes UNIQUE in the SQL index command, specifying that
the columns indexed must contain unique values. The default value of
UNIQUE is nil. The default value of DATABASE is *DEFAULT-DATABASE*."
  (let* ((index-name (database-identifier name))
         (table-name (database-identifier on))
         (attributes (mapcar #'database-identifier (listify attributes)))
         (stmt (format nil "CREATE ~A INDEX ~A ON ~A (~{~A~^, ~})"
                       (if unique "UNIQUE" "")
                       index-name table-name attributes)))
    (execute-command stmt :database database)))

(defun drop-index (name &key (if-does-not-exist :error)
                        (on nil)
                        (database *default-database*))
  "Deletes index NAME from table FROM in DATABASE which defaults to
*DEFAULT-DATABASE*. If the index does not exist and IF-DOES-NOT-EXIST
is :ignore then DROP-INDEX returns nil whereas an error is signalled
if IF-DOES-NOT-EXIST is :error. The argument ON allows the optional
specification of a table to drop the index from."
  (let ((index-name (database-identifier name)))
    (ecase if-does-not-exist
      (:ignore
       (unless (index-exists-p index-name :database database)
         (return-from drop-index)))
      (:error t))
    (execute-command (format nil "DROP INDEX ~A~A" index-name
                             (if (null on) ""
                                 (concatenate 'string " ON "
                                              (database-identifier on))))
                     :database database)))

(defun list-indexes (&key (owner nil) (database *default-database*))
  "List all indexes in DATABASE, which defaults to
*default-database*. If OWNER is :all , all indexs are considered. If
OWNER is a string, this denotes a username and only indexs owned by
OWNER are considered. Index names are returned as a list of strings."
  (database-list-indexes database :owner owner))

(defun list-table-indexes (table &key (owner nil)
				      (database *default-database*))
  "List all indexes in DATABASE for a TABLE, which defaults to
*default-database*. If OWNER is :all , all indexs are considered. If
OWNER is a string, this denotes a username and only indexs owned by
OWNER are considered. Index names are returned as a list of strings."
  (database-list-table-indexes (database-identifier table)
			       database :owner owner))
  
(defun index-exists-p (name &key (owner nil) (database *default-database*))
  "Test for existence of an index called NAME in DATABASE which
defaults to *DEFAULT-DATABASE*. If OWNER is :all , all indexs are
considered. If OWNER is a string, this denotes a username and only
indexs owned by OWNER are considered. Index names are returned as a
list of strings."
  (when (member (database-identifier name)
                (list-indexes :owner owner :database database)
                :test #'string-equal)
    t))

;; Attributes 

(defun list-attributes (name &key (owner nil) (database *default-database*))
  "List the attributes of a attribute called NAME in DATABASE which
defaults to *DEFAULT-DATABASE*. If OWNER is nil, only user-owned
attributes are considered. This is the default. If OWNER is :all , all
attributes are considered. If OWNER is a string, this denotes a
username and only attributes owned by OWNER are considered. Attribute
names are returned as a list of strings. Attributes are returned as a
list of strings."
  (database-list-attributes (database-identifier name) database :owner owner))

(defun attribute-type (attribute table &key (owner nil)
                                 (database *default-database*))
  "Return the field type of the ATTRIBUTE in TABLE.  The optional
keyword argument DATABASE specifies the database to query, defaulting
to *DEFAULT-DATABASE*. If OWNER is nil, only user-owned attributes are
considered. This is the default. If OWNER is :all , all attributes are
considered. If OWNER is a string, this denotes a username and only
attributes owned by OWNER are considered. Attribute names are returned
as a list of strings. Attributes are returned as a list of strings."
  (database-attribute-type (database-identifier attribute)
                           (database-identifier table)
                           database
                           :owner owner))

(defun list-attribute-types (table &key (owner nil)
                                   (database *default-database*))
  "Returns type information for the attributes in TABLE from DATABASE
which has a default value of *default-database*. If OWNER is nil, only
user-owned attributes are considered. This is the default. If OWNER is
:all, all attributes are considered. If OWNER is a string, this
denotes a username and only attributes owned by OWNER are
considered. Returns a list in which each element is a list (attribute
datatype). Attribute is a string denoting the atribute name. Datatype
is the vendor-specific type returned by ATTRIBUTE-TYPE."
  (mapcar #'(lambda (type)
              (list type (attribute-type type table :database database
                                         :owner owner)))
          (list-attributes table :database database :owner owner)))

;(defun add-attribute (table attribute &key (database *default-database*))
;  (database-add-attribute table attribute database))

;(defun rename-attribute (table oldatt newname
;                               &key (database *default-database*))
;  (error "(rename-attribute ~a ~a ~a ~a) is not implemented"
;         table oldatt newname database))


;; Sequences 

(defun create-sequence (name &key (database *default-database*))
  "Create a sequence called NAME in DATABASE which defaults to
*DEFAULT-DATABASE*."
  (let ((sequence-name (database-identifier name)))
    (database-create-sequence sequence-name database))
  (values))

(defun drop-sequence (name &key (if-does-not-exist :error)
                           (database *default-database*))
  "Drops sequence NAME from DATABASE which defaults to
*DEFAULT-DATABASE*. If the sequence does not exist and
IF-DOES-NOT-EXIST is :ignore then DROP-SEQUENCE returns nil whereas an
error is signalled if IF-DOES-NOT-EXIST is :error."
  (let ((sequence-name (database-identifier name)))
    (ecase if-does-not-exist
      (:ignore
       (unless (sequence-exists-p sequence-name :database database)
         (return-from drop-sequence)))
      (:error t))
    (database-drop-sequence sequence-name database))
  (values))

(defun list-sequences (&key (owner nil) (database *default-database*))
  "List all sequences in DATABASE, which defaults to
*default-database*. If OWNER is nil, only user-owned sequences are
considered. This is the default. If OWNER is :all , all sequences are
considered. If OWNER is a string, this denotes a username and only
sequences owned by OWNER are considered. Sequence names are returned
as a list of strings."
  (database-list-sequences database :owner owner))

(defun sequence-exists-p (name &key (owner nil)
                               (database *default-database*))
  "Test for existence of a sequence called NAME in DATABASE which
defaults to *DEFAULT-DATABASE*."
  (when (member (database-identifier name)
                (list-sequences :owner owner :database database)
                :test #'string-equal)
    t))
  
(defun sequence-next (name &key (database *default-database*))
  "Return the next value in the sequence NAME in DATABASE."
  (database-sequence-next (database-identifier name) database))

(defun set-sequence-position (name position &key (database *default-database*))
  "Explicitly set the the position of the sequence NAME in DATABASE to
POSITION."
  (database-set-sequence-position (database-identifier name) position database))

(defun sequence-last (name &key (database *default-database*))
  "Return the last value of the sequence NAME in DATABASE."
  (database-sequence-last (database-identifier name) database))
