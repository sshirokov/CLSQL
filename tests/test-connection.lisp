;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:     test-connection.lisp
;;;; Purpose:  Tests for CLSQL database connections
;;;; Authors:  Marcus Pearce and Kevin M. Rosenberg
;;;; Created:  March 2004
;;;;
;;;; This file is part of CLSQL.
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(in-package #:clsql-tests)

(setq *rt-connection*
      '(

(deftest :connection/1
    (let ((database (clsql:find-database
                     (clsql:database-name clsql:*default-database*)
                     :db-type (clsql-sys:database-type clsql:*default-database*))))
      (eql (clsql-sys:database-type database) *test-database-type*))
  t)

(deftest :connection/2
    (clsql-sys::string-to-list-connection-spec
     "localhost/dbname/user/passwd")
  ("localhost" "dbname" "user" "passwd"))

(deftest :connection/3
    (clsql-sys::string-to-list-connection-spec
     "dbname/user@hostname")
  ("hostname" "dbname" "user"))

(deftest :connection/execute-command
    ;;check that we can issue basic commands.
    (values
      (clsql-sys:execute-command "CREATE TABLE DUMMY (foo integer)")
      (clsql-sys:execute-command "DROP TABLE DUMMY"))
  nil nil)

(deftest :connection/query
    ;;check that we can do a basic query
    (first (clsql:query "SELECT 1" :flatp t :field-names nil))
  1)

(deftest :connection/query-command
    ;;queries that are commands (no result set) shouldn't cause breakage
    (values
      (clsql-sys:query "CREATE TABLE DUMMY (foo integer)")
      (clsql-sys:query "DROP TABLE DUMMY"))
  nil nil)

))
