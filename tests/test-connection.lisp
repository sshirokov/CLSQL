;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; ======================================================================
;;;; File:    test-connection.lisp
;;;; Authors: Marcus Pearce <m.t.pearce@city.ac.uk>, Kevin Rosenberg
;;;; Created: 30/03/2004
;;;; Updated: $Id$
;;;; ======================================================================
;;;;
;;;; Description ==========================================================
;;;; ======================================================================
;;;;
;;;; Tests for CLSQL database connections. 
;;;;
;;;; ======================================================================

(in-package #:clsql-tests)

(setq *rt-connection*
      '(
	
(deftest :connection/1
    (let ((database (clsql:find-database
                     (clsql:database-name clsql:*default-database*)
                     :db-type (clsql:database-type clsql:*default-database*))))
      (eql (clsql:database-type database) *test-database-type*))
  t)

(deftest :connection/2
    (clsql-base-sys::string-to-list-connection-spec 
     "localhost/dbname/user/passwd")
  ("localhost" "dbname" "user" "passwd"))

(deftest :connection/3
    (clsql-base-sys::string-to-list-connection-spec 
     "dbname/user@hostname")
  ("hostname" "dbname" "user"))

))
