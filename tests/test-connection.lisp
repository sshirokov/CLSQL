;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; ======================================================================
;;;; File:    test-connection.lisp
;;;; Author:  Marcus Pearce <m.t.pearce@city.ac.uk>
;;;; Created: 30/03/2004
;;;; Updated: <04/04/2004 11:53:49 marcusp>
;;;; ======================================================================
;;;;
;;;; Description ==========================================================
;;;; ======================================================================
;;;;
;;;; Tests for CLSQL-USQL database connections. 
;;;;
;;;; ======================================================================

(in-package :clsql-usql-tests)


(deftest :connection/1
    (let ((database (usql:find-database
                     (usql:database-name usql:*default-database*)
                     :db-type (usql:database-type usql:*default-database*))))
      (eql (usql:database-type database) *test-database-type*))
  t)
