;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          package.lisp
;;;; Purpose:       Package file clsql testing suite
;;;; Author:        Kevin M. Rosenberg
;;;; Date Started:  Apr 2003
;;;;
;;;; $Id$
;;;; *************************************************************************

(in-package #:cl-user)

(defpackage #:clsql-classic-tests
  (:use #:asdf #:cl #:clsql #:ptester)
  (:export
   #:*config-pathname*
   #:+all-db-types+
   #:conn-specs
   #:aodbc-spec
   #:mysql-spec
   #:postgresql-spec
   #:postgresql-socket-spec
   #:sqlite-spec
   #:read-specs
   #:db-type-spec
   #:db-type-ensure-system
   ))



