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

(in-package #:clsql-sys)

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
(defgeneric postinitialize  (object)
  )
(defgeneric add-to-relation  (target slot-name value)
  )
(defgeneric remove-from-relation  (target slot-name value)
  )

