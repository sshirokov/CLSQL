;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:    odbc-api.lisp
;;;; Purpose: Low-level Odbc interface using UFFI
;;;; Author:  Kevin M. Rosenberg based on 
;;;; Created: April 2004
;;;;
;;;; $Id: odbc-api.lisp 8811 2004-04-02 20:45:48Z kevin $
;;;;
;;;; This file, part of CLSQL, is Copyright (c) 2004 by Kevin M. Rosenberg
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(in-package #:odbc)

(defun connect (&key user password data-source-name)
  (warn "Not implemented.")
  nil)

(defun disconnect (conn)
  (warn "Not implemented."))  


(defun query (expr &key db result-types row-count column-names)
  (warn "Not implemented."))

(defun execute (expr conn)
  (warn "Not implemented."))

(defun close-query (result-set)
  (warn "Not implemented."))

(defun fetch-row (result-set error-eof eof-value)
  (warn "Not implemented."))
