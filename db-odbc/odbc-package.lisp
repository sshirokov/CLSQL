;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:     odbc-package.lisp
;;;; Purpose:  Package definition for low-level ODBC interface
;;;; Author:   Kevin M. Rosenberg
;;;; Created:  April 2004
;;;;
;;;; $Id: odbc-package.lisp 7061 2003-09-07 06:34:45Z kevin $
;;;;
;;;; This file, part of CLSQL, is Copyright (c) 2004 by Kevin M. Rosenberg
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(in-package #:cl-user)

(defpackage #:odbc
    (:use #:cl #:clsql-uffi)
    (:export 
     #:database-library-loaded

     #:connect
     #:disconnect
     #:query
     #:execute
     #:close-query
     #:fetch-row
     )
    (:documentation "This is the low-level interface ODBC."))
