;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          sqlite-package.lisp
;;;; Purpose:       Package definition for low-level SQLite interface
;;;; Programmer:    Aurelio Bignoli
;;;; Date Started:  Aug 2003
;;;;
;;;; $Id: sqlite-package.lisp,v 1.2 2003/11/27 20:23:26 aurelio Exp $
;;;;
;;;; This file, part of CLSQL, is Copyright (c) 2003 by Aurelio Bignoli
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(in-package :cl-user)

(defpackage :clsql-sqlite
  (:use :common-lisp :clsql-base-sys)
  (:export #:sqlite-database))
