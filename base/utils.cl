;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:         utils.cl
;;;; Purpose:      SQL utility functions
;;;; Programmer:   Kevin M. Rosenberg
;;;; Date Started: Mar 2002
;;;;
;;;; $Id: utils.cl,v 1.1 2002/05/15 17:19:42 kevin Exp $
;;;;
;;;; This file, part of CLSQL, is Copyright (c) 2002 by Kevin M. Rosenberg
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(declaim (optimize (debug 3) (speed 3) (safety 1) (compilation-speed 0)))
(in-package :clsql-sys)

(defun number-to-sql-string (num)
  (etypecase num
    (integer
     num)
    (rational
     (float-to-sql-string (coerce num 'double-float)))
    (number
     (float-to-sql-string num))))

(defun float-to-sql-string (num)
  "Convert exponent character for SQL"
  (substitute #\e #\f (substitute #\e #\d (write-to-string num :readably t))))

(defun sql-escape-quotes (s)
  "Escape single quotes for SQL"
  (substitute-string-for-char s #\' "''"))

(defun substitute-string-for-char (procstr match-char subst-str) 
"Substitutes a string for a single matching character of a string"
  (let ((pos (position match-char procstr)))
    (if pos
	(concatenate 'string
	  (subseq procstr 0 pos) subst-str
	  (substitute-string-for-char 
	   (subseq procstr (1+ pos)) match-char subst-str))
      procstr)))


