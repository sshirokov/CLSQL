;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:         utils.lisp
;;;; Purpose:      SQL utility functions
;;;; Programmer:   Kevin M. Rosenberg
;;;; Date Started: Mar 2002
;;;;
;;;; $Id$
;;;;
;;;; This file, part of CLSQL, is Copyright (c) 2002-2004 by Kevin M. Rosenberg
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(in-package #:clsql-base-sys)

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
  (let ((str (write-to-string num :readably t)))
    (cond
     ((find #\f str)
      (substitute #\e #\f str))
     ((find #\d str)
      (substitute #\e #\d str))
     ((find #\l str)
      (substitute #\e #\l str))
     ((find #\s str)
      (substitute #\e #\S str))
     ((find #\F str)
      (substitute #\e #\F str))
     ((find #\D str)
      (substitute #\e #\D str))
     ((find #\L str)
      (substitute #\e #\L str))
     ((find #\S str)
      (substitute #\e #\S str))
     (t
      str))))

(defun sql-escape (identifier)
  "Change hyphens to underscores, ensure string"
  (let* ((unescaped (etypecase identifier
                      (symbol (symbol-name identifier))
                      (string identifier)))
         (escaped (make-string (length unescaped))))
    (dotimes (i (length unescaped))
      (setf (char escaped i)
            (cond ((equal (char unescaped i) #\-)
                   #\_)
                  ;; ...
                  (t
                   (char unescaped i)))))
    escaped))


(defun sql-escape-quotes (s)
  "Escape quotes for SQL string writing"
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


(defun delimited-string-to-list (string &optional (separator #\space) 
						  skip-terminal)
  "Split a string with delimiter, from KMRCL."
  (declare (optimize (speed 3) (safety 0) (space 0) (compilation-speed 0))
	   (type string string)
	   (type character separator))
  (do* ((len (length string))
	(output '())
	(pos 0)
	(end (position-char separator string pos len)
	     (position-char separator string pos len)))
       ((null end)
	(if (< pos len)
	    (push (subseq string pos) output)
	    (when (or (not skip-terminal) (zerop len))
	      (push "" output)))
	(nreverse output))
    (declare (type fixnum pos len)
	     (type (or null fixnum) end))
    (push (subseq string pos end) output)
    (setq pos (1+ end))))

(defun string-to-list-connection-spec (str)
  (let ((at-pos (position #\@ str)))
    (cond
      ((and at-pos (> (length str) at-pos))
       ;; Connection spec is SQL*NET format
       (append (delimited-string-to-list (subseq str 0 at-pos) #\/)
	       (list (subseq str (1+ at-pos)))))
      (t
       (delimited-string-to-list str #\/)))))
