;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          clsql-uffi.cl
;;;; Purpose:       Common functions for interfaces using UFFI
;;;; Programmers:   Kevin M. Rosenberg
;;;; Date Started:  Mar 2002
;;;;
;;;; $Id: clsql-uffi.cl,v 1.1 2002/03/27 07:58:42 kevin Exp $
;;;;
;;;; This file, part of CLSQL, is Copyright (c) 2002 by Kevin M. Rosenberg
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(declaim (optimize (debug 3) (speed 3) (safety 1) (compilation-speed 0)))
(in-package :clsql-uffi)


(defun canonicalize-type-list (types num-fields)
  "Ensure a field type list meets expectations"
  (let ((length-types (length types))
	(new-types '()))
    (loop for i from 0 below num-fields
	  do
	  (if (>= i length-types)
	      (push t new-types) ;; types is shorted than num-fields
	      (push
	       (case (nth i types)
		 ((:int :long :double :longlong t)
		  (nth i types))
		 (t
		  t))
	       new-types)))
    (nreverse new-types)))

(uffi:def-function "atoi"
    ((str (* :unsigned-char)))
  :returning :int)

(uffi:def-function "atol"
    ((str (* :unsigned-char)))
  :returning :long)

(uffi:def-function "atof"
    ((str (* :unsigned-char)))
  :returning :double)

(uffi:def-function "atol64"
    ((str (* :unsigned-char))
     (high32 (* :int)))
  :returning :int)

(uffi:def-constant +2^32+ 4294967296)
(uffi:def-constant +2^32-1+ (1- +2^32+))

(defmacro make-64-bit-integer (high32 low32)
  `(+ ,low32 (* ,high32 +2^32+)))

(defmacro split-64-bit-integer (int64)
  `(values (ash ,int64 -32) (logand ,int64 +2^32-1+)))

(defun convert-raw-field (char-ptr types index)
  (let ((type (if (listp types)
		  (nth index types)
		  types)))
    (case type
      (:int
       (atoi char-ptr))
      (:long
       (atol char-ptr))
      (:double
       (atof char-ptr))
      (:longlong
       (uffi:with-foreign-object (high32-ptr :int)
	 (let ((low32 (atol64 char-ptr high32-ptr))
	       (high32 (uffi:deref-pointer high32-ptr :int)))
	   (if (zerop high32)
	       low32
	       (make-64-bit-integer high32 low32)))))
      (otherwise
       (uffi:convert-from-foreign-string char-ptr)))))
