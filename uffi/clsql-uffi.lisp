;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          clsql-uffi.cl
;;;; Purpose:       Common functions for interfaces using UFFI
;;;; Programmers:   Kevin M. Rosenberg
;;;; Date Started:  Mar 2002
;;;;
;;;; $Id: clsql-uffi.lisp,v 1.29 2003/06/06 21:59:09 kevin Exp $
;;;;
;;;; This file, part of CLSQL, is Copyright (c) 2002 by Kevin M. Rosenberg
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(in-package #:clsql-uffi)


(defun canonicalize-type-list (types auto-list)
  "Ensure a field type list meets expectations"
  (let ((length-types (length types))
	(new-types '()))
    (loop for i from 0 below (length auto-list)
	  do
	  (if (>= i length-types)
	      (push t new-types) ;; types is shorted than num-fields
	      (push
	       (case (nth i types)
		 (:int
		  (case (nth i auto-list)
		    (:int32
		     :int32)
		    (:int64
		     :int64)
		    (t
		     t)))
		 (:double
		  (case (nth i auto-list)
		    (:double
		     :double)
		    (t
		     t)))
		 (:int32
		  (if (eq :int32 (nth i auto-list))
		      :int32
		    t))
		 (:int64
		  (if (eq :int64 (nth i auto-list))
		      :int64
		    t))
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
  :returning :unsigned-int)

(uffi:def-constant +2^32+ 4294967296)
(uffi:def-constant +2^32-1+ (1- +2^32+))

(defmacro make-64-bit-integer (high32 low32)
  `(+ ,low32 (* ,high32 +2^32+)))

(defmacro split-64-bit-integer (int64)
  `(values (ash ,int64 -32) (logand ,int64 +2^32-1+)))

(uffi:def-type char-ptr-def (* :unsigned-char))

(defun char-ptr-points-to-null (char-ptr)
  "Returns T if foreign character pointer refers to 'NULL' string. Only called for numeric entries"
  ;; Uses short cut and returns T if first character is #\N. It should
  ;; never be non-numeric
  (declare (type char-ptr-def char-ptr))
  (char-equal #\N (uffi:ensure-char-character
		   (uffi:deref-pointer char-ptr :char))))
    
(defun convert-raw-field (char-ptr types index)
  (let ((type (if (listp types)
		  (nth index types)
		  types)))
    (cond
      ((and (or (eq type :double) (eq type :int32) (eq type :int)
		(eq type :int64))
	    (char-ptr-points-to-null char-ptr))
       nil)
      (t
       (case type
	 (:double
	  (atof char-ptr))
	 ((or :int32 :int)
	  (atoi char-ptr))
	 (:int64
	  (uffi:with-foreign-object (high32-ptr :int)
	    (let ((low32 (atol64 char-ptr high32-ptr))
		  (high32 (uffi:deref-pointer high32-ptr :int)))
	      (if (zerop high32)
		  low32
		  (make-64-bit-integer high32 low32)))))
	 (t
	  (uffi:convert-from-foreign-string char-ptr :locale :none)))))))
  
