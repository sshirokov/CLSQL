;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          clsql-uffi.cl
;;;; Purpose:       Common functions for interfaces using UFFI
;;;; Programmers:   Kevin M. Rosenberg
;;;; Date Started:  Mar 2002
;;;;
;;;; $Id: clsql-uffi.lisp,v 1.15 2003/05/16 09:16:25 kevin Exp $
;;;;
;;;; This file, part of CLSQL, is Copyright (c) 2002 by Kevin M. Rosenberg
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(declaim (optimize (debug 3) (speed 3) (safety 1) (compilation-speed 0)))
(in-package :clsql-uffi)


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

(defvar +ascii-N-value+ (char-code #\N))
(defvar +ascii-U-value+ (char-code #\U))

(uffi:def-type char-ptr-def (* :unsigned-char))

(defun char-ptr-points-to-null (char-ptr)
  "Returns T if foreign character pointer refers to 'NULL' string. Only called for numeric entries"
  ;; Uses short cut and returns T if first character is #\N. It should
  ;; never be non-numeric
  (declare (type char-ptr-def char-ptr))
  (let ((char (uffi:ensure-char-character
	       (uffi:deref-pointer char-ptr :char))))
    (eql char #\N)))
    
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
	  ;; Choose optimized routine
	  #-(or cmu sbcl)
	  (native-to-string char-ptr)
	  #+(or cmu sbcl)
	  (uffi:convert-from-foreign-string char-ptr)))))))
  

(uffi:def-function "strlen"
    ((str (* :unsigned-char)))
  :returning :unsigned-int)

(defun native-to-string (s)
  (declare (optimize (speed 3) (space 0) (safety 0) (compilation-speed 0))
	   (type char-ptr-def s))
  (let* ((len (strlen s))
	 (str (make-string len)))
    (declare (fixnum len)
	     (simple-string str))
    (do ((i 0))
	((= i len))
      (declare (fixnum i))
      (setf (schar str i)
	(code-char (uffi:deref-array s '(:array :unsigned-char) i)))
      (incf i))
    str))

#-allegro
(defun native-to-string (s)
  (declare (optimize (speed 3) (space 0) (safety 0) (compilation-speed 0))
	   (type char-ptr-def s))
  (let* ((len (strlen s))
	 (str (make-string len)))
    (declare (fixnum len)
	     (simple-string str))
    (do ((i 0))
	((= i len))
      (declare (fixnum i))
      (setf (schar str i)
	(code-char (uffi:deref-array s '(:array :unsigned-char) i)))
      (incf i))
    str))

#+allegro
(excl:ics-target-case
 (:+ics
  (defun native-to-string (s)
    (declare (optimize (speed 3) (space 0) (safety 0) (compilation-speed 0))
	     (type char-ptr-def s))
    (let* ((len (strlen s))
	   (str (make-string len)))
      (declare (fixnum len)
	       (simple-string str))
      (do ((i 0))
	  ((= i len))
	(declare (fixnum i))
	(setf (schar str i)
	      (code-char (uffi:deref-array s '(:array :unsigned-char) i)))
	(incf i))
    str)))
 (:-ics
  (defun native-to-string (s)
  (declare (optimize (speed 3) (space 0) (safety 0) (compilation-speed 0))
	   (type char-ptr-def s))
  (let* ((len (strlen s))
	 (str (make-string len)))
    (declare (fixnum len)
	     (type (simple-array (unsigned-byte 8) (*)) str))
    #+ignore
    (do ((i 0))
	((= i len4))
      (declare (fixnum i))
      (setf (aref (the (simple-array (unsigned-byte 32) (*)) str) i)
	    (uffi:deref-array s '(:array :unsigned-int) i))
      (incf i))
    (do ((i 0))
	((= i len))
      (declare (fixnum i))
      (setf (aref str i) (uffi:deref-array s '(:array :unsigned-char) i))
      (incf i))
    str))))
