;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          cmucl-compat.sql
;;;; Purpose:       Compatiblity library for CMUCL functions
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  Feb 2002
;;;;
;;;; $Id: cmucl-compat.cl,v 1.3 2002/09/17 17:16:43 kevin Exp $
;;;;
;;;; This file, part of CLSQL, is Copyright (c) 2002 by Kevin M. Rosenberg
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(declaim (optimize (debug 3) (speed 3) (safety 1) (compilation-speed 0)))
(in-package :cl-user)

(defpackage :cmucl-compat
  (:export
   #:shrink-vector
   #:make-sequence-of-type
   #:result-type-or-lose
   #:required-argument
   ))
(in-package :cmucl-compat)

#+cmu
(defmacro required-argument ()
  `(ext:required-argument))

#-cmu
(defun required-argument ()
  (error "~&A required keyword argument was not supplied"))

#+cmu
(defmacro shrink-vector (vec len)
  `(lisp::shrink-vector ,vec ,len))

#-cmu
(defmacro shrink-vector (vec len)
  "Shrinks a vector. Optimized if vector has a fill pointer.
Needs to be a macro to overwrite value of VEC."
  (let ((new-vec (gensym)))
    `(cond
      ((adjustable-array-p ,vec)
       (adjust-array ,vec ,len))
      ((typep ,vec 'simple-array)
       (let ((,new-vec (make-array ,len :element-type
				   (array-element-type ,vec))))
	 (dotimes (i ,len)
	   (declare (fixnum i))
	   (setf (aref ,new-vec i) (aref ,vec i)))
	 (setq ,vec ,new-vec)))
      ((typep ,vec 'vector)
	(setf (fill-pointer ,vec) ,len)
	,vec)
      (t
       (error "Unable to shrink vector ~S which is type-of ~S" ,vec (type-of ,vec))) 
       )))



#-cmu
(defun make-sequence-of-type (type length)
  "Returns a sequence of the given TYPE and LENGTH."
  (declare (fixnum length))
  (case type
    (list 
     (make-list length))
    ((bit-vector simple-bit-vector) 
     (make-array length :element-type '(mod 2)))
    ((string simple-string base-string simple-base-string)
     (make-string length))
    (simple-vector 
     (make-array length))
    ((array simple-array vector)
     (if (listp type)
	 (make-array length :element-type (cadr type))
       (make-array length)))
    (t
     (make-sequence-of-type (result-type-or-lose type t) length))))


#+cmu
(if (fboundp 'lisp::make-sequence-of-type)
    (defun make-sequence-of-type (type len)
      (lisp::make-sequence-of-type type len))
  (defun make-sequence-of-type (type len)
    (system::make-sequence-of-type type len)))
  

#-cmu
(defun result-type-or-lose (type nil-ok)
  (unless (or type nil-ok)
    (error "NIL output type invalid for this sequence function"))
  (case type
    ((list cons)
     'list)
    ((string simple-string base-string simple-base-string)
     'string)
    (simple-vector
     'simple-vector)
    (vector
     'vector)
    (t
     (error "~S is a bad type specifier for sequence functions." type))
    ))

#+cmu
(defun result-type-or-lose (type nil-ok)
  (lisp::result-type-or-lose type nil-ok))
