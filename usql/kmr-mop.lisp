;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          kmr-mop.lisp
;;;; Purpose:       MOP support for multiple-implementions
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  Apr 2003
;;;;
;;;; $Id: mop.lisp 8573 2004-01-29 23:30:50Z kevin $
;;;;
;;;; This file was extracted from the KMRCL utilities
;;;; *************************************************************************

;;; This file imports MOP symbols into the USQL-MOP package and then
;;; re-exports into CLSQL-USQL-SYS them to hide differences in
;;; MOP implementations.

(in-package #:clsql-usql-sys)

#+lispworks
(defun intern-eql-specializer (slot)
  `(eql ,slot))

(defmacro process-class-option (metaclass slot-name &optional required)
  #+lispworks
  `(defmethod clos:process-a-class-option ((class ,metaclass)
					   (name (eql ,slot-name))
					   value)
    (when (and ,required (null value))
      (error "metaclass ~A class slot ~A must have a value" (quote ,metaclass) name))
    (list name `',value))
  #-lispworks
    (declare (ignore metaclass slot-name required))
    )

(defmacro process-slot-option (metaclass slot-name)
  #+lispworks
  `(defmethod clos:process-a-slot-option ((class ,metaclass)
					  (option (eql ,slot-name))
					  value
					  already-processed-options
					  slot)
    (list* option `',value already-processed-options))
  #-lispworks
  (declare (ignore metaclass slot-name))
  )

