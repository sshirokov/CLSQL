;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          alien-resources.lisp
;;;;
;;;; $Id$
;;;;
;;;; This file, part of CLSQL, is Copyright (c) 2002 by Kevin M. Rosenberg
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(in-package #:clsql-oracle)

(defparameter *alien-resource-hash* (make-hash-table :test #'equal))

(defun %get-resource (type sizeof)
  (let ((resources (gethash type *alien-resource-hash*)))
    (car (member-if
	  #'(lambda (res)
	      (and (= (alien-resource-sizeof res) sizeof)
		   (not (alien-resource-in-use res))))
	  resources))))

(defun %insert-alien-resource (type res)
  (let ((resource (gethash type *alien-resource-hash*)))
    (setf (gethash type *alien-resource-hash*)
	  (cons res (gethash type *alien-resource-hash*)))))

(defmacro acquire-alien-resource (type &optional size)
  `(let ((res (%get-resource ',type ,size)))
     (unless res
       (setf res (make-alien-resource
		  :type ',type :sizeof ,size
		  :buffer (alien:make-alien ,type ,size)))
       (%insert-alien-resource ',type res))
     (claim-alien-resource res)))
	       
(defstruct (alien-resource)
  (type (error "Missing TYPE.")
	:read-only t)
  (sizeof (error "Missing SIZEOF.")
	  :read-only t)
  (buffer (error "Missing BUFFER.")
	  :read-only t)
  (in-use nil :type boolean))

(defun free-alien-resource (ares)
  (setf (alien-resource-in-use ares) nil)
  ares)

(defun claim-alien-resource (ares)
  (setf (alien-resource-in-use ares) t)
  ares)



