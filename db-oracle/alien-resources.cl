;;; -*- Mode: Lisp -*-
;;; $Id: alien-resources.cl,v 1.1 2002/09/18 07:43:41 kevin Exp $

;;; This is copyrighted software.  See documentation for terms.
;;; 
;;; oracle-sql.lisp --- SQL-Interface implementation for Oracle
;;;
;;; derived from postgresql.lisp

(in-package :clsql-oracle)

(declaim (optimize (speed 3)
		   (debug 1)))

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
		  :buffer (make-alien ,type ,size)))
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



