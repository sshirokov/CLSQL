;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          pool.cl
;;;; Purpose:       Support function for connection pool
;;;; Programmers:   Kevin M. Rosenberg
;;;; Date Started:  Apr 2002
;;;;
;;;; $Id: pool.cl,v 1.1 2002/04/27 20:58:11 kevin Exp $
;;;;
;;;; This file, part of CLSQL, is Copyright (c) 2002 by Kevin M. Rosenberg
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(declaim (optimize (debug 3) (speed 3) (safety 1) (compilation-speed 0)))
(in-package :clsql-sys)

(defvar *db-pool* (make-hash-table :test #'equal))

(defun make-conn-vector ()
  "Creates an empty connection vector"
  (make-array 5 :fill-pointer 0 :adjustable t))

(defun find-or-create-conn-vector (connection-spec database-type)
  "Find connection vector in hash table, creates a new conn-vector if not found"
  (let* ((key (list connection-spec database-type))
	 (conn-vector (gethash *db-pool* key)))
    (unless conn-vector
      (setq conn-vector (make-conn-vector))
      (setf (gethash *db-pool* key) conn-vector))
    conn-vector))

(defun acquire-from-pool (connection-spec database-type)
  (let ((conn-vector (find-or-create-conn-vector connection-spec database-type)))
    (when (zerop (length conn-vector))
      (vector-push-extend 
       (connect connection-spec :database-type database-type :if-exists :new) 
       conn-vector))
    (vector-pop conn-vector)))

(defun release-to-pool (database)
  (let ((conn-vector (find-or-create-conn-vector (connection-spec database)
					   (database-type database))))
    (vector-push-extend database conn-vector)))

(defun disconnect-pooled ()
  "Disconnects all connections in the pool"
  (maphash
   #'(lambda (key conn-vector)
       (declare (ignore key))
       (dotimes (i (length conn-vector))
	 (disconnect (aref conn-vector i)))
       (setf (fill-pointer conn-vector) 0))
   *db-pool*)
  t)
