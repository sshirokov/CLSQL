;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          pool.cl
;;;; Purpose:       Support function for connection pool
;;;; Programmers:   Kevin M. Rosenberg, Marc Battyani
;;;; Date Started:  Apr 2002
;;;;
;;;; $Id: pool.cl,v 1.3 2002/05/01 20:22:16 marc.battyani Exp $
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

(defclass conn-pool ()
  ((connection-spec :accessor connection-spec :initarg :connection-spec)
   (database-type :accessor database-type :initarg :database-type)
   (free-connections :accessor free-connections
		     :initform (make-array 5 :fill-pointer 0 :adjustable t))
   (all-connections :accessor all-connections
		    :initform (make-array 5 :fill-pointer 0 :adjustable t))))

(defun acquire-from-conn-pool (pool)
  (if (zerop (length (free-connections pool)))
    (let ((conn (connect (connection-spec pool)
			 :database-type (database-type pool) :if-exists :new)))
      (vector-push-extend conn (all-connections pool))
      (setf (conn-pool conn) pool)
      conn)
    (vector-pop (free-connections pool))))

(defun release-to-conn-pool (conn)
  (vector-push-extend conn (free-connections (conn-pool conn))))

(defun clear-conn-pool (pool)
  (loop for conn across (all-connections pool)
	do (disconnect :database conn))
  (setf (fill-pointer (free-connections pool)) 0)
  (setf (fill-pointer (all-connections pool)) 0))

(defun find-or-create-conn-pool (connection-spec database-type)
  "Find connection vector in hash table, creates a new conn-vector if not found"
  (let* ((key (list connection-spec database-type))
	 (conn-pool (gethash key *db-pool*)))
    (unless conn-pool
      (setq conn-pool (make-instance 'conn-pool
				     :connection-spec connection-spec
				     :database-type database-type))
      (setf (gethash key *db-pool*) conn-pool))
    conn-pool))

(defun acquire-from-pool (connection-spec database-type &optional pool)
  (unless pool (setf pool (find-or-create-conn-pool connection-spec database-type)))
  (acquire-from-conn-pool pool))

(defun release-to-pool (database)
  (release-to-conn-pool database))

(defun disconnect-pooled (&optional clear)
  "Disconnects all connections in the pool"
  (maphash
   #'(lambda (key conn-pool)
       (declare (ignore key))
       (clear-conn-pool conn-pool))
   *db-pool*)
  (when clear (clrhash *db-pool*))
  t)

;;; with-db-from-pool is the macro you should use if you want to use pooled connections.
;;; You can use it with a connection spec and database type or directly with a conn-pool.
;;; When you give a conn-pool the connection spec and database type are ignored

(defmacro with-db-from-pool ((db-var connection-spec database-type &optional conn-pool) &body body)
  "Evaluate the body in an environment, where `db-var' is bound to a
database connection acquired from the connection pool
The connection is automatically released to the connection pool on exit from the body.
If a pool is given then the connection-spec database-type are ignored."
  `(let ((,db-var (acquire-from-pool ,connection-spec ,database-type ,conn-pool)))
     (unwind-protect
	  (let ((,db-var ,db-var)) ,@body)
       (release-to-pool ,db-var))))
