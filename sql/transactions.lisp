;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          transactions.cl
;;;; Purpose:       Transaction support
;;;; Programmers:   Marc Battyani
;;;; Date Started:  Apr 2002
;;;;
;;;; $Id$
;;;;
;;;; This file, part of CLSQL, is Copyright (c) 2002 by Kevin M. Rosenberg
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(declaim (optimize (debug 3) (speed 3) (safety 1) (compilation-speed 0)))
(in-package :clsql-sys)

;; I removed the USQL transaction stuff to put a smaller, lighter one (MB)

(defclass transaction ()
  ((commit-hooks :initform () :accessor commit-hooks)
   (rollback-hooks :initform () :accessor rollback-hooks)
   (status :initform nil :accessor status))) ;can be nil :rolled-back or :commited

(defgeneric database-start-transaction (database))
(defmethod database-start-transaction ((database closed-database))
  (error 'clsql-closed-database-error database))

(defmethod database-start-transaction (database)
  (unless (transaction database)
    (setf (transaction database) (make-instance 'transaction)))
  (when (= (incf (transaction-level database)) 1)
    (let ((transaction (transaction database)))
      (setf (commit-hooks transaction) nil
	    (rollback-hooks transaction) nil
	    (status transaction) nil)
      (execute-command "BEGIN" :database database))))

(defgeneric database-end-transaction (database))
(defmethod database-end-transaction ((database closed-database))
  (error 'clsql-closed-database-error database))

(defmethod database-end-transaction (database)
  (if (> (transaction-level database) 0)
    (when (zerop (decf (transaction-level database)))
      (let ((transaction (transaction database)))
	(if (eq (status transaction) :commited)
	  (progn
	    (execute-command "COMMIT" :database database)
	    (map nil #'funcall (commit-hooks transaction)))
	  (unwind-protect ;status is not :commited
	       (execute-command "ROLLBACK" :database database)
	    (map nil #'funcall (rollback-hooks transaction))))))
    (warn "Continue without commit."
	  'clsql-simple-error
	  :format-control "Cannot commit transaction against ~A because there is no transaction in progress."
	  :format-arguments (list database))))

(defun rollback-transaction (database)
  (when (and (transaction database)(not (status (transaction database))))
    (setf (status (transaction database)) :rolled-back)))

(defun commit-transaction (database)
  (when (and (transaction database)(not (status (transaction database))))
    (setf (status (transaction database)) :commited)))

(defun add-transaction-commit-hook (database commit-hook)
  (when (transaction database)
    (push commit-hook (commit-hooks (transaction database)))))

(defun add-transaction-rollback-hook (database rollback-hook)
  (when (transaction database)
    (push rollback-hook (rollback-hooks (transaction database)))))

(defmacro with-transaction ((&key (database '*default-database*)) &rest body)
  (let ((db (gensym "db-")))
    `(let ((,db ,database))
      (unwind-protect
	   (progn
	     (database-start-transaction ,db)
	     ,@body
	     (commit-transaction ,db))
	(database-end-transaction ,db)))))
