;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          transactions.cl
;;;; Purpose:       Transaction support
;;;; Programmers:   Kevin M. Rosenberg, Marc Battyani
;;;; Date Started:  Apr 2002
;;;;
;;;; $Id: transactions.cl,v 1.2 2002/05/07 10:19:13 marc.battyani Exp $
;;;;
;;;; This file, part of CLSQL, is Copyright (c) 2002 by Kevin M. Rosenberg
;;;; and Copyright (c) 2000-2002 by onShore Development
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
   (aborted :initform nil :accessor aborted)))

(defmethod database-start-transaction ((database closed-database))
  (signal-closed-database-error database))

(defmethod database-start-transaction (database)
  (unless (transaction database)
    (setf (transaction database) (make-instance 'transaction)))
  (when (= (incf (transaction-level database)) 1)
    (let ((transaction (transaction database)))
      (setf (commit-hooks transaction) nil
	    (rollback-hooks transaction) nil
	    (aborted transaction) nil)
      (execute-command "BEGIN" :database database))))

(defmethod database-end-transaction ((database closed-database) commit)
  (signal-closed-database-error database))

(defmethod database-end-transaction (database commit)
  (when (not commit)
    (setf (aborted (transaction database)) t))
  (if (> (transaction-level database) 0)
    (when (zerop (decf (transaction-level database)))
      (let ((transaction (transaction database)))
	(if (aborted transaction)
	  (unwind-protect
	       (execute-command "ROLLBACK" :database database)
	    (map nil #'funcall (rollback-hooks database)))
	  (progn
	    (execute-command "COMMIT" :database database)
	    (map nil #'funcall (commit-hooks transaction))))))
    (warn "Continue without commit."
	  'clsql-simple-error
	  :format-control "Cannot commit transaction against ~A because there is no transaction in progress."
	  :format-arguments (list database))))

(defun abort-transaction (database)
  (when (transaction database)
    (setf (aborted (transaction database)) t)))

(defun add-transaction-commit-hook (database abort-hook)
  (when (transaction database)
    (push abort-hook (abort-hooks (transaction database)))))

(defun add-transaction-rollback-hook (database rollback-hook)
  (when (transaction database)
    (push rollback-hook (rollback-hooks (transaction database)))))

(defmacro with-transaction ((&key (database '*default-database*) abort-function)
			    &rest body)
  (let ((db (gensym "db"))
	(commit (gensym "commit-")))
    `(let ((,db ,database)
	   (,commit nil))
      (unwind-protect
        (progn
          (database-start-transaction ,db)
          (setf ,commit t)
          ,@body
          (setf ,commit nil))
	(database-end-transaction ,db ,commit)))))
