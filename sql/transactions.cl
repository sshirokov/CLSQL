;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          transactions.cl
;;;; Purpose:       Transaction support
;;;; Programmers:   Kevin M. Rosenberg based
;;;;                 Original code by onShore Development Inc.
;;;; Date Started:  Apr 2002
;;;;
;;;; $Id: transactions.cl,v 1.1 2002/04/27 21:48:08 kevin Exp $
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

;; This code is copied verbatim from UncommonSQL. It is intended to
;; provide transaction support to CLSQL users that, for whatever reason, 
;; don't want to use the upcoming UncommonSQL/CLSQL combination.

(defmethod database-start-transaction ((database closed-database))
  (signal-closed-database-error database))

(defmethod database-start-transaction (database)
  (unless database
    (error 'clsql-nodb-error))
  (with-accessors ((level transaction-level))
    database
    (incf level)
    (when (= level 1)
      (execute-command "BEGIN" :database database))))

(defmethod database-commit-transaction ((database closed-database))
  (signal-closed-database-error database))

(defmethod database-commit-transaction (database)
  (with-accessors ((level transaction-level))
    database
    (if (< 0 level)
        (progn
          (decf level)
          (when (= level 0)
            (execute-command "COMMIT" :database database)))
      (warn "Continue without commit."
	    'clsql-simple-error
	    :format-control "Cannot commit transaction against ~A because there is no transaction in progress."
	    :format-arguments (list database)))))

(defmethod database-abort-transaction ((database closed-database))
  (signal-closed-database-error database))

(defmethod database-abort-transaction (database)
  (with-accessors ((level transaction-level))
    database
  (if (< 0 level)
      (progn
        (setf level 0)
        (execute-command "ROLLBACK" :database database))
      (warn "Continue without abort."
              'clsql-simple-error
              :format-control "Cannot abort transaction against ~A because there is no transaction in progress."
              :format-arguments (list database)))))

(defvar *transaction-level* 0)
(defvar *transaction-id* nil)

(defvar *transaction-aborts* (make-hash-table))
(defvar *transaction-completes* (make-hash-table))

(defun on-txn-abort (fn)
  (push (cons *transaction-level* fn) (gethash *transaction-id* *transaction-aborts*)))

(defun on-txn-complete (fn)
  (if (> *transaction-level* 0)
      (push fn (gethash *transaction-id* *transaction-completes*))
      (warn "Cannot define on-txn-complete actions outside of transactions.")))

(defun run-abort-hooks ()
  (let ((remainder (remove-if (lambda (hook)
                                (< (car hook) *transaction-level*))
                              (gethash *transaction-id* *transaction-aborts*))))
    (mapcar #'(lambda (hook)
                (funcall (cdr hook)))
            (gethash *transaction-id* *transaction-aborts*))
    (setf (gethash *transaction-id* *transaction-aborts*) remainder)))
    

(defmacro with-transaction ((&key database)
			    &rest body)
  (let ((dbsym (gensym "db"))
	(transym (gensym "tran")))
    `(let ((,dbsym (or ,database *default-database*))
	   (,transym nil)
           (*transaction-id* (or *transaction-id*
                                 (gensym "txn")))
           (*transaction-level* (1+ *transaction-level*)))
      (unwind-protect
        (progn
          (database-start-transaction ,dbsym)
          (setf ,transym t)
          ,@body
          (database-commit-transaction ,dbsym)
          (setf ,transym nil))
        (if ,transym
            (progn                      ; was aborted
              (database-abort-transaction ,dbsym)
              ;; (format t "~&;; Transaction Abort, level ~d~%" *transaction-level*)
              (run-abort-hooks)
              (when (= 1 *transaction-level*)
                (remhash  *transaction-id* *transaction-aborts*)))
            (when (= 1 *transaction-level*)
              (let ((completes (gethash *transaction-id* *transaction-completes*)))
                ;; (format t "~&;; Running ~d post actions.~%" (length completes))
                (mapcar #'funcall completes)
                (remhash  *transaction-id* *transaction-completes*))))))))

