;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          conditions.cl
;;;; Purpose:       Error conditions for high-level SQL interface
;;;; Programmers:   Kevin M. Rosenberg based on
;;;;                 Original code by Pierre R. Mai 
;;;; Date Started:  Feb 2002
;;;;
;;;; $Id$
;;;;
;;;; This file, part of CLSQL, is Copyright (c) 2002 by Kevin M. Rosenberg
;;;; and Copyright (c) 1999-2001 by Pierre R. Mai
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(declaim (optimize (debug 3) (speed 3) (safety 1) (compilation-speed 0)))
(in-package :clsql-base-sys)

;;; Conditions
(define-condition clsql-condition ()
  ())

(define-condition clsql-error (error clsql-condition)
  ())

(define-condition clsql-simple-error (simple-condition clsql-error)
  ())

(define-condition clsql-warning (warning clsql-condition)
  ())

(define-condition clsql-simple-warning (simple-condition clsql-warning)
  ())

(define-condition clsql-invalid-spec-error (clsql-error)
  ((connection-spec :initarg :connection-spec
		    :reader clsql-invalid-spec-error-connection-spec)
   (database-type :initarg :database-type
		  :reader clsql-invalid-spec-error-database-type)
   (template :initarg :template
	     :reader clsql-invalid-spec-error-template))
  (:report (lambda (c stream)
	     (format stream "The connection specification ~A~%is invalid for database type ~A.~%The connection specification must conform to ~A"
		     (clsql-invalid-spec-error-connection-spec c)
		     (clsql-invalid-spec-error-database-type c)
		     (clsql-invalid-spec-error-template c)))))

(defmacro check-connection-spec (connection-spec database-type template)
  "Check the connection specification against the provided template,
and signal an clsql-invalid-spec-error if they don't match."
  `(handler-case
    (destructuring-bind ,template ,connection-spec 
      (declare (ignore ,@(remove '&optional template)))
      t)
    (error () (error 'clsql-invalid-spec-error
		     :connection-spec ,connection-spec
		     :database-type ,database-type
		     :template (quote ,template)))))

(define-condition clsql-connect-error (clsql-error)
  ((database-type :initarg :database-type
		  :reader clsql-connect-error-database-type)
   (connection-spec :initarg :connection-spec
		    :reader clsql-connect-error-connection-spec)
   (errno :initarg :errno :reader clsql-connect-error-errno)
   (error :initarg :error :reader clsql-connect-error-error))
  (:report (lambda (c stream)
	     (format stream "While trying to connect to database ~A~%  using database-type ~A:~%  Error ~D / ~A~%  has occurred."
		     (database-name-from-spec
		      (clsql-connect-error-connection-spec c)
		      (clsql-connect-error-database-type c))
		     (clsql-connect-error-database-type c)
		     (clsql-connect-error-errno c)
		     (clsql-connect-error-error c)))))

(define-condition clsql-sql-error (clsql-error)
  ((database :initarg :database :reader clsql-sql-error-database)
   (expression :initarg :expression :reader clsql-sql-error-expression)
   (errno :initarg :errno :reader clsql-sql-error-errno)
   (error :initarg :error :reader clsql-sql-error-error))
  (:report (lambda (c stream)
	     (format stream "While accessing database ~A~%  with expression ~S:~%  Error ~D / ~A~%  has occurred."
		     (clsql-sql-error-database c)
		     (clsql-sql-error-expression c)
		     (clsql-sql-error-errno c)
		     (clsql-sql-error-error c)))))

(define-condition clsql-database-warning (clsql-warning)
  ((database :initarg :database :reader clsql-database-warning-database)
   (message :initarg :message :reader clsql-database-warning-message))
  (:report (lambda (c stream)
	     (format stream "While accessing database ~A~%  Warning: ~A~%  has occurred."
		     (clsql-database-warning-database c)
		     (clsql-database-warning-message c)))))

(define-condition clsql-exists-condition (clsql-condition)
   ((old-db :initarg :old-db :reader clsql-exists-condition-old-db)
    (new-db :initarg :new-db :reader clsql-exists-condition-new-db
	    :initform nil))
   (:report (lambda (c stream)
	      (format stream "In call to ~S:~%" 'connect)
	      (cond
		((null (clsql-exists-condition-new-db c))
		 (format stream
			 "  There is an existing connection ~A to database ~A."
			 (clsql-exists-condition-old-db c)
			 (database-name (clsql-exists-condition-old-db c))))
		((eq (clsql-exists-condition-new-db c)
		     (clsql-exists-condition-old-db c))
		 (format stream
			 "  Using existing connection ~A to database ~A."
			 (clsql-exists-condition-old-db c)
			 (database-name (clsql-exists-condition-old-db c))))
		(t
		 (format stream
			 "  Created new connection ~A to database ~A~%, although there is an existing connection (~A)."
			 (clsql-exists-condition-new-db c)
			 (database-name (clsql-exists-condition-new-db c))
			 (clsql-exists-condition-old-db c)))))))

(define-condition clsql-exists-warning (clsql-exists-condition
					 clsql-warning)
  ())

(define-condition clsql-exists-error (clsql-exists-condition
				       clsql-error)
  ())

(define-condition clsql-closed-error (clsql-error)
  ((database :initarg :database :reader clsql-closed-error-database))
  (:report (lambda (c stream)
	     (format stream "The database ~A has already been closed."
		     (clsql-closed-error-database c)))))

(define-condition clsql-nodb-error (clsql-error)
  ((database :initarg :database :reader clsql-nodb-error-database))
  (:report (lambda (c stream)
	     (format stream "No such database ~S is open." 
		     (clsql-nodb-error-database c)))))


;; Signal conditions


(defun signal-closed-database-error (database)
  (cerror "Ignore this error and return nil."
	  'clsql-closed-error
	  :database database))

(defun signal-nodb-error (database)
  (cerror "Ignore this error and return nil."
	  'clsql-nodb-error
	  :database database))

