;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          conditions.lisp
;;;; Purpose:       Error conditions for high-level SQL interface
;;;; Programmers:   Kevin M. Rosenberg based on
;;;;                 Original code by Pierre R. Mai 
;;;; Date Started:  Feb 2002
;;;;
;;;; $Id$
;;;;
;;;; This file, part of CLSQL, is Copyright (c) 2002-2004 by Kevin M. Rosenberg
;;;; and Copyright (c) 1999-2001 by Pierre R. Mai
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(in-package #:clsql-sys)

(defvar *backend-warning-behavior* :warn
  "Action to perform on warning messages from backend. Default is to :warn. May also be
set to :error to signal an error or :ignore/nil to silently ignore the warning.")

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

(define-condition clsql-generic-error (clsql-error)
  ((message :initarg :message
	    :reader clsql-generic-error-message))
  (:report (lambda (c stream)
	     (format stream (clsql-generic-error-message c)))))

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

(define-condition clsql-access-error (clsql-error)
  ((database-type :initarg :database-type
		  :reader clsql-access-error-database-type)
   (connection-spec :initarg :connection-spec
		    :reader clsql-access-error-connection-spec)
   (error :initarg :error :reader clsql-access-error-error))
  (:report (lambda (c stream)
	     (format stream "While trying to access database ~A~%  using database-type ~A:~%  Error ~A~%  has occurred."
		     (database-name-from-spec
		      (clsql-access-error-connection-spec c)
		      (clsql-access-error-database-type c))
		     (clsql-access-error-database-type c)
		     (clsql-access-error-error c)))))

(define-condition clsql-connect-error (clsql-access-error)
  ((errno :initarg :errno :reader clsql-connect-error-errno))
  (:report (lambda (c stream)
	     (format stream "While trying to connect to database ~A~%  using database-type ~A:~%  Error ~D / ~A~%  has occurred."
		     (database-name-from-spec
		      (clsql-access-error-connection-spec c)
		      (clsql-access-error-database-type c))
		     (clsql-access-error-database-type c)
		     (clsql-connect-error-errno c)
		     (clsql-access-error-error c)))))

(define-condition clsql-sql-error (clsql-error)
  ((database :initarg :database :reader clsql-sql-error-database)
   (message :initarg :message :initform nil :reader clsql-sql-error-message)
   (expression :initarg :expression :initarg nil :reader clsql-sql-error-expression)
   (errno :initarg :errno :initarg nil :reader clsql-sql-error-errno)
   (error :initarg :error :initarg nil :reader clsql-sql-error-error))
  (:report (lambda (c stream)
	     (if (clsql-sql-error-message c)
		 (format stream "While accessing database ~A~%, Error~%  ~A~%  has occurred."
			 (clsql-sql-error-database c)
			 (clsql-sql-error-message c))
	       (format stream "While accessing database ~A~%  with expression ~S:~%  Error ~D / ~A~%  has occurred."
		       (clsql-sql-error-database c)
		       (clsql-sql-error-expression c)
		       (clsql-sql-error-errno c)
		       (clsql-sql-error-error c))))))

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

(define-condition clsql-no-database-error (clsql-error)
  ((database :initarg :database :reader clsql-no-database-error-database))
  (:report (lambda (c stream)
	     (format stream "~S is not a CLSQL database." 
		     (clsql-no-database-error-database c)))))

(define-condition clsql-odbc-error (clsql-error)
  ((odbc-message :initarg :odbc-message
		 :reader clsql-odbc-error-message)
   (sql-state :initarg :sql-state :initform nil
	      :reader clsql-odbc-error-sql-state))
  (:report (lambda (c stream)
	     (format stream "[ODBC error] ~A; state: ~A"
		     (clsql-odbc-error-message c)
		     (clsql-odbc-error-sql-state c)))))

;; Signal conditions


(defun signal-closed-database-error (database)
  (cerror "Ignore this error and return nil."
	  'clsql-closed-error
	  :database database))

(defun signal-no-database-error (database)
  (error 'clsql-no-database-error :database database))

(define-condition clsql-type-error (clsql-error clsql-condition)
  ((slotname :initarg :slotname
	     :reader clsql-type-error-slotname)
   (typespec :initarg :typespec
	     :reader clsql-type-error-typespec)
   (value :initarg :value
	  :reader clsql-type-error-value))
  (:report (lambda (c stream)
	     (format stream
		     "Invalid value ~A in slot ~A, not of type ~A."
		     (clsql-type-error-value c)
		     (clsql-type-error-slotname c)
		     (clsql-type-error-typespec c)))))

(define-condition clsql-sql-syntax-error (clsql-error)
  ((reason :initarg :reason
	   :reader clsql-sql-syntax-error-reason))
  (:report (lambda (c stream)
	     (format stream "Invalid SQL syntax: ~A"
		     (clsql-sql-syntax-error-reason c)))))

