;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:    utils.lisp
;;;; Purpose: Classes and utilities for testing
;;;; Author:  Kevin M. Rosenberg
;;;; Created: Mar 2002
;;;;
;;;; $Id: tests.lisp 8926 2004-04-10 21:12:52Z kevin $
;;;;
;;;; This file, part of CLSQL, is Copyright (c) 2002-2004 by Kevin M. Rosenberg
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(in-package #:clsql-tests)

(defvar *config-pathname*
  (make-pathname :defaults (user-homedir-pathname)
		 :name ".clsql-test"
		 :type "config"))

(defvar +all-db-types+
  #-clisp '(:postgresql :postgresql-socket :sqlite :mysql :odbc 
	    #+allegro :aodbc)
  #+clisp '(:sqlite))

(defclass conn-specs ()
  ((aodbc :accessor aodbc-spec :initform nil)
   (odbc :accessor odbc-spec :initform nil)
   (mysql :accessor mysql-spec :initform nil)
   (postgresql :accessor postgresql-spec :initform nil)
   (postgresql-socket :accessor postgresql-socket-spec :initform nil)
   (sqlite :accessor sqlite-spec :initform nil))
  (:documentation "Connection specs for CLSQL testing"))


(defun read-specs (&optional (path *config-pathname*))
  (if (probe-file path)
      (with-open-file (stream path :direction :input)
	(let ((specs (make-instance 'conn-specs)))
	  (dolist (spec (read stream) specs)
	    (push (second spec)
		  (slot-value specs (intern (symbol-name (first spec))
					    (find-package '#:clsql-tests)))))))
      (progn
	(warn "CLSQL test config file ~S not found" path)
	nil)))

(defun spec-fn (db-type)
  (intern (concatenate 'string (symbol-name db-type)
		       (symbol-name '#:-spec))
	  (find-package '#:clsql-tests)))

(defun db-type-spec (db-type specs)
  (funcall (spec-fn db-type) specs))

(defun db-type-ensure-system (db-type)
  (unless (find-package (symbol-name db-type))
    (asdf:operate 'asdf:load-op
		  (intern (concatenate 'string
				       (symbol-name '#:clsql-)
				       (symbol-name db-type))))))


