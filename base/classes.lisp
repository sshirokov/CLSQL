;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          classes.cl
;;;; Purpose:       Classes for High-level SQL interface
;;;; Programmers:   Kevin M. Rosenberg based on
;;;;                 original code by Pierre R. Mai 
;;;; Date Started:  Feb 2002
;;;;
;;;; $Id: classes.lisp,v 1.1 2002/09/30 10:19:01 kevin Exp $
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


(defclass database ()
  ((name :initform nil :initarg :name :reader database-name)
   (connection-spec :initform nil :initarg :connection-spec :reader connection-spec
		    :documentation "Require to use connection pool")
   (transaction-level :initform 0 :accessor transaction-level)
   (transaction :initform nil :accessor transaction)
   (conn-pool :initform nil :initarg :conn-pool :accessor conn-pool))
  (:documentation
   "This class is the supertype of all databases handled by CLSQL."))

(defmethod print-object ((object database) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (write-string (if (slot-boundp object 'name)
		      (database-name object)
		      "<unbound>")
		  stream)))

;; Closed database idea and original code comes from UncommonSQL

(defclass closed-database ()
  ((name :initarg :name :reader database-name))
  (:documentation
   "This class represents databases after they are closed via 'disconnect'."))

(defmethod print-object ((object closed-database) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (write-string (if (slot-boundp object 'name)
		      (database-name object)
		      "<unbound>")
		  stream)))

