;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          mysql-sql.cl
;;;; Purpose:       High-level MySQL interface using UFFI
;;;; Programmers:   Kevin M. Rosenberg based on
;;;;                Original code by Pierre R. Mai 
;;;; Date Started:  Feb 2002
;;;;
;;;; $Id: mysql-sql.cl,v 1.6 2002/03/24 22:25:51 kevin Exp $
;;;;
;;;; This file, part of CLSQL, is Copyright (c) 2002 by Kevin M. Rosenberg
;;;; and Copyright (c) 1999-2001 by Pierre R. Mai
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(declaim (optimize (debug 3) (speed 3) (safety 1) (compilation-speed 0)))

;;;; Modified by Kevin Rosenberg, Feb 20002
;;;; -- Added support for Allegro CL and Lispworks using UFFI layer
;;;; -- Changed database-connect to use mysql-real-connect. This way,
;;;;    can avoid using double (unwind-protect)
;;;; -- Changed database-connect to have MySQL library allocate space
;;;;    for MYSQL structure. This will make the code more robust in
;;;;    the event that MySQL library changes the size of the mysql-mysql
;;;;    structure.
;;;;
;;;; Mar 2002
;;;; Added field types

(defpackage :clsql-mysql
    (:use :common-lisp :clsql-sys :mysql)
    (:export #:mysql-database)
    (:documentation "This is the CLSQL interface to MySQL."))

(in-package :clsql-mysql)

(defmethod database-initialize-database-type ((database-type (eql :mysql)))
  t)

(uffi:def-type mysql-mysql-ptr-def (* mysql-mysql))
(uffi:def-type mysql-row-def mysql-row)
(uffi:def-type mysql-mysql-res-ptr-def (* mysql-mysql-res))

(defclass mysql-database (database)
  ((mysql-ptr :accessor database-mysql-ptr :initarg :mysql-ptr
	      :type mysql-mysql-ptr-def)))

(defmethod database-name-from-spec (connection-spec (database-type (eql :mysql)))
  (check-connection-spec connection-spec database-type (host db user password))
  (destructuring-bind (host db user password) connection-spec
    (declare (ignore password))
    (concatenate 'string host "/" db "/" user)))

(defmethod database-connect (connection-spec (database-type (eql :mysql)))
  (check-connection-spec connection-spec database-type (host db user password))
  (destructuring-bind (host db user password) connection-spec
    (let ((mysql-ptr (mysql-init (uffi:make-null-pointer 'mysql-mysql)))
	  (socket nil))
      (if (uffi:null-pointer-p mysql-ptr)
	  (error 'clsql-connect-error
		 :database-type database-type
		 :connection-spec connection-spec
		 :errno (mysql-errno mysql-ptr)
		 :error (mysql-error-string mysql-ptr))
	(uffi:with-cstrings ((host-native host)
			    (user-native user)
			    (password-native password)
			    (db-native db)
			    (socket-native socket))
	  (let ((error-occurred nil))
	    (unwind-protect
		(if (uffi:null-pointer-p 
		     (mysql-real-connect 
		      mysql-ptr host-native user-native password-native
		      db-native 0 socket-native 0))
		    (progn
		      (setq error-occurred t)
		      (error 'clsql-connect-error
			     :database-type database-type
			     :connection-spec connection-spec
			     :errno (mysql-errno mysql-ptr)
			     :error (mysql-error-string mysql-ptr)))
		  (make-instance 'mysql-database
		    :name (database-name-from-spec connection-spec
						   database-type)
		    :mysql-ptr mysql-ptr))
	      (when error-occurred (mysql-close mysql-ptr)))))))))


(defmethod database-disconnect ((database mysql-database))
  (mysql-close (database-mysql-ptr database))
  (setf (database-mysql-ptr database) nil)
  t)


(defstruct mysql-result-set
  (res-ptr (uffi:make-null-pointer 'mysql-mysql-res)
	   :type mysql-mysql-res-ptr-def)
  (field-types nil)
  (full-set nil :type boolean))

(defmethod database-dump-result-set (result-set (database mysql-database))
  (mysql-free-result (mysql-result-set-res-ptr result-set))
  t)


(defmethod database-store-next-row (result-set (database mysql-database) list)
  (let* ((res-ptr (mysql-result-set-res-ptr result-set))
	 (row (mysql-fetch-row res-ptr)))
    (declare (type mysql-mysql-res-ptr-def res-ptr)
	     (type mysql-row-def row))
    (unless (uffi:null-pointer-p row)
      (loop for i from 0 below (mysql-num-fields res-ptr)
	  for rest on list
	  do
	    (setf (car rest) 
	      (uffi:convert-from-foreign-string (uffi:deref-array row 'mysql-row i))))
      list)))


(defmethod database-execute-command (sql-expression (database mysql-database))
  (uffi:with-cstring (sql-native sql-expression)
    (let ((mysql-ptr (database-mysql-ptr database)))
      (declare (type mysql-mysql-ptr-def mysql-ptr))
      (if (zerop (mysql-query mysql-ptr sql-native))
	  t
	(error 'clsql-sql-error
	       :database database
	       :expression sql-expression
	       :errno (mysql-errno mysql-ptr)
	       :error (mysql-error-string mysql-ptr))))))



(defmethod database-query (query-expression (database mysql-database) 
			   field-types)
  (with-slots (mysql-ptr) database
    (uffi:with-cstring (query-native query-expression)
       (if (zerop (mysql-query mysql-ptr query-native))
	   (let ((res-ptr (mysql-use-result mysql-ptr)))
	     (if res-ptr
		 (unwind-protect
		     (loop for row = (mysql-fetch-row res-ptr)
			 until (uffi:null-pointer-p row)
			 collect
			   (loop for i from 0 below (mysql-num-fields res-ptr)
			       collect
				 (uffi:convert-from-foreign-string
				  (uffi:deref-array row 'mysql-row i))))
		   (mysql-free-result res-ptr))
	       (error 'clsql-sql-error
		      :database database
		      :expression query-expression
		      :errno (mysql-errno mysql-ptr)
		      :error (mysql-error-string mysql-ptr))))
	 (error 'clsql-sql-error
		:database database
		:expression query-expression
		:errno (mysql-errno mysql-ptr)
		:error (mysql-error-string mysql-ptr))))))


(defmethod database-query-result-set (query-expression (database mysql-database)
				      &key full-set field-types)
  (uffi:with-cstring (query-native query-expression)
    (let ((mysql-ptr (database-mysql-ptr database)))
     (declare (type mysql-mysql-ptr-def mysql-ptr))
      (if (zerop (mysql-query mysql-ptr query-native))
	  (let ((res-ptr (if full-set
			     (mysql-store-result mysql-ptr)
			   (mysql-use-result mysql-ptr))))
	    (declare (type mysql-mysql-res-ptr-def res-ptr))
	    (if (not (uffi:null-pointer-p res-ptr))
		(if full-set
		    (values (make-mysql-result-set :res-ptr res-ptr :full-set t
						   :field-types field-types)
			    (mysql-num-fields res-ptr)
			    (mysql-num-rows res-ptr))
		  (values (make-mysql-result-set :res-ptr res-ptr)
			  (mysql-num-fields res-ptr)))
	      (error 'clsql-sql-error
		     :database database
		     :expression query-expression
		     :errno (mysql-errno mysql-ptr)
		     :error (mysql-error-string mysql-ptr))))
	(error 'clsql-sql-error
	       :database database
	       :expression query-expression
	       :errno (mysql-errno mysql-ptr)
	       :error (mysql-error-string mysql-ptr))))))


