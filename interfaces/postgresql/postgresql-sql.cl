;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          postgresql-sql.sql
;;;; Purpose:       High-level PostgreSQL interface using UFFI
;;;; Programmers:   Kevin M. Rosenberg based on
;;;;                Original code by Pierre R. Mai 
;;;; Date Started:  Feb 2002
;;;;
;;;; $Id: postgresql-sql.cl,v 1.3 2002/03/24 18:08:27 kevin Exp $
;;;;
;;;; This file, part of CLSQL, is Copyright (c) 2002 by Kevin M. Rosenberg
;;;; and Copyright (c) 1999-2001 by Pierre R. Mai
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(declaim (optimize (debug 3) (speed 3) (safety 1) (compilation-speed 0)))
(in-package :cl-user)

(defpackage :clsql-postgresql
    (:use :common-lisp :clsql-sys :postgresql)
    (:export #:postgresql-database)
    (:documentation "This is the CLSQL interface to PostgreSQL."))

(in-package :clsql-postgresql)


(defun tidy-error-message (message)
  (unless (stringp message)
    (setq message (uffi:convert-from-foreign-string message)))
  (let ((message (string-right-trim '(#\Return #\Newline) message)))
    (cond
      ((< (length message) (length "ERROR:"))
       message)
      ((string= message "ERROR:" :end1 6)
       (string-left-trim '(#\Space) (subseq message 6)))
      (t
       message))))

(defmethod database-initialize-database-type ((database-type
					       (eql :postgresql)))
  t)

(uffi:def-type pgsql-conn-def pgsql-conn)
(uffi:def-type pgsql-result-def pgsql-result)


(defclass postgresql-database (database)
  ((conn-ptr :accessor database-conn-ptr :initarg :conn-ptr
	     :type pgsql-conn-def)))

(defmethod database-name-from-spec (connection-spec (database-type
						     (eql :postgresql)))
  (check-connection-spec connection-spec database-type
			 (host db user password &optional port options tty))
  (destructuring-bind (host db user password &optional port options tty)
      connection-spec
    (declare (ignore password options tty))
    (concatenate 'string host (if port ":") (if port port) "/" db "/" user)))


(defmethod database-connect (connection-spec (database-type (eql :postgresql)))
  (check-connection-spec connection-spec database-type
			 (host db user password &optional port options tty))
  (destructuring-bind (host db user password &optional port options tty)
      connection-spec
    (uffi:with-cstrings ((host-native host)
			 (user-native user)
			 (password-native password)
			 (db-native db)
			 (port-native port)
			 (options-native options)
			 (tty-native tty))
      (let ((connection (PQsetdbLogin host-native port-native
				      options-native tty-native
				      db-native user-native
				      password-native)))
	(declare (type pgsql-conn-def connection))
	(when (not (eq (PQstatus connection) 
		       pgsql-conn-status-type#connection-ok))
	  (error 'clsql-connect-error
		 :database-type database-type
		 :connection-spec connection-spec
		 :errno (PQstatus connection)
		 :error (tidy-error-message 
			 (PQerrorMessage connection))))
	(make-instance 'postgresql-database
		       :name (database-name-from-spec connection-spec
						      database-type)
		       :conn-ptr connection)))))


(defmethod database-disconnect ((database postgresql-database))
  (PQfinish (database-conn-ptr database))
  (setf (database-conn-ptr database) nil)
  t)

(defmethod database-query (query-expression (database postgresql-database) field-types)
  (let ((conn-ptr (database-conn-ptr database)))
    (declare (type pgsql-conn-def conn-ptr))
    (uffi:with-cstring (query-native query-expression)
      (let ((result (PQexec conn-ptr query-native)))
        (when (uffi:null-pointer-p result)
          (error 'clsql-sql-error
                 :database database
                 :expression query-expression
                 :errno nil
                 :error (tidy-error-message (PQerrorMessage conn-ptr))))
        (unwind-protect
            (case (PQresultStatus result)
              (#.pgsql-exec-status-type#empty-query
               nil)
              (#.pgsql-exec-status-type#tuples-ok
               (loop for tuple-index from 0 below (PQntuples result)
                   collect
                     (loop for i from 0 below (PQnfields result)
                         collect
                           (if (zerop (PQgetisnull result tuple-index i))
                               (uffi:convert-from-cstring
                                (PQgetvalue result tuple-index i))
                             nil))))
              (t
               (error 'clsql-sql-error
                      :database database
                      :expression query-expression
                      :errno (PQresultStatus result)
                      :error (tidy-error-message
                              (PQresultErrorMessage result)))))
          (PQclear result))))))

(defmethod database-execute-command (sql-expression
                                     (database postgresql-database))
  (let ((conn-ptr (database-conn-ptr database)))
    (declare (type pgsql-conn-def conn-ptr))
    (uffi:with-cstring (sql-native sql-expression)
      (let ((result (PQexec conn-ptr sql-native)))
        (when (uffi:null-pointer-p result)
          (error 'clsql-sql-error
                 :database database
                 :expression sql-expression
                 :errno nil
                 :error (tidy-error-message (PQerrorMessage conn-ptr))))
        (unwind-protect
            (case (PQresultStatus result)
              (#.pgsql-exec-status-type#command-ok
               t)
              ((#.pgsql-exec-status-type#empty-query
                #.pgsql-exec-status-type#tuples-ok)
               (warn "Strange result...")
               t)
              (t
               (error 'clsql-sql-error
                      :database database
                      :expression sql-expression
                      :errno (PQresultStatus result)
                      :error (tidy-error-message
                              (PQresultErrorMessage result)))))
          (PQclear result))))))

(defstruct postgresql-result-sset
  (res-ptr (uffi:make-null-pointer 'pgsql-result) 
	   :type pgsql-result-def)
  (field-types nil :type cons) 
  (num-tuples 0 :type integer)
  (num-fields 0 :type integer)
  (tuple-index 0 :type integer))

(defmethod database-query-result-set (query-expression (database postgresql-database) 
                                      &key full-set field-types)
  (let ((conn-ptr (database-conn-ptr database)))
    (declare (type pgsql-conn-def conn-ptr))
    (uffi:with-cstring (query-native query-expression)
      (let ((result (PQexec conn-ptr query-native)))
        (when (uffi:null-pointer-p result)
          (error 'clsql-sql-error
                 :database database
                 :expression query-expression
                 :errno nil
                 :error (tidy-error-message (PQerrorMessage conn-ptr))))
        (case (PQresultStatus result)
          ((#.pgsql-exec-status-type#empty-query
            #.pgsql-exec-status-type#tuples-ok)
           (if full-set
               (values (make-postgresql-result-set
                        :res-ptr result
                        :num-fields (PQnfields result)
                        :num-tuples (PQntuples result))
                       (PQnfields result)
                       (PQntuples result))
	     (values (make-postgresql-result-set
		      :res-ptr result
		      :num-fields (PQnfields result)
		      :num-tuples (PQntuples result))
		     (PQnfields result))))
          (t
           (unwind-protect
               (error 'clsql-sql-error
                      :database database
                      :expression query-expression
                      :errno (PQresultStatus result)
                      :error (tidy-error-message
                              (PQresultErrorMessage result)))
             (PQclear result))))))))
  
(defmethod database-dump-result-set (result-set (database postgresql-database))
  (let ((res-ptr (postgresql-result-set-res-ptr result-set))) 
    (declare (type pgsql-result-def res-ptr))
    (PQclear res-ptr)
    t))

(defmethod database-store-next-row (result-set (database postgresql-database) 
                                    list)
  (let ((result (postgresql-result-set-res-ptr result-set)))
    (declare (type pgsql-result-def result))
    (if (>= (postgresql-result-set-tuple-index result-set)
	    (postgresql-result-set-num-tuples result-set))
	nil
      (loop with tuple-index = (postgresql-result-set-tuple-index result-set)
          for i from 0 below (postgresql-result-set-num-fields result-set)
          for rest on list
          do
            (setf (car rest)
              (if (zerop (PQgetisnull result tuple-index i))
                  (uffi:convert-from-cstring 
                   (PQgetvalue result tuple-index i))
                nil))
          finally
            (incf (postgresql-result-set-tuple-index result-set))
            (return list)))))
