;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          sqlite-sql.lisp
;;;; Purpose:       High-level SQLite interface
;;;; Programmers:   Aurelio Bignoli
;;;; Date Started:  Aug 2003
;;;;
;;;; $Id: sqlite-sql.lisp,v 1.5 2004/03/09 20:57:44 aurelio Exp $
;;;;
;;;; This file, part of CLSQL, is Copyright (c) 2003 by Aurelio Bignoli
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(declaim (optimize (speed 3) (debug 0) (safety 0)))

(in-package :clsql-sqlite)

(defclass sqlite-database (database)
  ((sqlite-db :initarg :sqlite-db :accessor sqlite-db)))

(defmethod database-type ((database sqlite-database))
  :sqlite)

(defmethod database-initialize-database-type ((database-type (eql :sqlite)))
  t)

(defun check-sqlite-connection-spec (connection-spec)
  (check-connection-spec connection-spec :sqlite (name)))

(defmethod database-name-from-spec (connection-spec
				    (database-type (eql :sqlite)))
  (check-sqlite-connection-spec connection-spec)
  (first connection-spec))

(defmethod database-connect (connection-spec (database-type (eql :sqlite)))
  (check-sqlite-connection-spec connection-spec)
  (handler-case
      (make-instance 'sqlite-database
		     :name (database-name-from-spec connection-spec :sqlite)
		     :sqlite-db (sqlite:sqlite-open (first connection-spec)))
    (sqlite:sqlite-error (err)
      (error 'clsql-connect-error
	     :database-type database-type
	     :connection-spec connection-spec
	     :errno (sqlite:sqlite-error-code err)
	     :error (sqlite:sqlite-error-message err)))))

(defmethod database-disconnect ((database sqlite-database))
  (sqlite:sqlite-close (sqlite-db database))
  (setf (sqlite-db database) nil)
  t)

(defmethod database-execute-command (sql-expression (database sqlite-database))
  (handler-case
      (multiple-value-bind (data row-n col-n)
	  (sqlite:sqlite-get-table (sqlite-db database) sql-expression)
	#+clisp (declare (ignore data))
	#-clisp (sqlite:sqlite-free-table data)
	(unless (= row-n 0)
	  (error 'clsql-simple-warning
		 :format-control
		 "Result set not empty: ~@(~A~) row~:P, ~@(~A~) column~:P "
		 :format-arguments (list row-n col-n))))
    (sqlite:sqlite-error (err)
      (error 'clsql-sql-error
	     :database database
	     :expression sql-expression
	     :errno (sqlite:sqlite-error-code err)
	     :error (sqlite:sqlite-error-message err))))
  t)

(defmethod database-query (query-expression (database sqlite-database) types)
  (declare (ignore types))		; SQLite is typeless!
  (handler-case
      (multiple-value-bind (data row-n col-n)
	  (sqlite:sqlite-get-table (sqlite-db database) query-expression)
	#-clisp (declare (type sqlite:sqlite-row-pointer data))
	(if (= row-n 0)
	    nil
	    (prog1
		;; The first col-n elements are column names.
		(loop for i from col-n below (* (1+ row-n) col-n) by col-n
		      collect (loop for j from 0 below col-n
				    collect
				    (#+clisp aref
				     #-clisp sqlite:sqlite-aref
				             data (+ i j))))
	        #-clisp (sqlite:sqlite-free-table data))
	      ))
    (sqlite:sqlite-error (err)
      (error 'clsql-sql-error
	     :database database
	     :expression query-expression
	     :errno (sqlite:sqlite-error-code err)
	     :error (sqlite:sqlite-error-message err)))))

#-clisp
(defstruct sqlite-result-set
  (vm (sqlite:make-null-vm)
      :type sqlite:sqlite-vm-pointer)
  (first-row (sqlite:make-null-row)
	     :type sqlite:sqlite-row-pointer)
  (n-col 0 :type fixnum))
#+clisp
(defstruct sqlite-result-set
  (vm nil)
  (first-row nil)
  (n-col 0 :type fixnum))

(defmethod database-query-result-set
    (query-expression (database sqlite-database) &key full-set types)
  (declare (ignore full-set types))
  (handler-case
      (let* ((vm (sqlite:sqlite-compile (sqlite-db database)
					query-expression))
	     (result-set (make-sqlite-result-set :vm vm)))
	#-clisp (declare (type sqlite:sqlite-vm-pointer vm))

	;;; To obtain column number we have to read the first row.
	(multiple-value-bind (n-col cols col-names)
	    (sqlite:sqlite-step vm)
	  (declare (ignore col-names)
		   #-clisp (type sqlite:sqlite-row-pointer cols)
		   )
	  (setf (sqlite-result-set-first-row result-set) cols
		(sqlite-result-set-n-col result-set) n-col)
	  (values result-set n-col nil)))
    (sqlite:sqlite-error (err)
      (error 'clsql-sql-error
	     :database database
	     :expression query-expression
	     :errno (sqlite:sqlite-error-code err)
	     :error (sqlite:sqlite-error-message err)))))

(defmethod database-dump-result-set (result-set (database sqlite-database))
  (declare (ignore database))
  (handler-case
      (sqlite:sqlite-finalize (sqlite-result-set-vm result-set))
    (sqlite:sqlite-error (err)
      (error 'clsql-simple-error
	     :format-control "Error finalizing SQLite VM: ~A"
	     :format-arguments (list (sqlite:sqlite-error-message err))))))

(defmethod database-store-next-row (result-set (database sqlite-database) list)
  (let ((n-col (sqlite-result-set-n-col result-set)))
    (if (= n-col 0)
	;; empty result set
	nil
	(let ((row (sqlite-result-set-first-row result-set)))
	  (if (sqlite:null-row-p row)
	      ;; First row already used. fetch another row from DB.
	      (handler-case
		  (multiple-value-bind (n new-row col-names)
		      (sqlite:sqlite-step (sqlite-result-set-vm result-set))
		    (declare (ignore n col-names)
			     #-clisp (type sqlite:sqlite-row-pointer new-row)
			     )
		    (if (sqlite:null-row-p new-row)
			(return-from database-store-next-row nil)
			(setf row new-row)))
		(sqlite:sqlite-error (err)
		  (error 'clsql-simple-error
			 :format-control "Error in sqlite-step: ~A"
			 :format-arguments
			 (list (sqlite:sqlite-error-message err)))))

	      ;; Use the row previously read by database-query-result-set.
	      (setf (sqlite-result-set-first-row result-set)
		    (sqlite:make-null-row)))
	  (loop for i = 0 then (1+ i)
		for rest on list
		do (setf (car rest)
			 (#+clisp aref
			  #-clisp sqlite:sqlite-aref
			  row i)))
	  #-clisp (sqlite:sqlite-free-row row)
	  t))))
