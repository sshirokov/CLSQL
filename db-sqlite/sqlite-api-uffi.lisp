;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          sqlite-api-uffi.lisp
;;;; Purpose:       Low-level SQLite interface using UFFI
;;;; Programmers:   Aurelio Bignoli
;;;; Date Started:  Nov 2003
;;;;
;;;; $Id: sqlite-api-uffi.lisp,v 1.5 2004/03/09 20:57:19 aurelio Exp $
;;;;
;;;; This file, part of CLSQL, is Copyright (c) 2003 by Aurelio Bignoli
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************
(declaim (optimize (debug 0) (speed 3) (safety 0) (compilation-speed 0)))

(in-package :cl-user)

(defpackage :sqlite
  (:use :common-lisp :uffi)
    (:export
           ;;; Conditions
           #:sqlite-error
	   #:sqlite-error-code
	   #:sqlite-error-message
	   
	   ;;; Core API.
           #:sqlite-open
	   #:sqlite-close

	   ;;; New API.
	   #:sqlite-compile
	   #:sqlite-step
	   #:sqlite-finalize
	   
	   ;;; Extended API.
	   #:sqlite-get-table
	   #:sqlite-free-table
	   #:sqlite-version		; Defined as constant.
	   #:sqlite-encoding		; Defined as constant.
	   #:sqlite-last-insert-rowid

	   ;;; Utility functions.
	   #:make-null-row
	   #:make-null-vm
	   #:null-row-p
	   #:sqlite-aref
	   #:sqlite-free-row
	   
	   ;;; Types.
	   #:sqlite-row
	   #:sqlite-row-pointer
	   #:sqlite-vm-pointer))

(in-package :sqlite)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Return values for sqlite_exec() and sqlite_step()
;;;;
(defconstant SQLITE-OK            0  "Successful result")
(defconstant SQLITE-ERROR         1  "SQL error or missing database")
(defconstant SQLITE-ROW         100  "sqlite_step() has another row ready")
(defconstant SQLITE-DONE        101  "sqlite_step() has finished executing")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Conditions.
;;;;
(define-condition sqlite-error ()
  ((message :initarg :message :reader sqlite-error-message :initform "")
   (code :initarg :code :reader sqlite-error-code))
  (:report (lambda (condition stream)
             (let ((code (sqlite-error-code condition)))
               (format stream "SQLite error [~A]: ~A"
		       code (sqlite-error-message condition))))))

(defun signal-sqlite-error (code &optional message)
  (let ((condition
	 (make-condition 'sqlite-error
			 :code code
			 :message (if message
				      message
				      (sqlite-error-string code)))))
    (unless (signal condition)
      (invoke-debugger condition))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Foreign types definitions.
;;;;
(def-foreign-type sqlite-db :pointer-void)
(def-foreign-type sqlite-vm :pointer-void)
(def-foreign-type errmsg :cstring)

(def-array-pointer string-array-pointer :cstring)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Lisp types used in declarations.
;;;;
(def-type sqlite-db-pointer '(* sqlite-db))
(def-type sqlite-int-pointer '(* :int))
(def-type sqlite-row 'string-array-pointer)
(def-type sqlite-row-pointer '(* string-array-pointer))
(def-type sqlite-vm-pointer '(* sqlite-vm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Library functions.
;;;;
(defmacro def-sqlite-function (name args &key (returning :void))
  `(def-function ,name ,args
    :module "sqlite"
    :returning ,returning))

(def-sqlite-function
    "sqlite_error_string"
    ((error-code :int))
  :returning :cstring)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Core API.
;;;;
(declaim (inline %open))
(def-sqlite-function
    ("sqlite_open" %open)
    ((dbname :cstring)
     (mode :int)
     (error-message '(* errmsg)))
  :returning sqlite-db)

(declaim (inline sqlite-close))
(def-sqlite-function
    "sqlite_close"
    ((db sqlite-db)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; New API.
;;;;
(declaim (inline %compile))
(def-sqlite-function
    ("sqlite_compile" %compile)
    ((db sqlite-db)
     (sql :cstring)
     (sql-tail '(* :cstring))
     (vm '(* sqlite-vm))
     (error-message '(* errmsg)))
  :returning :int)

(declaim (inline %step))
(def-sqlite-function
    ("sqlite_step" %step)
    ((vm sqlite-vm)
     (cols-n '(* :int))
     (cols '(* (* :cstring)))
     (col-names '(* (* :cstring))))
  :returning :int)

(declaim (inline %finalize))
(def-sqlite-function
    ("sqlite_finalize" %finalize)
    ((vm sqlite-vm)
     (error-message '(* errmsg)))
  :returning :int)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Extended API.
;;;;
(declaim (inline sqlite-last-insert-rowid))
(def-sqlite-function
    "sqlite_last_insert_rowid"
    ((db 'sqlite-db))
  :returning :int)

(declaim (inline %get-table))
(def-sqlite-function
    ("sqlite_get_table" %get-table)
    ((db sqlite-db)
     (sql :cstring)
     (result '(* (* :cstring)))
     (rows-n '(* :int))
     (cols-n '(* :int))
     (error-message '(* errmsg)))
  :returning :int)

(declaim (inline %free-table))
(def-sqlite-function
    ("sqlite_free_table" %free-table)
    ((rows :pointer-void)))

(declaim (inline sqlite-libversion))
(def-sqlite-function
    "sqlite_libversion"
    ()
  :returning :cstring)

(declaim (inline sqlite-libencoding))
(def-sqlite-function
    "sqlite_libencoding"
    ()
  :returning :cstring)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Wrapper functions.
;;;;
(defparameter sqlite-version (sqlite-libversion))
(defparameter sqlite-encoding (sqlite-libencoding))

(defun sqlite-open (db-name &optional (mode 0))
  (let ((db (%open db-name mode nil)))
    (declare (type sqlite-db-pointer db))
    (if (null-pointer-p db)
	(signal-sqlite-error SQLITE-ERROR
			     (format nil "unable to open ~A" db-name))
	db)))

(defun sqlite-compile (db sql)
  (declare (type sqlite-db-pointer db))
  (let ((vm (allocate-foreign-object 'sqlite-vm)))
    (with-foreign-object (sql-tail :cstring)
      (let ((result (%compile db sql sql-tail vm nil)))
	(if (= result SQLITE-OK)
	    vm
	    (progn
	      (free-foreign-object vm)
	      (signal-sqlite-error result)))))))

(defun sqlite-step (vm)
  (declare (type sqlite-vm-pointer vm))
  (with-foreign-object (cols-n :int)
    (let ((cols (allocate-foreign-object '(* :cstring)))
	  (col-names (allocate-foreign-object '(* :cstring))))
      (declare (type sqlite-row-pointer cols col-names))
      (let ((result (%step (deref-pointer vm 'sqlite-vm)
			   cols-n cols col-names)))
	(cond
	  ((= result SQLITE-ROW)
	   (let ((n (deref-pointer cols-n :int)))
	     (values n cols col-names)))
	  ((= result SQLITE-DONE)
	   (free-foreign-object cols)
	   (free-foreign-object col-names)
	   (values 0 (make-null-pointer 'string-array-pointer)
		   (make-null-pointer 'string-array-pointer)))
	  (t
	   (free-foreign-object cols)
	   (free-foreign-object col-names)
	   (signal-sqlite-error result)))))))

(defun sqlite-finalize (vm)
  (declare (type sqlite-vm-pointer vm))
  (let ((result (%finalize (deref-pointer vm 'sqlite-vm) nil)))
    (if (= result SQLITE-OK)
	(progn
	  (free-foreign-object vm)
	  t)
	(signal-sqlite-error result))))

(defun sqlite-get-table (db sql)
  (declare (type sqlite-db-pointer db))
  (let ((rows (allocate-foreign-object '(* :cstring))))
    (with-foreign-object (rows-n :int)
      (with-foreign-object (cols-n :int)
        (declare (type sqlite-row-pointer rows))
	(let ((result (%get-table db sql rows rows-n cols-n nil)))
	  (if (= result SQLITE-OK)
	      (let ((cn (deref-pointer cols-n :int))
		    (rn (deref-pointer rows-n :int)))
		(values rows rn cn))
	      (progn
		(free-foreign-object rows)
		(signal-sqlite-error result))))))))

(declaim (inline sqlite-free-table))
(defun sqlite-free-table (table)
  (declare (type sqlite-row-pointer table))
  (%free-table (deref-pointer table 'sqlite-row-pointer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Utility functions.
;;;;
(declaim (inline make-null-row))
(defun make-null-row ()
  (uffi:make-null-pointer 'string-array-pointer))

(declaim (inline make-null-vm))
(defun make-null-vm ()
  (uffi:make-null-pointer 'sqlite-vm))

(declaim (inline null-row-p))
(defun null-row-p (row)
  (null-pointer-p row))

(declaim (inline sqlite-aref))
(defun sqlite-aref (a n)
  (declare (type sqlite-row-pointer a))
  (deref-array  (deref-pointer a 'sqlite-row-pointer) '(:array :cstring) n))

(declaim (inline sqlite-free-row))
(defun sqlite-free-row (row)
  (declare (type sqlite-row-pointer row))
  (free-foreign-object row))
