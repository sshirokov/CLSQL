;; sqlite.lisp  --- CLISP FFI for SQLite (http://www.sqlite.org).

;; Copyright (C) 2003 Aurelio Bignoli
          
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.
          
;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.
          
;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

;; $Id: sqlite.lisp,v 1.4 2003/11/28 21:02:43 aurelio Exp $

(in-package :cl-user)

(defpackage :sqlite
  (:use :common-lisp :ffi)
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
	   #:sqlite-version		; Defined as constant.
	   #:sqlite-encoding		; Defined as constant.
	   #:sqlite-last-insert-rowid

	   ;;; Utility functions (used by CLSQL)
	   #:make-null-row
	   #:null-row-p
	   
	   ;;; Macros.
	   #:with-open-sqlite-db
	   #:with-sqlite-vm))

(in-package :sqlite)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Return values for sqlite_exec() and sqlite_step()
;;;;
(defconstant SQLITE-OK           0   "Successful result")
(defconstant SQLITE-ERROR        1   "SQL error or missing database")
(defconstant SQLITE-INTERNAL     2   "An internal logic error in SQLite")
(defconstant SQLITE-PERM         3   "Access permission denied")
(defconstant SQLITE-ABORT        4   "Callback routine requested an abort")
(defconstant SQLITE-BUSY         5   "The database file is locked")
(defconstant SQLITE-LOCKED       6   "A table in the database is locked")
(defconstant SQLITE-NOMEM        7   "A malloc() failed")
(defconstant SQLITE-READONLY     8   "Attempt to write a readonly database")
(defconstant SQLITE-INTERRUPT    9   "Operation terminated by sqlite_interrupt()")
(defconstant SQLITE-IOERR       10   "Some kind of disk I/O error occurred")
(defconstant SQLITE-CORRUPT     11   "The database disk image is malformed")
(defconstant SQLITE-NOTFOUND    12   "(Internal Only) Table or record not found")
(defconstant SQLITE-FULL        13   "Insertion failed because database is full")
(defconstant SQLITE-CANTOPEN    14   "Unable to open the database file")
(defconstant SQLITE-PROTOCOL    15   "Database lock protocol error")
(defconstant SQLITE-EMPTY       16   "(Internal Only) Database table is empty")
(defconstant SQLITE-SCHEMA      17   "The database schema changed")
(defconstant SQLITE-TOOBIG      18   "Too much data for one row of a table")
(defconstant SQLITE-CONSTRAINT  19   "Abort due to contraint violation")
(defconstant SQLITE-MISMATCH    20   "Data type mismatch")
(defconstant SQLITE-MISUSE      21   "Library used incorrectly")
(defconstant SQLITE-NOLFS       22   "Uses OS features not supported on host")
(defconstant SQLITE-AUTH        23   "Authorization denied")
(defconstant SQLITE-FORMAT      24   "Auxiliary database format error")
(defconstant SQLITE-ROW         100  "sqlite_step() has another row ready")
(defconstant SQLITE-DONE        101  "sqlite_step() has finished executing")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; C types.
;;;;
(def-c-type sqlite-db c-pointer)
(def-c-type sqlite-vm c-pointer)
(def-c-type error-message (c-ptr c-pointer))
					; It is not NULL only in case of error.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Conditions.
;;;;
(define-condition sqlite-error ()
  ((message :initarg :message :reader sqlite-error-message :initform "")
   (code :initarg :code :reader sqlite-error-code))
  (:report (lambda (condition stream)
             (let ((code (sqlite-error-code condition)))
               (format stream "SQLite error [~A] - ~A : ~A"
		       code (error-string code)
		       (sqlite-error-message condition))))))

(defun signal-sqlite-error (code message)
  (let ((condition
	 (make-condition 'sqlite-error
			 :code code
			 :message
			 (typecase message
			     (string message)
			     (t (error-message-as-string message))))))
    (unless (signal condition)
      (invoke-debugger condition))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Library functions.
;;;;
(defmacro def-sqlite-call-out (name &rest args)
  `(def-call-out ,name
    (:language :stdc)
    (:library "libsqlite.so")
    ,@args))

(def-sqlite-call-out error-string
    (:name "sqlite_error_string")
  (:arguments
   (error-code int :in))
  (:return-type c-string))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Core API.
;;;;
(def-sqlite-call-out %open
    (:name "sqlite_open")
  (:arguments
   (dbname c-string :in)
   (mode int :in)
   (errmsg error-message :out))
  (:return-type sqlite-db))

(def-sqlite-call-out sqlite-close
    (:name "sqlite_close")
  (:arguments (db sqlite-db :in))
  (:return-type nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; New API.
;;;;
(def-sqlite-call-out %compile
    (:name "sqlite_compile")
  (:arguments
   (db sqlite-db :in)
   (sql c-string :in)
   (sql-tail (c-ptr c-string) :out)
   (vm (c-ptr sqlite-vm) :out)
   (errmsg error-message :out))
  (:return-type int))

(def-sqlite-call-out %step
    (:name "sqlite_step")
  (:arguments
   (vm sqlite-vm :in)
   (cols-n (c-ptr int) :out)
   (cols (c-ptr c-pointer) :out)
   (col-names (c-ptr c-pointer) :out))
  (:return-type int))

(def-sqlite-call-out %finalize
    (:name "sqlite_finalize")
  (:arguments
   (vm sqlite-vm :in)
   (errmsg error-message :out))
  (:return-type int))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Extended API.
;;;;
(def-sqlite-call-out sqlite-last-insert-rowid
    (:name "sqlite_last_insert_rowid")
  (:arguments
   (db sqlite-db :in))
  (:return-type int))

(def-sqlite-call-out %get-table
    (:name "sqlite_get_table")
  (:arguments
   (db sqlite-db :in)
   (sql c-string :in)
   (result (c-ptr c-pointer) :out)
   (n-row (c-ptr int) :out)
   (n-column (c-ptr int) :out)
   (errmsg error-message :out))
  (:return-type int))

(def-sqlite-call-out %free-table
    (:name "sqlite_free_table")
  (:arguments
   (rows c-pointer :in))
  (:return-type nil))

(def-c-var %version
    (:name "sqlite_version")
  (:library "libsqlite.so")
  (:type (c-array-max char 32))
  (:read-only t))

(def-c-var %encoding
    (:name "sqlite_encoding")
  (:library "libsqlite.so")
  (:type (c-array-max char 32))
  (:read-only t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Wrapper functions.
;;;;
(defconstant sqlite-version
  (ext:convert-string-from-bytes %version custom:*terminal-encoding*))

(defconstant sqlite-encoding
  (ext:convert-string-from-bytes %encoding custom:*terminal-encoding*))

(defun error-message-as-string (p)
  (with-c-var (p1 'c-pointer p)
    (prog1
	(cast p1 'c-string)
      (foreign-free p1))))

(defun sqlite-open (db-name &optional (mode 0))
  (multiple-value-bind (db error-message)
      (%open db-name mode)
    (if db
	db
	(signal-sqlite-error SQLITE-ERROR error-message))))

(defun c-pointer-to-string-array (p element-n)
  (if (null p)
      p
      (with-c-var (p1 'c-pointer p)
	(cast p1 `(c-ptr (c-array c-string ,element-n))))))

(defun sqlite-compile (db sql)
  (multiple-value-bind (result sql-tail vm error-message)
      (%compile db sql)
    (declare (ignore sql-tail))
    (if (= result SQLITE-OK)
	vm
	(signal-sqlite-error result error-message))))

(defun sqlite-step (vm)
  (multiple-value-bind (result n-col cols col-names)
      (%step vm)
    (cond
      ((= result SQLITE-ROW)
       (values n-col (c-pointer-to-string-array cols n-col)
	       (c-pointer-to-string-array col-names (* 2 n-col))))
      ((= result SQLITE-DONE) (values 0 nil nil))
      (t (signal-sqlite-error result "sqlite-step")))))

(defun sqlite-finalize (vm)
  (multiple-value-bind (result error-message)
      (%finalize vm)
    (if (= result SQLITE-OK)
	t
	(signal-sqlite-error result error-message))))

(defun sqlite-get-table (db sql)
  (multiple-value-bind (result rows n-row n-col error-message)
      (%get-table db sql)
    (if (= result SQLITE-OK)
	(let ((x (c-pointer-to-string-array rows (* (1+ n-row) n-col))))
	  (%free-table rows)
	  (values x n-row n-col))
      (signal-sqlite-error result error-message))))

(defmacro with-open-sqlite-db ((db dbname &key (mode 0)) &body body)
  (let ((error-message (gensym)))
    `(multiple-value-bind (,db ,error-message)
      (sqlite-open ,dbname ,mode)
      (if (null ,db)
	  (signal-sqlite-error SQLITE-ERROR ,error-message)
	  (unwind-protect
	       (progn ,@body)
	    (sqlite-close ,db))))))

(defmacro with-sqlite-vm ((vm db sql) &body body)
  `(let ((,vm (sqlite-compile ,db ,sql)))
    (unwind-protect
	 (progn ,@body)
      (sqlite-finalize ,vm))))

(declaim (inline null-row-p))
(defun null-row-p (row)
  (null row))

(declaim (inline make-null-row))
(defun make-null-row ()
  nil)

#+nil
(defun test-function (db-name)
  (with-open-sqlite-db (db db-name)
    (let ((x (sqlite-get-table db "select * from sqlite_master;")))
      (with-sqlite-vm (vm db "select * from sqlite_master;")
	(let ((error-n 0))
	  (loop  for i = 1 then (1+ i)
		 do (multiple-value-bind (n-col cols col-names)
			(sqlite-step vm)
		      (declare (ignore col-names))
		      (if (= n-col 0)
			  (return-from nil)
			  (loop for j from 0 to (1- n-col)
				for j1 = (* n-col i) then (1+ j1)
				do
				(when (string/= (aref x j1) (aref cols j))
				  (format t "~&row=~A, col=~A: ~A - ~A~%"
					  i j
					  (aref x j1) (aref cols j))
				  (incf error-n))))))
	  (if (= error-n 0)
	      (format t "~&Test passed!~%")
	      (format t "~&Test not passed. ~A errors" error-n)))))))

(defun get-column-types (db-name table-name)
  (with-open-sqlite-db (db db-name)
    (with-sqlite-vm (vm db (format nil "pragma table_info('~A')" table-name))
      (loop
       (multiple-value-bind (n-col cols col-names)
	   (sqlite-step vm)
	 (declare (ignore col-names))
	 (if (= n-col 0)
	     (return-from nil)
	     (format t "~&column name = ~A, type = ~A~%"
		     (aref cols 1) (aref cols 2))))))))

;;;; Local Variables:
;;;; Mode: lisp
;;;; Syntax: ANSI-Common-Lisp
;;;; Package: sqlite
;;;; End: