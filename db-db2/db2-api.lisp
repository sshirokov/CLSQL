;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          db2.lisp
;;;; Purpose:       Package definition for CLSQL Db2 interface
;;;;
;;;; $Id$
;;;;
;;;; This file is part of CLSQL.
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(in-package #:clsql-db2)


;;
;; Opaque pointer types
;;

(uffi:def-foreign-type cli-handle :pointer-void)
(uffi:def-foreign-type cli-pointer :pointer-void)
(uffi:def-foreign-type cli-char :byte)
(uffi:def-foreign-type cli-ulen :unsigned-int)
(uffi:def-foreign-type cli-len :int)
(uffi:def-foreign-type cli-smallint :short)
(uffi:def-foreign-type cli-usmallint :unsigned-short)


(defvar +null-void-pointer+ (uffi:make-null-pointer :void))
(defvar +null-void-pointer-pointer+ (uffi:make-null-pointer :pointer-void))

;;; Check an CLI return code for erroricity and signal a reasonably
;;; informative condition if so.
;;;
;;; ERRHP provides an error handle which can be used to find
;;; subconditions; if it's not provided, subcodes won't be checked.
;;;
;;; NULLS-OK says that a NULL-VALUE-RETURNED subcondition condition is
;;; normal and needn't cause any signal. An error handle is required
;;; to detect this subcondition, so it doesn't make sense to set ERRHP
;;; unless NULLS-OK is set.

(defmacro def-cli-routine ((c-cli-symbol lisp-cli-fn) c-return &rest c-parms)
  (let ((ll (mapcar (lambda (x) (declare (ignore x)) (gensym)) c-parms)))
    `(let ((%lisp-cli-fn (uffi:def-function
			     (,c-cli-symbol ,(intern (concatenate 'string "%" (symbol-name lisp-cli-fn))))
			     ,c-parms
			     :returning ,c-return)))
       (defun ,lisp-cli-fn (,@ll &key database nulls-ok)
	 (let ((result (funcall %lisp-cli-fn ,@ll)))
	   #+ignore
	   (case result
	     (#.+oci-success+
	      +oci-success+)
	     (#.+oci-error+
	      (handle-oci-error :database database :nulls-ok nulls-ok))
	     (#.+oci-no-data+
	      (error 'sql-database-error :message "OCI No Data Found"))
	     (#.+oci-success-with-info+
	      (error 'sql-database-error :message "internal error: unexpected +oci-success-with-info"))
	     (#.+oci-no-data+
	      (error 'sql-database-error :message "OCI No Data"))
	     (#.+oci-invalid-handle+
	      (error 'sql-database-error :message "OCI Invalid Handle"))
	     (#.+oci-need-data+
	      (error 'sql-database-error :message "OCI Need Data"))
	     (#.+oci-still-executing+
	      (error 'sql-temporary-error :message "OCI Still Executing"))
	     (#.+oci-continue+
	      (error 'sql-database-error :message "OCI Continue"))
	     (1804
	      (error 'sql-database-error :message "Check CLI_HOME and NLS settings."))
	     (t
	      (error 'sql-database-error
		     :message
		     (format nil "OCI unknown error, code=~A" result)))))))))
  

(defmacro def-raw-cli-routine
  ((c-cli-symbol lisp-cli-fn) c-return &rest c-parms)
  (let ((ll (mapcar (lambda (x) (declare (ignore x)) (gensym)) c-parms)))
    `(let ((%lisp-cli-fn (uffi:def-function (,c-cli-symbol ,(intern (concatenate 'string "%" (symbol-name lisp-cli-fn))))
			     ,c-parms
			   :returning ,c-return)))
       (defun ,lisp-cli-fn (,@ll &key database nulls-ok)
	 (funcall %lisp-cli-fn ,@ll)))))


(def-cli-routine ("SQLAllocHandle" sql-allocate-handle)
    :int
  (fHandleType cli-smallint)
  (hInput cli-handle)
  (phOuput (* cli-handle)))


;;; CLI Functions needed
;;;   SQLBindParameter
;;;   SQLExecDirect
;;;   SQLNumResultCols
;;;   SQLDescribeCol
;;;   SQLColAttribute
;;;   SQLRowCount
;;;   SQLBindCol
;;;   SQLFetch
;;;   SQLGetData
;;;   SQLEndTran
;;;   SQLFreeHandle
;;;   SQLDisconnect
;;;   SQLSetConnectAttr
