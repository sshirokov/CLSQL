;;; -*- Mode: Lisp -*-
;;; $Id: oracle-package.cl,v 1.1 2002/08/01 03:06:26 kevin Exp $
;;;
;;; This is copyrighted software.  See documentation for terms.

(in-package :cl-user)

(defpackage :clsql-oracle
  (:nicknames :oracle)
  (:use :common-lisp :clsql-sys "ALIEN" "C-CALL" "SYSTEM")
  (:export #:oracle-database
	   #:*oracle-so-load-path*
	   #:*oracle-so-libraries*)
  (:documentation "This is the CLSQL interface to Oracle."))
