;;; -*- Mode: Lisp -*-
;;; $Id: oracle-package.cl,v 1.1 2002/05/13 03:52:24 kevin Exp $
;;;
;;; MaiSQL --- Common Lisp Interface Layer to SQL Databases
;;; This is copyrighted software.  See documentation for terms.
;;; 
;;; oracle-package.lisp --- Package definition for the Oracle interface
;;; 

(in-package :cl-user)

(defpackage "MAISQL-ORACLE"
  (:nicknames "ORACLE")
  (:use "COMMON-LISP" "MAISQL-SYS" "ALIEN" "C-CALL" "SYSTEM")
  (:export "ORACLE-DATABASE"
	   "*ORACLE-SO-LOAD-PATH*"
	   "*ORACLE-SO-LIBRARIES*")
  (:documentation "This is the MaiSQL interface to Oracle."))
