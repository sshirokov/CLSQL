;;; -*- Mode: Lisp -*-
;;;; MaiSQL --- Common Lisp Interface Layer to SQL Databases
;;;; This is copyrighted software.  See documentation for terms.
;;;; 
;;;; MaiSQL.system --- System definition for UncommonSQL-PostgreSQL
;;;; 
;;;; Checkout Tag: $Name:  $
;;;; $Id: system.lisp,v 1.1 2002/04/01 05:27:55 kevin Exp $

#+CLISP
(in-package "USER")
#-CLISP
(in-package :CL-USER)

;;; System definition

(mk:defsystem "UncommonSQL-Oracle"
    :source-pathname "cl-library:uncommonsql;dbms;oracle"
    :source-extension "lisp"
    :components
    ((:file "oracle-package")
     (:file "oracle-loader"
	    :depends-on ("oracle-package"))
     (:file "alien-resources"
	    :depends-on ("oracle-package"))
     (:file "oracle-constants"
	    :depends-on ("oracle-package"))
     (:file "oracle"
	    :depends-on ("oracle-constants"
			 "oracle-loader"))
     (:file "oracle-sql"
	    :depends-on ("oracle" "alien-resources"))
     (:file "oracle-objects"
	    :depends-on ("oracle-sql"))
     )
    :depends-on (:uncommonsql)
    )

(mk:oos "UncommonSQL-Oracle" :compile)




