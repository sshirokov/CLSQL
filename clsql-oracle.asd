;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; This is copyrighted software.  See interfaces/oracle/* files for terms.
;;;; 
;;;; $Id$

(defpackage #:clsql-oracle-system (:use #:asdf #:cl))
(in-package #:clsql-oracle-system)

;;; System definition

#+cmu
(defsystem clsql-oracle
  :name "cl-sql-oracle"
  :author "Kevin M. Rosenberg <kmr@debian.org>"
  :maintainer "Kevin M. Rosenberg <kmr@debian.org>"
  :licence "Lessor Lisp General Public License"
  :description "Common Lisp SQL Oracle Driver"
  :long-description "cl-sql-oracle package provides a database driver to the Oracle database system."

  :depends-on (clsql-base)
  :components
    ((:module :db-oracle
	      :components
	      ((:file "oracle-package")
	       (:file "oracle-loader" :depends-on ("oracle-package"))
	       (:file "alien-resources" :depends-on ("oracle-package"))
	       (:file "oracle-constants" :depends-on ("oracle-package"))
	       (:file "oracle" :depends-on ("oracle-constants" "oracle-loader"))
	       (:file "oracle-sql" :depends-on ("oracle" "alien-resources"))
	       (:file "oracle-objects" :depends-on ("oracle-sql"))))))

#-cmu
(defsystem clsql-oracle)
