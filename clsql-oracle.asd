;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; This is copyrighted software.  See interfaces/oracle/* files for terms.
;;;; 
;;;; $Id: clsql-oracle.asd,v 1.1 2002/08/18 02:57:50 kevin Exp $

(in-package :asdf)

;;; System definition

(defsystem :clsql-oracle
    :default-component-class clsql-cl-source-file
    :pathname "cl-library:clsql-oracle"
    :components
    ((:file "oracle-package")
     (:file "oracle-loader" :depends-on ("oracle-package"))
     (:file "alien-resources" :depends-on ("oracle-package"))
     (:file "oracle-constants" :depends-on ("oracle-package"))
     (:file "oracle" :depends-on ("oracle-constants" "oracle-loader"))
     (:file "oracle-sql" :depends-on ("oracle" "alien-resources"))
     (:file "oracle-objects" :depends-on ("oracle-sql")))
    :depends-on (:clsql-base))




