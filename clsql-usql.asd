;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; ======================================================================
;;;; File:    usql.asd
;;;; Author:  Marcus Pearce <m.t.pearce@city.ac.uk>
;;;; Created: 30/03/2004
;;;; Updated: <04/04/2004 11:58:21 marcusp>
;;;; ======================================================================
;;;;
;;;; Description ==========================================================
;;;; ======================================================================
;;;;
;;;; ASDF system definition for CLSQL-USQL. 
;;;;
;;;; ======================================================================

(asdf:defsystem #:clsql-usql
    :name "CLSQL-USQL"
    :author ""
    :maintainer ""
    :version ""
    :licence ""
    :description "A high level Common Lisp interface to SQL RDBMS."
    :long-description "A high level Common Lisp interface to SQL RDBMS
based on the Xanalys CommonSQL interface for Lispworks. It depends on
the low-level database interfaces provided by CLSQL and includes both
a functional and an object oriented interface."
    :components
    ((:module usql
	      :components
	      ((:module :patches
			:pathname ""
			:components (#+(or cmu sbcl) (:file "pcl-patch")))
	       (:module :package
			:pathname ""
			:components ((:file "package"))
			:depends-on (:patches))
	       (:module :core
			:pathname ""
			:components ((:file "classes")
				     (:file "operations" :depends-on ("classes"))
				     (:file "syntax"))
			:depends-on (:package))
	       (:module :functional
			:pathname ""
			:components ((:file "sql")
				     (:file "table"))
			:depends-on (:core))
	       (:module :object
			:pathname ""
			:components ((:file "metaclasses")
				     (:file "objects" :depends-on ("metaclasses")))
			:depends-on (:functional)))))
    :depends-on (:clsql-base))
     
