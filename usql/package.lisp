;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; ======================================================================
;;;; File:    package.lisp
;;;; Author:  Marcus Pearce <m.t.pearce@city.ac.uk>
;;;; Created: 30/03/2004
;;;; Updated: <04/04/2004 12:21:50 marcusp>
;;;; ======================================================================
;;;;
;;;; Description ==========================================================
;;;; ======================================================================
;;;;
;;;; Package definitions for CLSQL-USQL. 
;;;;
;;;; ======================================================================

(in-package #:cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  
(defpackage #:clsql-usql-sys
  (:nicknames #:usql-sys #:sql-sys)
  (:use #:common-lisp #:clsql-base-sys #+lispworks #:clos)
  ;; This is for working with the CMUCL/SBCL PCL MOP, which is kinda whacky
  #+(or cmu sbcl)
  (:shadowing-import-from #+cmu :pcl #+sbcl :sb-pcl 
                          :built-in-class 
                          :class-direct-slots
                          :class-name
                          :class-of
                          :class-slots
                          :compute-effective-slot-definition
                          :direct-slot-definition-class
                          :effective-slot-definition-class
                          :find-class
                          :slot-boundp
                          :slot-definition-name
                          :slot-definition-type
                          :slot-value-using-class
                          :standard-direct-slot-definition
                          :standard-effective-slot-definition
                          :validate-superclass
                          :class-direct-superclasses
                          :name
                          :standard-class)
  (:import-from :clsql-base-sys
                ;; conditions 
                :clsql-condition
                :clsql-error
                :clsql-simple-error
                :clsql-warning
                :clsql-simple-warning
                :clsql-invalid-spec-error
                :clsql-invalid-spec-error-connection-spec
                :clsql-invalid-spec-error-database-type
                :clsql-invalid-spec-error-template
                :clsql-connect-error
                :clsql-connect-error-database-type
                :clsql-connect-error-connection-spec
                :clsql-connect-error-errno
                :clsql-connect-error-error
                :clsql-sql-error
                :clsql-sql-error-database
                :clsql-sql-error-expression
                :clsql-sql-error-errno
                :clsql-sql-error-error
                :clsql-database-warning
                :clsql-database-warning-database
                :clsql-database-warning-message
                :clsql-exists-condition
                :clsql-exists-condition-new-db
                :clsql-exists-condition-old-db
                :clsql-exists-warning
                :clsql-exists-error
                :clsql-closed-error
                :clsql-closed-error-database
                :clsql-type-error
                :clsql-sql-syntax-error
                ;; db-interface
                :check-connection-spec
                :database-initialize-database-type
                :database-type-load-foreign
                :database-name-from-spec
                :database-create-sequence
                :database-drop-sequence
                :database-sequence-next
                :database-set-sequence-position
                :database-query-result-set
                :database-dump-result-set
                :database-store-next-row
                :database-get-type-specifier
                :database-list-tables
                :database-list-views
                :database-list-indexes
                :database-list-sequences
                :database-list-attributes
                :database-attribute-type
                :database-add-attribute
                :database-type 
                ;; initialize
                :*loaded-database-types*
                :reload-database-types
                :*default-database-type*
                :*initialized-database-types*
                :initialize-database-type
                ;; classes
                :database
                :closed-database
                :database-name
                :command-recording-stream
                :result-recording-stream
                :database-view-classes
                :database-schema
                :conn-pool
                :print-object 
                ;; utils
                :sql-escape)
  (:export
   ;; "Private" exports for use by interface packages
   :check-connection-spec
   :database-initialize-database-type
   :database-type-load-foreign
   :database-name-from-spec
   :database-connect
   :database-query
   :database-execute-command
   :database-create-sequence
   :database-drop-sequence
   :database-sequence-next
   :database-set-sequence-position
   :database-query-result-set
   :database-dump-result-set
   :database-store-next-row
   :database-get-type-specifier
   :database-list-tables
   :database-table-exists-p
   :database-list-views
   :database-view-exists-p
   :database-list-indexes
   :database-index-exists-p
   :database-list-sequences
   :database-sequence-exists-p
   :database-list-attributes
   :database-attribute-type
   .
   ;; Shared exports for re-export by USQL. 
   ;; I = Implemented, D = Documented
   ;;  name                                 file       ID
   ;;====================================================
   #1=(;;------------------------------------------------
       ;; CommonSQL API 
       ;;------------------------------------------------
      ;;FDML 
       :select                            ; objects    xx
       :cache-table-queries               ; 
       :*cache-table-queries-default*     ; 
       :delete-records                    ; sql	       xx
       :insert-records                    ; sql        xx
       :update-records                    ; sql	       xx
       :execute-command		          ; sql        xx
       :query                             ; sql        xx
       :print-query			  ; sql	       xx
       :do-query		          ; sql	       xx
       :map-query			  ; sql	       xx
       :loop				  ; loop-ext   x
       ;;FDDL
       :create-table		          ; table      xx
       :drop-table		          ; table      xx
       :list-tables		          ; table      xx
       :table-exists-p                    ; table      xx 
       :list-attributes		          ; table      xx
       :attribute-type                    ; table      xx
       :list-attribute-types              ; table      xx
       :create-view		          ; table      xx
       :drop-view		          ; table      xx
       :create-index		          ; table      xx		
       :drop-index		          ; table      xx		
       ;;OODDL
       :standard-db-object		  ; objects    xx
       :def-view-class                    ; objects    xx
       :create-view-from-class            ; objects    xx
       :drop-view-from-class	          ; objects    xx
       ;;OODML
       :instance-refreshed                ;
       :update-object-joins               ;
       :*default-update-objects-max-len*  ; 
       :update-slot-from-record           ; objects    xx
       :update-instance-from-records      ; objects    xx
       :update-records-from-instance	  ; objects    xx
       :update-record-from-slot	          ; objects    xx
       :update-record-from-slots	  ; objects    xx
       :list-classes		          ; objects    xx
       :delete-instance-records	          ; objects    xx
       ;;Symbolic SQL Syntax 
       :sql				  ; syntax     xx
       :sql-expression                    ; syntax     xx
       :sql-operation                     ; syntax     xx
       :sql-operator			  ; syntax     xx  	
       :disable-sql-reader-syntax         ; syntax     xx
       :enable-sql-reader-syntax          ; syntax     xx
       :locally-disable-sql-reader-syntax ; syntax     xx
       :locally-enable-sql-reader-syntax  ; syntax     xx
       :restore-sql-reader-syntax-state   ; syntax     xx

       ;;------------------------------------------------
       ;; Miscellaneous Extensions
       ;;------------------------------------------------
       ;;Initialization
       :*loaded-database-types*           ; clsql-base xx
       :reload-database-types             ; clsql-base xx
       :closed-database 	          ; database   xx
       :database-type                     ; database   x
       :in-schema                         ; classes    x
       ;;FDDL 
       :list-views                        ; table      xx
       :view-exists-p                     ; table      xx
       :list-indexes                      ; table      xx
       :index-exists-p                    ; table      xx
       :create-sequence                   ; table      xx
       :drop-sequence                     ; table      xx
       :list-sequences                    ; table      xx
       :sequence-exists-p                 ; table      xx
       :sequence-next                     ; table      xx
       :sequence-last                     ; table      xx
       :set-sequence-position             ; table      xx
       ;;OODDL
       :view-table                        ; metaclass  x
       :create-sequence-from-class        ; objects    x
       :drop-sequence-from-class          ; objects    x	
       ;;OODML
       :add-to-relation                   ; objects    x
       :remove-from-relation              ; objects    x
       :read-sql-value                    ; objects    x
       :database-output-sql-as-type       ; objects    x
       :database-get-type-specifier       ; objects    x
       :database-output-sql               ; sql/class  xx

       ;;-----------------------------------------------
       ;; Conditions/Warnings/Errors
       ;;-----------------------------------------------
       :clsql-condition
       :clsql-error
       :clsql-simple-error
       :clsql-warning
       :clsql-simple-warning
       :clsql-invalid-spec-error
       :clsql-invalid-spec-error-connection-spec
       :clsql-invalid-spec-error-database-type
       :clsql-invalid-spec-error-template
       :clsql-connect-error
       :clsql-connect-error-database-type
       :clsql-connect-error-connection-spec
       :clsql-connect-error-errno
       :clsql-connect-error-error
       :clsql-sql-error
       :clsql-type-error
       :clsql-sql-error-database
       :clsql-sql-error-expression
       :clsql-sql-error-errno
       :clsql-sql-error-error
       :clsql-exists-condition
       :clsql-exists-condition-new-db
       :clsql-exists-condition-old-db
       :clsql-exists-warning
       :clsql-exists-error
       :clsql-closed-error
       :clsql-closed-error-database

       ;;-----------------------------------------------
       ;; Symbolic Sql Syntax 
       ;;-----------------------------------------------
       :sql-and-qualifier
       :sql-escape
       :sql-query
       :sql-any
       :sql-all
       :sql-not
       :sql-union
       :sql-intersection
       :sql-minus
       :sql-group-by
       :sql-having
       :sql-null
       :sql-not-null
       :sql-exists
       :sql-*
       :sql-+
       :sql-/
       :sql-like
       :sql-uplike
       :sql-and
       :sql-or
       :sql-in
       :sql-||
       :sql-is
       :sql-=
       :sql-==
       :sql-<
       :sql->
       :sql->=
       :sql-<=
       :sql-count
       :sql-max
       :sql-min
       :sql-avg
       :sql-sum
       :sql-view-class
       :sql_slot-value

))
  (:documentation "This is the INTERNAL SQL-Interface package of USQL."))


;; see http://thread.gmane.org/gmane.lisp.lispworks.general/681
#+lispworks
(setf *packages-for-warn-on-redefinition* 
      (delete "SQL" *packages-for-warn-on-redefinition* :test 'string=))

(defpackage #:clsql-usql
  (:nicknames #:usql #:sql)
  (:use :common-lisp)
  (:import-from :clsql-usql-sys . #1#)
  (:export . #1#)
  (:documentation "This is the SQL-Interface package of USQL."))

);eval-when                                      


