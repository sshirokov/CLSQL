;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;;
;;;; $Id$
;;;;
;;;; Package definitions for CLSQL. 
;;;;
;;;; This file is part of CLSQL.
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(in-package #:cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)

#+sbcl
  (if (find-package 'sb-mop)
      (pushnew :clsql-sbcl-mop cl:*features*)
      (pushnew :clsql-sbcl-pcl cl:*features*))

  #+cmu
  (if (eq (symbol-package 'pcl:find-class)
	  (find-package 'common-lisp))
      (pushnew :clsql-cmucl-mop cl:*features*)
      (pushnew :clsql-cmucl-pcl cl:*features*)))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defpackage #:clsql-sys
    (:use #:common-lisp #:clsql-base-sys
	  #+clsql-sbcl-mop #:sb-mop
	  #+clsql-cmucl-mop #:mop
	  #+allegro #:mop
	  #+lispworks #:clos
	  #+scl #:clos
	  #+openmcl #:openmcl-mop)
    
    #+allegro
    (:shadowing-import-from 
     #:excl)
   #+lispworks
   (:shadowing-import-from 
    #:clos)
   #+clsql-sbcl-mop 
   (:shadowing-import-from 
    #:sb-pcl
    #:generic-function-lambda-list)
   #+clsql-sbcl-pcl
   (:shadowing-import-from 
    #:sb-pcl
    #:name
    #:class-direct-slots
    #:class-of #:class-name #:class-slots #:find-class
    #:slot-boundp
    #:standard-class
    #:slot-definition-name #:finalize-inheritance
    #:standard-direct-slot-definition
    #:standard-effective-slot-definition #:validate-superclass
    #:direct-slot-definition-class #:compute-effective-slot-definition
    #:effective-slot-definition-class
    #:slot-value-using-class
    #:class-prototype #:generic-function-method-class #:intern-eql-specializer
    #:make-method-lambda #:generic-function-lambda-list
    #:class-precedence-list #:slot-definition-type
    #:class-direct-superclasses
    #:compute-class-precedence-list)
   #+clsql-cmucl-mop 
   (:shadowing-import-from 
    #:pcl
    #:generic-function-lambda-list)
   #+clsql-cmucl-pcl
   (:shadowing-import-from 
    #:pcl
    #:class-direct-slots
    #:name
    #:class-of  #:class-name #:class-slots #:find-class #:standard-class
    #:slot-boundp
    #:slot-definition-name #:finalize-inheritance
    #:standard-direct-slot-definition #:standard-effective-slot-definition
    #:validate-superclass #:direct-slot-definition-class
    #:effective-slot-definition-class
    #:compute-effective-slot-definition
    #:slot-value-using-class
    #:class-prototype #:generic-function-method-class #:intern-eql-specializer
    #:make-method-lambda #:generic-function-lambda-list
    #:class-precedence-list #:slot-definition-type
    #:class-direct-superclasses
    #:compute-class-precedence-list)
   #+scl
   (:shadowing-import-from 
    #:clos
    #:class-prototype  ;; note: make-method-lambda is not fbound
    )
   
   (:import-from 
    #:clsql-base-sys
    .
    #1=(
       ;; conditions 
       #:clsql-condition
       #:clsql-error
       #:clsql-simple-error
       #:clsql-warning
       #:clsql-simple-warning
       #:clsql-invalid-spec-error
       #:clsql-invalid-spec-error-connection-spec
       #:clsql-invalid-spec-error-database-type
       #:clsql-invalid-spec-error-template
       #:clsql-access-error
       #:clsql-access-error-database-type
       #:clsql-access-error-connection-spec
       #:clsql-access-error-error
       #:clsql-connect-error
       #:clsql-connect-error-errno
       #:clsql-sql-error
       #:clsql-sql-error-database
       #:clsql-sql-error-expression
       #:clsql-sql-error-errno
       #:clsql-sql-error-error
       #:clsql-database-warning
       #:clsql-database-warning-database
       #:clsql-database-warning-message
       #:clsql-exists-condition
       #:clsql-exists-condition-new-db
       #:clsql-exists-condition-old-db
       #:clsql-exists-warning
       #:clsql-exists-error
       #:clsql-closed-error
       #:clsql-closed-error-database
       #:clsql-type-error
       #:clsql-sql-syntax-error

       ;; db-interface
       #:check-connection-spec
       #:database-initialize-database-type
       #:database-type-load-foreign
       #:database-name-from-spec
       #:database-create-sequence
       #:database-drop-sequence
       #:database-sequence-next
       #:database-set-sequence-position
       #:database-query-result-set
       #:database-dump-result-set
       #:database-store-next-row
       #:database-get-type-specifier
       #:database-list-tables
       #:database-list-views
       #:database-list-indexes
       #:database-list-table-indexes
       #:database-list-sequences
       #:database-list-attributes
       #:database-attribute-type
       #:database-add-attribute
       #:database-type 

       ;; initialize
       #:*loaded-database-types*
       #:reload-database-types
       #:*default-database-type*
       #:*initialized-database-types*
       #:initialize-database-type
       ;; classes
       #:database
       #:database-name
       #:command-recording-stream
       #:result-recording-stream
       #:database-view-classes
       #:conn-pool
       #:print-object 
       ;; utils
       #:sql-escape

       ;; database.lisp -- Connection
       #:*default-database-type*	          ; clsql-base xx
       #:*default-database*	          ; classes    xx
       #:connect			          ; database   xx
       #:*connect-if-exists*	          ; database   xx
       #:connected-databases	          ; database   xx
       #:database		          ; database   xx
       #:database-name                     ; database   xx
       #:disconnect		          ; database   xx
       #:reconnect                         ; database
       #:find-database                     ; database   xx
       #:status                            ; database   xx
       #:with-database
       #:with-default-database
       #:create-database
       #:destroy-database
       #:probe-database
       
       ;; pool.lisp
       #:disconnect-pooled

       ;; basic-sql.lisp
       #:query
       #:execute-command
       #:write-large-object
       #:read-large-object
       #:delete-large-object
       #:do-query
       #:map-query
       #:describe-table

       
       ;; recording.lisp -- SQL I/O Recording 
       #:record-sql-action
       #:add-sql-stream                 ; recording  xx
       #:delete-sql-stream	          ; recording  xx
       #:list-sql-streams	          ; recording  xx
       #:sql-recording-p	          ; recording  xx
       #:sql-stream			  ; recording  xx
       #:start-sql-recording		  ; recording  xx
       #:stop-sql-recording		  ; recording  xx
       
       ;; Transactions
       #:with-transaction
       #:commit-transaction
       #:rollback-transaction
       #:add-transaction-commit-hook
       #:add-transaction-rollback-hook
       #:commit                            ; transact   xx
       #:rollback			  ; transact   xx
       #:with-transaction		  ; transact   xx		.
       #:start-transaction                 ; transact   xx
       #:in-transaction-p                  ; transact   xx
       #:database-start-transaction
       #:database-abort-transaction
       #:database-commit-transaction
       #:transaction-level
       #:transaction
       
       ;; Database capabilities
       #:db-type-use-column-on-drop-index?
       #:db-backend-has-create/destroy-db?
       #:db-type-has-views?
       #:db-type-has-subqueries?
       #:db-type-has-boolean-where?
       #:db-type-transaction-capable?
       #:database-underlying-type
       ))
   (:export
    ;; "Private" exports for use by interface packages
    #:check-connection-spec
    #:database-initialize-database-type
    #:database-type-load-foreign
    #:database-name-from-spec
    #:database-connect
    #:database-query
    #:database-execute-command
    #:database-create-sequence
    #:database-drop-sequence
    #:database-sequence-next
    #:database-set-sequence-position
    #:database-query-result-set
    #:database-dump-result-set
    #:database-store-next-row
    #:database-get-type-specifier
    #:database-list-tables
    #:database-table-exists-p
    #:database-list-views
    #:database-view-exists-p
    #:database-list-indexes
    #:database-list-table-indexes
    #:database-index-exists-p
    #:database-list-sequences
    #:database-sequence-exists-p
    #:database-list-attributes
    #:database-attribute-type
    #:database-describe-table

    #:db-backend-has-create/destroy-db?
    #:db-type-has-views?
    #:db-type-has-subqueries?
    #:db-type-has-boolean-where?
    #:db-type-transaction-capable?
    #:database-underlying-type
   
   .
   ;; Shared exports for re-export by CLSQL. 
   ;; I = Implemented, D = Documented
   ;;  name                                 file       ID
   ;;====================================================
   #2=(;;------------------------------------------------
       ;; CommonSQL API 
       ;;------------------------------------------------
      ;;FDML 
       #:select                            ; objects    xx
       #:cache-table-queries               ; 
       #:*cache-table-queries-default*     ; 
       #:delete-records                    ; sql	       xx
       #:insert-records                    ; sql        xx
       #:update-records                    ; sql	       xx
       #:execute-command		          ; sql        xx
       #:query                             ; sql        xx
       #:print-query			  ; sql	       xx
       #:do-query		          ; sql	       xx
       #:map-query			  ; sql	       xx
       #:loop				  ; loop-ext   x
       ;;FDDL
       #:create-table		          ; table      xx
       #:drop-table		          ; table      xx
       #:list-tables		          ; table      xx
       #:table-exists-p                    ; table      xx 
       #:list-attributes		          ; table      xx
       #:attribute-type                    ; table      xx
       #:list-attribute-types              ; table      xx
       #:create-view		          ; table      xx
       #:drop-view		          ; table      xx
       #:create-index		          ; table      xx		
       #:drop-index		          ; table      xx		
       #:truncate-database
       ;;OODDL
       #:standard-db-object		  ; objects    xx
       #:def-view-class                    ; objects    xx
       #:create-view-from-class            ; objects    xx
       #:drop-view-from-class	          ; objects    xx
       ;;OODML
       #:instance-refreshed                ;
       #:update-object-joins               ;
       #:*default-update-objects-max-len*  ; 
       #:update-slot-from-record           ; objects    xx
       #:update-instance-from-records      ; objects    xx
       #:update-records-from-instance	  ; objects    xx
       #:update-record-from-slot	          ; objects    xx
       #:update-record-from-slots	  ; objects    xx
       #:list-classes		          ; objects    xx
       #:delete-instance-records	          ; objects    xx
       ;;Symbolic SQL Syntax 
       #:sql				  ; syntax     xx
       #:sql-expression                    ; syntax     xx
       #:sql-operation                     ; syntax     xx
       #:sql-operator			  ; syntax     xx  	
       #:disable-sql-reader-syntax         ; syntax     xx
       #:enable-sql-reader-syntax          ; syntax     xx
       #:locally-disable-sql-reader-syntax ; syntax     xx
       #:locally-enable-sql-reader-syntax  ; syntax     xx
       #:restore-sql-reader-syntax-state   ; syntax     xx

       ;;------------------------------------------------
       ;; Miscellaneous Extensions
       ;;------------------------------------------------
       ;;Initialization
       #:*loaded-database-types*           ; clsql-base xx
       #:reload-database-types             ; clsql-base xx
       #:database-type                     ; database   x
       #:is-database-open
       ;;FDDL 
       #:list-views                        ; table      xx
       #:view-exists-p                     ; table      xx
       #:list-indexes                      ; table      xx
       #:list-table-indexes                ; table      xx
       #:index-exists-p                    ; table      xx
       #:create-sequence                   ; table      xx
       #:drop-sequence                     ; table      xx
       #:list-sequences                    ; table      xx
       #:sequence-exists-p                 ; table      xx
       #:sequence-next                     ; table      xx
       #:sequence-last                     ; table      xx
       #:set-sequence-position             ; table      xx
       ;;OODDL
       #:view-table                        ; metaclass  x
       #:create-sequence-from-class        ; objects    x
       #:drop-sequence-from-class          ; objects    x	
       ;;OODML
       #:add-to-relation                   ; objects    x
       #:remove-from-relation              ; objects    x
       #:read-sql-value                    ; objects    x
       #:database-output-sql-as-type       ; objects    x
       #:database-get-type-specifier       ; objects    x
       #:database-output-sql               ; sql/class  xx

       ;;-----------------------------------------------
       ;; Symbolic Sql Syntax 
       ;;-----------------------------------------------
       #:sql-and-qualifier
       #:sql-escape
       #:sql-query
       #:sql-any
       #:sql-all
       #:sql-not
       #:sql-union
       #:sql-intersection
       #:sql-minus
       #:sql-group-by
       #:sql-having
       #:sql-null
       #:sql-not-null
       #:sql-exists
       #:sql-*
       #:sql-+
       #:sql-/
       #:sql-like
       #:sql-uplike
       #:sql-and
       #:sql-or
       #:sql-in
       #:sql-||
       #:sql-is
       #:sql-=
       #:sql-==
       #:sql-<
       #:sql->
       #:sql->=
       #:sql-<=
       #:sql-count
       #:sql-max
       #:sql-min
       #:sql-avg
       #:sql-sum
       #:sql-view-class
       #:sql_slot-value

       . 
       #1#
       ))
  (:documentation "This is the INTERNAL SQL-Interface package of CLSQL."))


;; see http://thread.gmane.org/gmane.lisp.lispworks.general/681
#+lispworks
(setf *packages-for-warn-on-redefinition* 
      (delete "SQL" *packages-for-warn-on-redefinition* :test 'string=))

(defpackage #:clsql
  (:use #:common-lisp)
  (:import-from #:clsql-sys . #2#)
  (:export . #2#)
  (:documentation "This is the SQL-Interface package of CLSQL."))

(defpackage #:clsql-user
  (:use #:common-lisp)
  (:import-from #:clsql-sys . #2#)
  (:export . #2#)
  (:documentation "This is the user package with CLSQL symbols."))

  ;; This is from USQL's pcl-patch  
  #+(or clsql-sbcl-pcl clsql-cmucl-pcl)
  (progn
    ;; Note that this will no longer required for cmucl as of version 19a. 
    (in-package #+cmu :pcl #+sbcl :sb-pcl)
    (defmacro pv-binding1 ((pv calls pv-table-symbol pv-parameters slot-vars) 
			   &body body)
      `(pv-env (,pv ,calls ,pv-table-symbol ,pv-parameters)
	(let (,@(mapcar #'(lambda (slot-var p) `(,slot-var (get-slots-or-nil ,p)))
			slot-vars pv-parameters))
	  ,@(mapcar #'(lambda (slot-var) `(declare (ignorable ,slot-var))) slot-vars)
	  ,@body))))
  
  
  #+sbcl
  (if (find-package 'sb-mop)
      (setq cl:*features* (delete :clsql-sbcl-mop cl:*features*))
      (setq cl:*features* (delete :clsql-sbcl-pcl cl:*features*)))
  
  #+cmu
  (if (find-package 'mop)
      (setq cl:*features* (delete :clsql-cmucl-mop cl:*features*))
      (setq cl:*features* (delete :clsql-cmucl-pcl cl:*features*)))
  
);eval-when                                      


