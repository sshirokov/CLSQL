;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          oracle.lisp
;;;; Purpose:       Package definition for CLSQL Oracle interface
;;;;
;;;; $Id$
;;;;
;;;; This file is part of CLSQL.
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(in-package #:clsql-oracle)

(defvar *oci-initialized* nil)

(defvar *oci-env* nil)


;;
;; Opaque pointer types
;;

(uffi:def-foreign-type oci-env (* :void))

(uffi:def-foreign-type oci-server (* :void))

(uffi:def-foreign-type oci-error (* :void))

(uffi:def-foreign-type oci-svc-ctx (* :void))

(uffi:def-foreign-type oci-stmt (* :void))


(defvar *oci-handle-types*
  '(:error				; error report handle (OCIError)
    :service-context			; service context handle (OCISvcCtx)
    :statement				; statement (application request) handle (OCIStmt)
    :describe				; select list description handle (OCIDescribe)
    :server				; server context handle (OCIServer)
    :session				; user session handle (OCISession)
    :transaction			; transaction context handle (OCITrans)
    :complex-object			; complex object retrieval handle (OCIComplexObject)
    :security))				; security handle (OCISecurity)

(defstruct oci-handle
  (type :unknown)
  (pointer (uffi:allocate-foreign-object '(* :void))))

(defvar +null-void-pointer+ (uffi:make-null-pointer :void))
(defvar +null-void-pointer-pointer+ (uffi:make-null-pointer (* :void)))

(uffi:def-function "OCIInitialize"
    ((a :int)
     (b (* :void))
     (c (* :void))
     (d (* :void))
     (e (* :void)))
  :returning :int)

(uffi:def-function "OCIEnvInit"
    ((a (* :void))
     (b :int)
     (c :int)
     (d (* :void)))
  :returning :int)

(uffi:def-function "OCIHandleAlloc" 
    ((a :unsigned-int)
     (b (* :void))
     (c :int)
     (d :int)
     (e (* :void)))
  :returning :int)

;;; Check an OCI return code for erroricity and signal a reasonably
;;; informative condition if so.
;;;
;;; ERRHP provides an error handle which can be used to find
;;; subconditions; if it's not provided, subcodes won't be checked.
;;;
;;; NULLS-OK says that a NULL-VALUE-RETURNED subcondition condition is
;;; normal and needn't cause any signal. An error handle is required
;;; to detect this subcondition, so it doesn't make sense to set ERRHP
;;; unless NULLS-OK is set.

(defmacro def-oci-routine ((c-oci-symbol lisp-oci-fn) c-return &rest c-parms)
  (let ((ll (mapcar (lambda (x) (gensym)) c-parms)))
    `(let ((%lisp-oci-fn (uffi:def-function
			     (,c-oci-symbol ,(intern (concatenate 'string "%" (symbol-name lisp-oci-fn))))
			     ,c-parms
			     :returning ,c-return)))
       (defun ,lisp-oci-fn (,@ll &key database nulls-ok)
	 (let ((result (funcall %lisp-oci-fn ,@ll)))
	   (case result
	     (#.+oci-success+
	      +oci-success+)
	     (#.+oci-error+
	      (handle-oci-error :database database :nulls-ok nulls-ok))
	     (#.+oci-no-data+
	      (error "OCI No Data Found"))
	     (#.+oci-success-with-info+
	      (error "internal error: unexpected +oci-SUCCESS-WITH-INFO"))
	     (#.+oci-no-data+
	      (error "OCI No Data"))
	     (#.+oci-invalid-handle+
	      (error "OCI Invalid Handle"))
	     (#.+oci-need-data+
	      (error "OCI Need Data"))
	     (#.+oci-still-executing+
	      (error "OCI Still Executing"))
	     (#.+oci-continue+
	      (error "OCI Continue"))
	     (1804
	      (error "Check ORACLE_HOME and NLS settings."))
	     (t
	      (error "OCI unknown error, code=~A" result))))))))
  

(defmacro def-raw-oci-routine
  ((c-oci-symbol lisp-oci-fn) c-return &rest c-parms)
  (let ((ll (mapcar (lambda (x) (declare (ignore x)) (gensym)) c-parms)))
    `(let ((%lisp-oci-fn (uffi:def-function (,c-oci-symbol ,(intern (concatenate 'string "%" (symbol-name lisp-oci-fn))))
			     ,c-parms
			   :returning ,c-return)))
       (defun ,lisp-oci-fn (,@ll &key database nulls-ok)
	 (funcall %lisp-oci-fn ,@ll)))))


(def-oci-routine ("OCIInitialize" oci-initialize)
    :int
  (mode :unsigned-long)			; ub4
  (ctxp (* :void))			; dvoid *
  (malocfp (* :void))			; dvoid *(*)
  (ralocfp (* :void))			; dvoid *(*)
  (mfreefp (* (* :void))))		; void *(*)


(def-oci-routine ("OCIEnvInit" oci-env-init)
    :int
  (envpp (* :void))                         ; OCIEnv **
  (mode :unsigned-long)                  ; ub4
  (xtramem-sz :unsigned-long)            ; size_t
  (usermempp (* (* :void))))                    ; dvoid **
  
#+oci-8-1-5
(def-oci-routine ("OCIEnvCreate" oci-env-create)
    :int
  (p0 (* :void))
  (p1 :unsigned-int)
  (p2 (* :void))
  (p3 (* :void))
  (p4 (* :void))
  (p5 (* :void))
  (p6 :unsigned-long)
  (p7 (* :void)))

(def-oci-routine ("OCIHandleAlloc" oci-handle-alloc)
    :int
  (parenth      (* :void))		; const dvoid *
  (hndlpp       (* (* :void)))		; dvoid **
  (type         :unsigned-long)		; ub4
  (xtramem_sz   :unsigned-long)		; size_t
  (usrmempp     (* (* :void))))		; dvoid **

(def-oci-routine ("OCIServerAttach" oci-server-attach)
    :int
  (srvhp        (* :void))                  ; oci-server
  (errhp        (* :void))                  ; oci-error
  (dblink       :cstring)               ; :in
  (dblink-len   :unsigned-long)          ; int
  (mode         :unsigned-long))         ; int


(def-oci-routine ("OCIHandleFree" oci-handle-free)
    :int
  (p0 (* :void)) ;; handle
  (p1 :unsigned-long)) ;;type

(def-oci-routine ("OCILogon" oci-logon)
    :int
  (envhp        (* :void))		; env
  (errhp        (* :void))		; err
  (svchpp       (* (* :void)))		; svc
  (username     :cstring)		; username
  (uname-len    :unsigned-long)		;
  (passwd       :cstring)		; passwd
  (password-len :unsigned-long)		;
  (dsn          :cstring)		; datasource
  (dsn-len      :unsigned-long))	;

(def-oci-routine ("OCILogoff" oci-logoff)
    :int
  (p0	(* :void))        ; svc
  (p1	(* :void)))       ; err

(uffi:def-function ("OCIErrorGet" oci-error-get)
    ((handlp  (* :void))
     (recordno  :unsigned-long)
     (sqlstate   :cstring)
     (errcodep   (* :long))
     (bufp      (* :unsigned-char))
     (bufsize      :unsigned-long)
     (type      :unsigned-long))
  :returning :void)

(def-oci-routine ("OCIStmtPrepare" oci-stmt-prepare)
    :int
  (stmtp      (* :void))
  (errhp      (* :void))
  (stmt      :cstring)
  (stmt_len      :unsigned-long)
  (language      :unsigned-long)
  (mode      :unsigned-long))

(def-oci-routine ("OCIStmtExecute" oci-stmt-execute)
    :int
  (svchp      (* :void))
  (stmtp1      (* :void))
  (errhp      (* :void))
  (iters      :unsigned-long)
  (rowoff      :unsigned-long)
  (snap_in      (* :void))
  (snap_out      (* :void))
  (mode     :unsigned-long))

(def-raw-oci-routine ("OCIParamGet" oci-param-get)
    :int
  (hndlp      (* :void))
  (htype      :unsigned-long)
  (errhp      (* :void))
  (parmdpp      (* (* :void)))
  (pos      :unsigned-long))

(def-oci-routine ("OCIAttrGet" oci-attr-get)
    :int
  (trgthndlp      (* :void))
  (trghndltyp      :unsigned-int)
  (attributep      (* :void))
  (sizep      (* :unsigned-int))
  (attrtype      :unsigned-int)
  (errhp      (* :void)))

(def-oci-routine ("OCIAttrSet" oci-attr-set)
    :int
  (trgthndlp (* :void))
  (trgthndltyp :int :in)
  (attributep (* :void))
  (size :int)
  (attrtype :int)
  (errhp oci-error))

(def-oci-routine ("OCIDefineByPos" oci-define-by-pos)
    :int
  (p0      (* :void))
  (p1      (* :void))
  (p2      (* :void))
  (p3      :unsigned-long)
  (p4      (* :void))
  (p5      :unsigned-long)
  (p6      :unsigned-short)         
  (p7      (* :void))
  (p8      (* :void))          
  (p9      (* :void))          
  (p10     :unsigned-long))

(def-oci-routine ("OCIStmtFetch" oci-stmt-fetch)
    :int
  (stmthp       (* :void))
  (errhp        (* :void))
  (p2           :unsigned-long)
  (p3           :unsigned-short)
  (p4           :unsigned-long))


(def-oci-routine ("OCITransStart" oci-trans-start)
  :int
  (svchp       (* :void))
  (errhp        (* :void))
  (p2           :unsigned-short)
  (p3           :unsigned-short))

(def-oci-routine ("OCITransCommit" oci-trans-commit)
  :int
  (svchp       (* :void))
  (errhp        (* :void))
  (p2           :unsigned-short))

(def-oci-routine ("OCITransRollback" oci-trans-rollback)
    :int
  (svchp       (* :void))
  (errhp        (* :void))
  (p2           :unsigned-short))



;;; Functions

(defun oci-init (&key (mode +oci-default+))
  (let ((x (OCIInitialize mode +null-void-pointer+ +null-void-pointer+ +null-void-pointer+ +null-void-pointer+)))
    (if (= x 0)
	(let ((env (uffi:make-pointer 0 oci-env)))
	  (setq *oci-initialized* mode)
	  (let ((x (OCIEnvInit env +oci-default+ 0 +null-void-pointer+)))
	    (format t ";; OEI: returned ~d~%" x)
	    (setq *oci-env* env))))))

(defun oci-check-return (value)
  (when (= value +oci-invalid-handle+)
    (error "Invalid Handle")))

(defun oci-get-handle (&key type)
  (if (null *oci-initialized*)
      (oci-init))
  (case type
    (:error
     (let ((ptr (uffi:make-null-pointer (* :void))))
       (let ((x (OCIHandleAlloc
		 (uffi:pointer-address (uffi:deref-pointer *oci-env* oci-env))
		 ptr
		 +oci-default+
		 0
		 +null-void-pointer+)))
	 (oci-check-return x)
	 ptr)))
    (:service-context
     "OCISvcCtx")
    (:statement
     "OCIStmt")
    (:describe
     "OCIDescribe")
    (:server
     "OCIServer")
    (:session
     "OCISession")
    (:transaction
     "OCITrans")
    (:complex-object
     "OCIComplexObject")
    (:security
     "OCISecurity")
    (t
     (error "'~s' is not a valid OCI handle type" type))))

(defun oci-environment ()
  (let ((envhp (oci-get-handle :type :env)))
    (oci-env-init envhp 0 0 +null-void-pointer+)
    envhp))
