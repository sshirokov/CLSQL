;;; -*- Mode: Lisp -*-
;;; $Id: oracle.cl,v 1.2 2002/05/13 03:57:09 kevin Exp $

;;; MaiSQL --- Common Lisp Interface Layer to SQL Databases
;;; This is copyrighted software.  See documentation for terms.
;;; 
;;; oracle.lisp --- FFI interface to Oracle on Unix
;;;
;;; The present content of this file is orented specifically towards
;;; Oracle 8.0.5.1 under Linux, linking against libclntsh.so

(in-package :clsql-oracle)

;;

(defvar *oci-initialized* nil)

(defvar *oci-env* nil)


;;
;; Opaque pointer types
;;

(def-alien-type oci-env (* t))

(def-alien-type oci-server (* t))

(def-alien-type oci-error (* t))

(def-alien-type oci-svc-ctx (* t))

(def-alien-type oci-stmt (* t))


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
  (pointer (make-alien (* t))))

(defun oci-init (&key (mode +oci-default+))
  (let ((x (alien-funcall (extern-alien "OCIInitialize" (function int int (* t) (* t) (* t) (* t)))
		   mode nil nil nil nil)))
    (if (= x 0)
	(let ((env (make-alien oci-env)))
	  (setq *oci-initialized* mode)
	  (let ((x (alien-funcall (extern-alien "OCIEnvInit" (function int (* t) int int (* t)))
				  env +oci-default+ 0 nil)))
	    (format t ";; OEI: reutrned ~d~%" x)
	    (setq *oci-env* env))))))

(defun oci-check-return (value)
  (if (= value +oci-invalid-handle+)
      (error "Invalid Handle")))

(defun oci-get-handle (&key type)
  (if (null *oci-initialized*)
      (oci-init))
  (case type
    (:error
     (let ((ptr (make-alien (* t))))
       (let ((x (alien-funcall (extern-alien "OCIHandleAlloc" (function int unsigned-int (* t) int int (* t)))
			       (sap-ref-32 (alien-sap (deref *oci-env*)) 0)
			       ptr
			       +oci-default+
			       0
			       nil)))
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
  (let ((envhp (oci-handle-alloc :type :env)))
    (oci-env-init envhp)
    envhp))

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
    `(let ((%lisp-oci-fn (def-alien-routine (,c-oci-symbol ,(intern (concatenate 'string "%" (symbol-name lisp-oci-fn))))
			   ,c-return ,@c-parms)))
       (defun ,lisp-oci-fn (,@ll &key database nulls-ok)
	 (case (funcall %lisp-oci-fn ,@ll)
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
	   (t
	    (error "OCI unknown error, code=~A" (values))))))))
  

(defmacro def-raw-oci-routine
  ((c-oci-symbol lisp-oci-fn) c-return &rest c-parms)
  (let ((ll (mapcar (lambda (x) (declare (ignore x)) (gensym)) c-parms)))
    `(let ((%lisp-oci-fn (def-alien-routine (,c-oci-symbol ,(intern (concatenate 'string "%" (symbol-name lisp-oci-fn))))
			   ,c-return ,@c-parms)))
       (defun ,lisp-oci-fn (,@ll &key database nulls-ok)
	 (funcall %lisp-oci-fn ,@ll)))))


(def-oci-routine ("OCIInitialize" OCI-INITIALIZE)
    int
  (mode unsigned-long)                  ; ub4
  (ctxp (* t))                          ; dvoid *
  (malocfp (* t))                       ; dvoid *(*)
  (ralocfp (* t))                       ; dvoid *(*)
  (mfreefp (* t)))                      ; void *(*)


(def-oci-routine ("OCIEnvInit" OCI-ENV-INIT)
    int
  (envpp (* t))                         ; OCIEnv **
  (mode unsigned-long)                  ; ub4
  (xtramem-sz unsigned-long)            ; size_t
  (usermempp (* t)))                    ; dvoid **
  
#+oci-8-1-5
(def-oci-routine ("OCIEnvCreate" OCI-ENV-CREATE)
    int
  (p0 (* t))
  (p1 unsigned-int)
  (p2 (* t))
  (p3 (* t))
  (p4 (* t))
  (p5 (* t))
  (p6 unsigned-long)
  (p7 (* t)))

(def-oci-routine ("OCIHandleAlloc" OCI-HANDLE-ALLOC)
    int
  (parenth      (* t))                  ; const dvoid *
  (hndlpp       (* t))                  ; dvoid **
  (type         unsigned-long)          ; ub4
  (xtramem_sz   unsigned-long)          ; size_t
  (usrmempp     (* t)))                 ; dvoid **

(def-oci-routine ("OCIServerAttach" OCI-SERVER-ATTACH)
    int
  (srvhp        (* t))                  ; oci-server
  (errhp        (* t))                  ; oci-error
  (dblink       c-string)               ; :in
  (dblink-len   unsigned-long)          ; int
  (mode         unsigned-long))         ; int


(def-oci-routine ("OCIHandleFree" OCI-HANDLE-FREE)
    int
  (p0 (* t)) ;; handle
  (p1 unsigned-long)) ;;type

(def-oci-routine ("OCILogon" OCI-LOGON)
    int
  (envhp        (* t))                  ; env
  (errhp        (* t))                  ; err
  (svchp        (* t))                  ; svc
  (username     c-string)               ; username
  (uname-len    unsigned-long)          ;
  (passwd       c-string)               ; passwd
  (password-len unsigned-long)          ;
  (dsn          c-string)               ; datasource
  (dsn-len      unsigned-long))         ;

(def-oci-routine ("OCILogoff" OCI-LOGOFF)
    int
  (p0	(* t))        ; svc
  (p1	(* t)))       ; err

(def-alien-routine ("OCIErrorGet" OCI-ERROR-GET)
    void
  (p0      (* t))
  (p1      unsigned-long)
  (p2      c-string)
  (p3      (* long))
  (p4      (* t))
  (p5      unsigned-long)
  (p6      unsigned-long))

(def-oci-routine ("OCIStmtPrepare" OCI-STMT-PREPARE)
    int
  (p0      (* t))
  (p1      (* t))
  (p2      c-string)
  (p3      unsigned-long)
  (p4      unsigned-long)
  (p5      unsigned-long))

(def-oci-routine ("OCIStmtExecute" OCI-STMT-EXECUTE)
    int
  (p0      (* t))
  (p1      (* t))
  (p2      (* t))
  (p3      unsigned-long)
  (p4      unsigned-long)
  (p5      (* t))
  (p6      (* t))
  (p7      unsigned-long))

(def-raw-oci-routine ("OCIParamGet" OCI-PARAM-GET)
    int
  (p0      (* t))
  (p1      unsigned-long)
  (p2      (* t))
  (p3      (* t))
  (p4      unsigned-long))

(def-oci-routine ("OCIAttrGet" OCI-ATTR-GET)
    int
  (p0      (* t))
  (p1      unsigned-long)
  (p2      (* t))
  (p3      (* unsigned-long))
  (p4      unsigned-long)
  (p5      (* t)))

#+nil
(def-oci-routine ("OCIAttrSet" OCI-ATTR-SET)
    int
  (trgthndlp (* t))
  (trgthndltyp int :in)
  (attributep (* t))
  (size int)
  (attrtype int)
  (errhp oci-error))

(def-oci-routine ("OCIDefineByPos" OCI-DEFINE-BY-POS)
    int
  (p0      (* t))
  (p1      (* t))
  (p2      (* t))
  (p3      unsigned-long)
  (p4      (* t))
  (p5      unsigned-long)
  (p6      unsigned-short)         
  (p7      (* t))
  (p8      (* t))          
  (p9      (* t))          
  (p10     unsigned-long))

(def-oci-routine ("OCIStmtFetch" OCI-STMT-FETCH)
    int
  (stmthp       (* t))
  (errhp        (* t))
  (p2           unsigned-long)
  (p3           unsigned-short)
  (p4           unsigned-long))


(def-oci-routine ("OCITransStart" OCI-TRANS-START)
  int
  (svchp       (* t))
  (errhp        (* t))
  (p2           unsigned-short)
  (p3           unsigned-short))

(def-oci-routine ("OCITransCommit" OCI-TRANS-COMMIT)
  int
  (svchp       (* t))
  (errhp        (* t))
  (p2           unsigned-short))

(def-oci-routine ("OCITransRollback" OCI-TRANS-ROLLBACK)
    int
  (svchp       (* t))
  (errhp        (* t))
  (p2           unsigned-short))


