;;; -*- Mode: Lisp -*-
;;; $Id: oracle-loader.cl,v 1.1 2002/08/01 03:06:26 kevin Exp $
;;;
;;; MaiSQL --- Common Lisp Interface Layer to SQL Databases
;;; This is copyrighted software.  See documentation for terms.
;;; 
;;; oracle-loader.cl --- Foreign Object Loader for Oracle

(in-package :clsql-oracle)

;; Load the foreign library

(eval-when (:load-toplevel :compile-toplevel)
  (defvar *oracle-home*
    nil
    "The root of the Oracle installation, usually $ORACLE_HOME is set to this.")
  (unless *oracle-home*
    (setf *oracle-home*
          (cdr (assoc ':ORACLE_HOME ext:*environment-list* :test #'eq)))))

(defparameter *oracle-libs*
  '(#-oracle-9i "rdbms/lib/ssdbaed.o"
    "rdbms/lib/defopt.o"
    #-oracle-9i "rdbms/lib/homts.o"
    "lib/nautab.o"
    "lib/naeet.o"
    "lib/naect.o"
    "lib/naedhs.o"
    #-oracle-9i"lib/libnsslb8.a"
    #+oracle-9i "lib/homts.o"
    )
  "Oracle client libraries, relative to ORACLE_HOME.")

(defun make-oracle-load-path ()
  (mapcar (lambda (x)
	    (concatenate 'string *oracle-home* "/" x))
	  *oracle-libs*))


; ;(defparameter *oracle-so-libraries*
; ;;  `(,(concatenate 'string "-L" *oracle-home* "/lib/")
;     '(
;       "-lclntsh"
;       "-lnetv2"
;       "-lnttcp"
;       "-lnetwork"
;       "-lncr"
;       "-lclient"
;       "-lvsn"
;       "-lcommon"
;       "-lgeneric"
;       "-lmm"
;       "-lnlsrtl3"
;       "-lcore4"
;       "-lnlsrtl3"
;       "-lepc"
;       "-ldl"
;       "-lm")
;   "List of library flags needed to be passed to ld to load the
; Oracle client library succesfully.  If this differs at your site,
; set *oracle-so-libraries* to the right path before compiling or
; loading the system.")


#-oracle-9i
(defun oracle-libraries ()
  `(,(concatenate 'string
		 "-L" *oracle-home* "/lib")
    "-lagtsh"
;;    "-locijdbc8"
    "-lclntsh"
    "-lclient8"
    "-lvsn8"
    "-lcommon8"
    "-lskgxp8"
    "-lmm"
    "-lnls8"
    "-lcore8"
    "-lgeneric8"
    "-ltrace8"
    "-ldl"
    "-lm"))

;;  "List of library flags needed to be passed to ld to load the
;;Oracle client library succesfully.  If this differs at your site,
;;set *oracle-so-libraries* to the right path before compiling or
;;loading the system.")

#+oracle-9i
(defun oracle-libraries ()
  `(,(concatenate 'string
		 "-L" *oracle-home* "/lib")
    "-lagent9"
    "-lagtsh"
;;    "-locijdbc8"
    "-lclntsh"
    "-lclntst9"
    "-lclient9"
    "-lvsn9"
    "-lcommon9"
    "-lskgxp9"
    "-lmm"
    "-lnls9"
    "-lcore9"
    "-lgeneric9"
    "-ltrace9"
    "-ldl"
    #+redhat-linux "-L/usr/lib/gcc-lib/i386-redhat-linux/2.96"
    "-lgcc"
    "-lm"))

(defmethod database-type-load-foreign ((database-type (eql :oracle)))
  (progv '(sys::*dso-linker*)
      '("/usr/bin/ld")
    (ext:load-foreign (make-oracle-load-path)
		    :libraries (oracle-libraries))))


(database-type-load-foreign :oracle)
