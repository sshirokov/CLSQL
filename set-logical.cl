;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          set-logical.cl
;;;; Purpose:       Sets a logical host for src/binaries based on a pathname.
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  Feb 2002
;;;;
;;;; This file, part of CLSQL, is Copyright (c) 2002 by Kevin M. Rosenberg
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************


;;; Setup logical pathname translaton with separate binary directories
;;; for each implementation

;; push allegro case sensitivity on *features*
#+allegro
(eval-when (:compile-toplevel :load-toplevel :execute)
  (if (or (eq excl:*current-case-mode* :case-sensitive-lower)
	  (eq excl:*current-case-mode* :case-sensitive-upper))
      (pushnew :case-sensitive cl:*features*)
    (pushnew :case-insensitive cl:*features*)))

(defconstant +set-logical-compiler-name+
    #+(and allegro ics case-sensitive) "acl-modern"
    #+(and allegro (not ics) case-sensitive) "acl-modern8"
    #+(and allegro ics (not case-sensitive)) "acl-ansi"
    #+(and allegro (not ics) (not case-sensitive)) "acl-ansi8"
    #+lispworks "lispworks"
    #+clisp "clisp"
    #+cmu "cmucl"
    #+sbcl "sbcl"
    #+corman "corman"
    #+mcl "mcl"
    #+openmcl "openmcl"
    #-(or allegro lispworks clisp cmu sbcl corman mcl openmcl) "unknown")

(defun set-logical-host-for-pathname (host base-pathname)
  (setf (logical-pathname-translations host)
    `(("ROOT;" ,(make-pathname
		:host (pathname-host base-pathname)
		:device (pathname-device base-pathname)
		:directory (pathname-directory base-pathname)))
      ("**;*.cl.*" ,(merge-pathnames
		    (make-pathname
		     :name :wild
		     :type :wild
		     :directory '(:relative :wild-inferiors))
		    base-pathname))
      ("**;*.lisp.*" ,(merge-pathnames
		    (make-pathname
		     :name :wild
		     :type :wild
		     :directory '(:relative :wild-inferiors))
		    base-pathname))
      ("**;*.c.*" ,(merge-pathnames
		    (make-pathname
		     :name :wild
		     :type :wild
		     :directory '(:relative :wild-inferiors))
		    base-pathname))
      ("**;*.h.*" ,(merge-pathnames
		    (make-pathname
		     :name :wild
		     :type :wild
		     :directory '(:relative :wild-inferiors))
		    base-pathname))
      ("**;bin;*.*.*" ,(merge-pathnames
			(make-pathname 
			 :name :wild
			 :type :wild
			 :directory 
			 (append '(:relative :wild-inferiors
					     ".bin" #.+set-logical-compiler-name+)))
			base-pathname))
      ;; default is to place in .bin/<compiler> directory
      ("**;*.*.*" ,(merge-pathnames
		    (make-pathname 
		     :name :wild
		     :type :wild
		     :directory 
		     (append '(:relative :wild-inferiors
					 ".bin" #.+set-logical-compiler-name+)))
		    base-pathname)))))

