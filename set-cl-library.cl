;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          set-cl-library.cl
;;;; Purpose:       Sets CL-LIBRARY logical host name if it does not exist
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  May 2002
;;;;
;;;; This file, part of CLSQL, is Copyright (c) 2002 by Kevin M. Rosenberg
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

;; Set logical pathname CL-LIBRARY to be directory above *load-truename*
;; This mirrors the expectation of Common Lisp Controller's pathnames

(handler-case
    (logical-pathname-translations "CL-LIBRARY")
  (error ()
	 (let* ((dir (pathname-directory *load-truename*))
		(parent-dir (subseq dir 0 (1- (length dir)))))
	   (load (make-pathname :name "set-logical" :type "cl"
				:defaults *load-truename*))
	   (set-logical-host-for-pathname 
	    "CL-LIBRARY" 
	    (make-pathname :host (pathname-host *load-truename*)
			   :device (pathname-device *load-truename*)
			   :directory parent-dir))))
  (:no-error (translation)
	     ;; Make sure that CL-LIBRARY points to this installation
	     (let* ((dir (pathname-directory *load-truename*))
		    (base-dir (car (last dir)))
		    (logical-dir (translate-logical-pathname 
				  (concatenate 'string
					       "CL-LIBRARY:" base-dir ";"))))
	       (unless (equalp dir (pathname-directory logical-dir))
		 (let ((*print-circle* nil))
		   (error "CL-LIBRARY:~A; directory ~S does not equal *load-truename*'s directory ~S"
			  base-dir (cdr dir)
			  (cdr (pathname-directory logical-dir))))))))
