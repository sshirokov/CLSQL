;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          clsql-uffi.asd
;;;; Purpose:       ASDF definition file for CLSQL UFFI Helper package
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  Aug 2002
;;;;
;;;; $Id: clsql-uffi.asd,v 1.9 2002/09/18 07:50:01 kevin Exp $
;;;;
;;;; This file, part of CLSQL, is Copyright (c) 2002 by Kevin M. Rosenberg
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(declaim (optimize (debug 3) (speed 3) (safety 1) (compilation-speed 0)))

(in-package :asdf)

;;; System definition

(defsystem :clsql-uffi
  :components
  ((:module :uffi
	    :components
	    ((:file "clsql-uffi-package")
	     (:file "clsql-uffi-loader" :depends-on ("clsql-uffi-package"))
	     (:file "clsql-uffi" :depends-on ("clsql-uffi-loader")))))
  :depends-on (:uffi :clsql-base))


(defmethod source-file-type  ((c cl-source-file)
			      (s (eql (find-system :clsql-uffi)))) 
   "cl")
