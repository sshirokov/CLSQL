;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          mysql-client-info.lisp
;;;; Purpose:       Check mysql client version
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  April 2004
;;;;
;;;; $Id: mysql-api.lisp 8801 2004-03-31 23:48:44Z kevin $
;;;;
;;;; This file, part of CLSQL, is Copyright (c) 2002 by Kevin M. Rosenberg
;;;; and Copyright (c) 1999-2001 by Pierre R. Mai
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(in-package #:mysql)

(declaim (inline mysql-get-client-info))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (uffi:def-function "mysql_get_client_info"
      ()
    :module "mysql"
    :returning :cstring)

  (let ((version (uffi:convert-from-cstring (mysql-get-client-info))))
    (cond
      ((eql (schar version 0) #\3)
       (pushnew :mysql-client-v3 cl:*features*))
      ((eql (schar version 0) #\4)
       (pushnew :mysql-client-v4 cl:*features*))
      (t
       (error "Unknown mysql client version '~A'." version)))))

;;#-(or :mysql-client-v3 :mysql-client-v4)
;;(eval-when (:compile-toplevel :load-toplevel :execute)
;;  (pushnew :mysql-client-v3 cl:*features*))

