;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          postgresql.cl
;;;; Purpose:       Low-level PostgreSQL interface using UFFI
;;;; Programmers:   Kevin M. Rosenberg based on 
;;;;                Original code by Pierre R. Mai 
;;;; Date Started:  Feb 2002
;;;;
;;;; $Id: postgresql-api.cl,v 1.3 2002/03/25 14:13:41 kevin Exp $
;;;;
;;;; This file, part of CLSQL, is Copyright (c) 2002 by Kevin M. Rosenberg
;;;; and Copyright (c) 1999-2001 by Pierre R. Mai
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(declaim (optimize (debug 3) (speed 3) (safety 1) (compilation-speed 0)))
(in-package :postgresql)


;;;; This file implements as little of the FFI bindings to the
;;;; PostgreSQL client libraries as we could get away with.
;;;; Especially all the PostgreSQL-specific goodies aren't there, and
;;;; we just use void pointers where we can get away with it, which
;;;; thanks to the design of the PostgreSQL client libraries is pretty
;;;; much everywhere, in contrast to the MySQL client libraries for
;;;; example.

;;;; Type definitions

;;; Basic Types

(uffi:def-foreign-type pgsql-oid :unsigned-int)

(uffi:def-enum pgsql-conn-status-type 
    (:connection-ok
     :connection-bad))

(uffi:def-enum pgsql-exec-status-type
    (:empty-query
     :command-ok
     :tuples-ok
     :copy-out
     :copy-in
     :bad-response
     :nonfatal-error
     :fatal-error))

(uffi:def-foreign-type pgsql-conn :pointer-void)
(uffi:def-foreign-type pgsql-result :pointer-void)

(uffi:def-enum pgsql-ftype
    ((:bytea 17)
     (:int2 21)
     (:int4 23)
     (:float4 700)
     (:float8 701)))
  
    
;;(declaim (inline PQsetdbLogin)) ;; causes compile error in LW 4.2.0
(uffi:def-function ("PQsetdbLogin" PQsetdbLogin)
  ((pghost :cstring)
   (pgport :cstring)
   (pgoptions :cstring)
   (pgtty :cstring)
   (dbName :cstring)
   (login :cstring)
   (pwd :cstring))
  :returning pgsql-conn)

(declaim (inline PQfinish))
(uffi:def-function ("PQfinish" PQfinish)
  ((conn pgsql-conn))
  :module "postgresql"
  :returning :void)

(declaim (inline PQstatus))
(uffi:def-function ("PQstatus" PQstatus)
  ((conn pgsql-conn))
  :module "postgresql"
  :returning pgsql-conn-status-type)

(declaim (inline PQerrorMessage))
(uffi:def-function ("PQerrorMessage" PQerrorMessage)
  ((conn pgsql-conn))
  :module "postgresql"
  :returning :cstring)

(declaim (inline PQexec))
(uffi:def-function ("PQexec" PQexec)
  ((conn pgsql-conn)
   (query :cstring))
  :module "postgresql"
  :returning pgsql-result)

(declaim (inline PQresultStatus))
(uffi:def-function ("PQresultStatus" PQresultStatus)
  ((res pgsql-result))
  :module "postgresql"
  :returning pgsql-exec-status-type)

(declaim (inline PQresultErrorMessage))
(uffi:def-function ("PQresultErrorMessage" PQresultErrorMessage)
  ((res pgsql-result))
  :module "postgresql"
  :returning :cstring)

(declaim (inline PQntuples))
(uffi:def-function ("PQntuples" PQntuples) 
  ((res pgsql-result))
  :module "postgresql"
  :returning :int)

(declaim (inline PQnfields))
(uffi:def-function ("PQnfields" PQnfields)
  ((res pgsql-result))
  :module "postgresql"
  :returning :int)

(declaim (inline PQfname))
(uffi:def-function ("PQfname" PQfname)
  ((res pgsql-result)
   (field-num :int))
  :module "postgresql"
  :returning :cstring)

(declaim (inline PQfnumber))
(uffi:def-function ("PQfnumber" PQfnumber)
  ((res pgsql-result)
  (field-name :cstring))
  :module "postgresql"
  :returning :int)

(declaim (inline PQftype))
(uffi:def-function ("PQftype" PQftype)
  ((res pgsql-result)
   (field-num :int))
  :module "postgresql"
  :returning pgsql-oid)

(declaim (inline PQfsize))
(uffi:def-function ("PQfsize" PQfsize)
  ((res pgsql-result)
   (field-num :int))
  :module "postgresql"
  :returning :short)

(declaim (inline PQcmdStatus))
(uffi:def-function ("PQcmdStatus" PQcmdStatus)
  ((res pgsql-result))
  :module "postgresql"
  :returning :cstring)

(declaim (inline PQoidStatus))
(uffi:def-function ("PQoidStatus" PQoidStatus)
  ((res pgsql-result))
  :module "postgresql"
  :returning :cstring)

(declaim (inline PQcmdTuples))
(uffi:def-function ("PQcmdTuples" PQcmdTuples)
  ((res pgsql-result))
  :module "postgresql"
  :returning :cstring)

(declaim (inline PQgetvalue))
(uffi:def-function ("PQgetvalue" PQgetvalue)
  ((res pgsql-result)
   (tup-num :int)
   (field-num :int))
  :module "postgresql"
  :returning (* :unsigned-char))

(declaim (inline PQgetlength))
(uffi:def-function ("PQgetlength" PQgetlength)
  ((res pgsql-result)
   (tup-num :int)
   (field-num :int))
  :module "postgresql"
  :returning :int)

(declaim (inline PQgetisnull))
(uffi:def-function ("PQgetisnull" PQgetisnull)
  ((res pgsql-result)
   (tup-num :int)
   (field-num :int))
  :module "postgresql"
  :returning :int)

(declaim (inline PQclear))
(uffi:def-function ("PQclear" PQclear)
  ((res pgsql-result))
  :module "postgresql"
  :returning :void)
