;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          sqlite-usql.lisp
;;;; Purpose:       SQLite interface for USQL routines
;;;; Programmers:   Aurelio Bignoli
;;;; Date Started:  Aug 2003
;;;;
;;;; $Id: sqlite-usql.lisp,v 1.3 2004/03/09 20:58:38 aurelio Exp $
;;;;
;;;; This file, part of CLSQL, is Copyright (c) 2003 by Aurelio Bignoli
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(in-package :clsql-sqlite)

(defun %sequence-name-to-table-name (sequence-name)
  (concatenate 'string "_usql_seq_" (sql-escape sequence-name)))

(defmethod database-create-sequence (sequence-name
				     (database sqlite-database))
  (let ((table-name (%sequence-name-to-table-name sequence-name)))
    (database-execute-command
     (concatenate 'string "CREATE TABLE " table-name
		  " (id INTEGER PRIMARY KEY)")
     database)
    (database-execute-command 
     (format nil "INSERT INTO ~A VALUES (-1)" table-name)
     database)))

(defmethod database-drop-sequence (sequence-name
				   (database sqlite-database))
  (database-execute-command
   (concatenate 'string "DROP TABLE "
		(%sequence-name-to-table-name sequence-name)) 
   database))

(defmethod database-sequence-next (sequence-name (database sqlite-database))
  (let ((table-name (%sequence-name-to-table-name sequence-name)))
    (database-execute-command
     (format nil "UPDATE ~A SET id=(SELECT id FROM ~A)+1"
	     table-name table-name)
     database))
  (sqlite:sqlite-last-insert-rowid (sqlite-db database)))

(defmethod database-list-tables ((database sqlite-database) &key system-tables)
  (declare (ignore system-tables))
  ;; Query is copied from .table command of sqlite comamnd line utility.
  (mapcar #'car (database-query
		 "SELECT name FROM sqlite_master WHERE type='table' UNION ALL SELECT name FROM sqlite_temp_master WHERE type='table' ORDER BY name"
		 database '())))

(declaim (inline sqlite-table-info))
(defun sqlite-table-info (table database)
  (database-query (format nil "PRAGMA table_info('~A')" table)
			  database '()))

(defmethod database-list-attributes (table (database sqlite-database))
  (mapcar #'(lambda (table-info) (third table-info))
	  (sqlite-table-info table database)))

(defmethod database-attribute-type (attribute table 
				    (database sqlite-database))
  (loop for field-info in (sqlite-table-info table database)
	when (string= attribute (second field-info))
	return (third field-info)))
