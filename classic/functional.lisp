;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          functional.lisp
;;;; Purpose:       Functional interface
;;;;
;;;; Copyright (c) 1999-2001 Pierre R. Mai
;;;;
;;;; $Id$
;;;;
;;;; This file is part of CLSQL. 
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(in-package #:clsql-classic-sys)


;;; This file implements the more advanced functions of the
;;; functional SQL interface, which are just nicer layers above the
;;; basic SQL interface.

;;; These functions are no longer exported since they conflict with names
;;; exported by CLSQL

(defun insert-records
    (&key into attributes values av-pairs query (database *default-database*))
  "Insert records into the given table according to the given options."
  (cond
    ((and av-pairs (or attributes values))
     (error "Supply either av-pairs or values (and possibly attributes) to call of insert-records."))
    ((and (or av-pairs values) query)
     (error
      "Supply either query or values/av-pairs to call of insert-records."))
    ((and attributes (not query)
          (or (not (listp values)) (/= (length attributes) (length values))))
     (error "You must supply a matching values list when using attributes in call of insert-records."))
    (query
     (execute-command
      (format nil "insert into ~A ~@[(~{~A~^,~}) ~]~A" into attributes query)
      :database database))
    (t
     (execute-command
      (multiple-value-bind (attributes values)
          (if av-pairs
              (values (mapcar #'first av-pairs) (mapcar #'second av-pairs))
              (values attributes values))
	(format nil "insert into ~A ~@[(~{~A~^,~}) ~]values (~{'~A'~^,~})"
		into attributes values))
      :database database))))

(defun delete-records (&key from where (database *default-database*))
  "Delete the indicated records from the given database."
  (execute-command (format nil "delete from ~A ~@[where ~A ~]" from where)
                   :database database))

(defun update-records (table &key attributes values av-pairs where (database *default-database*))
  "Update the specified records in the given database."
  (cond
    ((and av-pairs (or attributes values))
     (error "Supply either av-pairs or values (and possibly attributes) to call of update-records."))
    ((and attributes
          (or (not (listp values)) (/= (length attributes) (length values))))
     (error "You must supply a matching values list when using attributes in call of update-records."))
    ((or (and attributes (not values)) (and values (not attributes)))
     (error "You must supply both values and attributes in call of update-records."))
    (t
     (execute-command
      (format nil "update ~A set ~:{~A = '~A'~:^, ~}~@[ where ~A~]"
              table
              (or av-pairs
                  (mapcar #'list attributes values))
              where)
      :database database))))

