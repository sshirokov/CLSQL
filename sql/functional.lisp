;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          functional.lisp
;;;; Purpose:       Functional interface
;;;; Programmer:    Pierre R. Mai
;;;;
;;;; Copyright (c) 1999-2001 Pierre R. Mai
;;;;
;;;; $Id$
;;;;
;;;; This file is part of CLSQL. 
;;;;
;;;; CLSQL is free software; you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License (version 2) as
;;;; published by the Free Software Foundation.
;;;;
;;;; CLSQL is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with CLSQL; if not, write to the Free Software Foundation, Inc.,
;;;; 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
;;;; *************************************************************************

(in-package #:clsql-sys)


;;; This file implements the more advanced functions of the
;;; functional SQL interface, which are just nicer layers above the
;;; basic SQL interface.

;;; With the integration of CLSQL-USQL, these functions are no
;;; longer exported by the CLSQL package since they conflict with names
;;; exported by CLSQL-USQL

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

