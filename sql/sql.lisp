;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:         sql.lisp
;;;; Purpose:      High-level SQL interface
;;;; Authors:      Kevin M. Rosenberg based on code by Pierre R. Mai 
;;;; Date Started: Feb 2002
;;;;
;;;; $Id$
;;;;
;;;; This file, part of CLSQL, is Copyright (c) 2002-2004 by Kevin M. Rosenberg
;;;; and Copyright (c) 1999-2001 by Pierre R. Mai
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(in-package #:clsql-sys)


(defun map-query (output-type-spec function query-expression
		  &key (database *default-database*)
		  (types nil))
  "Map the function over all tuples that are returned by the query in
query-expression.  The results of the function are collected as
specified in output-type-spec and returned like in MAP."
  ;; DANGER Will Robinson: Parts of the code for implementing
  ;; map-query (including the code below and the helper functions
  ;; called) are highly CMU CL specific.
  ;; KMR -- these have been replaced with cross-platform instructions above
  (macrolet ((type-specifier-atom (type)
	       `(if (atom ,type) ,type (car ,type))))
    (case (type-specifier-atom output-type-spec)
      ((nil) 
       (map-query-for-effect function query-expression database types))
      (list 
       (map-query-to-list function query-expression database types))
      ((simple-vector simple-string vector string array simple-array
	bit-vector simple-bit-vector base-string
	simple-base-string)
       (map-query-to-simple output-type-spec function query-expression database types))
      (t
       (funcall #'map-query (cmucl-compat:result-type-or-lose output-type-spec t)
              function query-expression :database database :types types)))))

(defun map-query-for-effect (function query-expression database types)
  (multiple-value-bind (result-set columns)
      (database-query-result-set query-expression database :full-set nil
				 :types types)
    (when result-set
      (unwind-protect
	   (do ((row (make-list columns)))
	       ((not (database-store-next-row result-set database row))
		nil)
	     (apply function row))
	(database-dump-result-set result-set database)))))
		     
(defun map-query-to-list (function query-expression database types)
  (multiple-value-bind (result-set columns)
      (database-query-result-set query-expression database :full-set nil
				 :types types)
    (when result-set
      (unwind-protect
	   (let ((result (list nil)))
	     (do ((row (make-list columns))
		  (current-cons result (cdr current-cons)))
		 ((not (database-store-next-row result-set database row))
		  (cdr result))
	       (rplacd current-cons (list (apply function row)))))
	(database-dump-result-set result-set database)))))


(defun map-query-to-simple (output-type-spec function query-expression database types)
  (multiple-value-bind (result-set columns rows)
      (database-query-result-set query-expression database :full-set t
				 :types types)
    (when result-set
      (unwind-protect
	   (if rows
	       ;; We know the row count in advance, so we allocate once
	       (do ((result
		     (cmucl-compat:make-sequence-of-type output-type-spec rows))
		    (row (make-list columns))
		    (index 0 (1+ index)))
		   ((not (database-store-next-row result-set database row))
		    result)
		 (declare (fixnum index))
		 (setf (aref result index)
		       (apply function row)))
	       ;; Database can't report row count in advance, so we have
	       ;; to grow and shrink our vector dynamically
	       (do ((result
		     (cmucl-compat:make-sequence-of-type output-type-spec 100))
		    (allocated-length 100)
		    (row (make-list columns))
		    (index 0 (1+ index)))
		   ((not (database-store-next-row result-set database row))
		    (cmucl-compat:shrink-vector result index))
		 (declare (fixnum allocated-length index))
		 (when (>= index allocated-length)
		   (setq allocated-length (* allocated-length 2)
			 result (adjust-array result allocated-length)))
		 (setf (aref result index)
		       (apply function row))))
	(database-dump-result-set result-set database)))))

(defmacro do-query (((&rest args) query-expression
		     &key (database '*default-database*)
		     (types nil))
		    &body body)
  (let ((result-set (gensym))
	(columns (gensym))
	(row (gensym))
	(db (gensym)))
    `(let ((,db ,database))
       (multiple-value-bind (,result-set ,columns)
	   (database-query-result-set ,query-expression ,db
				      :full-set nil :types ,types)
	 (when ,result-set
	   (unwind-protect
		(do ((,row (make-list ,columns)))
		    ((not (database-store-next-row ,result-set ,db ,row))
		     nil)
		  (destructuring-bind ,args ,row
		    ,@body))
	     (database-dump-result-set ,result-set ,db)))))))


;;; Row processing macro



(defun lisp->sql-name (field)
  (typecase field
    (string field)
    (symbol (string-upcase (symbol-name field)))
    (cons (cadr field))
    (t (format nil "~A" field))))

(defun field-names (field-forms)
  "Return a list of field name strings from a fields form"
  (loop for field-form in field-forms
	collect
	(lisp->sql-name
	 (if (cadr field-form)
	     (cadr field-form)
	     (car field-form)))))

(defun from-names (from)
  "Return a list of field name strings from a fields form"
  (loop for table in (if (atom from) (list from) from)
	collect (lisp->sql-name table)))


(defun where-strings (where)
  (loop for w in (if (atom (car where)) (list where) where)
	collect
	(if (consp w)
	    (format nil "~A ~A ~A" (second w) (first w) (third w))
	    (format nil "~A" w))))

(defun order-by-strings (order-by)
  (loop for o in order-by
	collect
	(if (atom o)
	    (lisp->sql-name o)
	    (format nil "~A ~A" (lisp->sql-name (car o))
		    (lisp->sql-name (cadr o))))))

(defun query-string (fields from where distinct order-by limit)
  (concatenate
   'string
   (format nil "select ~A~{~A~^,~} from ~{~A~^ and ~}" 
	   (if distinct "distinct " "") (field-names fields)
	   (from-names from))
   (if where (format nil " where ~{~A~^ ~}"
		     (where-strings where)) "")
   (if order-by (format nil " order by ~{~A~^, ~}"
			(order-by-strings order-by)))
   (if limit (format nil " limit ~D" limit) "")))

(defmacro for-each-row (((&rest fields) &key from order-by where distinct limit) &body body)
  (let ((d (gensym "DISTINCT-"))
	(bind-fields (loop for f in fields collect (car f)))
	(w (gensym "WHERE-"))
	(o (gensym "ORDER-BY-"))
	(frm (gensym "FROM-"))
	(l (gensym "LIMIT-"))
	(q (gensym "QUERY-")))
    `(let ((,frm ,from)
	   (,w ,where)
	   (,d ,distinct)
	   (,l ,limit)
	   (,o ,order-by))
      (let ((,q (query-string ',fields ,frm ,w ,d ,o ,l)))
	(loop for tuple in (query ,q)
	      collect (destructuring-bind ,bind-fields tuple
		   ,@body))))))

;;; Marc Battyani : Large objects support

(defun create-large-object (&key (database *default-database*))
  "Creates a new large object in the database and returns the object identifier"
  (database-create-large-object database))

(defun write-large-object (object-id data &key (database *default-database*))
  "Writes data to the large object"
  (database-write-large-object object-id data database))

(defun read-large-object (object-id &key (database *default-database*))
  "Reads the large object content"
  (database-read-large-object object-id database))

(defun delete-large-object (object-id &key (database *default-database*))
  "Deletes the large object in the database"
  (database-delete-large-object object-id database))


