;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;;
;;;; $Id$
;;;;
;;;; Base SQL functions
;;;;
;;;; This file is part of CLSQL.
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(in-package #:clsql-base)

;;; Query

(defgeneric query (query-expression &key database result-types flatp)
  (:documentation
   "Execute the SQL query expression QUERY-EXPRESSION on the given
DATABASE which defaults to *default-database*. RESULT-TYPES is a list
of symbols such as :string and :integer, one for each field in the
query, which are used to specify the types to return. The FLATP
argument, which has a default value of nil, specifies if full
bracketed results should be returned for each matched entry. If FLATP
is nil, the results are returned as a list of lists. If FLATP is t,
the results are returned as elements of a list, only if there is only
one result per row. Returns a list of lists of values of the result of
that expression and a list of field names selected in sql-exp."))

(defmethod query ((query-expression string) &key (database *default-database*)
                  (result-types :auto) (flatp nil) (field-names t))
  (record-sql-action query-expression :query database)
  (multiple-value-bind (rows names) (database-query query-expression database result-types
                                                    field-names)
    (let ((result (if (and flatp (= 1 (length (car rows))))
                      (mapcar #'car rows)
                    rows)))
      (record-sql-action result :result database)
      (if field-names
	  (values result names)
	result))))

;;; Execute

(defgeneric execute-command (expression &key database)
  (:documentation
   "Executes the SQL command specified by EXPRESSION for the database
specified by DATABASE, which has a default value of
*DEFAULT-DATABASE*. The argument EXPRESSION may be any SQL statement
other than a query. To run a stored procedure, pass an appropriate
string. The call to the procedure needs to be wrapped in a BEGIN END
pair."))

(defmethod execute-command ((sql-expression string)
                            &key (database *default-database*))
  (record-sql-action sql-expression :command database)
  (let ((res (database-execute-command sql-expression database)))
    (record-sql-action res :result database))
  (values))

(defmacro do-query (((&rest args) query-expression
		     &key (database '*default-database*) (result-types :auto))
		    &body body)
  "Repeatedly executes BODY within a binding of ARGS on the
attributes of each record resulting from QUERY-EXPRESSION. The
return value is determined by the result of executing BODY. The
default value of DATABASE is *DEFAULT-DATABASE*."
  (let ((result-set (gensym))
	(columns (gensym))
	(row (gensym))
	(db (gensym)))
    `(if (listp ,query-expression)
	 ;; Object query 
         (dolist (,row ,query-expression)
           (destructuring-bind ,args 
               ,row
             ,@body))
	 ;; Functional query 
	 (let ((,db ,database))
	   (multiple-value-bind (,result-set ,columns)
	       (database-query-result-set ,query-expression ,db
					  :full-set nil 
					  :result-types ,result-types)
	     (when ,result-set
	       (unwind-protect
		    (do ((,row (make-list ,columns)))
			((not (database-store-next-row ,result-set ,db ,row))
			 nil)
		      (destructuring-bind ,args ,row
			,@body))
		 (database-dump-result-set ,result-set ,db))))))))

(defun map-query (output-type-spec function query-expression
		  &key (database *default-database*)
		  (result-types :auto))
  "Map the function over all tuples that are returned by the
query in QUERY-EXPRESSION. The results of the function are
collected as specified in OUTPUT-TYPE-SPEC and returned like in
MAP."
  (if (listp query-expression)
      ;; Object query 
      (map output-type-spec #'(lambda (x) (apply function x)) query-expression)
      ;; Functional query 
      (macrolet ((type-specifier-atom (type)
		   `(if (atom ,type) ,type (car ,type))))
	(case (type-specifier-atom output-type-spec)
	  ((nil) 
	   (map-query-for-effect function query-expression database 
				 result-types))
	  (list 
	   (map-query-to-list function query-expression database result-types))
	  ((simple-vector simple-string vector string array simple-array
			  bit-vector simple-bit-vector base-string
			  simple-base-string)
	   (map-query-to-simple output-type-spec function query-expression 
				database result-types))
	  (t
	   (funcall #'map-query 
		    (cmucl-compat:result-type-or-lose output-type-spec t)
		    function query-expression :database database 
		    :result-types result-types))))))

(defun map-query-for-effect (function query-expression database result-types)
  (multiple-value-bind (result-set columns)
      (database-query-result-set query-expression database :full-set nil
				 :result-types result-types)
    (when result-set
      (unwind-protect
	   (do ((row (make-list columns)))
	       ((not (database-store-next-row result-set database row))
		nil)
	     (apply function row))
	(database-dump-result-set result-set database)))))
		     
(defun map-query-to-list (function query-expression database result-types)
  (multiple-value-bind (result-set columns)
      (database-query-result-set query-expression database :full-set nil
				 :result-types result-types)
    (when result-set
      (unwind-protect
	   (let ((result (list nil)))
	     (do ((row (make-list columns))
		  (current-cons result (cdr current-cons)))
		 ((not (database-store-next-row result-set database row))
		  (cdr result))
	       (rplacd current-cons (list (apply function row)))))
	(database-dump-result-set result-set database)))))


(defun map-query-to-simple (output-type-spec function query-expression database result-types)
  (multiple-value-bind (result-set columns rows)
      (database-query-result-set query-expression database :full-set t
				 :result-types result-types)
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

;;; Large objects support

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

