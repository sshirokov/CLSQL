;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;;  $Id: $

(in-package #:clsql-base-sys)

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
                  (result-types nil) (flatp nil))
  (record-sql-command query-expression database)
  (let* ((res (database-query query-expression database result-types))
         (res (if (and flatp (= (length
                                 (slot-value query-expression 'selections))
                                1))
                  (mapcar #'car res)
                  res)))
    (record-sql-result res database)
    res))

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
  (record-sql-command sql-expression database)
  (let ((res (database-execute-command sql-expression database)))
    (record-sql-result res database))
  (values))


(defmacro do-query (((&rest args) query-expression
		     &key (database '*default-database*) (types nil))
		    &body body)
  "Repeatedly executes BODY within a binding of ARGS on the attributes
of each record resulting from QUERY. The return value is determined by
the result of executing BODY. The default value of DATABASE is
*DEFAULT-DATABASE*."
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

(defun map-query (output-type-spec function query-expression
		  &key (database *default-database*)
		  (types nil))
  "Map the function over all tuples that are returned by the query in
query-expression.  The results of the function are collected as
specified in output-type-spec and returned like in MAP."
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



