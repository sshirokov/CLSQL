;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;;
;;;; $Id$
;;;;
;;;; The CLSQL Functional Data Manipulation Language (FDML). 
;;;;
;;;; This file is part of CLSQL.
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(in-package #:clsql)
  
;;; Basic operations on databases

(defmethod database-query-result-set ((expr %sql-expression) database
                                      &key full-set result-types)
  (database-query-result-set (sql-output expr database) database
                             :full-set full-set :result-types result-types))

(defmethod execute-command ((expr %sql-expression)
                            &key (database *default-database*))
  (execute-command (sql-output expr database) :database database)
  (values))


(defmethod query ((expr %sql-expression) &key (database *default-database*)
                  (result-types :auto) (flatp nil) (field-names t))
  (query (sql-output expr database) :database database :flatp flatp
         :result-types result-types :field-names field-names))

(defmethod query ((expr sql-object-query) &key (database *default-database*)
					       (result-types :auto) (flatp nil))
  (declare (ignore result-types))
  (apply #'select (append (slot-value expr 'objects)
			  (slot-value expr 'exp) 
			  (when (slot-value expr 'refresh) 
			    (list :refresh (sql-output expr database)))
			  (when (or flatp (slot-value expr 'flatp) )
			    (list :flatp t))
			  (list :database database))))

(defun truncate-database (&key (database *default-database*))
  (unless (typep database 'database)
    (clsql-base::signal-no-database-error database))
  (unless (is-database-open database)
    (database-reconnect database))
  (when (db-type-has-views? (database-underlying-type database))
    (dolist (view (list-views :database database))
      (drop-view view :database database)))
  (dolist (table (list-tables :database database))
    (drop-table table :database database))
  (dolist (index (list-indexes :database database))
    (drop-index index :database database))
  (dolist (seq (list-sequences :database database))
    (drop-sequence seq :database database)))

(defun print-query (query-exp &key titles (formats t) (sizes t) (stream t)
			      (database *default-database*))
  "The PRINT-QUERY function takes a symbolic SQL query expression and
formatting information and prints onto STREAM a table containing the
results of the query. A list of strings to use as column headings is
given by TITLES, which has a default value of NIL. The FORMATS
argument is a list of format strings used to print each attribute, and
has a default value of T, which means that ~A or ~VA are used if sizes
are provided or computed. The field sizes are given by SIZES. It has a
default value of T, which specifies that minimum sizes are
computed. The output stream is given by STREAM, which has a default
value of T. This specifies that *STANDARD-OUTPUT* is used."
  (flet ((compute-sizes (data)
           (mapcar #'(lambda (x) (apply #'max (mapcar #'length x)))
                   (apply #'mapcar (cons #'list data))))
         (format-record (record control sizes)
           (format stream "~&~?" control
                   (if (null sizes) record
                       (mapcan #'(lambda (s f) (list s f)) sizes record)))))
    (let* ((query-exp (etypecase query-exp
                        (string query-exp)
                        (sql-query (sql-output query-exp database))))
           (data (query query-exp :database database))
           (sizes (if (or (null sizes) (listp sizes)) sizes 
                      (compute-sizes (if titles (cons titles data) data))))
           (formats (if (or (null formats) (not (listp formats)))
                        (make-list (length (car data)) :initial-element
                                   (if (null sizes) "~A " "~VA "))
                        formats))
           (control-string (format nil "~{~A~}" formats)))
      (when titles (format-record titles control-string sizes))
      (dolist (d data (values)) (format-record d control-string sizes)))))

(defun insert-records (&key (into nil)
			    (attributes nil)
			    (values nil)
			    (av-pairs nil)
			    (query nil)
			    (database *default-database*))
  "Inserts a set of values into a table. The records created contain
values for attributes (or av-pairs). The argument VALUES is a list of
values. If ATTRIBUTES is supplied then VALUES must be a corresponding
list of values for each of the listed attribute names. If AV-PAIRS is
non-nil, then both ATTRIBUTES and VALUES must be nil. If QUERY is
non-nil, then neither VALUES nor AV-PAIRS should be. QUERY should be a
query expression, and the attribute names in it must also exist in the
table INTO. The default value of DATABASE is *DEFAULT-DATABASE*."
  (let ((stmt (make-sql-insert :into into :attrs attributes
			       :vals values :av-pairs av-pairs
			       :subquery query)))
    (execute-command stmt :database database)))

(defun make-sql-insert (&key (into nil)
			    (attrs nil)
			    (vals nil)
			    (av-pairs nil)
			    (subquery nil))
  (unless into
      (error 'clsql-sql-syntax-error :reason ":into keyword not supplied"))
  (let ((insert (make-instance 'sql-insert :into into)))
    (with-slots (attributes values query)
      insert
      (cond ((and vals (not attrs) (not query) (not av-pairs))
	     (setf values vals))
	    ((and vals attrs (not subquery) (not av-pairs))
	     (setf attributes attrs)
	     (setf values vals))
	    ((and av-pairs (not vals) (not attrs) (not subquery))
	     (setf attributes (mapcar #'car av-pairs))
	     (setf values (mapcar #'cadr av-pairs)))
	    ((and subquery (not vals) (not attrs) (not av-pairs))
	     (setf query subquery))
	    ((and subquery attrs (not vals) (not av-pairs))
	     (setf attributes attrs)
	     (setf query subquery))
	    (t
	     (error 'clsql-sql-syntax-error
                    :reason "bad or ambiguous keyword combination.")))
      insert)))
    
(defun delete-records (&key (from nil)
                            (where nil)
                            (database *default-database*))
  "Deletes rows from a database table specified by FROM in which the
WHERE condition is true. The argument DATABASE specifies a database
from which the records are to be removed, and defaults to
*default-database*."
  (let ((stmt (make-instance 'sql-delete :from from :where where)))
    (execute-command stmt :database database)))

(defun update-records (table &key (attributes nil)
			    (values nil)
			    (av-pairs nil)
			    (where nil)
			    (database *default-database*))
  "Changes the values of existing fields in TABLE with columns
specified by ATTRIBUTES and VALUES (or AV-PAIRS) where the WHERE
condition is true."
  (when av-pairs
    (setf attributes (mapcar #'car av-pairs)
          values (mapcar #'cadr av-pairs)))
  (let ((stmt (make-instance 'sql-update :table table
			     :attributes attributes
			     :values values
			     :where where)))
    (execute-command stmt :database database)))


;; iteration 

;; output-sql

(defmethod database-output-sql ((str string) database)
  (declare (ignore database)
           (optimize (speed 3) (safety 1) #+cmu (extensions:inhibit-warnings 3))
           (type (simple-array * (*)) str))
  (let ((len (length str)))
    (declare (type fixnum len))
    (cond ((= len 0)
           +empty-string+)
          ((and (null (position #\' str))
                (null (position #\\ str)))
           (concatenate 'string "'" str "'"))
          (t
           (let ((buf (make-string (+ (* len 2) 2) :initial-element #\')))
             (do* ((i 0 (incf i))
                   (j 1 (incf j)))
                  ((= i len) (subseq buf 0 (1+ j)))
               (declare (type integer i j))
               (let ((char (aref str i)))
                 (cond ((eql char #\')
                        (setf (aref buf j) #\\)
                        (incf j)
                        (setf (aref buf j) #\'))
                       ((eql char #\\)
                        (setf (aref buf j) #\\)
                        (incf j)
                        (setf (aref buf j) #\\))
                       (t
                        (setf (aref buf j) char))))))))))

(let ((keyword-package (symbol-package :foo)))
  (defmethod database-output-sql ((sym symbol) database)
    (convert-to-db-default-case
     (if (equal (symbol-package sym) keyword-package)
	 (concatenate 'string "'" (string sym) "'")
	 (symbol-name sym))
     database)))

(defmethod database-output-sql ((tee (eql t)) database)
  (declare (ignore database))
  "'Y'")

(defmethod database-output-sql ((num number) database)
  (declare (ignore database))
  (princ-to-string num))

(defmethod database-output-sql ((arg list) database)
  (if (null arg)
      "NULL"
      (format nil "(~{~A~^,~})" (mapcar #'(lambda (val)
                                            (sql-output val database))
                                        arg))))

(defmethod database-output-sql ((arg vector) database)
  (format nil "~{~A~^,~}" (map 'list #'(lambda (val)
					 (sql-output val database))
			       arg)))

(defmethod database-output-sql ((self wall-time) database)
  (declare (ignore database))
  (db-timestring self))

(defmethod database-output-sql ((self duration) database)
  (declare (ignore database))
  (format nil "'~a'" (duration-timestring self)))

(defmethod database-output-sql (thing database)
  (if (or (null thing)
	  (eq 'null thing))
      "NULL"
    (error 'clsql-simple-error
           :format-control
           "No type conversion to SQL for ~A is defined for DB ~A."
           :format-arguments (list (type-of thing) (type-of database)))))


(defmethod output-sql-hash-key ((arg vector) database)
  (list 'vector (map 'list (lambda (arg)
                             (or (output-sql-hash-key arg database)
                                 (return-from output-sql-hash-key nil)))
                     arg)))

(defmethod output-sql (expr database)
  (write-string (database-output-sql expr database) *sql-stream*)
  (values))

(defmethod output-sql ((expr list) database)
  (if (null expr)
      (write-string +null-string+ *sql-stream*)
      (progn
        (write-char #\( *sql-stream*)
        (do ((item expr (cdr item)))
            ((null (cdr item))
             (output-sql (car item) database))
          (output-sql (car item) database)
          (write-char #\, *sql-stream*))
        (write-char #\) *sql-stream*)))
  t)

(defmethod describe-table ((table sql-create-table)
			   &key (database *default-database*))
  (database-describe-table
   database
   (convert-to-db-default-case 
    (symbol-name (slot-value table 'name)) database)))

#+nil
(defmethod add-storage-class ((self database) (class symbol) &key (sequence t))
  (let ((tablename (view-table (find-class class))))
    (unless (tablep tablename)
      (create-view-from-class class)
      (when sequence
        (create-sequence-from-class class)))))
 
;;; Iteration


(defmacro do-query (((&rest args) query-expression
		     &key (database '*default-database*) (result-types :auto))
		    &body body)
  "Repeatedly executes BODY within a binding of ARGS on the
attributes of each record resulting from QUERY-EXPRESSION. The
return value is determined by the result of executing BODY. The
default value of DATABASE is *DEFAULT-DATABASE*."
  (let ((result-set (gensym "RESULT-SET-"))
	(qe (gensym "QUERY-EXPRESSION-"))
	(columns (gensym "COLUMNS-"))
	(row (gensym "ROW-"))
	(db (gensym "DB-")))
    `(let ((,qe ,query-expression))
      (typecase ,qe
	(sql-object-query
         (dolist (,row (query ,qe))
           (destructuring-bind ,args 
               ,row
             ,@body)))
	(t
	 ;; Functional query 
	 (let ((,db ,database))
	   (multiple-value-bind (,result-set ,columns)
	       (database-query-result-set ,qe ,db
					  :full-set nil 
					  :result-types ,result-types)
	     (when ,result-set
	       (unwind-protect
		    (do ((,row (make-list ,columns)))
			((not (database-store-next-row ,result-set ,db ,row))
			 nil)
		      (destructuring-bind ,args ,row
			,@body))
		 (database-dump-result-set ,result-set ,db))))))))))

(defun map-query (output-type-spec function query-expression
		  &key (database *default-database*)
		  (result-types :auto))
  "Map the function over all tuples that are returned by the
query in QUERY-EXPRESSION. The results of the function are
collected as specified in OUTPUT-TYPE-SPEC and returned like in
MAP."
  (typecase query-expression
    (sql-object-query
     (map output-type-spec #'(lambda (x) (apply function x))
	  (query query-expression)))
    (t
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
		   :result-types result-types)))))))
  
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
