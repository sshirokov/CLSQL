;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          sql.cl
;;;; Purpose:       High-level SQL interface
;;;; Programmers:   Kevin M. Rosenberg based on
;;;;                 Original code by Pierre R. Mai 
;;;; Date Started:  Feb 2002
;;;;
;;;; $Id: sql.cl,v 1.6 2002/03/25 23:48:46 kevin Exp $
;;;;
;;;; This file, part of CLSQL, is Copyright (c) 2002 by Kevin M. Rosenberg
;;;; and Copyright (c) 1999-2001 by Pierre R. Mai
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(declaim (optimize (debug 3) (speed 3) (safety 1) (compilation-speed 0)))
(in-package :clsql-sys)

;;;; Modified to use CMUCL-COMPAT library and to fix format strings in
;;;; error messages

;;;; Simple implementation of SQL along the lines of Harlequin's Common SQL

;;; Conditions
(define-condition clsql-condition ()
  ())

(define-condition clsql-error (error clsql-condition)
  ())

(define-condition clsql-simple-error (simple-condition clsql-error)
  ())

(define-condition clsql-warning (warning clsql-condition)
  ())

(define-condition clsql-simple-warning (simple-condition clsql-warning)
  ())

(define-condition clsql-invalid-spec-error (clsql-error)
  ((connection-spec :initarg :connection-spec
		    :reader clsql-invalid-spec-error-connection-spec)
   (database-type :initarg :database-type
		  :reader clsql-invalid-spec-error-database-type)
   (template :initarg :template
	     :reader clsql-invalid-spec-error-template))
  (:report (lambda (c stream)
	     (format stream "The connection specification ~A~%is invalid for database type ~A.~%The connection specification must conform to ~A"
		     (clsql-invalid-spec-error-connection-spec c)
		     (clsql-invalid-spec-error-database-type c)
		     (clsql-invalid-spec-error-template c)))))

(defmacro check-connection-spec (connection-spec database-type template)
  "Check the connection specification against the provided template,
and signal an clsql-invalid-spec-error if they don't match."
  `(handler-case
    (destructuring-bind ,template ,connection-spec 
      (declare (ignore ,@template))
      t)
    (error () (error 'clsql-invalid-spec-error
		     :connection-spec ,connection-spec
		     :database-type ,database-type
		     :template (quote ,template)))))

(define-condition clsql-connect-error (clsql-error)
  ((database-type :initarg :database-type
		  :reader clsql-connect-error-database-type)
   (connection-spec :initarg :connection-spec
		    :reader clsql-connect-error-connection-spec)
   (errno :initarg :errno :reader clsql-connect-error-errno)
   (error :initarg :error :reader clsql-connect-error-error))
  (:report (lambda (c stream)
	     (format stream "While trying to connect to database ~A~%  using database-type ~A:~%  Error ~D / ~A~%  has occurred."
		     (database-name-from-spec
		      (clsql-connect-error-connection-spec c)
		      (clsql-connect-error-database-type c))
		     (clsql-connect-error-database-type c)
		     (clsql-connect-error-errno c)
		     (clsql-connect-error-error c)))))

(define-condition clsql-sql-error (clsql-error)
  ((database :initarg :database :reader clsql-sql-error-database)
   (expression :initarg :expression :reader clsql-sql-error-expression)
   (errno :initarg :errno :reader clsql-sql-error-errno)
   (error :initarg :error :reader clsql-sql-error-error))
  (:report (lambda (c stream)
	     (format stream "While accessing database ~A~%  with expression ~S:~%  Error ~D / ~A~%  has occurred."
		     (clsql-sql-error-database c)
		     (clsql-sql-error-expression c)
		     (clsql-sql-error-errno c)
		     (clsql-sql-error-error c)))))

(define-condition clsql-database-warning (clsql-warning)
  ((database :initarg :database :reader clsql-database-warning-database)
   (message :initarg :message :reader clsql-database-warning-message))
  (:report (lambda (c stream)
	     (format stream "While accessing database ~A~%  Warning: ~A~%  has occurred."
		     (clsql-database-warning-database c)
		     (clsql-database-warning-message c)))))

(define-condition clsql-exists-condition (clsql-condition)
   ((old-db :initarg :old-db :reader clsql-exists-condition-old-db)
    (new-db :initarg :new-db :reader clsql-exists-condition-new-db
	    :initform nil))
   (:report (lambda (c stream)
	      (format stream "In call to ~S:~%" 'connect)
	      (cond
		((null (clsql-exists-condition-new-db c))
		 (format stream
			 "  There is an existing connection ~A to database ~A."
			 (clsql-exists-condition-old-db c)
			 (database-name (clsql-exists-condition-old-db c))))
		((eq (clsql-exists-condition-new-db c)
		     (clsql-exists-condition-old-db c))
		 (format stream
			 "  Using existing connection ~A to database ~A."
			 (clsql-exists-condition-old-db c)
			 (database-name (clsql-exists-condition-old-db c))))
		(t
		 (format stream
			 "  Created new connection ~A to database ~A~%  ~
although there is an existing connection (~A)."
			 (clsql-exists-condition-new-db c)
			 (database-name (clsql-exists-condition-new-db c))
			 (clsql-exists-condition-old-db c)))))))

(define-condition clsql-exists-warning (clsql-exists-condition
					 clsql-warning)
  ())

(define-condition clsql-exists-error (clsql-exists-condition
				       clsql-error)
  ())

(define-condition clsql-closed-error (clsql-error)
  ((database :initarg :database :reader clsql-closed-error-database))
  (:report (lambda (c stream)
	     (format stream "The database ~A has already been closed."
		     (clsql-closed-error-database c)))))

;;; Database Types

(defvar *loaded-database-types* nil
  "Contains a list of database types which have been defined/loaded.")

(defun reload-database-types ()
  "Reloads any foreign code for the loaded database types after a dump."
  (mapc #'database-type-load-foreign *loaded-database-types*))

(defgeneric database-type-load-foreign (database-type)
  (:documentation
   "The internal generic implementation of reload-database-types.")
  (:method :after (database-type)
	   (pushnew database-type *loaded-database-types*)))

(defgeneric database-type-library-loaded (database-type)
  (:documentation
   "The internal generic implementation for checking if
database type library loaded successfully."))

(defvar *default-database-type* nil
  "Specifies the default type of database.  Currently only :mysql is
supported.")

(defvar *initialized-database-types* nil
  "Contains a list of database types which have been initialized by calls
to initialize-database-type.")

(defun initialize-database-type (&key (database-type *default-database-type*))
  "Initialize the given database-type, if it is not already
initialized, as indicated by `*initialized-database-types*'."
  (if (member database-type *initialized-database-types*)
      t
      (when (database-initialize-database-type database-type)
	(push database-type *initialized-database-types*)
	t)))

(defgeneric database-initialize-database-type (database-type)
  (:documentation
   "The internal generic implementation of initialize-database-type."))

;;; Database handling

(defvar *connect-if-exists* :error
  "Default value for the if-exists parameter of connect calls.")

(defvar *connected-databases* nil
  "List of active database objects.")

(defun connected-databases ()
  "Return the list of active database objects."
  *connected-databases*)

(defvar *default-database* nil
  "Specifies the default database to be used.")

(defclass database ()
  ((name :initarg :name :reader database-name))
  (:documentation
   "This class is the supertype of all databases handled by CLSQL."))

(defmethod print-object ((object database) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (write-string (if (slot-boundp object 'name)
		      (database-name object)
		      "<unbound>")
		  stream)))

(defclass closed-database ()
  ((name :initarg :name :reader database-name))
  (:documentation
   "This class represents all databases after they are closed via
`disconnect'."))

(defmethod print-object ((object closed-database) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (write-string (if (slot-boundp object 'name)
		      (database-name object)
		      "<unbound>")
		  stream)))

(defun signal-closed-database-error (database)
  (cerror "Ignore this error and return nil."
	  'clsql-closed-error
	  :database database))

(defun find-database (database &optional (errorp t))
  (etypecase database
    (database
     ;; Return the database object itself
     database)
    (string
     (or (find database (connected-databases)
	       :key #'database-name
	       :test #'string=)
	 (when errorp
	   (cerror "Return nil."
		   'clsql-simple-error
		   :format-control "There exists no database called ~A."
		   :format-arguments (list database)))))))

(defun connect (connection-spec
		&key (if-exists *connect-if-exists*)
		(database-type *default-database-type*))
  "Connects to a database of the given database-type, using the type-specific
connection-spec.  if-exists is currently ignored."
  (let* ((db-name (database-name-from-spec connection-spec database-type))
	 (old-db (find-database db-name nil))
	 (result nil))
    (if old-db
	(case if-exists
	  (:new
	   (setq result
		 (database-connect connection-spec database-type)))
	  (:warn-new
	   (setq result
		 (database-connect connection-spec database-type))
	   (warn 'clsql-exists-warning :old-db old-db :new-db result))
	  (:error
	   (restart-case
	       (error 'clsql-exists-error :old-db old-db)
	     (create-new ()
	       :report "Create a new connection."
	       (setq result
		     (database-connect connection-spec database-type)))
	     (use-old ()
	       :report "Use the existing connection."
	       (setq result old-db))))
	  (:warn-old
	   (setq result old-db)
	   (warn 'clsql-exists-warning :old-db old-db :new-db old-db))
	  (:old
	   (setq result old-db)))
	(setq result
	      (database-connect connection-spec database-type)))
    (when result
      (pushnew result *connected-databases*)
      (setq *default-database* result)
      result)))

(defgeneric database-name-from-spec (connection-spec database-type)
  (:documentation
   "Returns the name of the database that would be created if connect
was called with the connection-spec."))

(defgeneric database-connect (connection-spec database-type)
  (:documentation "Internal generic implementation of connect."))

(defun disconnect (&key (database *default-database*))
  "Closes the connection to database. Resets *default-database* if that
database was disconnected and only one other connection exists."
  (when (database-disconnect database)
    (setq *connected-databases* (delete database *connected-databases*))
    (when (eq database *default-database*)
      (setq *default-database* (car *connected-databases*)))
    (change-class database 'closed-database)
    t))

(defgeneric database-disconnect (database)
  (:method ((database closed-database))
	   (signal-closed-database-error database))
  (:documentation "Internal generic implementation of disconnect."))

;;; Basic operations on databases

(defmethod query (query-expression &key (database *default-database*)  
		  types)
  "Execute the SQL query expression query-expression on the given database.
Returns a list of lists of values of the result of that expression."
  (database-query query-expression database types))

(defgeneric database-query (query-expression database types)
  (:method (query-expression (database closed-database) types)
	   (declare (ignore query-expression types))
	   (signal-closed-database-error database))
  (:documentation "Internal generic implementation of query."))

(defmethod execute-command (sql-expression &key (database *default-database*))
  "Execute the SQL command expression sql-expression on the given database.
Returns true on success or nil on failure."
  (database-execute-command sql-expression database))

(defgeneric database-execute-command (sql-expression database)
  (:method (sql-expression (database closed-database))
	   (declare (ignore sql-expression))
	   (signal-closed-database-error database))
  (:documentation "Internal generic implementation of execute-command."))

;;; Mapping and iteration
(defgeneric database-query-result-set
    (query-expression database &key full-set types)
  (:method (query-expression (database closed-database) &key full-set types)
	   (declare (ignore query-expression full-set types))
	   (signal-closed-database-error database)
	   (values nil nil nil))
  (:documentation
   "Internal generic implementation of query mapping.  Starts the
query specified by query-expression on the given database and returns
a result-set to be used with database-store-next-row and
database-dump-result-set to access the returned data.  The second
value is the number of columns in the result-set, if there are any.
If full-set is true, the number of rows in the result-set is returned
as a third value, if this is possible (otherwise nil is returned for
the third value).  This might have memory and resource usage
implications, since many databases will require the query to be
executed in full to answer this question.  If the query produced no
results then nil is returned for all values that would have been
returned otherwise.  If an error occurs during query execution, the
function should signal a clsql-sql-error."))

(defgeneric database-dump-result-set (result-set database)
  (:method (result-set (database closed-database))
	   (declare (ignore result-set))
	   (signal-closed-database-error database))
  (:documentation "Dumps the received result-set."))

(defgeneric database-store-next-row (result-set database list)
  (:method (result-set (database closed-database) list)
	   (declare (ignore result-set list))
	   (signal-closed-database-error database))
  (:documentation
   "Returns t and stores the next row in the result set in list or
returns nil when result-set is finished."))



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



