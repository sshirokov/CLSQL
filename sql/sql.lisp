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
;;;; $Id: sql.lisp,v 1.2 2002/10/14 15:25:15 kevin Exp $
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

;;; Modified by KMR 
;;; - to use CMUCL-COMPAT library 
;;; - fix format strings in error messages 
;;; - use field types


;;; Simple implementation of SQL along the lines of Harlequin's Common SQL


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
		(database-type *default-database-type*)
		(pool nil))
  "Connects to a database of the given database-type, using the type-specific
connection-spec.  if-exists is currently ignored.
If pool is t the the connection will be taken from the general pool,
if pool is a conn-pool object the connection will be taken from this pool.
"
  (if pool
    (acquire-from-pool connection-spec database-type pool)
    (let* ((db-name (database-name-from-spec connection-spec database-type))
	   (old-db (unless (eq if-exists :new) (find-database db-name nil)))
	   (result nil))
      (if old-db
	(case if-exists
;	    (:new
;	     (setq result
;	       (database-connect connection-spec database-type)))
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
	result))))


(defun disconnect (&key (database *default-database*))
  "Closes the connection to database. Resets *default-database* if that
database was disconnected and only one other connection exists.
if the database is from a pool it will be released to this pool."
  (if (conn-pool database)
      (release-to-pool database)
    (when (database-disconnect database)
      (setq *connected-databases* (delete database *connected-databases*))
      (when (eq database *default-database*)
	(setq *default-database* (car *connected-databases*)))
      (change-class database 'closed-database)
      t)))

;;; Basic operations on databases

(defgeneric query (expression &key database types))
(defmethod query (query-expression &key (database *default-database*)  
		  types)
  "Execute the SQL query expression query-expression on the given database.
Returns a list of lists of values of the result of that expression."
  (database-query query-expression database types))


(defgeneric execute-command (expression &key database))
(defmethod execute-command (sql-expression &key (database *default-database*))
  "Execute the SQL command expression sql-expression on the given database.
Returns true on success or nil on failure."
  (database-execute-command sql-expression database))



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
