;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:     sqlite-sql.lisp
;;;; Purpose:  High-level SQLite interface
;;;; Authors:  Aurelio Bignoli and Marcus Pearce
;;;; Created:  Aug 2003
;;;;
;;;; $Id$
;;;;
;;;; This file, part of CLSQL, is Copyright (c) 2003 by Aurelio Bignoli and
;;;; Marcus Pearce
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(in-package #:clsql-sqlite)

(defclass sqlite-database (database)
  ((sqlite-db :initarg :sqlite-db :accessor sqlite-db)))

(defmethod database-type ((database sqlite-database))
  :sqlite)

(defmethod database-initialize-database-type ((database-type (eql :sqlite)))
  t)

(defun check-sqlite-connection-spec (connection-spec)
  (check-connection-spec connection-spec :sqlite (name)))

(defmethod database-name-from-spec (connection-spec
				    (database-type (eql :sqlite)))
  (check-sqlite-connection-spec connection-spec)
  (first connection-spec))

(defmethod database-connect (connection-spec (database-type (eql :sqlite)))
  (check-sqlite-connection-spec connection-spec)
  (handler-case
      (make-instance 'sqlite-database
		     :name (database-name-from-spec connection-spec :sqlite)
		     :database-type :sqlite
		     :connection-spec connection-spec
		     :sqlite-db (sqlite:sqlite-open (first connection-spec)))
    (sqlite:sqlite-error (err)
      (error 'clsql-connect-error
	     :database-type database-type
	     :connection-spec connection-spec
	     :errno (sqlite:sqlite-error-code err)
	     :error (sqlite:sqlite-error-message err)))))

(defmethod database-disconnect ((database sqlite-database))
  (sqlite:sqlite-close (sqlite-db database))
  (setf (sqlite-db database) nil)
  t)

(defmethod database-execute-command (sql-expression (database sqlite-database))
  (handler-case
      (multiple-value-bind (data row-n col-n)
	  (sqlite:sqlite-get-table (sqlite-db database) sql-expression)
	#+clisp (declare (ignore data))
	#-clisp (sqlite:sqlite-free-table data)
	(unless (= row-n 0)
	  (error 'clsql-simple-warning
		 :format-control
		 "Result set not empty: ~@(~A~) row~:P, ~@(~A~) column~:P "
		 :format-arguments (list row-n col-n))))
    (sqlite:sqlite-error (err)
      (error 'clsql-sql-error
	     :database database
	     :expression sql-expression
	     :errno (sqlite:sqlite-error-code err)
	     :error (sqlite:sqlite-error-message err))))
  t)

(defmethod database-query (query-expression (database sqlite-database) result-types field-names)
  (declare (ignore result-types))		; SQLite is typeless!
  (handler-case
      (multiple-value-bind (data row-n col-n)
	  (sqlite:sqlite-get-table (sqlite-db database) query-expression)
	#-clisp (declare (type sqlite:sqlite-row-pointer-type data))
	(let ((rows
	       (when (plusp row-n)
		 (loop for i from col-n below (* (1+ row-n) col-n) by col-n
		     collect (loop for j from 0 below col-n
				 collect
				   (#+clisp aref
					    #-clisp sqlite:sqlite-aref
					    data (+ i j))))))
	      (names
	       (when field-names
		 (loop for j from 0 below col-n
		     collect (#+clisp aref
				      #-clisp sqlite:sqlite-aref
				      data j)))))
	  #-clisp (sqlite:sqlite-free-table data)
	  (values rows names)))
    (sqlite:sqlite-error (err)
                         (error 'clsql-sql-error
                                :database database
                                :expression query-expression
                                :errno (sqlite:sqlite-error-code err)
                                :error (sqlite:sqlite-error-message err)))))

#-clisp
(defstruct sqlite-result-set
  (vm (sqlite:make-null-vm)
      :type sqlite:sqlite-vm-pointer)
  (first-row (sqlite:make-null-row)
	     :type sqlite:sqlite-row-pointer-type)
  (n-col 0 :type fixnum))
#+clisp
(defstruct sqlite-result-set
  (vm nil)
  (first-row nil)
  (n-col 0 :type fixnum))

(defmethod database-query-result-set
    ((query-expression string) (database sqlite-database) &key full-set result-types)
  (declare (ignore full-set result-types))
  (handler-case
      (let* ((vm (sqlite:sqlite-compile (sqlite-db database)
					query-expression))
	     (result-set (make-sqlite-result-set :vm vm)))
	#-clisp (declare (type sqlite:sqlite-vm-pointer vm))

	;;; To obtain column number we have to read the first row.
	(multiple-value-bind (n-col cols col-names)
	    (sqlite:sqlite-step vm)
	  (declare (ignore col-names)
		   #-clisp (type sqlite:sqlite-row-pointer-type cols)
		   )
	  (setf (sqlite-result-set-first-row result-set) cols
		(sqlite-result-set-n-col result-set) n-col)
	  (values result-set n-col nil)))
    (sqlite:sqlite-error (err)
      (error 'clsql-sql-error
	     :database database
	     :expression query-expression
	     :errno (sqlite:sqlite-error-code err)
	     :error (sqlite:sqlite-error-message err)))))

(defmethod database-dump-result-set (result-set (database sqlite-database))
  (handler-case
      (sqlite:sqlite-finalize (sqlite-result-set-vm result-set))
    (sqlite:sqlite-error (err)
      (error 'clsql-simple-error
	     :format-control "Error finalizing SQLite VM: ~A"
	     :format-arguments (list (sqlite:sqlite-error-message err))))))

(defmethod database-store-next-row (result-set (database sqlite-database) list)
  (let ((n-col (sqlite-result-set-n-col result-set)))
    (if (= n-col 0)
	;; empty result set
	nil
	(let ((row (sqlite-result-set-first-row result-set)))
	  (if (sqlite:null-row-p row)
	      ;; First row already used. fetch another row from DB.
	      (handler-case
		  (multiple-value-bind (n new-row col-names)
		      (sqlite:sqlite-step (sqlite-result-set-vm result-set))
		    (declare (ignore n col-names)
			     #-clisp (type sqlite:sqlite-row-pointer-type new-row)
			     )
		    (if (sqlite:null-row-p new-row)
			(return-from database-store-next-row nil)
			(setf row new-row)))
		(sqlite:sqlite-error (err)
		  (error 'clsql-simple-error
			 :format-control "Error in sqlite-step: ~A"
			 :format-arguments
			 (list (sqlite:sqlite-error-message err)))))

	      ;; Use the row previously read by database-query-result-set.
	      (setf (sqlite-result-set-first-row result-set)
		    (sqlite:make-null-row)))
	  (loop for i = 0 then (1+ i)
		for rest on list
		do (setf (car rest)
			 (#+clisp aref
			  #-clisp sqlite:sqlite-aref
			  row i)))
	  #-clisp (sqlite:sqlite-free-row row)
	  t))))

;;; Object listing

(defmethod database-list-tables ((database sqlite-database) &key owner)
  (declare (ignore owner))
  ;; Query is copied from .table command of sqlite comamnd line utility.
  (remove-if #'(lambda (s)
                 (and (>= (length s) 11)
                      (string-equal (subseq s 0 11) "_CLSQL_SEQ_")))
             (mapcar #'car (database-query
                            "SELECT name FROM sqlite_master WHERE type='table' UNION ALL SELECT name FROM sqlite_temp_master WHERE type='table' ORDER BY name"
                            database nil nil))))

(defmethod database-list-views ((database sqlite-database)
                                &key (owner nil))
  (declare (ignore owner))
  (mapcar #'car (database-query
                 "SELECT name FROM sqlite_master WHERE type='view' UNION ALL SELECT name FROM sqlite_temp_master WHERE type='view' ORDER BY name"
                 database nil nil)))

(defmethod database-list-indexes ((database sqlite-database)
                                  &key (owner nil))
  (declare (ignore owner))
  (mapcar #'car (database-query
                 "SELECT name FROM sqlite_master WHERE type='index' UNION ALL SELECT name FROM sqlite_temp_master WHERE type='index' ORDER BY name"
                 database nil nil)))

(defmethod database-list-table-indexes (table (database sqlite-database)
					&key (owner nil))
  (declare (ignore owner))
  (let ((*print-circle* nil))
    (mapcar #'car 
	    (database-query
	     (format
	      nil
	      "SELECT name FROM sqlite_master WHERE type='index' AND tbl_name='~A' UNION ALL SELECT name FROM sqlite_temp_master WHERE type='index' AND tbl_name='~A' ORDER BY name"
	      table table)
	     database nil nil))))

(declaim (inline sqlite-table-info))
(defun sqlite-table-info (table database)
  (database-query (format nil "PRAGMA table_info('~A')" table)
		  database nil nil))

(defmethod database-list-attributes (table (database sqlite-database)
                                           &key (owner nil))
  (declare (ignore owner))
  (mapcar #'(lambda (table-info) (second table-info))
	  (sqlite-table-info table database)))

(defmethod database-attribute-type (attribute table 
				    (database sqlite-database)
                                    &key (owner nil))
  (declare (ignore owner))
  (loop for field-info in (sqlite-table-info table database)
      when (string= attribute (second field-info))
      return 
	(let* ((raw-type (third field-info))
	       (start-length (position #\( raw-type))
	       (type (if start-length
			 (subseq raw-type 0 start-length)
		       raw-type))
	       (length (if start-length
			   (parse-integer (subseq raw-type (1+ start-length))
					  :junk-allowed t)
			 nil)))
	  (values (when type (ensure-keyword type)) 
		  length
		  nil
		  (if (string-equal (fourth field-info) "0")
		      1 0)))))

(defun %sequence-name-to-table-name (sequence-name)
  (concatenate 'string "_CLSQL_SEQ_" (sql-escape sequence-name)))

(defun %table-name-to-sequence-name (table-name)
  (and (>= (length table-name) 11)
       (string= (subseq table-name 0 11) "_CLSQL_SEQ_")
       (subseq table-name 11)))


(defmethod database-create-sequence (sequence-name
				     (database sqlite-database))
  (let ((table-name (%sequence-name-to-table-name sequence-name)))
    (database-execute-command
     (concatenate 'string "CREATE TABLE " table-name
		  " (last_value integer PRIMARY KEY, increment_by integer, min_value integer, is_called char(1))")
     database)
    (database-execute-command 
     (concatenate 'string "INSERT INTO " table-name
		  " VALUES (1,1,1,'f')")
     database)))

(defmethod database-drop-sequence (sequence-name
				   (database sqlite-database))
  (database-execute-command
   (concatenate 'string "DROP TABLE " (%sequence-name-to-table-name sequence-name)) 
   database))

(defmethod database-list-sequences ((database sqlite-database)
                                    &key (owner nil))
  (declare (ignore owner))
  (mapcan #'(lambda (s)
              (let ((sn (%table-name-to-sequence-name (car s))))
                (and sn (list sn))))
          (database-query
           "SELECT name FROM sqlite_master WHERE type='table' UNION ALL SELECT name FROM sqlite_temp_master WHERE type='table' ORDER BY name"
           database nil nil)))

(defmethod database-sequence-next (sequence-name (database sqlite-database))
  (without-interrupts
   (let* ((table-name (%sequence-name-to-table-name sequence-name))
	  (tuple
	   (car (database-query 
		 (concatenate 'string "SELECT last_value,is_called FROM " 
			      table-name)
		 database :auto nil))))
     (cond
       ((char-equal (schar (second tuple) 0) #\f)
	(database-execute-command
	 (format nil "UPDATE ~A SET is_called='t'" table-name)
	 database)
	(parse-integer (car tuple)))
       (t
	(let ((new-pos (1+ (parse-integer (car tuple)))))
	 (database-execute-command
	  (format nil "UPDATE ~A SET last_value=~D" table-name new-pos)
	  database)
	 new-pos))))))
	     
(defmethod database-sequence-last (sequence-name (database sqlite-database))
  (without-interrupts
    (parse-integer 
     (caar (database-query 
	    (concatenate 'string "SELECT last_value FROM " 
			 (%sequence-name-to-table-name sequence-name))
	    database :auto nil)))))

(defmethod database-set-sequence-position (sequence-name
                                           (position integer)
                                           (database sqlite-database))
  (database-execute-command
   (format nil "UPDATE ~A SET last_value=~A,is_called='t'" 
	   (%sequence-name-to-table-name sequence-name)
           position)
   database)
  position)

(defmethod database-create (connection-spec (type (eql :sqlite)))
  (declare (ignore connection-spec))
  ;; databases are created automatically by SQLite
  t)

(defmethod database-destroy (connection-spec (type (eql :sqlite)))
  (destructuring-bind (name) connection-spec
    (if (probe-file name)
	(delete-file name)
	nil)))

(defmethod database-probe (connection-spec (type (eql :sqlite)))
  (destructuring-bind (name) connection-spec
    ;; TODO: Add a test that this file is a real sqlite database
    (or (string-equal ":memory:" name)
	(and (probe-file name) t))))

;;; Database capabilities

(defmethod db-type-has-boolean-where? ((db-type (eql :sqlite)))
  nil)



