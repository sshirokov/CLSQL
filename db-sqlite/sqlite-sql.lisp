;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:     sqlite-sql.lisp
;;;; Purpose:  High-level SQLite interface
;;;; Authors:  Aurelio Bignoli and Marcus Pearce
;;;; Created:  Aug 2003
;;;;
;;;; $Id: sqlite-sql.lisp,v 1.5 2004/03/09 20:57:44 aurelio Exp $
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

(defmethod database-query (query-expression (database sqlite-database) types)
  (declare (ignore types))		; SQLite is typeless!
  (handler-case
      (multiple-value-bind (data row-n col-n)
	  (sqlite:sqlite-get-table (sqlite-db database) query-expression)
	#-clisp (declare (type sqlite:sqlite-row-pointer data))
	(if (= row-n 0)
	    nil
	    (prog1
		;; The first col-n elements are column names.
		(loop for i from col-n below (* (1+ row-n) col-n) by col-n
		      collect (loop for j from 0 below col-n
				    collect
				    (#+clisp aref
				     #-clisp sqlite:sqlite-aref
				             data (+ i j))))
	        #-clisp (sqlite:sqlite-free-table data))
	      ))
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
	     :type sqlite:sqlite-row-pointer)
  (n-col 0 :type fixnum))
#+clisp
(defstruct sqlite-result-set
  (vm nil)
  (first-row nil)
  (n-col 0 :type fixnum))

(defmethod database-query-result-set
    (query-expression (database sqlite-database) &key full-set types)
  (declare (ignore full-set types))
  (handler-case
      (let* ((vm (sqlite:sqlite-compile (sqlite-db database)
					query-expression))
	     (result-set (make-sqlite-result-set :vm vm)))
	#-clisp (declare (type sqlite:sqlite-vm-pointer vm))

	;;; To obtain column number we have to read the first row.
	(multiple-value-bind (n-col cols col-names)
	    (sqlite:sqlite-step vm)
	  (declare (ignore col-names)
		   #-clisp (type sqlite:sqlite-row-pointer cols)
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
  (declare (ignore database))
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
			     #-clisp (type sqlite:sqlite-row-pointer new-row)
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
                 (and (>= (length s) 10)
                      (string= (subseq s 0 10) "_usql_seq_")))
             (mapcar #'car (database-query
                            "SELECT name FROM sqlite_master WHERE type='table' UNION ALL SELECT name FROM sqlite_temp_master WHERE type='table' ORDER BY name"
                            database '()))))

(defmethod database-list-views ((database sqlite-database)
                                &key (owner nil))
  (declare (ignore owner))
  (mapcar #'car (database-query
                 "SELECT name FROM sqlite_master WHERE type='view' UNION ALL SELECT name FROM sqlite_temp_master WHERE type='view' ORDER BY name"
                 database nil)))

(defmethod database-list-indexes ((database sqlite-database)
                                  &key (owner nil))
  (declare (ignore owner))
  (mapcar #'car (database-query
                 "SELECT name FROM sqlite_master WHERE type='index' UNION ALL SELECT name FROM sqlite_temp_master WHERE type='index' ORDER BY name"
                 database nil)))

(declaim (inline sqlite-table-info))
(defun sqlite-table-info (table database)
  (database-query (format nil "PRAGMA table_info('~A')" table)
			  database '()))

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
	return (third field-info)))

(defun %sequence-name-to-table-name (sequence-name)
  (concatenate 'string "_usql_seq_" (sql-escape sequence-name)))

(defun %table-name-to-sequence-name (table-name)
  (and (>= (length table-name) 10)
       (string= (subseq table-name 0 10) "_usql_seq_")
       (subseq table-name 10)))

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

(defmethod database-list-sequences ((database sqlite-database)
                                    &key (owner nil))
  (declare (ignore owner))
  (mapcan #'(lambda (s)
              (let ((sn (%table-name-to-sequence-name (car s))))
                (and sn (list sn))))
          (database-query
           "SELECT name FROM sqlite_master WHERE type='table' UNION ALL SELECT name FROM sqlite_temp_master WHERE type='table' ORDER BY name"
           database '())))

(defmethod database-sequence-next (sequence-name (database sqlite-database))
  (let ((table-name (%sequence-name-to-table-name sequence-name)))
    (database-execute-command
     (format nil "UPDATE ~A SET id=(SELECT id FROM ~A)+1"
	     table-name table-name)
     database)
    (sqlite:sqlite-last-insert-rowid (sqlite-db database))
    (parse-integer
     (caar (database-query (format nil "SELECT id from ~A" table-name)
                           database nil)))))

(defmethod database-set-sequence-position (sequence-name
                                           (position integer)
                                           (database sqlite-database))
  (let ((table-name (%sequence-name-to-table-name sequence-name)))
    (database-execute-command
     (format nil "UPDATE ~A SET id=~A" table-name position)
     database)
    (sqlite:sqlite-last-insert-rowid (sqlite-db database))))

(defmethod database-sequence-last (sequence-name (database sqlite-database))
  (declare (ignore sequence-name database)))
