;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;;
;;;; $Id$
;;;;
;;;; Transaction support
;;;;
;;;; This file is part of CLSQL.
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(in-package #:clsql-sys)

(defclass transaction ()
  ((commit-hooks :initform () :accessor commit-hooks)
   (rollback-hooks :initform () :accessor rollback-hooks)
   (previous-autocommit :initarg :previous-autocommit
			:reader previous-autocommit)
   (status :initform nil :accessor transaction-status
	   :documentation "nil or :committed")))

(defun add-transaction-commit-hook (database commit-hook)
  (when (transaction database)
    (push commit-hook (commit-hooks (transaction database)))))

(defun add-transaction-rollback-hook (database rollback-hook)
  (when (transaction database)
    (push rollback-hook (rollback-hooks (transaction database)))))

(defmethod database-start-transaction ((database database))
  (unless (transaction database)
    (setf (transaction database) 
	  (make-instance 'transaction :previous-autocommit
			 (database-autocommit database))))
  (setf (database-autocommit database) nil)
  (when (= (incf (transaction-level database) 1))
    (let ((transaction (transaction database)))
      (setf (commit-hooks transaction) nil
            (rollback-hooks transaction) nil
            (transaction-status transaction) nil)
      (unless (eq :oracle (database-underlying-type database))
	(execute-command "BEGIN" :database database)))))

(defmethod database-commit-transaction ((database database))
  (with-slots (transaction transaction-level autocommit) database
    (if (plusp transaction-level)
        (when (zerop (decf transaction-level))
	  (execute-command "COMMIT" :database database)
	  (setf autocommit (previous-autocommit transaction))
          (map nil #'funcall (commit-hooks transaction)))
        (warn 'sql-warning
              :format-control
	      "Cannot commit transaction against ~A because there is no transaction in progress."
              :format-arguments (list database)))))

(defmethod database-abort-transaction ((database database))
  (with-slots (transaction transaction-level autocommit) database
    (if (plusp transaction-level)
        (when (zerop (decf transaction-level))
          (unwind-protect 
               (execute-command "ROLLBACK" :database database)
	    (setf autocommit (previous-autocommit transaction))
            (map nil #'funcall (rollback-hooks transaction))))
        (warn 'sql-warning
              :format-control
	      "Cannot abort transaction against ~A because there is no transaction in progress."
              :format-arguments (list database)))))

(defun mark-transaction-committed (database)
  (when (and (transaction database)
             (not (transaction-status (transaction database))))
    (setf (transaction-status (transaction database)) :committed)))

(defmacro with-transaction ((&key (database '*default-database*)) &rest body)
  "Starts a transaction in the database specified by DATABASE,
which is *DEFAULT-DATABASE* by default, and executes BODY within
that transaction. If BODY aborts or throws, DATABASE is rolled
back and otherwise the transaction is committed."
  (let ((db (gensym "db-")))
    `(let ((,db ,database))
      (unwind-protect
           (progn
             (database-start-transaction ,db)
             ,@body
             (mark-transaction-committed ,db))
        (if (eq (transaction-status (transaction ,db)) :committed)
            (database-commit-transaction ,db)
            (database-abort-transaction ,db))))))

(defun commit (&key (database *default-database*))
  "If DATABASE, which defaults to *DEFAULT-DATABASE*, is
currently within the scope of a transaction, commits changes made
since the transaction began."
  (database-commit-transaction database))

(defun rollback (&key (database *default-database*))
  "If DATABASE, which defaults to *DEFAULT-DATABASE*, is
currently within the scope of a transaction, rolls back changes
made since the transaction began."
  (database-abort-transaction database))

(defun start-transaction (&key (database *default-database*))
  "Starts a transaction block on DATABASE which defaults to
*DEFAULT-DATABASE* and which continues until ROLLBACK or COMMIT
are called."
  (unless (in-transaction-p :database database)
    (database-start-transaction database)))

(defun in-transaction-p (&key (database *default-database*))
  "A predicate to test whether DATABASE, which defaults to
*DEFAULT-DATABASE*, is currently within the scope of a
transaction."
  (and database (transaction database) (= (transaction-level database) 1)))

(defun autocommit (&key (database *default-database*) (set :unspecified))
  "Returns whether autocommit is currently active."
  (unless (eq set :unspecified)
    (setf (database-autocommit database) set))
  (database-autocommit database))
