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

(in-package #:clsql-base-sys)

(defclass transaction ()
  ((commit-hooks :initform () :accessor commit-hooks)
   (rollback-hooks :initform () :accessor rollback-hooks)
   (status :initform nil :accessor transaction-status))) ; nil or :committed

(defun commit-transaction (database)
  (when (and (transaction database)
             (not (transaction-status (transaction database))))
    (setf (transaction-status (transaction database)) :committed)))

(defun add-transaction-commit-hook (database commit-hook)
  (when (transaction database)
    (push commit-hook (commit-hooks (transaction database)))))

(defun add-transaction-rollback-hook (database rollback-hook)
  (when (transaction database)
    (push rollback-hook (rollback-hooks (transaction database)))))

(defmethod database-start-transaction ((database closed-database))
  (signal-closed-database-error database))

(defmethod database-start-transaction (database)
  (unless database (error 'clsql-nodb-error))
  (unless (transaction database)
    (setf (transaction database) (make-instance 'transaction)))
  (when (= (incf (transaction-level database) 1))
    (let ((transaction (transaction database)))
      (setf (commit-hooks transaction) nil
            (rollback-hooks transaction) nil
            (transaction-status transaction) nil)
      (execute-command "BEGIN" :database database))))

(defmethod database-commit-transaction ((database closed-database))
  (signal-closed-database-error database))

(defmethod database-commit-transaction (database)
    (if (> (transaction-level database) 0)
        (when (zerop (decf (transaction-level database)))
          (execute-command "COMMIT" :database database)
          (map nil #'funcall (commit-hooks (transaction database))))
        (warn 'clsql-simple-warning
              :format-control "Cannot commit transaction against ~A because there is no transaction in progress."
              :format-arguments (list database))))

(defmethod database-abort-transaction ((database closed-database))
  (signal-closed-database-error database))

(defmethod database-abort-transaction (database)
    (if (> (transaction-level database) 0)
        (when (zerop (decf (transaction-level database)))
          (unwind-protect 
               (execute-command "ROLLBACK" :database database)
            (map nil #'funcall (rollback-hooks (transaction database)))))
        (warn 'clsql-simple-warning
              :format-control "Cannot abort transaction against ~A because there is no transaction in progress."
              :format-arguments (list database))))


(defmacro with-transaction ((&key (database *default-database*)) &rest body)
  "Executes BODY within a transaction for DATABASE (which defaults to
*DEFAULT-DATABASE*). The transaction is committed if the body finishes
successfully (without aborting or throwing), otherwise the database is
rolled back."
  (let ((db (gensym "db-")))
    `(let ((,db ,database))
      (unwind-protect
           (progn
             (database-start-transaction ,db)
             ,@body
             (commit-transaction ,db))
        (if (eq (transaction-status (transaction ,db)) :committed)
            (database-commit-transaction ,db)
            (database-abort-transaction ,db))))))

(defun commit (&key (database *default-database*))
  "Commits changes made to DATABASE which defaults to *DEFAULT-DATABASE*."
  (database-commit-transaction database))

(defun rollback (&key (database *default-database*))
  "Rolls back changes made in DATABASE, which defaults to
*DEFAULT-DATABASE* since the last commit, that is changes made since
the last commit are not recorded."
  (database-abort-transaction database))

(defun start-transaction (&key (database *default-database*))
  "Starts a transaction block on DATABASE which defaults to
*default-database* and which continues until ROLLBACK or COMMIT are
called."
  (unless (in-transaction-p :database database)
    (database-start-transaction database)))

(defun in-transaction-p (&key (database *default-database*))
  "A predicate to test whether we are currently within the scope of a
transaction in DATABASE."
  (and database (transaction database) (= (transaction-level database) 1)))
