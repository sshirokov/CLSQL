;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;;
;;;; $Id$
;;;;
;;;; CLSQL square bracket symbolic query syntax. Functions for
;;;; enabling and disabling the syntax and for building SQL
;;;; expressions using the syntax.
;;;;
;;;; This file is part of CLSQL.
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************


(in-package #:clsql-sys)

(defvar *original-reader-enter* nil)

(defvar *original-reader-exit* nil)

(defvar *sql-macro-open-char* #\[)

(defvar *sql-macro-close-char* #\])

(defvar *restore-sql-reader-syntax* nil)


;; Exported functions for disabling SQL syntax.

(defmacro disable-sql-reader-syntax ()
  "Turn off SQL square bracket syntax changing syntax state. Set state
such that RESTORE-SQL-READER-SYNTAX-STATE will make the syntax
disabled if it is consequently locally enabled."
  '(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf *restore-sql-reader-syntax* nil)
     (%disable-sql-reader-syntax)))

(defmacro locally-disable-sql-reader-syntax ()
  "Turn off SQL square bracket syntax and do not change syntax state." 
  '(eval-when (:compile-toplevel :load-toplevel :execute)
    (%disable-sql-reader-syntax)))

(defun %disable-sql-reader-syntax ()
  (when *original-reader-enter*
    (set-macro-character *sql-macro-open-char* *original-reader-enter*))
  (setf *original-reader-enter* nil)
  (values))


;; Exported functions for enabling SQL syntax.

(defmacro enable-sql-reader-syntax ()
  "Turn on SQL square bracket syntax changing syntax state. Set state
such that RESTORE-SQL-READER-SYNTAX-STATE will make the syntax enabled
if it is consequently locally disabled."
  '(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf *restore-sql-reader-syntax* t)
     (%enable-sql-reader-syntax)))

(defmacro locally-enable-sql-reader-syntax ()
  "Turn on SQL square bracket syntax and do not change syntax state."
  '(eval-when (:compile-toplevel :load-toplevel :execute)
     (%enable-sql-reader-syntax)))

(defun %enable-sql-reader-syntax ()
  (unless *original-reader-enter*
    (setf *original-reader-enter* (get-macro-character *sql-macro-open-char*)))
  (set-macro-character *sql-macro-open-char* #'sql-reader-open)
  (enable-sql-close-syntax)
  (values))

(defmacro restore-sql-reader-syntax-state ()
  "Sets the enable/disable square bracket syntax state to reflect the
last call to either DISABLE-SQL-READER-SYNTAX or
ENABLE-SQL-READER-SYNTAX. The default state of the square bracket
syntax is disabled."
  '(eval-when (:compile-toplevel :load-toplevel :execute)
    (if *restore-sql-reader-syntax*
        (%enable-sql-reader-syntax)
        (%disable-sql-reader-syntax))))

(defun sql-reader-open (stream char)
  (declare (ignore char))
  (let ((sqllist (read-delimited-list #\] stream t)))
    (if (sql-operator (car sqllist))
	(cons (sql-operator (car sqllist)) (cdr sqllist))
      (apply #'generate-sql-reference sqllist))))

;; Internal function that disables the close syntax when leaving sql context.
(defun disable-sql-close-syntax ()
  (set-macro-character *sql-macro-close-char* *original-reader-exit*)
  (setf *original-reader-exit* nil))

;; Internal function that enables close syntax when entering SQL context.
(defun enable-sql-close-syntax ()
  (setf *original-reader-exit* (get-macro-character *sql-macro-close-char*))
  (set-macro-character *sql-macro-close-char* (get-macro-character #\))))

(defun generate-sql-reference (&rest arglist)
  (cond ((= (length arglist) 1)	; string, table or attribute
	 (if (stringp (car arglist))
	     (sql-expression :string (car arglist))
	   (sql-expression :attribute (car arglist))))
	((<= 2 (length arglist))
	 (let ((sqltype (if (keywordp (caddr arglist))
			    (caddr arglist) nil))
	       (sqlparam (if (keywordp (caddr arglist))
			     (caddr arglist))))
	   (cond
	    ((stringp (cadr arglist))
	     (sql-expression :table (car arglist)
			     :alias (cadr arglist)
			     :type sqltype))
	    ((keywordp (cadr arglist))
	     (sql-expression :attribute (car arglist)
			     :type (cadr arglist)
			     :params sqlparam))
	    (t
	     (sql-expression :attribute (cadr arglist)
			     :table (car arglist)
			     :params sqlparam
			     :type sqltype)))))
	(t
	 (error 'clsql-sql-syntax-error :reason "bad expression syntax"))))


;; Exported functions for dealing with SQL syntax 

(defun sql (&rest args)
  "Generates SQL from a set of expressions given by ARGS. Each
argument is translated into SQL and then the args are concatenated
with a single space between each pair."
  (format nil "~{~A~^ ~}" (mapcar #'sql-output args)))

(defun sql-expression (&key string table alias attribute type params)
  "Generates an SQL expression from the given keywords. Valid
combinations of the arguments are: string; table; table and alias;
table and attribute; table, attribute, and type; table or alias, and
attribute; table or alias, and attribute and type; attribute; and
attribute and type."
  (cond
    (string
     (make-instance 'sql :string string))
    (attribute
     (make-instance 'sql-ident-attribute  :name attribute
                    :qualifier (or table alias)
                    :type type
                    :params params))
    ((and table (not attribute))
     (make-instance 'sql-ident-table :name table
                    :table-alias alias))))

(defun sql-operator (operation)
  "Takes an SQL operator as an argument and returns the Lisp symbol
for the operator."
  (typecase operation
    (string nil)
    (symbol (gethash (symbol-name-default-case (symbol-name operation))
                     *sql-op-table*))))

(defun sql-operation (operation &rest rest)
  "Generates an SQL statement from an operator and arguments." 
  (if (sql-operator operation)
      (apply (symbol-function (sql-operator operation)) rest)
      (error "~A is not a recognized SQL operator." operation)))


