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
  "Turns off the SQL reader syntax setting the syntax state such
that if the syntax is subsequently enabled,
RESTORE-SQL-READER-SYNTAX-STATE will disable it again."
  '(eval-when (:compile-toplevel :load-toplevel :execute)
    (setf *restore-sql-reader-syntax* nil)
    (%disable-sql-reader-syntax)))

(defmacro locally-disable-sql-reader-syntax ()
  "Turns off the SQL reader syntax without changing the syntax
state such that RESTORE-SQL-READER-SYNTAX-STATE will re-establish
the current syntax state."
  '(eval-when (:compile-toplevel :load-toplevel :execute)
    (%disable-sql-reader-syntax)))

(defun %disable-sql-reader-syntax ()
  (when *original-reader-enter*
    (set-macro-character *sql-macro-open-char* *original-reader-enter*))
  (setf *original-reader-enter* nil)
  (values))


;; Exported functions for enabling SQL syntax.

(defmacro enable-sql-reader-syntax ()
  "Turns on the SQL reader syntax setting the syntax state such
that if the syntax is subsequently disabled,
RESTORE-SQL-READER-SYNTAX-STATE will enable it again."
  '(eval-when (:compile-toplevel :load-toplevel :execute)
    (setf *restore-sql-reader-syntax* t)
    (%enable-sql-reader-syntax)))

(defmacro locally-enable-sql-reader-syntax ()
  "Turns on the SQL reader syntax without changing the syntax
state such that RESTORE-SQL-READER-SYNTAX-STATE will re-establish
the current syntax state."
  '(eval-when (:compile-toplevel :load-toplevel :execute)
    (%enable-sql-reader-syntax)))

(defun %enable-sql-reader-syntax ()
  (unless *original-reader-enter*
    (setf *original-reader-enter* (get-macro-character *sql-macro-open-char*)))
  (set-macro-character *sql-macro-open-char* #'sql-reader-open)
  (enable-sql-close-syntax)
  (values))

(defmacro restore-sql-reader-syntax-state ()
  "Enables the SQL reader syntax if ENABLE-SQL-READER-SYNTAX has
been called more recently than DISABLE-SQL-READER-SYNTAX and
otherwise disables the SQL reader syntax. By default, the SQL
reader syntax is disabled."
  '(eval-when (:compile-toplevel :load-toplevel :execute)
    (if *restore-sql-reader-syntax*
        (%enable-sql-reader-syntax)
        (%disable-sql-reader-syntax))))

(defun sql-reader-open (stream char)
  (declare (ignore char))
  (let ((sqllist (read-delimited-list #\] stream t)))
    (cond ((string= (write-to-string (car sqllist)) "||")
           (cons (sql-operator 'concat) (cdr sqllist)))
          ((and (= (length sqllist) 1) (eql (car sqllist) '*))
           (apply #'generate-sql-reference sqllist))
          ((sql-operator (car sqllist))
           (cons (sql-operator (car sqllist)) (cdr sqllist)))
          (t (apply #'generate-sql-reference sqllist)))))

(defun disable-sql-close-syntax ()
  "Internal function that disables the close syntax when leaving
  sql context."
  (set-macro-character *sql-macro-close-char* *original-reader-exit*)
  (setf *original-reader-exit* nil))

(defun enable-sql-close-syntax ()
  "Internal function that enables close syntax when entering SQL
  context."
  (setf *original-reader-exit* (get-macro-character *sql-macro-close-char*))
  (set-macro-character *sql-macro-close-char* (get-macro-character #\))))

(defun generate-sql-reference (&rest arglist)
  (cond ((= (length arglist) 1)	; string, table or attribute
	 (if (stringp (car arglist))
	     (sql-expression :string (car arglist))
             (sql-expression :attribute (car arglist))))
	((<= 2 (length arglist))
	 (let ((sqltype (when (keywordp (caddr arglist)) (caddr arglist) nil)))
           (cond
             ((stringp (cadr arglist))
	     (sql-expression :table (car arglist)
			     :alias (cadr arglist)
			     :type sqltype))
	    ((keywordp (cadr arglist))
	     (sql-expression :attribute (car arglist)
			     :type (cadr arglist)))
	    (t
	     (sql-expression :attribute (cadr arglist)
			     :table (car arglist)
			     :type sqltype)))))
	(t
	 (error 'sql-user-error :message "bad expression syntax"))))


;; Exported functions for dealing with SQL syntax 

(defun sql (&rest args)
  "Returns an SQL string generated from the SQL expressions
ARGS. The expressions are translated into SQL strings and then
concatenated with a single space delimiting each expression."
  (format nil "~{~A~^ ~}" (mapcar #'sql-output args)))

(defun sql-expression (&key string table alias attribute type)
  "Returns an SQL expression constructed from the supplied arguments
which may be combined as follows: ATTRIBUTE and TYPE; ATTRIBUTE;
ALIAS or TABLE and ATTRIBUTE and TYPE; ALIAS or TABLE and
ATTRIBUTE; TABLE, ATTRIBUTE and TYPE; TABLE and ATTRIBUTE; TABLE
and ALIAS; TABLE; and STRING."
  (cond
    (string
     (make-instance 'sql :string string))
    (attribute
     (make-instance 'sql-ident-attribute  :name attribute
                    :qualifier (or table alias)
                    :type type))
    ((and table (not attribute))
     (make-instance 'sql-ident-table :name table
                    :table-alias alias))))

(defun sql-operator (operation)
  "Returns the Lisp symbol corresponding to the SQL operation
  represented by the symbol OPERATION."
  (typecase operation
    (string nil)
    (symbol (gethash (symbol-name-default-case (symbol-name operation))
                     *sql-op-table*))))

(defun sql-operation (operation &rest rest)
  "Returns an SQL expression constructed from the supplied SQL
operator or function OPERATION and its arguments REST. If
OPERATION is passed the symbol FUNCTION then the first value in
REST is taken to be a valid SQL function and the remaining values
in REST its arguments."
  (if (sql-operator operation)
      (apply (symbol-function (sql-operator operation)) rest)
      (error "~A is not a recognized SQL operator." operation)))


