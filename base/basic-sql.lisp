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


