
(defmethod database-query (query-expression (database closed-database) types)
  (declare (ignore query-expression types))
  (signal-closed-database-error database))

(defmethod database-query (query-expression (database t) types)
  (declare (ignore query-expression types))
  (signal-no-database-error))

(defmethod database-execute-command (sql-expression (database closed-database))
  (declare (ignore sql-expression))
  (signal-closed-database-error database))

(defmethod database-execute-command (sql-expression (database t))
  (declare (ignore sql-expression))
  (signal-no-database-error))

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
