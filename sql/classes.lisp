;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;;
;;;; $Id$
;;;;
;;;; Classes defining SQL expressions and methods for formatting the
;;;; appropriate SQL commands.
;;;;
;;;; This file is part of CLSQL.
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(in-package #:clsql-sys)

(defvar +empty-string+ "''")

(defvar +null-string+ "NULL")

(defvar *sql-stream* nil
  "stream which accumulates SQL output")

(defun sql-output (sql-expr &optional database)
  (progv '(*sql-stream*)
      `(,(make-string-output-stream))
    (output-sql sql-expr database)
    (get-output-stream-string *sql-stream*)))


(defclass %sql-expression ()
  ())

(defmethod output-sql ((expr %sql-expression) database)
  (declare (ignore database))
  (write-string +null-string+ *sql-stream*))

(defmethod print-object ((self %sql-expression) stream)
  (print-unreadable-object
   (self stream :type t)
   (write-string (sql-output self) stream)))

;; For straight up strings

(defclass sql (%sql-expression)
  ((text
    :initarg :string
    :initform ""))
  (:documentation "A literal SQL expression."))

(defmethod make-load-form ((sql sql) &optional environment)
  (declare (ignore environment))
  (with-slots (text)
    sql
    `(make-instance 'sql :string ',text)))

(defmethod output-sql ((expr sql) database)
  (declare (ignore database))
  (write-string (slot-value expr 'text) *sql-stream*)
  t)

(defmethod print-object ((ident sql) stream)
  (format stream "#<~S \"~A\">"
          (type-of ident)
          (sql-output ident nil)))

;; For SQL Identifiers of generic type
(defclass sql-ident (%sql-expression)
  ((name
    :initarg :name
    :initform "NULL"))
  (:documentation "An SQL identifer."))

(defmethod make-load-form ((sql sql-ident) &optional environment)
  (declare (ignore environment))
  (with-slots (name)
    sql
    `(make-instance 'sql-ident :name ',name)))

(defvar *output-hash* (make-hash-table :test #'equal))

(defmethod output-sql-hash-key (expr database)
  (declare (ignore expr database))
  nil)

#+ignore
(defmethod output-sql :around ((sql t) database)
  (let* ((hash-key (output-sql-hash-key sql database))
         (hash-value (when hash-key (gethash hash-key *output-hash*))))
    (cond ((and hash-key hash-value)
           (write-string hash-value *sql-stream*))
          (hash-key
           (let ((*sql-stream* (make-string-output-stream)))
             (call-next-method)
             (setf hash-value (get-output-stream-string *sql-stream*))
             (setf (gethash hash-key *output-hash*) hash-value))
           (write-string hash-value *sql-stream*))
          (t
           (call-next-method)))))

(defmethod output-sql ((expr sql-ident) database)
  (with-slots (name)
      expr
    (write-string
     (convert-to-db-default-case 
      (etypecase name
	(string name)
	(symbol (symbol-name name)))
      database)
     *sql-stream*))
  t)

;; For SQL Identifiers for attributes

(defclass sql-ident-attribute (sql-ident)
  ((qualifier
    :initarg :qualifier
    :initform "NULL")
   (type
    :initarg :type
    :initform "NULL")
   (params
    :initarg :params
    :initform nil))
  (:documentation "An SQL Attribute identifier."))

(defmethod collect-table-refs (sql)
  (declare (ignore sql))
  nil)

(defmethod collect-table-refs ((sql sql-ident-attribute))
  (let ((qual (slot-value sql 'qualifier)))
    (if (and qual (symbolp (slot-value sql 'qualifier)))
        (list (make-instance 'sql-ident-table :name
                             (slot-value sql 'qualifier))))))

(defmethod make-load-form ((sql sql-ident-attribute) &optional environment)
  (declare (ignore environment))
  (with-slots (qualifier type name)
    sql
    `(make-instance 'sql-ident-attribute :name ',name
      :qualifier ',qualifier
      :type ',type)))

(defmethod output-sql ((expr sql-ident-attribute) database)
  (with-slots (qualifier name type params)
      expr
    (if (and (not qualifier) (not type))
	(write-string (sql-escape (convert-to-db-default-case 
				   (symbol-name name) database)) *sql-stream*)
	;;; KMR: The TYPE field is used by CommonSQL for type conversion -- it
      ;;; should not be output in SQL statements
      #+ignore
      (format *sql-stream* "~@[~A.~]~A~@[ ~A~]"
	      (when qualifier
		(convert-to-db-default-case (sql-escape qualifier) database))
	      (sql-escape (convert-to-db-default-case name database))
	      (when type
		(convert-to-db-default-case (symbol-name type) database)))
      (format *sql-stream* "~@[~A.~]~A"
	      (when qualifier
                (typecase qualifier 
                  (string (format nil "~s" qualifier))
                  (t (convert-to-db-default-case (sql-escape qualifier) 
                                                 database))))
	      (sql-escape (convert-to-db-default-case name database))))
    t))

(defmethod output-sql-hash-key ((expr sql-ident-attribute) database)
  (declare (ignore database))
  (with-slots (qualifier name type params)
    expr
    (list 'sql-ident-attribute qualifier name type params)))

;; For SQL Identifiers for tables
(defclass sql-ident-table (sql-ident)
  ((alias
    :initarg :table-alias :initform nil))
  (:documentation "An SQL table identifier."))

(defmethod make-load-form ((sql sql-ident-table) &optional environment)
  (declare (ignore environment))
  (with-slots (alias name)
    sql
    `(make-instance 'sql-ident-table :name ',name :table-alias ',alias)))

(defun generate-sql (expr database)
  (let ((*sql-stream* (make-string-output-stream)))
    (output-sql expr database)
    (get-output-stream-string *sql-stream*)))

(defmethod output-sql ((expr sql-ident-table) database)
  (with-slots (name alias)
    expr
    (if (null alias)
        (write-string (sql-escape (convert-to-db-default-case (symbol-name name) database)) *sql-stream*)
        (progn
          (write-string (sql-escape (convert-to-db-default-case (symbol-name name) database)) *sql-stream*)
          (write-char #\Space *sql-stream*)
          (format *sql-stream* "~s" alias))))
  t)

#|
(defmethod database-output-sql ((self duration) database)
  (declare (ignore database))
  (format nil "'~a'" (duration-timestring self)))

(defmethod database-output-sql ((self money) database)
  (database-output-sql (slot-value self 'odcl::units) database))
|#


(defmethod output-sql-hash-key ((expr sql-ident-table) database)
  (declare (ignore database))
  (with-slots (name alias)
    expr
    (list 'sql-ident-table name alias)))

(defclass sql-relational-exp (%sql-expression)
  ((operator
    :initarg :operator
    :initform nil)
   (sub-expressions
    :initarg :sub-expressions
    :initform nil))
  (:documentation "An SQL relational expression."))

(defmethod collect-table-refs ((sql sql-relational-exp))
  (let ((tabs nil))
    (dolist (exp (slot-value sql 'sub-expressions))
      (let ((refs (collect-table-refs exp)))
        (if refs (setf tabs (append refs tabs)))))
    (remove-duplicates tabs
                       :test (lambda (tab1 tab2)
                               (equal (slot-value tab1 'name)
                                      (slot-value tab2 'name))))))




;; Write SQL for relational operators (like 'AND' and 'OR').
;; should do arity checking of subexpressions

(defmethod output-sql ((expr sql-relational-exp) database)
  (with-slots (operator sub-expressions)
    expr
    (let ((subs (if (consp (car sub-expressions))
                    (car sub-expressions)
                    sub-expressions)))
      (write-char #\( *sql-stream*)
      (do ((sub subs (cdr sub)))
          ((null (cdr sub)) (output-sql (car sub) database))
        (output-sql (car sub) database)
        (write-char #\Space *sql-stream*)
        (output-sql operator database)
        (write-char #\Space *sql-stream*))
      (write-char #\) *sql-stream*)))
  t)

(defclass sql-upcase-like (sql-relational-exp)
  ()
  (:documentation "An SQL 'like' that upcases its arguments."))
  
;; Write SQL for relational operators (like 'AND' and 'OR').
;; should do arity checking of subexpressions
  
(defmethod output-sql ((expr sql-upcase-like) database)
  (flet ((write-term (term)
           (write-string "upper(" *sql-stream*)
           (output-sql term database)
           (write-char #\) *sql-stream*)))
    (with-slots (sub-expressions)
      expr
      (let ((subs (if (consp (car sub-expressions))
                      (car sub-expressions)
                      sub-expressions)))
        (write-char #\( *sql-stream*)
        (do ((sub subs (cdr sub)))
            ((null (cdr sub)) (write-term (car sub)))
          (write-term (car sub))
          (write-string " LIKE " *sql-stream*))
        (write-char #\) *sql-stream*))))
  t)

(defclass sql-assignment-exp (sql-relational-exp)
  ()
  (:documentation "An SQL Assignment expression."))


(defmethod output-sql ((expr sql-assignment-exp) database)
  (with-slots (operator sub-expressions)
    expr
    (do ((sub sub-expressions (cdr sub)))
        ((null (cdr sub)) (output-sql (car sub) database))
      (output-sql (car sub) database)
      (write-char #\Space *sql-stream*)
      (output-sql operator database)
      (write-char #\Space *sql-stream*)))
  t)

(defclass sql-value-exp (%sql-expression)
  ((modifier
    :initarg :modifier
    :initform nil)
   (components
    :initarg :components
    :initform nil))
  (:documentation
   "An SQL value expression.")
  )

(defmethod collect-table-refs ((sql sql-value-exp))
  (let ((tabs nil))
    (if (listp (slot-value sql 'components))
        (progn
          (dolist (exp (slot-value sql 'components))
            (let ((refs (collect-table-refs exp)))
              (if refs (setf tabs (append refs tabs)))))
          (remove-duplicates tabs
                             :test (lambda (tab1 tab2)
                                     (equal (slot-value tab1 'name)
                                            (slot-value tab2 'name)))))
        nil)))



(defmethod output-sql ((expr sql-value-exp) database)
  (with-slots (modifier components)
    expr
    (if modifier
        (progn
          (write-char #\( *sql-stream*)
          (output-sql modifier database)
          (write-char #\Space *sql-stream*)
          (output-sql components database)
          (write-char #\) *sql-stream*))
        (output-sql components database))))

(defclass sql-typecast-exp (sql-value-exp)
  ()
  (:documentation "An SQL typecast expression."))

(defmethod output-sql ((expr sql-typecast-exp) database)
  (database-output-sql expr database))

(defmethod database-output-sql ((expr sql-typecast-exp) database)
  (with-slots (components)
    expr
    (output-sql components database)))


(defmethod collect-table-refs ((sql sql-typecast-exp))
  (when (slot-value sql 'components)
    (collect-table-refs (slot-value sql 'components))))

(defclass sql-function-exp (%sql-expression)
  ((name
    :initarg :name
    :initform nil)
   (args
    :initarg :args
    :initform nil))
  (:documentation
   "An SQL function expression."))

(defmethod collect-table-refs ((sql sql-function-exp))
  (let ((tabs nil))
    (dolist (exp (slot-value sql 'components))
      (let ((refs (collect-table-refs exp)))
        (if refs (setf tabs (append refs tabs)))))
    (remove-duplicates tabs
                       :test (lambda (tab1 tab2)
                               (equal (slot-value tab1 'name)
                                      (slot-value tab2 'name))))))

(defmethod output-sql ((expr sql-function-exp) database)
  (with-slots (name args)
    expr
    (output-sql name database)
    (when args (output-sql args database)))
  t)


(defclass sql-between-exp (sql-function-exp)
  () 
  (:documentation "An SQL between expression."))

(defmethod output-sql ((expr sql-between-exp) database)
  (with-slots (name args)
      expr 
    (output-sql (first args) database)
    (write-string " BETWEEN " *sql-stream*)
    (output-sql (second args) database)
    (write-string " AND " *sql-stream*)
    (output-sql (third args) database))
  t)

(defclass sql-query-modifier-exp (%sql-expression) 
  ((modifier :initarg :modifier :initform nil)
   (components :initarg :components :initform nil))
  (:documentation "An SQL query modifier expression."))

(defmethod output-sql ((expr sql-query-modifier-exp) database)
  (with-slots (modifier components)
      expr
    (output-sql modifier database)
    (write-string " " *sql-stream*)
    (output-sql (car components) database)
    (when components 
      (mapc #'(lambda (comp) 
		(write-string ", " *sql-stream*)
		(output-sql comp database))
	    (cdr components))))
  t)

(defclass sql-set-exp (%sql-expression)
  ((operator
    :initarg :operator
    :initform nil)
   (sub-expressions
    :initarg :sub-expressions
    :initform nil))
  (:documentation "An SQL set expression."))

(defmethod collect-table-refs ((sql sql-set-exp))
  (let ((tabs nil))
    (dolist (exp (slot-value sql 'sub-expressions))
      (let ((refs (collect-table-refs exp)))
        (if refs (setf tabs (append refs tabs)))))
    (remove-duplicates tabs
                       :test (lambda (tab1 tab2)
                               (equal (slot-value tab1 'name)
                                      (slot-value tab2 'name))))))

(defmethod output-sql ((expr sql-set-exp) database)
  (with-slots (operator sub-expressions)
      expr
    (let ((subs (if (consp (car sub-expressions))
                    (car sub-expressions)
                    sub-expressions)))
      (do ((sub subs (cdr sub)))
          ((null (cdr sub)) (output-sql (car sub) database))
        (output-sql (car sub) database)
        (write-char #\Space *sql-stream*)
        (output-sql operator database)
        (write-char #\Space *sql-stream*))))
  t)

(defclass sql-query (%sql-expression)
  ((selections
    :initarg :selections
    :initform nil)
   (all
    :initarg :all
    :initform nil)
   (flatp
    :initarg :flatp
    :initform nil)
   (set-operation
    :initarg :set-operation
    :initform nil)
   (distinct
    :initarg :distinct
    :initform nil)
   (from
    :initarg :from
    :initform nil)
   (where
    :initarg :where
    :initform nil)
   (group-by
    :initarg :group-by
    :initform nil)
   (having
    :initarg :having
    :initform nil)
   (limit
    :initarg :limit
    :initform nil)
   (offset
    :initarg :offset
    :initform nil)
   (order-by
    :initarg :order-by
    :initform nil)
   (order-by-descending
    :initarg :order-by-descending
    :initform nil)
   (inner-join
    :initarg :inner-join
    :initform nil)
   (on
    :initarg :on
    :initform nil))
  (:documentation "An SQL SELECT query."))

(defclass sql-object-query (%sql-expression)
  ((objects
    :initarg :objects
    :initform nil)
   (flatp
    :initarg :flatp
    :initform nil)
   (exp
    :initarg :exp
    :initform nil)
   (refresh
    :initarg :refresh
    :initform nil)))

(defmethod collect-table-refs ((sql sql-query))
  (remove-duplicates (collect-table-refs (slot-value sql 'where))
                     :test (lambda (tab1 tab2)
                             (equal (slot-value tab1 'name)
                                    (slot-value tab2 'name)))))

(defvar *select-arguments*
  '(:all :database :distinct :flatp :from :group-by :having :order-by
    :order-by-descending :set-operation :where :offset :limit
    :inner-join :on
    ;; below keywords are not a SQL argument, but these keywords may terminate select
    :caching :refresh))

(defun query-arg-p (sym)
  (member sym *select-arguments*))

(defun query-get-selections (select-args)
  "Return two values: the list of select-args up to the first keyword,
uninclusive, and the args from that keyword to the end."
  (let ((first-key-arg (position-if #'query-arg-p select-args)))
    (if first-key-arg
        (values (subseq select-args 0 first-key-arg)
                (subseq select-args first-key-arg))
        select-args)))

(defun make-query (&rest args)
  (flet ((select-objects (target-args)
           (and target-args
                (every #'(lambda (arg)
                           (and (symbolp arg)
                                (find-class arg nil)))
                       target-args))))
    (multiple-value-bind (selections arglist)
	(query-get-selections args)
      (if (select-objects selections) 
	  (destructuring-bind (&key flatp refresh &allow-other-keys) arglist
	    (make-instance 'sql-object-query :objects selections
			   :flatp flatp :refresh refresh
			   :exp arglist))
	  (destructuring-bind (&key all flatp set-operation distinct from where
				    group-by having order-by order-by-descending
				    offset limit inner-join on &allow-other-keys)
	      arglist
	    (if (null selections)
		(error "No target columns supplied to select statement."))
	    (if (null from)
		(error "No source tables supplied to select statement."))
	    (make-instance 'sql-query :selections selections
			   :all all :flatp flatp :set-operation set-operation
			   :distinct distinct :from from :where where
			   :limit limit :offset offset
			   :group-by group-by :having having :order-by order-by
			   :order-by-descending order-by-descending
			   :inner-join inner-join :on on))))))

(defvar *in-subselect* nil)

(defmethod output-sql ((query sql-query) database)
  (with-slots (distinct selections from where group-by having order-by
                        order-by-descending limit offset inner-join on)
      query
    (when *in-subselect*
      (write-string "(" *sql-stream*))
    (write-string "SELECT " *sql-stream*)
    (when distinct
      (write-string "DISTINCT " *sql-stream*)
      (unless (eql t distinct)
        (write-string "ON " *sql-stream*)
        (output-sql distinct database)
        (write-char #\Space *sql-stream*)))
    (output-sql (apply #'vector selections) database)
    (when from
      (write-string " FROM " *sql-stream*)
      (typecase from 
        (list (output-sql (apply #'vector from) database))
        (string (write-string 
                 (sql-escape 
                  (convert-to-db-default-case from database)) *sql-stream*))
        (t (output-sql from database))))
    (when inner-join
      (write-string " INNER JOIN " *sql-stream*)
      (output-sql inner-join database))
    (when on
      (write-string " ON " *sql-stream*)
      (output-sql on database))
    (when where
      (write-string " WHERE " *sql-stream*)
      (let ((*in-subselect* t))
        (output-sql where database)))
    (when group-by
      (write-string " GROUP BY " *sql-stream*)
      (output-sql group-by database))
    (when having
      (write-string " HAVING " *sql-stream*)
      (output-sql having database))
    (when order-by
      (write-string " ORDER BY " *sql-stream*)
      (if (listp order-by)
          (do ((order order-by (cdr order)))
              ((null order))
            (output-sql (car order) database)
            (when (cdr order)
              (write-char #\, *sql-stream*)))
          (output-sql order-by database)))
    (when order-by-descending
      (write-string " ORDER BY " *sql-stream*)
      (if (listp order-by-descending)
          (do ((order order-by-descending (cdr order)))
              ((null order))
            (output-sql (car order) database)
            (when (cdr order)
              (write-char #\, *sql-stream*)))
          (output-sql order-by-descending database))
      (write-string " DESC " *sql-stream*))
    (when limit
      (write-string " LIMIT " *sql-stream*)
      (output-sql limit database))
    (when offset
      (write-string " OFFSET " *sql-stream*)
      (output-sql offset database))
    (when *in-subselect*
      (write-string ")" *sql-stream*)))
  t)

(defmethod output-sql ((query sql-object-query) database)
  (declare (ignore database))
  (with-slots (objects)
      query
    (when objects
      (format *sql-stream* "(~{~A~^ ~})" objects))))


;; INSERT

(defclass sql-insert (%sql-expression)
  ((into
    :initarg :into
    :initform nil)
   (attributes
    :initarg :attributes
    :initform nil)
   (values
    :initarg :values
    :initform nil)
   (query
    :initarg :query
    :initform nil))
  (:documentation
   "An SQL INSERT statement."))

(defmethod output-sql ((ins sql-insert) database)
  (with-slots (into attributes values query)
    ins
    (write-string "INSERT INTO " *sql-stream*)
    (output-sql into database)
    (when attributes
      (write-char #\Space *sql-stream*)
      (output-sql attributes database))
    (when values
      (write-string " VALUES " *sql-stream*)
      (output-sql values database))
    (when query
      (write-char #\Space *sql-stream*)
      (output-sql query database)))
  t)

;; DELETE

(defclass sql-delete (%sql-expression)
  ((from
    :initarg :from
    :initform nil)
   (where
    :initarg :where
    :initform nil))
  (:documentation
   "An SQL DELETE statement."))

(defmethod output-sql ((stmt sql-delete) database)
  (with-slots (from where)
    stmt
    (write-string "DELETE FROM " *sql-stream*)
    (typecase from
      (symbol (write-string (sql-escape from) *sql-stream*))
      (t  (output-sql from database)))
    (when where
      (write-string " WHERE " *sql-stream*)
      (output-sql where database)))
  t)

;; UPDATE

(defclass sql-update (%sql-expression)
  ((table
    :initarg :table
    :initform nil)
   (attributes
    :initarg :attributes
    :initform nil)
   (values
    :initarg :values
    :initform nil)
   (where
    :initarg :where
    :initform nil))
  (:documentation "An SQL UPDATE statement."))

(defmethod output-sql ((expr sql-update) database)
  (with-slots (table where attributes values)
    expr
    (flet ((update-assignments ()
             (mapcar #'(lambda (a b)
                         (make-instance 'sql-assignment-exp
                                        :operator '=
                                        :sub-expressions (list a b)))
                     attributes values)))
      (write-string "UPDATE " *sql-stream*)
      (output-sql table database)
      (write-string " SET " *sql-stream*)
      (output-sql (apply #'vector (update-assignments)) database)
      (when where
        (write-string " WHERE " *sql-stream*)
        (output-sql where database))))
  t)

;; CREATE TABLE

(defclass sql-create-table (%sql-expression)
  ((name
    :initarg :name
    :initform nil)
   (columns
    :initarg :columns
    :initform nil)
   (modifiers
    :initarg :modifiers
    :initform nil)
   (transactions
    :initarg :transactions
    :initform nil))
  (:documentation
   "An SQL CREATE TABLE statement."))

;; Here's a real warhorse of a function!

(declaim (inline listify))
(defun listify (x)
  (if (atom x)
      (list x)
      x))

(defmethod output-sql ((stmt sql-create-table) database)
  (flet ((output-column (column-spec)
           (destructuring-bind (name type &optional db-type &rest constraints)
               column-spec
             (let ((type (listify type)))
               (output-sql name database)
               (write-char #\Space *sql-stream*)
               (write-string
                (if (stringp db-type) db-type ; override definition
                    (database-get-type-specifier (car type) (cdr type) database))
                *sql-stream*)
               (let ((constraints
                      (database-constraint-statement constraints database)))
                 (when constraints
                   (write-string " " *sql-stream*)
                   (write-string constraints *sql-stream*)))))))
    (with-slots (name columns modifiers transactions)
      stmt
      (write-string "CREATE TABLE " *sql-stream*)
      (output-sql name database)
      (write-string " (" *sql-stream*)
      (do ((column columns (cdr column)))
          ((null (cdr column))
           (output-column (car column)))
        (output-column (car column))
        (write-string ", " *sql-stream*))
      (when modifiers
        (do ((modifier (listify modifiers) (cdr modifier)))
            ((null modifier))
          (write-string ", " *sql-stream*)
          (write-string (car modifier) *sql-stream*)))
      (write-char #\) *sql-stream*)
      (when (and (eq :mysql (database-underlying-type database))
		 transactions
		 (db-type-transaction-capable? :mysql database))
	(write-string " Type=InnoDB" *sql-stream*)))) 
  t)


;; CREATE VIEW

(defclass sql-create-view (%sql-expression)
  ((name :initarg :name :initform nil)
   (column-list :initarg :column-list :initform nil)
   (query :initarg :query :initform nil)
   (with-check-option :initarg :with-check-option :initform nil))
  (:documentation "An SQL CREATE VIEW statement."))

(defmethod output-sql ((stmt sql-create-view) database)
  (with-slots (name column-list query with-check-option) stmt
    (write-string "CREATE VIEW " *sql-stream*)
    (output-sql name database)
    (when column-list (write-string " " *sql-stream*)
          (output-sql (listify column-list) database))
    (write-string " AS " *sql-stream*)
    (output-sql query database)
    (when with-check-option (write-string " WITH CHECK OPTION" *sql-stream*))))


;;
;; Column constraint types
;;
(defparameter *constraint-types*
  (list 
   (cons (symbol-name-default-case "NOT-NULL") "NOT NULL") 
   (cons (symbol-name-default-case "PRIMARY-KEY") "PRIMARY KEY")))

;;
;; Convert type spec to sql syntax
;;

(defmethod database-constraint-description (constraint database)
  (declare (ignore database))
  (let ((output (assoc (symbol-name constraint) *constraint-types*
                       :test #'equal)))
    (if (null output)
        (error 'clsql-sql-syntax-error
               :reason (format nil "unsupported column constraint '~a'"
                               constraint))
        (cdr output))))

(defmethod database-constraint-statement (constraint-list database)
  (declare (ignore database))
  (make-constraints-description constraint-list))
  
(defun make-constraints-description (constraint-list)
  (if constraint-list
      (let ((string ""))
        (do ((constraint constraint-list (cdr constraint)))
            ((null constraint) string)
          (let ((output (assoc (symbol-name (car constraint))
                               *constraint-types*
                               :test #'equal)))
            (if (null output)
                (error 'clsql-sql-syntax-error
                       :reason (format nil "unsupported column constraint '~a'"
                                       constraint))
                (setq string (concatenate 'string string (cdr output))))
            (if (< 1 (length constraint))
                (setq string (concatenate 'string string " "))))))))

