;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:    odbc-dbi.cl
;;;; Purpose: Mid-level (DBI) interface for CLSQL ODBC backend
;;;; Author:  Kevin M. Rosenberg
;;;; Create:  April 2004
;;;;
;;;; $Id: odbc-sql.lisp 8983 2004-04-12 21:16:48Z kevin $
;;;;
;;;; This file, part of CLSQL, is Copyright (c) 2002 by Kevin M. Rosenberg
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(in-package #:cl-user)

(defpackage #:odbc-dbi
  (:use #:cl #:odbc)
  (:export
   #:bind-parameter
   #:close-query
   #:connect
   #:db-external-format
   #:db-hstmt
   #:db-width
   #:disconnect
   #:end-transaction
   #:fetch-row
   #:list-all-database-tables
   #:list-all-table-columns
   #:loop-over-results
   #:prepare-sql
   #:rr-sql
   #:run-prepared-sql
   #:set-autocommit
   #:sql
   
   #:*auto-trim-strings*
   #:*default-database*
   #:*default-odbc-external-format*
   #:*null-value*
   )
  (:documentation "This is the mid-level interface ODBC."))

(in-package #:odbc-dbi)

;;; SQL Interface

(defclass odbc-db ()
  (;; any reason to have more than one henv?
   (henv :initform nil :allocation :class :initarg :henv :accessor henv)
   (hdbc :initform nil :initarg :hdbc :accessor hdbc)
   ;; info returned from SQLGetInfo
   (info :initform (make-hash-table) :reader db-info)
   (type :initform nil :initarg :db-type :reader db-type)
   (connected-p :initform nil :accessor db-connected-p)
   ;; not used yet
   (count :initform 0 :initarg :count :accessor db-count)
   ;; not used yet
   (total-count :initform 0 :allocation :class :accessor db-total-count)
   ;; the use of this slot is deprecated; it will be removed when dtf works without it.
   (query :initform nil :accessor db-query-object)
   ;; resource of (active and inactive) query objects
   (queries :initform () :accessor db-queries)))

(defclass query ()
  ((hstmt :initform nil :initarg :hstmt :accessor hstmt) ; = cursor??
   (column-count :initform nil :accessor column-count)
   (column-names :initform (make-array 0 :element-type 'string :adjustable t :fill-pointer t)
		 :accessor column-names)
   (column-c-types :initform (make-array 0 :element-type 'fixnum :adjustable t :fill-pointer t)
		   :accessor column-c-types)
   (column-sql-types :initform (make-array 0 :element-type 'fixnum :adjustable t :fill-pointer t)
		     :accessor column-sql-types)
   (column-data-ptrs :initform (make-array 0 :adjustable t :fill-pointer t)
		     :accessor data-ptrs)
   (column-out-len-ptrs :initform (make-array 0 :adjustable t :fill-pointer t)
			:accessor column-out-len-ptrs)
   (column-precisions :initform (make-array 0 :element-type 'fixnum :adjustable t :fill-pointer t)
		      :accessor column-precisions)
   (column-scales :initform (make-array 0 :element-type 'fixnum :adjustable t :fill-pointer t)
		  :accessor column-scales)
   (column-nullables-p :initform (make-array 0 :element-type 'fixnum :adjustable t :fill-pointer t)
		       :accessor column-nullables-p)
      ;;(parameter-count :initform 0 :accessor parameter-count)
   (parameter-data-ptrs :initform (make-array 0 :adjustable t :fill-pointer t)
			:accessor parameter-ptrs)
   ;; a query string or a query expression object
   (sql-expression :initform nil :initarg :sql-expression :accessor sql-expression)
   ;; database object the query is to be run against
   (database :initarg :database :reader query-database)
   (active-p :initform nil :initarg :active-p :accessor query-active-p))
  (:documentation
   "Stores query information, like SQL query string/expression and database to run
the query against." ))

(defun connect (&key data-source-name user password (autocommit t))
  (let ((db (make-instance 'odbc-db)))
    (unless (henv db) ;; has class allocation!
      (setf (henv db) (%new-environment-handle)))
    (setf (hdbc db) (%new-db-connection-handle (henv db)))
    (%sql-connect (hdbc db) data-source-name user password)
    ;; FIXME: Check if connected
    (when (/= (get-odbc-info db odbc::$SQL_TXN_CAPABLE) odbc::$SQL_TC_NONE)
      (if autocommit
	  (enable-autocommit (hdbc db))
	  (disable-autocommit (hdbc db))))
    db))

(defun disconnect (database)
  (with-slots (hdbc queries) database
    (dolist (query queries)
      (if (query-active-p query)
	  (with-slots (hstmt) query
	    (when hstmt 
	      (%free-statement hstmt :drop)
	      (setf hstmt nil)))))
    (%disconnect hdbc)))


(defun sql (expr &key db result-types row-count column-names query)
  (if query 
      (db-query db expr)
      ;; fixme: don't return all query results. 
      (db-query db expr)))

(defun close-query (result-set)
  (warn "Not implemented."))

(defun fetch-row (result-set error-eof eof-value)
  (warn "Not implemented."))

(defclass odbc-query (query)
  ((hstmt :initform nil :initarg :hstmt :accessor hstmt) ; = cursor??
   (column-count :initform nil :accessor column-count)
   (column-names :initform (make-array 0 :element-type 'string :adjustable t :fill-pointer t)
                 :accessor column-names)
   (column-c-types :initform (make-array 0 :element-type 'fixnum :adjustable t :fill-pointer t)
                   :accessor column-c-types)
   (column-sql-types :initform (make-array 0 :element-type 'fixnum :adjustable t :fill-pointer t)
                     :accessor column-sql-types)
   (column-data-ptrs :initform (make-array 0 :adjustable t :fill-pointer t)
                     :accessor data-ptrs)
   (column-out-len-ptrs :initform (make-array 0 :adjustable t :fill-pointer t)
                        :accessor column-out-len-ptrs)
   (column-precisions :initform (make-array 0 :element-type 'fixnum :adjustable t :fill-pointer t)
                      :accessor column-precisions)
   (column-scales :initform (make-array 0 :element-type 'fixnum :adjustable t :fill-pointer t)
                  :accessor column-scales)
   (column-nullables-p :initform (make-array 0 :element-type 'fixnum :adjustable t :fill-pointer t)
                       :accessor column-nullables-p)
   ;;(parameter-count :initform 0 :accessor parameter-count)
   (parameter-data-ptrs :initform (make-array 0 :adjustable t :fill-pointer t)
                     :accessor parameter-ptrs)))

(defmethod db-commit ((database odbc-db))
  (%commit (henv database) (hdbc database)))

(defmethod db-rollback ((database odbc-db))
  (%rollback (henv database) (hdbc database)))

(defmethod db-cancel-query ((query odbc-query))
  (with-slots (hstmt) query
    (%sql-cancel hstmt)
    (setf (query-active-p query) nil)))

#+simple-version
(defmacro with-transaction (&body body)
  `(%with-transaction
     (:henv (henv ,*default-database*) :hdbc (hdbc ,*default-database*))
     ,@body))

(defmethod initialize-instance :after ((query odbc-query) 
                                       &key sql henv hdbc &allow-other-keys)
  (when sql
    (let ((hstmt (%new-statement-handle hdbc)))
      (%sql-exec-direct sql hstmt henv hdbc)
      (with-slots (column-count 
                   column-names column-c-types column-sql-types column-data-ptrs 
                   column-out-len-ptrs column-precisions column-scales
                   column-nullables-p active-p) query
        (setf (hstmt query) hstmt)
        (%initialize-query query)
        (setf active-p t)))))

;; one for odbc-db is missing
(defmethod terminate ((query odbc-query))
  ;;(format tb::*local-output* "~%*** terminated: ~s" query)
  (with-slots (hstmt) query
    (when hstmt
      ;(%free-statement hstmt :drop)
      (uffi:free-foreign-object hstmt)) ;; ??
    (%dispose-column-ptrs query)))

(defmethod %dispose-column-ptrs ((query odbc-query))
  (with-slots (column-data-ptrs column-out-len-ptrs hstmt) query
    (loop for data-ptr across column-data-ptrs
          when data-ptr do (uffi:free-foreign-object data-ptr))
    (loop for out-len-ptr across column-out-len-ptrs
          when out-len-ptr do (uffi:free-foreign-object out-len-ptr))))

(defmethod db-open-query ((database odbc-db) query-expression
                             &key arglen col-positions
                             &allow-other-keys)
  (db-open-query (get-free-query database) query-expression
                 :arglen arglen :col-positions col-positions))

(defmethod db-open-query ((query odbc-query) query-expression
                             &key arglen col-positions &allow-other-keys)
  (%db-execute query query-expression)
  (%initialize-query query arglen col-positions))

(defmethod db-fetch-query-results ((database odbc-db) &optional count)
  (db-fetch-query-results (db-query-object database) count))

(defmethod db-fetch-query-results ((query odbc-query) &optional count)
  (when (query-active-p query)
    (let (#+ignore(no-data nil))
      (with-slots (column-count column-data-ptrs column-c-types column-sql-types 
                                column-out-len-ptrs column-precisions hstmt)
                  query
        (values
	 (loop for i from 0 
	     until (or (and count (= i count))
		       (= (%sql-fetch hstmt) odbc::$SQL_NO_DATA_FOUND))
	     collect
	       (loop for data-ptr across column-data-ptrs
		   for c-type across column-c-types
		   for sql-type across column-sql-types
		   for out-len-ptr across column-out-len-ptrs
		   for precision across column-precisions
		   for j from 0		; column count is zero based in lisp
		   collect 
		     (cond ((< 0 precision +max-precision+)
			    (read-data data-ptr c-type sql-type out-len-ptr nil))
			   ((zerop (get-cast-long out-len-ptr))
			    nil)
			   (t
			    (read-data-in-chunks hstmt j data-ptr c-type sql-type
						 out-len-ptr nil)))))
	 query)))))

(defmethod db-query ((database odbc-db) query-expression)
  (let ((free-query
         ;; make it thread safe 
         (get-free-query database)))
    ;;(format tb::*local-output* "~%new query: ~s" free-query)
    (setf (sql-expression free-query) query-expression)
    (unwind-protect
      (progn
        (%db-execute free-query query-expression)
        (%initialize-query free-query)
        (values
         (db-fetch-query-results free-query nil)
         ;; LMH return the column names as well
         (column-names free-query)))
      (db-close-query free-query)
      ;;(format tb::*local-output* "~%query closed: ~s" free-query)
      )))

(defmethod %db-execute ((database odbc-db) sql-expression &key &allow-other-keys)
  (%db-execute (get-free-query database) sql-expression))

(defmethod %db-execute ((query odbc-query) sql-expression &key &allow-other-keys)
  (with-slots (henv hdbc) (odbc::query-database query)
    (with-slots (hstmt) query
      (unless hstmt (setf hstmt (%new-statement-handle hdbc))) 
      ;;(print (list :new hstmt) tb::*local-output*)
      (setf (sql-expression query) sql-expression)
      (%sql-exec-direct sql-expression hstmt henv hdbc)
      query)))

;; reuse inactive queries
(defmethod get-free-query ((database odbc-db))
  "get-free-query finds or makes a nonactive query object, and then sets it to active.
This makes the functions db-execute-command and db-query thread safe."
  (with-slots (queries) database
    (or (clsql-base-sys:without-interrupts ;; not context switch allowed here 
         (let ((inactive-query (find-if (lambda (query)
                                          (not (query-active-p query)))
                                        queries)))
           (when inactive-query 
             (with-slots (column-count column-names column-c-types 
                                       column-sql-types column-data-ptrs
                                       column-out-len-ptrs column-precisions
                                       column-scales column-nullables-p)
                         inactive-query
               ;;(print column-data-ptrs tb::*local-output*)
               ;;(%dispose-column-ptrs inactive-query)
               (setf column-count 0
                     (fill-pointer column-names) 0
                     (fill-pointer column-c-types) 0
                     (fill-pointer column-sql-types) 0
                     (fill-pointer column-data-ptrs) 0
                     (fill-pointer column-out-len-ptrs) 0
                     (fill-pointer column-precisions) 0
                     (fill-pointer column-scales) 0
                     (fill-pointer column-nullables-p) 0))
             (setf (query-active-p inactive-query) t))
           inactive-query))
        (let ((new-query (make-instance 'odbc-query
                                        :database database
                                        ;;(clone-database database)
                                        :active-p t)))
          (push new-query queries)
          new-query))))

(defmethod db-execute-command ((database odbc-db) sql-string)
  (db-execute-command (get-free-query database) sql-string))

(defmethod db-execute-command ((query odbc-query) sql-string)
  (with-slots (hstmt database) query
    (with-slots (henv hdbc) database
      (unless hstmt (setf hstmt (%new-statement-handle hdbc))) 
      (unwind-protect 
          (%sql-exec-direct sql-string hstmt henv hdbc)
        (db-close-query query)))))

(defmethod %initialize-query ((database odbc-db) &optional arglen col-positions)
  (%initialize-query (db-query-object database) arglen col-positions))

(defmethod %initialize-query ((query odbc-query) &optional arglen col-positions)
  (with-slots (hstmt 
               column-count column-names column-c-types column-sql-types
               column-data-ptrs column-out-len-ptrs column-precisions
               column-scales column-nullables-p) 
              query 
    (setf column-count (if arglen
                         (min arglen (result-columns-count hstmt))
                         (result-columns-count hstmt)))
    ;;(format tb::*local-output* "~%column-count: ~d, col-positions: ~d" column-count col-positions)
    (labels ((initialize-column (col-nr)
                (multiple-value-bind (name sql-type precision scale nullable-p)
                                     (%describe-column hstmt (1+ col-nr))
                  ;; allocate space to bind result rows to
                  (multiple-value-bind (c-type data-ptr out-len-ptr size long-p)
                                       (%allocate-bindings sql-type precision)
                    (unless long-p ;; if long-p we fetch in chunks with %sql-get-data
                      (%bind-column hstmt col-nr c-type data-ptr (1+ size) out-len-ptr))
                    (vector-push-extend name column-names) 
                    (vector-push-extend sql-type column-sql-types)
                    (vector-push-extend (sql-to-c-type sql-type) column-c-types)
                    (vector-push-extend precision column-precisions)
                    (vector-push-extend scale column-scales)
                    (vector-push-extend nullable-p column-nullables-p)
                    (vector-push-extend data-ptr column-data-ptrs)
                    (vector-push-extend out-len-ptr column-out-len-ptrs)))))
      (if col-positions
        (dolist (col-nr col-positions)
          (initialize-column col-nr))
        (dotimes (col-nr column-count)
          ;; get column information
          (initialize-column col-nr)))))
  query)

(defmethod db-close-query ((query odbc-query) &key drop-p)
  (with-slots (hstmt column-count column-names column-c-types column-sql-types
                     column-data-ptrs column-out-len-ptrs column-precisions
                     column-scales column-nullables-p) query
    (let ((count (fill-pointer column-data-ptrs)))
      (when (not (zerop count))
        (dotimes (col-nr count)
          (let ((data-ptr (aref column-data-ptrs col-nr))
                (out-len-ptr (aref column-out-len-ptrs col-nr)))
            (when data-ptr (uffi:free-foreign-object data-ptr)) ; we *did* allocate them
            (when out-len-ptr (uffi:free-foreign-object out-len-ptr)))))
      (cond ((null hstmt)
             nil)
            (drop-p
             (%free-statement hstmt :drop)
             (setf hstmt nil))
            (t
             (%free-statement hstmt :unbind)
             (%free-statement hstmt :reset)
             (%free-statement hstmt :close)))
      (setf (query-active-p query) nil)))
  query)

(defmethod %read-query-data ((database odbc-db) ignore-columns)
  (%read-query-data (db-query-object database) ignore-columns))

(defmethod %read-query-data ((query odbc-query) ignore-columns)
  (with-slots (hstmt column-count column-c-types column-sql-types
                     column-data-ptrs column-out-len-ptrs column-precisions)
              query
    (unless (= (SQLFetch hstmt) odbc::$SQL_NO_DATA_FOUND)
      (values
       (loop for col-nr from 0 to (- column-count 
                                     (if (eq ignore-columns :last) 2 1))
             collect
             (let ((precision (aref column-precisions col-nr))
                   (sql-type (aref column-sql-types col-nr)))
               (cond ((or (< 0 precision +max-precision+)
                          (and (zerop precision) (not (find sql-type '($SQL_C_CHAR)))))
                      (read-data (aref column-data-ptrs col-nr) 
                                 (aref column-c-types col-nr)
                                 sql-type
                                 (aref column-out-len-ptrs col-nr)
                                 nil))
                     ((zerop (get-cast-long (aref column-out-len-ptrs col-nr)))
                      *null*)
                     (t
                      (read-data-in-chunks hstmt col-nr
                                           (aref column-data-ptrs col-nr) 
                                           (aref column-c-types col-nr)
                                           (aref column-sql-types col-nr)
                                           (aref column-out-len-ptrs col-nr)
                                           nil)))))
       t))))

(defmethod db-map-query ((database odbc-db) type function query-exp)
  (db-map-query (get-free-query database) type function query-exp))

(defmethod db-map-query ((query odbc-query) type function query-exp)
  (declare (ignore type)) ; preliminary. Do a type coersion here
  (%db-execute query (sql-expression query-exp))
  (unwind-protect
    (progn
      (%initialize-query query)
      ;; the main loop
      (loop for data = (%read-query-data query nil)
            while data
            do (apply function data)))
    ;; dispose of memory and set query inactive or get rid of it
    (db-close-query query)))

(defmethod db-map-bind-query ((query odbc-query) type function 
                                 &rest parameters)
  (declare (ignore type)) ; preliminary. Do a type coersion here
  (unwind-protect
    (progn
      (apply #'%db-bind-execute query parameters)
      ;; the main loop
      (loop with data
            while (setf data (%read-query-data query nil))
            do (apply function data)))
    ;; dispose of memory and set query inactive or get rid of it
    (%db-reset-query query)))

;; does not always return exactly a lisp type...
(defun sql-to-lisp-type (sql-type)
  (ecase sql-type
    ((#.odbc::$SQL_CHAR #.odbc::$SQL_VARCHAR #.odbc::$SQL_LONGVARCHAR) :string)
    ((#.odbc::$SQL_NUMERIC #.odbc::$SQL_DECIMAL #.odbc::$SQL_BIGINT) :string) ; ??
    (#.odbc::$SQL_INTEGER :long)
    (#.odbc::$SQL_SMALLINT :short)
    ((#.odbc::$SQL_FLOAT #.odbc::$SQL_DOUBLE) :long)
    (#.odbc::$SQL_REAL :long)
    (#.odbc::$SQL_DATE 'sql-c-date)
    (#.odbc::$SQL_TIME 'sql-c-time)
    (#.odbc::$SQL_TIMESTAMP 'sql-c-timestamp)
    ;;((#.odbc::$SQL_BINARY #.odbc::$SQL_VARBINARY #.odbc::$SQL_LONGVARBINARY) odbc::$SQL_C_BINARY) ; ??
    (#.odbc::$SQL_TINYINT :short)
    ;;(#.odbc::$SQL_BIT odbc::$SQL_C_BIT) ; ??
    ((#.odbc::$SQL_VARBINARY #.odbc::$SQL_LONGVARBINARY) :binary)
    ))

;; prepared queries

(defmethod db-prepare-statement ((database odbc-db) sql
                                     &key parameter-table parameter-columns)
  (with-slots (hdbc) database
    (let ((query (get-free-query database))) 
      (with-slots (hstmt) query
        (unless hstmt (setf hstmt (%new-statement-handle hdbc))))
      (db-prepare-statement query sql parameter-table parameter-columns))))

(defmethod db-prepare-statement ((query odbc-query) (sql string)
                                     &key parameter-table parameter-columns)
  ;; this is a workaround to get hold of the column types when the driver does not
  ;; support SQLDescribeParam. To do: put code in here for drivers that do
  ;; support it.
  (unless (string-equal sql "insert" :end1 6)
    (error "Only insert expressions are supported in literal ODBC: '~a'." sql))
  (%db-execute query (format nil "select ~{~a~^,~} from ~a where 0 = 1"
                             (or parameter-columns '("*")) parameter-table))
  (%initialize-query query)
  (with-slots (hstmt) query
    (%free-statement hstmt :unbind)
    (%free-statement hstmt :reset)
    (setf (sql-expression query) sql)
    (%sql-prepare hstmt sql))
  query)


(defmethod %db-bind-execute ((query odbc-query) &rest parameters)
  (with-slots (hstmt parameter-data-ptrs) query
    (loop for parameter in parameters
          with data-ptr and size and parameter-string
          do
          (setf parameter-string
                (if (stringp parameter)
                  parameter
                  (write-to-string parameter))
           size (length parameter-string)
                data-ptr 
                (uffi:allocate-foreign-string (1+ size)))
          (vector-push-extend data-ptr parameter-data-ptrs)
          (%sql-bind-parameter 
           hstmt (1- (fill-pointer parameter-data-ptrs)) odbc::$SQL_PARAM_INPUT
           odbc::$SQL_C_CHAR ; (aref column-c-types parameter-count)
           odbc::$SQL_CHAR ; sql-type
           +max-precision+ ;precision ; this should be the actual precision!
           ;; scale
           0 ;; should be calculated for odbc::$SQL_DECIMAL,
           ;;$SQL_NUMERIC and odbc::$SQL_TIMESTAMP
           data-ptr ;; = rgbValue
           0
           ;; *pcbValue;
           ;; change this for output and binary input! (see 3-32)
           (%null-ptr))
          (%put-str data-ptr parameter-string size))
        (%sql-execute hstmt)))


(defmethod %db-reset-query ((query odbc-query))
  (with-slots (hstmt parameter-data-ptrs) query
    (prog1
      (db-fetch-query-results query nil
                              nil) 
      (%free-statement hstmt :reset) ;; but _not_ :unbind !
      (%free-statement hstmt :close)
      (dotimes (param-nr (fill-pointer parameter-data-ptrs))
        (let ((data-ptr (aref parameter-data-ptrs param-nr)))
          (when data-ptr (uffi:free-foreign-object data-ptr))))
      (setf (fill-pointer parameter-data-ptrs) 0))))

(defun data-parameter-ptr (hstmt)
  (uffi:with-foreign-object (param-ptr (* :pointer-void))
    (let ((return-code (%sql-param-data hstmt param-ptr)))
      ;;(format t "~%return-code from %sql-param-data: ~a~%" return-code)
      (when (= return-code odbc::$SQL_NEED_DATA)
        ;;(ffc::%pointer-to-address (%get-ptr param-ptr))
        (uffi:deref-pointer param-ptr :pointer-void)))))

;; database inquiery functions

(defmethod db-describe-columns ((database odbc-db) 
                                    table-qualifier table-owner table-name column-name)
  (with-slots (hdbc) database
    (%describe-columns hdbc table-qualifier table-owner table-name column-name)))

;; should translate info-type integers to keywords in order to make this 
;; more readable?
(defmethod get-odbc-info ((database odbc-db) info-type)
  (with-slots (hdbc info) database
    (or (gethash info-type info)
        (setf (gethash info-type info)
              (%sql-get-info hdbc info-type)))))

(defmethod get-odbc-info ((query odbc-query) info-type)
  (get-odbc-info (odbc::query-database query) info-type))

;; driver inquiery
(defmethod db-data-sources ((db-type (eql :odbc)))
   "Returns a list of (data-source description) - pairs"
   (let ((henv (%new-environment-handle)))
    (unwind-protect
          (loop with direction = :first
               for data-source+description 
               = (multiple-value-list (%sql-data-sources henv :direction direction))
               while (car data-source+description)
               collect data-source+description
               do (setf direction :next))
      (%sql-free-environment henv))))

; EOF
