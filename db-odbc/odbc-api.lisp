;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: odbc -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:     odbc-ff-interface.lisp
;;;; Purpose:  Function definitions for UFFI interface to ODBC
;;;; Author:   Kevin M. Rosenberg, Paul Meurer
;;;;
;;;; $Id: odbc-package.lisp 7061 2003-09-07 06:34:45Z kevin $
;;;;
;;;; This file, part of CLSQL, is Copyright (c) 2004 by Kevin M. Rosenberg
;;;; and Copyright (C) Paul Meurer 1999 - 2001. All rights reserved.
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(in-package #:odbc)

(defvar *null* (make-null-pointer :byte))
(defvar *binary-format* :unsigned-byte-vector)
(defvar *time-conversion-function* 'identity)
(defvar *trace-sql* nil)

(defun %null-ptr ()
  (make-null-pointer :byte))

(defmacro %put-str (ptr string &optional max-length)
  (let ((size (gensym)))
    `(let ((,size (length ,string)))
       (when (and ,max-length (> ,size ,max-length))
         (error "string \"~a\" of length ~d is longer than max-length: ~d"
                ,string ,size ,max-length))
       (dotimes (i ,size)
         (setf (deref-array ,ptr '(:array :unsigned-char) i) (char ,string i)))
       (setf (deref-array ,ptr '(:array :unsigned-char) ,size) 0))))

(defun %cstring-into-vector (ptr vector offset size-in-bytes)
    (dotimes (i size-in-bytes)
      (setf (aref vector offset)
            (deref-array ptr '(:array :unsigned-char) i))
      (incf offset))
    offset)
  
(defun handle-error (henv hdbc hstmt)
  (with-foreign-objects ((sql-state '(:array :unsigned-char 256))
			 (error-message '(:array :unsigned-char
					  #.$SQL_MAX_MESSAGE_LENGTH))
			 (error-code :long)
			 (msg-length :short))
    (SQLError henv hdbc hstmt sql-state
              error-code error-message
	      $SQL_MAX_MESSAGE_LENGTH msg-length)
    (values
     (convert-from-foreign-string error-message)
     (convert-from-foreign-string sql-state)
     (deref-pointer msg-length :short) 
     (deref-pointer error-code :long))))

; test this: return a keyword for efficiency
(defun sql-state (henv hdbc hstmt)
  (with-foreign-objects ((sql-state '(:array :unsigned-char 256))
			 (error-message '(:array :unsigned-char
					  #.$SQL_MAX_MESSAGE_LENGTH))
			 (error-code :long)
			 (msg-length :short))
    (SQLError henv hdbc hstmt sql-state error-code
	      error-message $SQL_MAX_MESSAGE_LENGTH msg-length)
    (convert-from-foreign-string sql-state) ;(%cstring-to-keyword sql-state)
    ))

(defmacro with-error-handling ((&key henv hdbc hstmt (print-info t))
                                   odbc-call &body body)
  (let ((result-code (gensym "RC-")))
    `(let ((,result-code ,odbc-call))
       (case ,result-code
         (#.$SQL_SUCCESS
          (progn ,result-code ,@body))
         (#.$SQL_SUCCESS_WITH_INFO
          (when ,print-info
            (multiple-value-bind (error-message sql-state)
                                 (handle-error (or ,henv (%null-ptr))
                                               (or ,hdbc (%null-ptr))
                                               (or ,hstmt (%null-ptr)))
              (warn "[ODBC info] ~a state: ~a"
		    ,result-code error-message
		    sql-state)))
          (progn ,result-code ,@body))
         (#.$SQL_INVALID_HANDLE
          (error "[ODBC error] Invalid handle"))
         (#.$SQL_STILL_EXECUTING
          (error "[ODBC error] Still executing"))
         (#.$SQL_ERROR
          (multiple-value-bind (error-message sql-state)
                               (handle-error (or ,henv (%null-ptr))
                                             (or ,hdbc (%null-ptr))
                                             (or ,hstmt (%null-ptr)))
            (error "[ODBC error] ~a; state: ~a" error-message sql-state)))
         (otherwise
          (progn ,result-code ,@body))))))

(defun %new-environment-handle ()
  (with-foreign-object (phenv 'sql-handle-ptr)
    (with-error-handling
	()
      (SQLAllocEnv phenv)
      (deref-pointer phenv 'sql-handle-ptr))))

(defun %sql-free-environment (henv)
  (with-error-handling 
    (:henv henv)
    (SQLFreeEnv henv)))

(defun %new-db-connection-handle (henv)
  (with-foreign-object (phdbc 'sql-handle-ptr)
    (with-error-handling
      (:henv henv)
      (SQLAllocConnect henv phdbc)
      (deref-pointer phdbc 'sql-handle-ptr))))

(defun %free-statement (hstmt option)
  (with-error-handling 
      (:hstmt hstmt)
      (SQLFreeStmt 
       hstmt 
       (ecase option
         (:drop $SQL_DROP)
         (:close $SQL_CLOSE)
         (:unbind $SQL_UNBIND)
         (:reset $SQL_RESET_PARAMS)))))

(defmacro with-statement-handle ((hstmt hdbc) &body body)
  `(let ((,hstmt (%new-statement-handle ,hdbc)))
     (unwind-protect
       (progn ,@body)
       (%free-statement ,hstmt :drop))))

;; functional interface

(defun %sql-connect (hdbc server uid pwd)
  (with-cstrings ((server-ptr server)
		  (uid-ptr uid)
		  (pwd-ptr pwd))
    (with-error-handling 
	(:hdbc hdbc)
      (SQLConnect hdbc server-ptr $SQL_NTS uid-ptr 
		  $SQL_NTS pwd-ptr $SQL_NTS))))


(defun %disconnect (hdbc)
  (with-error-handling 
    (:hdbc hdbc)
    (SQLDisconnect hdbc)))

(defun %commit (henv hdbc)
  (with-error-handling 
    (:henv henv :hdbc hdbc)
    (SQLTransact 
     henv hdbc $SQL_COMMIT)))

(defun %rollback (henv hdbc)
  (with-error-handling 
    (:henv henv :hdbc hdbc)
    (SQLTransact 
     henv hdbc $SQL_ROLLBACK)))

; col-nr is zero-based in Lisp
; col-nr = :bookmark retrieves a bookmark.
(defun %bind-column (hstmt column-nr c-type data-ptr precision out-len-ptr)
  (with-error-handling
    (:hstmt hstmt)
    (SQLBindCol hstmt
                (if (eq column-nr :bookmark) 0 (1+ column-nr))
                c-type data-ptr precision out-len-ptr)))

; parameter-nr is zero-based in Lisp
(defun %sql-bind-parameter (hstmt parameter-nr parameter-type c-type
                                      sql-type precision scale data-ptr
                                      max-value out-len-ptr)
  (with-error-handling
    (:hstmt hstmt)
    (SQLBindParameter hstmt (1+ parameter-nr)
                      parameter-type ;$SQL_PARAM_INPUT 
                      c-type ;$SQL_C_CHAR
                      sql-type ;$SQL_VARCHAR
                      precision ;(1- (length str))
                      scale ;0
                      data-ptr
                      max-value
                      out-len-ptr ;#.(%null-ptr)
                      )))

(defun %sql-fetch (hstmt)
  (with-error-handling 
      (:hstmt hstmt)
      (SQLFetch hstmt)))

(defun %new-statement-handle (hdbc)
  (with-foreign-object (hstmt-ptr 'sql-handle-ptr)
    (with-error-handling 
      (:hdbc hdbc)
      (SQLAllocStmt hdbc hstmt-ptr) 
      (deref-pointer hstmt-ptr 'sql-handle-ptr))))

(defun %sql-get-info (hdbc info-type)
  (ecase info-type
    ;; those return string
    ((#.$SQL_ACCESSIBLE_PROCEDURES
      #.$SQL_ACCESSIBLE_TABLES
      #.$SQL_COLUMN_ALIAS
      #.$SQL_DATA_SOURCE_NAME
      #.$SQL_DATA_SOURCE_READ_ONLY
      #.$SQL_DBMS_NAME
      #.$SQL_DBMS_VER
      #.$SQL_DRIVER_NAME
      #.$SQL_DRIVER_ODBC_VER
      #.$SQL_DRIVER_VER
      #.$SQL_EXPRESSIONS_IN_ORDERBY
      #.$SQL_IDENTIFIER_QUOTE_CHAR
      #.$SQL_KEYWORDS
      #.$SQL_LIKE_ESCAPE_CLAUSE
      #.$SQL_MAX_ROW_SIZE_INCLUDES_LONG
      #.$SQL_MULT_RESULT_SETS
      #.$SQL_MULTIPLE_ACTIVE_TXN
      #.$SQL_NEED_LONG_DATA_LEN
      #.$SQL_ODBC_SQL_OPT_IEF
      #.$SQL_ODBC_VER
      #.$SQL_ORDER_BY_COLUMNS_IN_SELECT
      #.$SQL_OUTER_JOINS
      #.$SQL_OWNER_TERM
      #.$SQL_PROCEDURE_TERM
      #.$SQL_PROCEDURES
      #.$SQL_QUALIFIER_NAME_SEPARATOR
      #.$SQL_QUALIFIER_TERM
      #.$SQL_ROW_UPDATES
      #.$SQL_SEARCH_PATTERN_ESCAPE
      #.$SQL_SERVER_NAME
      #.$SQL_SPECIAL_CHARACTERS
      #.$SQL_TABLE_TERM
      #.$SQL_USER_NAME)
     (with-foreign-objects ((info-ptr '(:array :unsigned-char 1024))
			    (info-length-ptr :short))
       (with-error-handling 
         (:hdbc hdbc)
	 #-pcl
         (SQLGetInfo hdbc info-type info-ptr 1023 info-length-ptr)
	 #+pcl
         (SQLGetInfo-Str hdbc info-type info-ptr 1023 info-length-ptr)
         (convert-from-foreign-string info-ptr))))
    ;; those returning a word
    ((#.$SQL_ACTIVE_CONNECTIONS
      #.$SQL_ACTIVE_STATEMENTS
      #.$SQL_CONCAT_NULL_BEHAVIOR
      #.$SQL_CORRELATION_NAME
      #.$SQL_CURSOR_COMMIT_BEHAVIOR
      #.$SQL_CURSOR_ROLLBACK_BEHAVIOR
      #.$SQL_MAX_COLUMN_NAME_LEN
      #.$SQL_MAX_COLUMNS_IN_GROUP_BY
      #.$SQL_MAX_COLUMNS_IN_INDEX
      #.$SQL_MAX_COLUMNS_IN_ORDER_BY
      #.$SQL_MAX_COLUMNS_IN_SELECT
      #.$SQL_MAX_COLUMNS_IN_TABLE
      #.$SQL_MAX_CURSOR_NAME_LEN
      #.$SQL_MAX_OWNER_NAME_LEN
      #.$SQL_MAX_PROCEDURE_NAME_LEN
      #.$SQL_MAX_QUALIFIER_NAME_LEN
      #.$SQL_MAX_TABLE_NAME_LEN
      #.$SQL_MAX_TABLES_IN_SELECT
      #.$SQL_MAX_USER_NAME_LEN
      #.$SQL_NON_NULLABLE_COLUMNS
      #.$SQL_NULL_COLLATION
      #.$SQL_ODBC_API_CONFORMANCE
      #.$SQL_ODBC_SAG_CLI_CONFORMANCE
      #.$SQL_ODBC_SQL_CONFORMANCE
      #.$SQL_QUALIFIER_LOCATION
      #.$SQL_QUOTED_IDENTIFIER_CASE
      #.$SQL_TXN_CAPABLE)
     (with-foreign-objects ((info-ptr :short)
			    (info-length-ptr :short))
       (with-error-handling 
	(:hdbc hdbc)
         (SQLGetInfo hdbc
		     info-type
		     info-ptr
		     255
		     info-length-ptr)
         (deref-pointer info-ptr :short)))
     )
    ;; those returning a long bitmask
    ((#.$SQL_ALTER_TABLE 
      #.$SQL_BOOKMARK_PERSISTENCE
      #.$SQL_CONVERT_BIGINT
      #.$SQL_CONVERT_BINARY
      #.$SQL_CONVERT_BIT
      #.$SQL_CONVERT_CHAR
      #.$SQL_CONVERT_DATE
      #.$SQL_CONVERT_DECIMAL
      #.$SQL_CONVERT_DOUBLE
      #.$SQL_CONVERT_FLOAT
      #.$SQL_CONVERT_INTEGER
      #.$SQL_CONVERT_LONGVARCHAR
      #.$SQL_CONVERT_NUMERIC
      #.$SQL_CONVERT_REAL
      #.$SQL_CONVERT_SMALLINT
      #.$SQL_CONVERT_TIME
      #.$SQL_CONVERT_TIMESTAMP
      #.$SQL_CONVERT_TINYINT
      #.$SQL_CONVERT_VARBINARY
      #.$SQL_CONVERT_VARCHAR
      #.$SQL_CONVERT_LONGVARBINARY
      #.$SQL_CONVERT_FUNCTIONS
      #.$SQL_FETCH_DIRECTION
      #.$SQL_FILE_USAGE
      #.$SQL_GETDATA_EXTENSIONS
      #.$SQL_LOCK_TYPES
      #.$SQL_MAX_INDEX_SIZE
      #.$SQL_MAX_ROW_SIZE
      #.$SQL_MAX_STATEMENT_LEN
      #.$SQL_NUMERIC_FUNCTIONS
      #.$SQL_OWNER_USAGE
      #.$SQL_POS_OPERATIONS
      #.$SQL_POSITIONED_STATEMENTS
      #.$SQL_QUALIFIER_USAGE
      #.$SQL_SCROLL_CONCURRENCY
      #.$SQL_SCROLL_OPTIONS
      #.$SQL_STATIC_SENSITIVITY
      #.$SQL_STRING_FUNCTIONS
      #.$SQL_SUBQUERIES
      #.$SQL_SYSTEM_FUNCTIONS
      #.$SQL_TIMEDATE_ADD_INTERVALS
      #.$SQL_TIMEDATE_DIFF_INTERVALS
      #.$SQL_TIMEDATE_FUNCTIONS
      #.$SQL_TXN_ISOLATION_OPTION
      #.$SQL_UNION)
     (with-foreign-objects ((info-ptr :long)
			    (info-length-ptr :short))
       (with-error-handling 
         (:hdbc hdbc)
         (SQLGetInfo hdbc
		     info-type
		     info-ptr
		     255
		     info-length-ptr)
         (deref-pointer info-ptr :long)))
     )
    ;; those returning a long integer
    ((#.$SQL_DEFAULT_TXN_ISOLATION
      #.$SQL_DRIVER_HDBC
      #.$SQL_DRIVER_HENV
      #.$SQL_DRIVER_HLIB
      #.$SQL_DRIVER_HSTMT
      #.$SQL_GROUP_BY
      #.$SQL_IDENTIFIER_CASE
      #.$SQL_MAX_BINARY_LITERAL_LEN
      #.$SQL_MAX_CHAR_LITERAL_LEN
      #.$SQL_ACTIVE_ENVIRONMENTS
      )
     (with-foreign-objects ((info-ptr :long)
			    (info-length-ptr :short))
       (with-error-handling 
         (:hdbc hdbc)
         (SQLGetInfo hdbc info-type info-ptr 255 info-length-ptr)
         (deref-pointer info-ptr :long))))))
     
(defun %sql-exec-direct (sql hstmt henv hdbc)
  (with-cstring (sql-ptr sql)
    (with-error-handling
      (:hstmt hstmt :henv henv :hdbc hdbc)
      (SQLExecDirect hstmt sql-ptr $SQL_NTS))))

(defun %sql-cancel (hstmt)
  (with-error-handling
    (:hstmt hstmt)
    (SQLCancel hstmt)))

(defun %sql-execute (hstmt)
  (with-error-handling
    (:hstmt hstmt)
    (SQLExecute hstmt)))

(defun result-columns-count (hstmt)
  (with-foreign-objects ((columns-nr-ptr :short))
    (with-error-handling (:hstmt hstmt)
                         (SQLNumResultCols hstmt columns-nr-ptr)
      (deref-pointer columns-nr-ptr :short))))

(defun result-rows-count (hstmt)
  (with-foreign-objects ((row-count-ptr :long))
    (with-error-handling (:hstmt hstmt)
                         (SQLRowCount hstmt row-count-ptr)
      (deref-pointer row-count-ptr :long))))

;; column counting is 1-based
(defun %describe-column (hstmt column-nr)
  (with-foreign-objects ((column-name-ptr '(:array :unsigned-char 256))
			 (column-name-length-ptr :short)
			 (column-sql-type-ptr :short)
			 (column-precision-ptr :long)
			 (column-scale-ptr :short)
			 (column-nullable-p-ptr :short))
    (with-error-handling (:hstmt hstmt)
                         (SQLDescribeCol hstmt column-nr column-name-ptr 256
                                         column-name-length-ptr
					 column-sql-type-ptr
                                         column-precision-ptr
					 column-scale-ptr
                                         column-nullable-p-ptr)
      (values
       (convert-from-foreign-string column-name-ptr)
       (deref-pointer column-sql-type-ptr :short)
       (deref-pointer column-precision-ptr :long)
       (deref-pointer column-scale-ptr :short)
       (deref-pointer column-nullable-p-ptr :short)))))

;; parameter counting is 1-based
(defun %describe-parameter (hstmt parameter-nr)
  (with-foreign-objects ((column-sql-type-ptr :short)
			 (column-precision-ptr :long)
			 (column-scale-ptr :short)
			 (column-nullable-p-ptr :short))
    (with-error-handling 
      (:hstmt hstmt)
      (SQLDescribeParam hstmt parameter-nr
			column-sql-type-ptr
                        column-precision-ptr
			column-scale-ptr
                        column-nullable-p-ptr)
      (values
       (deref-pointer column-sql-type-ptr :short)
       (deref-pointer column-precision-ptr :long)
       (deref-pointer column-scale-ptr :short)
       (deref-pointer column-nullable-p-ptr :short)))))

(defun %column-attributes (hstmt column-nr descriptor-type)
  (with-foreign-objects ((descriptor-info-ptr '(:array :unsigned-char 256))
			 (descriptor-length-ptr :short)
			 (numeric-descriptor-ptr :long))
    (with-error-handling
      (:hstmt hstmt) 
      (SQLColAttributes hstmt column-nr descriptor-type descriptor-info-ptr 256
                        descriptor-length-ptr
			numeric-descriptor-ptr)
      (values
       (convert-from-foreign-string descriptor-info-ptr)
       (deref-pointer numeric-descriptor-ptr :long)))))

(defun %prepare-describe-columns (hstmt table-qualifier table-owner 
                                   table-name column-name)
  (with-cstrings ((table-qualifier-ptr table-qualifier)
		  (table-owner-ptr table-owner) 
		  (table-name-ptr table-name)
		  (column-name-ptr column-name))
    (with-error-handling
	(:hstmt hstmt) 
      (SQLColumns hstmt
		  table-qualifier-ptr (length table-qualifier)
		  table-owner-ptr (length table-owner)
		  table-name-ptr (length table-name)
		  column-name-ptr (length column-name)))))

(defun %describe-columns (hdbc table-qualifier table-owner 
			  table-name column-name)
  (with-statement-handle (hstmt hdbc)
    (%prepare-describe-columns hstmt table-qualifier table-owner 
                               table-name column-name)
    (fetch-all-rows hstmt)))

(defun %sql-data-sources (henv &key (direction :first))
  (with-foreign-objects 
   ((name-ptr '(:array :unsigned-char #.(1+ $SQL_MAX_DSN_LENGTH)))
    (name-length-ptr :short)
    (description-ptr '(:array :unsigned-char 1024))
    (description-length-ptr :short))
    (let ((res (with-error-handling
                 (:henv henv)
                 (SQLDataSources henv
                                 (ecase direction
				   (:first $SQL_FETCH_FIRST)
				   (:next $SQL_FETCH_NEXT))
                                 name-ptr
                                 (1+ $SQL_MAX_DSN_LENGTH)
                                 name-length-ptr
                                 description-ptr
                                 1024
                                 description-length-ptr))))
      (unless (= res $SQL_NO_DATA_FOUND)
        (values (convert-from-foreign-string name-ptr)
                (convert-from-foreign-string description-ptr))))))

(defun sql-to-c-type (sql-type)
  (ecase sql-type
    ((#.$SQL_CHAR #.$SQL_VARCHAR #.$SQL_LONGVARCHAR 
      #.$SQL_NUMERIC #.$SQL_DECIMAL #.$SQL_BIGINT -8 -9) $SQL_C_CHAR)
    (#.$SQL_INTEGER $SQL_C_SLONG)
    (#.$SQL_SMALLINT $SQL_C_SSHORT)
    ((#.$SQL_FLOAT #.$SQL_DOUBLE) $SQL_C_DOUBLE)
    (#.$SQL_REAL $SQL_C_FLOAT)
    (#.$SQL_DATE $SQL_C_DATE)
    (#.$SQL_TIME $SQL_C_TIME)
    (#.$SQL_TIMESTAMP $SQL_C_TIMESTAMP)
    ((#.$SQL_BINARY #.$SQL_VARBINARY #.$SQL_LONGVARBINARY) $SQL_C_BINARY)
    (#.$SQL_TINYINT $SQL_C_STINYINT)
    (#.$SQL_BIT $SQL_C_BIT)))

(defun get-cast-byte (ptr)
  (declare (type long-ptr-type out-len-ptr))
  (with-cast-pointer (casted ptr '(* :byte))
    (deref-pointer casted :byte)))

(defun get-cast-short (ptr)
  (declare (type long-ptr-type out-len-ptr))
  (with-cast-pointer (casted ptr '(* :short))
    (deref-pointer casted :short)))

(defun get-cast-int (ptr)
  (declare (type long-ptr-type out-len-ptr))
  (with-cast-pointer (casted ptr '(* :int))
    (deref-pointer casted :int)))

(defun get-cast-long (ptr)
  (declare (type long-ptr-type out-len-ptr))
  (with-cast-pointer (casted ptr '(* :long))
    (deref-pointer casted :long)))

(defun get-cast-single-float (ptr)
  (declare (type long-ptr-type out-len-ptr))
  (with-cast-pointer (casted ptr '(* :float))
    (deref-pointer casted :float)))

(defun get-cast-double-float (ptr)
  (declare (type long-ptr-type out-len-ptr))
  (with-cast-pointer (casted ptr '(* :double))
    (deref-pointer casted :double)))

(defun get-cast-foreign-string (ptr)
  (declare (type long-ptr-type out-len-ptr))
  (with-cast-pointer (casted ptr '(* :unsigned-char))
    (convert-from-foreign-string casted)))

(defun get-cast-binary (ptr len format)
  "FORMAT is one of :unsigned-byte-vector, :bit-vector (:string, :hex-string)"
  (with-cast-pointer (casted ptr '(* :byte))
    (ecase format
      (:unsigned-byte-vector
       (let ((vector (make-array len :element-type '(unsigned-byte 8))))
	 (dotimes (i len)
	   (setf (aref vector i)
		 (deref-array casted '(:array :byte) i)))
	 vector))
      (:bit-vector
       (let ((vector (make-array (ash len 3) :element-type 'bit)))
	 (dotimes (i len)
	   (let ((byte (deref-array casted '(:array :byte) i)))
	     (dotimes (j 8)
	       (setf (bit vector (+ (ash i 3) j)) (logand (ash byte (- j 7)) 1)))))
	 vector)))))


(defun read-data (data-ptr c-type sql-type out-len-ptr convert-to-string-p)
  (declare (type long-ptr-type out-len-ptr))
  (let ((out-len (deref-pointer out-len-ptr :long)))
    (cond ((= out-len $SQL_NULL_DATA)
           *null*)
          ;; obsolete?
          (convert-to-string-p
           (convert-from-foreign-string data-ptr))
          (t
           (case sql-type
             ;; SQL extended datatypes
             (#.$SQL_TINYINT  (get-cast-short data-ptr))
             (#.$SQL_C_STINYINT (get-cast-short data-ptr)) ;; ?
             (#.$SQL_C_SSHORT (get-cast-short data-ptr)) ;; ?
             (#.$SQL_SMALLINT (deref-pointer data-ptr :short)) ; ??
             (#.$SQL_INTEGER (deref-pointer data-ptr :long))
             (#.$SQL_DECIMAL 
              (let ((*read-base* 10))
                (read-from-string (get-cast-foreign-string data-ptr))))
             (t 
              (case c-type
                (#.$SQL_C_DATE
                 (funcall *time-conversion-function* (date-to-universal-time data-ptr)))
                (#.$SQL_C_TIME
                 (multiple-value-bind (universal-time frac) (time-to-universal-time data-ptr)
                   (funcall *time-conversion-function* universal-time frac)))
                (#.$SQL_C_TIMESTAMP
                 (multiple-value-bind (universal-time frac) (timestamp-to-universal-time data-ptr)
                   (funcall *time-conversion-function* universal-time frac)))
                (#.$SQL_INTEGER
                 (get-cast-int data-ptr))
                (#.$SQL_C_FLOAT
                 (get-cast-single-float data-ptr))
                (#.$SQL_C_DOUBLE
                 (get-cast-double-float data-ptr))
                (#.$SQL_C_SLONG
                 (get-cast-long data-ptr))
                #+lispworks
                (#.$SQL_C_BIT ; encountered only in Access
                 (get-cast-byte data-ptr))
                (#.$SQL_C_BINARY
                 (get-cast-binary data-ptr out-len *binary-format*))
                ((#.$SQL_C_SSHORT #.$SQL_C_STINYINT) ; LMH short ints
                 (get-cast-short data-ptr))	; LMH
                #+ignore
                (#.$SQL_C_CHAR
                 (code-char (get-cast-short data-ptr)))
                (t
                 (convert-from-foreign-string data-ptr)))))))))

;; which value is appropriate?
(defparameter +max-precision+ 
  #+mcl 512
  #-mcl 4001)

(defvar *break-on-unknown-data-type* t)

;; C. Stacy's idea to factor this out
;; "Make it easy to add new datatypes by making new subroutine %ALLOCATE-BINDINGS,
;; so that I don't have to remember to make changes in more than one place.
;; Just keep it in synch with READ-DATA."
(defun %allocate-bindings (sql-type precision)
  (let* ((c-type (sql-to-c-type sql-type))
         (size (if (zerop precision)
                   +max-precision+ ;; if the precision cannot be determined
                 (min precision +max-precision+)))
         (long-p (= size +max-precision+))
         (data-ptr
          (case c-type ;; add more?
            (#.$SQL_C_SLONG (uffi:allocate-foreign-object :long))
            (#.$SQL_C_DOUBLE (uffi:allocate-foreign-object :double))
            (#.$SQL_C_DATE (allocate-foreign-object 'sql-c-date))
            (#.$SQL_C_TIME (allocate-foreign-object 'sql-c-time))
            (#.$SQL_C_TIMESTAMP (allocate-foreign-object 'sql-c-timestamp))
            #+lispworks(#.$SQL_C_FLOAT (uffi:allocate-foreign-object :float))
            (#.$SQL_C_BIT (uffi:allocate-foreign-object :boolean))
            (#.$SQL_C_STINYINT (uffi:allocate-foreign-object :byte))
            (#.$SQL_C_SSHORT (uffi:allocate-foreign-object :short))
            (#.$SQL_C_CHAR (uffi:allocate-foreign-string (1+ size)))
            (#.$SQL_C_BINARY (uffi:allocate-foreign-string (1+ (* 2 size))))
            (t 
                ;; Maybe should signal a restartable condition for this?
                (when *break-on-unknown-data-type*
                  (break "SQL type is ~A, precision ~D, size ~D, C type is ~A" 
                         sql-type precision size c-type))
                (uffi:allocate-foreign-object :ptr (1+ size)))))
         (out-len-ptr (uffi:allocate-foreign-object :long)))
    (values c-type data-ptr out-len-ptr size long-p)))

(defun fetch-all-rows (hstmt &key free-option flatp)
  (let ((column-count (result-columns-count hstmt)))
    (unless (zerop column-count)
      (let ((names (make-array column-count :element-type 'string))
            (sql-types (make-array column-count :element-type 'fixnum))
            (c-types (make-array column-count :element-type 'fixnum))
            (precisions (make-array column-count :element-type 'fixnum))
            (data-ptrs (make-array column-count :initial-element nil))
            (out-len-ptrs (make-array column-count :initial-element nil))
            (scales (make-array column-count :element-type 'fixnum))
            (nullables-p (make-array column-count :element-type 'fixnum)))
        (unwind-protect
          (values
           (progn
             (dotimes (col-nr column-count)
               ;; get column information
               (multiple-value-bind (name sql-type precision scale nullable-p)
                                    (%describe-column hstmt (1+ col-nr))
                 ;; allocate space to bind result rows to
                 (multiple-value-bind (c-type data-ptr out-len-ptr)
                     (%allocate-bindings sql-type precision)
                   (%bind-column hstmt col-nr c-type data-ptr (1+ precision) out-len-ptr)
                   (setf (svref names col-nr) name
                         (aref sql-types col-nr) sql-type
                         (aref c-types col-nr) (sql-to-c-type sql-type)
                         (aref precisions col-nr) (if (zerop precision) nil precision)
                         (aref scales col-nr) scale
                         (aref nullables-p col-nr) nullable-p
                         (aref data-ptrs col-nr) data-ptr
                         (aref out-len-ptrs col-nr) out-len-ptr))))
             ;; the main loop
             (prog1
               (cond (flatp 
                      (when (> column-count 1)
                        (error "If more than one column is to be fetched, flatp has to be nil."))
                      (loop until (= (%sql-fetch hstmt) $SQL_NO_DATA_FOUND)
                            collect
                            (read-data (aref data-ptrs 0) 
                                       (aref c-types 0)
                                       (aref sql-types 0)
                                       (aref out-len-ptrs 0)
                                       nil)))
                     (t
                      (loop until (= (%sql-fetch hstmt) $SQL_NO_DATA_FOUND)
                            collect
                            (loop for col-nr from 0 to (1- column-count)
                                  collect
                                  (read-data (aref data-ptrs col-nr) 
                                             (aref c-types col-nr)
                                             (aref sql-types col-nr)
                                             (aref out-len-ptrs col-nr)
                                             nil)))))))
           names)
          ;; dispose of memory etc
          (when free-option (%free-statement hstmt free-option))
          (dotimes (col-nr column-count)
            (let ((data-ptr (aref data-ptrs col-nr))
                  (out-len-ptr (aref out-len-ptrs col-nr)))
              (when data-ptr (free-foreign-object data-ptr)) ; we *did* allocate them
              (when out-len-ptr (free-foreign-object out-len-ptr)))))))))

;; to do: factor out common parts, put the sceleton (the obligatory macro part)
;; of %do-fetch into sql package (has been done)

(defun %sql-prepare (hstmt sql)
  (with-cstring (sql-ptr sql)
    (with-error-handling (:hstmt hstmt)
      (SQLPrepare hstmt sql-ptr $SQL_NTS))))

;; depending on option, we return a long int or a string; string not implemented
(defun get-connection-option (hdbc option)
  (with-foreign-objects ((param-ptr :long #+ignore #.(1+ $SQL_MAX_OPTION_STRING_LENGTH)))
    (with-error-handling (:hdbc hdbc)
                         (SQLGetConnectOption hdbc option param-ptr)
      (deref-pointer param-ptr :long))))

(defun set-connection-option (hdbc option param)
  (with-error-handling (:hdbc hdbc)
    (SQLSetConnectOption hdbc option param)))

(defun disable-autocommit (hdbc)
  (set-connection-option hdbc $SQL_AUTOCOMMIT $SQL_AUTOCOMMIT_OFF))

(defun enable-autocommit (hdbc)
  (set-connection-option hdbc $SQL_AUTOCOMMIT $SQL_AUTOCOMMIT_ON))

(defun %sql-set-pos (hstmt row option lock)
  (with-error-handling 
    (:hstmt hstmt)
    (SQLSetPos hstmt row option lock)))

(defun %sql-extended-fetch (hstmt fetch-type row)
  (with-foreign-objects ((row-count-ptr :unsigned-long)
			 (row-status-ptr :short))
    (with-error-handling (:hstmt hstmt)
      (SQLExtendedFetch hstmt fetch-type row row-count-ptr
			row-status-ptr)
      (values (deref-pointer row-count-ptr :unsigned-long)
              (deref-pointer row-status-ptr :short)))))

; column-nr is zero-based
(defun %sql-get-data (hstmt column-nr c-type data-ptr precision out-len-ptr)
  (with-error-handling
    (:hstmt hstmt :print-info nil)
    (SQLGetData hstmt (if (eq column-nr :bookmark) 0 (1+ column-nr))
                c-type data-ptr precision out-len-ptr)))

(defun %sql-param-data (hstmt param-ptr)
  (with-error-handling (:hstmt hstmt :print-info t) ;; nil
      (SQLParamData hstmt param-ptr)))

(defun %sql-put-data (hstmt data-ptr size)
  (with-error-handling
    (:hstmt hstmt :print-info t) ;; nil
    (SQLPutData hstmt data-ptr size)))

(defconstant $sql-data-truncated (intern "01004" :keyword))

(defun read-data-in-chunks (hstmt column-nr data-ptr c-type sql-type 
                                      out-len-ptr convert-to-string-p)
  (declare (ignore convert-to-string-p) ; prelimianary
	   (type long-ptr-type out-len-ptr))
  (let* ((res (%sql-get-data hstmt column-nr c-type data-ptr 
                             +max-precision+ out-len-ptr))
         (out-len (deref-pointer out-len-ptr :long))
         (offset 0))
    (case out-len
      (#.$SQL_NULL_DATA
       (return-from read-data-in-chunks *null*))
      (#.$SQL_NO_TOTAL ;; don't know how long it is going to be
       (let ((str (make-array 0 :element-type 'character :adjustable t)))
         (loop do (if (= c-type #.$SQL_CHAR)
                      (let ((data-length (foreign-string-length data-ptr)))
                        (adjust-array str (+ offset data-length)
                                      :initial-element #\?)
                        (setf offset (%cstring-into-vector
                                      data-ptr str 
                                      offset 
                                      data-length)))
                    (error "wrong type. preliminary."))
               while (and (= res $SQL_SUCCESS_WITH_INFO)
                          (equal (sql-state (%null-ptr) (%null-ptr) hstmt)
                                 "01004"))
               do (setf res (%sql-get-data hstmt column-nr c-type data-ptr 
                                           +max-precision+ out-len-ptr)))
         (setf str (coerce str 'string))
         (if (= sql-type $SQL_DECIMAL)
             (let ((*read-base* 10))
               (read-from-string str))
           str)))
      (otherwise
       (let ((str (make-string out-len)))
         (loop do (if (= c-type #.$SQL_CHAR)
                      (setf offset (%cstring-into-vector ;string
                                    data-ptr str 
                                    offset 
                                    (min out-len (1- +max-precision+))))
                    (error "wrong type. preliminary."))
               while 
               (and (= res $SQL_SUCCESS_WITH_INFO)
                    #+ingore(eq (sql-state (%null-ptr) (%null-ptr) hstmt)
                                $sql-data-truncated)
                    (equal (sql-state (%null-ptr) (%null-ptr) hstmt)
                           "01004"))
               do (setf res (%sql-get-data hstmt column-nr c-type data-ptr 
                                           +max-precision+ out-len-ptr)
                        out-len (deref-pointer out-len-ptr :long)))
         (if (= sql-type $SQL_DECIMAL)
             (let ((*read-base* 10))
               (read-from-string str))
           str))))))

(defun timestamp-to-universal-time (ptr)
  (values
   (encode-universal-time 
    (get-slot-value ptr 'sql-c-timestamp 'second)
    (get-slot-value ptr 'sql-c-timestamp 'minute)
    (get-slot-value ptr 'sql-c-timestamp 'hour)
    (get-slot-value ptr 'sql-c-timestamp 'day)
    (get-slot-value ptr 'sql-c-timestamp 'month)
    (get-slot-value ptr 'sql-c-timestamp 'year))
   (get-slot-value ptr 'sql-c-timestamp 'fraction)))

(defun universal-time-to-timestamp (time &optional (fraction 0))
  (multiple-value-bind (sec min hour day month year)
      (decode-universal-time time)
    (with-foreign-object (ptr 'sql-c-timestamp)
      (setf (get-slot-value ptr 'sql-c-timestamp 'second) sec
            (get-slot-value ptr 'sql-c-timestamp 'minute) min
            (get-slot-value ptr 'sql-c-timestamp 'hour) hour
            (get-slot-value ptr 'sql-c-timestamp 'day) day
            (get-slot-value ptr 'sql-c-timestamp 'month) month
            (get-slot-value ptr 'sql-c-timestamp 'year) year
            (get-slot-value ptr 'sql-c-timestamp 'fraction) fraction)
      ptr)))

(defun %put-timestamp (ptr time &optional (fraction 0))
  (multiple-value-bind (sec min hour day month year)
      (decode-universal-time time)
    (setf (get-slot-value ptr 'sql-c-timestamp 'second) sec
          (get-slot-value ptr 'sql-c-timestamp 'minute) min
          (get-slot-value ptr 'sql-c-timestamp 'hour) hour
          (get-slot-value ptr 'sql-c-timestamp 'day) day
          (get-slot-value ptr 'sql-c-timestamp 'month) month
          (get-slot-value ptr 'sql-c-timestamp 'year) year
          (get-slot-value ptr 'sql-c-timestamp 'fraction) fraction)
      ptr))

(defun date-to-universal-time (ptr)
  (encode-universal-time
   0 0 0
   (get-slot-value ptr 'sql-c-timestamp 'day)
   (get-slot-value ptr 'sql-c-timestamp 'month)
   (get-slot-value ptr 'sql-c-timestamp 'year)))

(defun time-to-universal-time (ptr)
  (encode-universal-time 
   (get-slot-value ptr 'sql-c-timestamp 'second)
   (get-slot-value ptr 'sql-c-timestamp 'minute)
   (get-slot-value ptr 'sql-c-timestamp 'hour)
   0 0 0))

