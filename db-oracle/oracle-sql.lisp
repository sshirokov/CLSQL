;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          oracle-sql.lisp
;;;;
;;;; $Id$
;;;;
;;;; This file is part of CLSQL.
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(in-package #:clsql-oracle)

(defmethod database-initialize-database-type
    ((database-type (eql :oracle)))
  t)

;;;; arbitrary parameters, tunable for performance or other reasons

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +errbuf-len+ 512
    "the number of characters that we allocate for an error message buffer")
  (defconstant +n-buf-rows+ 200
    "the number of table rows that we buffer at once when reading a table.
CMUCL has a compiled-in limit on how much C data can be allocated
(through malloc() and friends) at any given time, typically 8 Mb.
Setting this constant to a moderate value should make it less
likely that we'll have to worry about the CMUCL limit."))


(defmacro deref-vp (foreign-object)
  `(uffi:deref-pointer ,foreign-object :pointer-void))

;; constants - from OCI?

(defconstant +var-not-in-list+       1007)
(defconstant +no-data-found+         1403)
(defconstant +null-value-returned+   1405)
(defconstant +field-truncated+       1406)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant SQLT-NUMBER 2)
  (defconstant SQLT-INT 3)
  (defconstant SQLT-STR 5)
  (defconstant SQLT-FLT 4)
  (defconstant SQLT-DATE 12))

;;; Note that despite the suggestive class name (and the way that the
;;; *DEFAULT-DATABASE* variable holds an object of this class), a DB
;;; object is not actually a database but is instead a connection to a
;;; database. Thus, there's no obstacle to having any number of DB
;;; objects referring to the same database.

(uffi:def-type pointer-pointer-void '(* :pointer-void))

(defclass oracle-database (database)    ; was struct db
  ((envhp
    :reader envhp
    :initarg :envhp
    :type pointer-pointer-void
    :documentation
    "OCI environment handle")
   (errhp
    :reader errhp
    :initarg :errhp
    :type pointer-pointer-void
    :documentation
    "OCI error handle")
   (svchp
    :reader svchp
    :initarg :svchp
    :type pointer-pointer-void
    :documentation
    "OCI service context handle")
   (data-source-name
    :initarg :dsn
    :initform nil
    :documentation
    "optional data source name (used only for debugging/printing)")
   (user
    :initarg :user
    :reader user
    :type string
    :documentation
    "the \"user\" value given when data source connection was made")
   (date-format
    :initarg :date-format
    :reader date-format
    :initform "YYYY-MM-DD HH24:MI:SS\"+00\"")
   (date-format-length
    :type number
    :documentation
    "Each database connection can be configured with its own date
output format.  In order to extract date strings from output buffers
holding multiple date strings in fixed-width fields, we need to know
the length of that format.")
   (server-version 
    :type string
    :initarg :server-version
    :reader server-version
    :documentation
    "Version string of Oracle server.")
   (major-version-number
    :type (or null fixnum)
    :initarg :major-version-number
    :reader major-version-number
    :documentation
    "The major version number of Oracle, should be 8, 9, or 10")))


;;; Handle the messy case of return code=+oci-error+, querying the
;;; system for subcodes and reporting them as appropriate. ERRHP and
;;; NULLS-OK are as in the OERR function.

(defun handle-oci-error (&key database nulls-ok)
  (cond (database
         (with-slots (errhp)
	     database
           (uffi:with-foreign-objects ((errbuf '(:array :unsigned-char
						 #.+errbuf-len+))
				       (errcode :long))
	     ;; ensure errbuf empty string
             (setf (uffi:deref-array errbuf '(:array :unsigned-char) 0)
		   (uffi:ensure-char-storable (code-char 0)))

             (setf (uffi:deref-pointer errcode :long) 0)
             (oci-error-get (deref-vp errhp) 1
			    (uffi:make-null-pointer :unsigned-char)
			    errcode errbuf +errbuf-len+ +oci-htype-error+)
             (let ((subcode (uffi:deref-pointer errcode :long)))
               (unless (and nulls-ok (= subcode +null-value-returned+))
                 (error 'sql-database-error
                        :database database
                        :error-id subcode
                        :message (uffi:convert-from-foreign-string errbuf)))))))
	(nulls-ok
	 (error 'sql-database-error
                :database database
                :message "can't handle NULLS-OK without ERRHP"))
	(t 
	 (error 'sql-database-error
                :database database
                :message "OCI Error (and no ERRHP available to find subcode)"))))

;;; Require an OCI success code.
;;;
;;; (The ordinary OCI error reporting mechanisms uses a fair amount of
;;; machinery (environments and other handles). In order to get to
;;; where we can use these mechanisms, we have to be able to allocate
;;; the machinery. The functions for allocating the machinery can
;;; return errors (e.g. out of memory) but shouldn't. Wrapping this function
;;; around function calls to such have-to-succeed functions enforces
;;; this condition.)

(defun osucc (code)
  (declare (type fixnum code))
  (unless (= code +oci-success+)
    (error 'dbi-error
	   :format-control "unexpected OCI failure, code=~S"
	   :format-arguments (list code))))


;;; Enabling this can be handy for low-level debugging.
#+nil
(progn
  (trace oci-initialize #+oci-8-1-5 oci-env-create oci-handle-alloc oci-logon
         oci-error-get oci-stmt-prepare oci-stmt-execute
         oci-param-get oci-logon oci-attr-get oci-define-by-pos oci-stmt-fetch)
  (setf debug::*debug-print-length* nil))


;;;; the OCI library, part V: converting from OCI representations to Lisp
;;;; representations

;; Return the INDEXth string of the OCI array, represented as Lisp
;; SIMPLE-STRING. SIZE is the size of the fixed-width fields used by
;; Oracle to store strings within the array.

;; In the wild world of databases, trailing spaces aren't generally
;; significant, since e.g. "LARRY " and "LARRY    " are the same string
;; stored in different fixed-width fields. OCI drops trailing spaces
;; for us in some cases but apparently not for fields of fixed
;; character width, e.g.
;;
;;   (dbi:sql "create table employees (name char(15), job char(15), city
;;            char(15), rate float)" :db orcl :types :auto)
;; In order to map the "same string" property above onto Lisp equality,
;; we drop trailing spaces in all cases:

(uffi:def-type string-array (:array :unsigned-char))

(defun deref-oci-string (arrayptr string-index size)
  (declare (type string-array arrayptr))
  (declare (type (mod #.+n-buf-rows+) string-index))
  (declare (type (and unsigned-byte fixnum) size))
  (let* ((raw (uffi:convert-from-foreign-string 
	       (uffi:make-pointer
		(+ (uffi:pointer-address arrayptr) (* string-index size))
		:unsigned-char)))
	 (trimmed (string-trim " " raw)))
     (if (equal trimmed "NULL") nil trimmed)))

;; the OCI library, part Z: no-longer used logic to convert from
;; Oracle's binary date representation to Common Lisp's native date
;; representation

#+nil
(defvar +oci-date-bytes+ 7)

;;; Return the INDEXth date in the OCI array, represented as
;;; a Common Lisp "universal time" (i.e. seconds since 1900).

#+nil
(defun deref-oci-date (arrayptr index)
  (oci-date->universal-time (uffi:pointer-address 
			     (uffi:deref-array arrayptr
					       '(:array :unsigned-char)
					       (* index +oci-date-bytes+)))))
#+nil
(defun oci-date->universal-time (oci-date)
  (declare (type (alien (* :unsigned-char)) oci-date))
  (flet (;; a character from OCI-DATE, interpreted as an unsigned byte
	 (ub (i)
	   (declare (type (mod #.+oci-date-bytes+) i))
	   (mod (uffi:deref-array oci-date string-array i) 256)))
    (let* ((century (* (- (ub 0) 100) 100))
	   (year    (+ century (- (ub 1) 100)))
	   (month   (ub 2))
	   (day     (ub 3))
	   (hour    (1- (ub 4)))
	   (minute  (1- (ub 5)))
	   (second  (1- (ub 6))))
      (encode-universal-time second minute hour day month year))))

(defmethod database-list-tables ((database oracle-database) &key owner)
  (mapcar #'car 
	  (database-query "select table_name from user_tables"
			  database nil nil))
  #+nil
  (values (database-query "select TABLE_NAME from all_catalog
	        where owner not in ('PUBLIC','SYSTEM','SYS','WMSYS','EXFSYS','CTXSYS','WKSYS','WK_TEST','MDSYS','DMSYS','OLAPSYS','ORDSYS','XDB')"
			  db nil nil)))


(defmethod database-list-views ((database oracle-database)
                                 &key owner)
  ;; (database-query "select table_name from all_catalog" database nil nil)
  (mapcar #'car
	  (database-query "select view_name from user_views" database nil nil)))


(defmethod database-list-indexes ((database oracle-database)
                                  &key (owner nil))
  (mapcar #'car
	  (database-query "select index_name from user_indexes" database nil nil)))

(defmethod list-all-table-columns (table (db oracle-database))
  (declare (string table))
  (let* ((sql-stmt (concatenate
		    'simple-string
		    "select "
		    "'',"
		    "all_tables.OWNER,"
		    "'',"
		    "user_tab_columns.COLUMN_NAME,"
		    "user_tab_columns.DATA_TYPE from user_tab_columns,"
		    "all_tables where all_tables.table_name = '" table "'"
		    " and user_tab_columns.table_name = '" table "'"))
	 (preresult (database-query sql-stmt db :auto nil)))
    ;; PRERESULT is like RESULT except that it has a name instead of
    ;; type codes in the fifth column of each row. To fix this, we
    ;; destructively modify PRERESULT.
    (dolist (preresult-row preresult)
      (setf (fifth preresult-row)
	    (if (find (fifth preresult-row)
		      #("NUMBER" "DATE")
		      :test #'string=)
		2 ; numeric
	        1))) ; string
    preresult))


(defmethod database-list-attributes (table (database oracle-database) &key owner)
  (mapcar #'car
	  (database-query
	   (format nil
		   "select column_name from user_tab_columns where table_name='~A'"
		   table)
	   database nil nil)))

(defmethod database-attribute-type (attribute (table string)
					 (database oracle-database)
					 &key (owner nil))
  (let ((rows
	 (database-query
	  (format nil
		  "select data_type,data_length,data_precision,data_scale,nullable from user_tab_columns where table_name='~A' and column_name='~A'"
		  table attribute)
	  database :auto nil)))
    (destructuring-bind (type length precision scale nullable) (car rows)
      (values (ensure-keyword type) length precision scale 
	      (if (char-equal #\Y (schar nullable 0)) 1 0)))))
    
;; Return one row of the table referred to by QC, represented as a
;; list; or if there are no more rows, signal an error if EOF-ERRORP,
;; or return EOF-VALUE otherwise.

;; KLUDGE: This CASE statement is a strong sign that the code would be
;; cleaner if CD were made into an abstract class, we made variant
;; classes for CD-for-column-of-strings, CD-for-column-of-floats,
;; etc., and defined virtual functions to handle operations like
;; get-an-element-from-column. (For a small special purpose module
;; like this, would arguably be overkill, so I'm not going to do it
;; now, but if this code ends up getting more complicated in
;; maintenance, it would become a really good idea.)

;; Arguably this would be a good place to signal END-OF-FILE, but
;; since the ANSI spec specifically says that END-OF-FILE means a
;; STREAM which has no more data, and QC is not a STREAM, we signal
;; DBI-ERROR instead.

(uffi:def-type short-array '(:array :short))
(uffi:def-type int-pointer '(* :int))
(uffi:def-type double-pointer '(* :double))

;;; the result of a database query: a cursor through a table
(defstruct (oracle-result-set (:print-function print-query-cursor)
                              (:conc-name qc-)
                              (:constructor %make-query-cursor))
  (db (error "missing DB")              ; db conn. this table is associated with
    :type oracle-database
    :read-only t)
  (stmthp (error "missing STMTHP")      ; the statement handle used to create
;;  :type alien			; this table. owned by the QUERY-CURSOR
    :read-only t)                       ; object, deallocated on CLOSE-QUERY
  (cds) ;  (error "missing CDS")            ; column descriptors
;    :type (simple-array cd 1)
					;    :read-only t)
  (n-from-oci 
   0                         ; buffered rows: number of rows recv'd
   :type (integer 0 #.+n-buf-rows+))   ; from the database on the last read
  (n-to-dbi
   0                           ; number of buffered rows returned, i.e.
   :type (integer 0 #.+n-buf-rows+))   ; the index, within the buffered rows,
                                        ; of the next row which hasn't already
                                        ; been returned
  (total-n-from-oci
   0                   ; total number of bytes recv'd from OCI
   :type unsigned-byte)                ; in all reads
  (oci-end-seen-p nil))                 ; Have we seen the end of OCI
                                        ; data, i.e. OCI returning
                                        ; less data than we requested?
                                        ; OCI doesn't seem to like us
                                        ; to try to read more data
                                        ; from it after that..


(defun fetch-row (qc &optional (eof-errorp t) eof-value)
  ;;(declare (optimize (speed 3)))
  (cond ((zerop (qc-n-from-oci qc))
	 (if eof-errorp
	     (error 'clsql-error :message
		    (format nil "no more rows available in ~S" qc))
	   eof-value))
	((>= (qc-n-to-dbi qc)
	     (qc-n-from-oci qc))
	 (refill-qc-buffers qc)
	 (fetch-row qc nil eof-value))
	(t
	 (let ((cds (qc-cds qc))
	       (reversed-result nil)
	       (irow (qc-n-to-dbi qc)))
	   (dotimes (icd (length cds))
	     (let* ((cd (aref cds icd))
		    (b (foreign-resource-buffer (cd-buffer cd)))
		    (value
		     (let* ((arb (foreign-resource-buffer (cd-indicators cd)))
			    (indicator (uffi:deref-array arb '(:array :short) irow)))
		       (declare (type short-array arb))
		       (unless (= indicator -1)
			 (ecase (cd-oci-data-type cd)
			   (#.SQLT-STR  
			    (deref-oci-string b irow (cd-sizeof cd)))
			   (#.SQLT-FLT  
			    (uffi:deref-array b '(:array :double) irow))
			   (#.SQLT-INT  
			    (uffi:deref-array b '(:array :int) irow))
			   (#.SQLT-DATE 
			    (deref-oci-string b irow (cd-sizeof cd))))))))
	       (when (and (eq :string (cd-result-type cd))
			  value
			  (not (stringp value)))
		   (setq value (write-to-string value)))
	       (push value reversed-result)))
	   (incf (qc-n-to-dbi qc))
	   (nreverse reversed-result)))))

(defun refill-qc-buffers (qc)
  (with-slots (errhp) (qc-db qc)
    (setf (qc-n-to-dbi qc) 0)
    (cond ((qc-oci-end-seen-p qc)
           (setf (qc-n-from-oci qc) 0))
          (t
           (let ((oci-code (%oci-stmt-fetch 
			    (deref-vp (qc-stmthp qc))
			    (deref-vp errhp)
			    +n-buf-rows+
			    +oci-fetch-next+ +oci-default+)))
             (ecase oci-code
               (#.+oci-success+ (values))
               (#.+oci-no-data+ (setf (qc-oci-end-seen-p qc) t)
                                (values))
               (#.+oci-error+ (handle-oci-error :database (qc-db qc)
                                                :nulls-ok t))))
           (uffi:with-foreign-object (rowcount :long)
             (oci-attr-get (deref-vp (qc-stmthp qc))
			   +oci-htype-stmt+
                           rowcount 
			   (uffi:make-null-pointer :unsigned-long)
			   +oci-attr-row-count+ 
                           (deref-vp errhp))
             (setf (qc-n-from-oci qc)
                   (- (uffi:deref-pointer rowcount :long)
		      (qc-total-n-from-oci qc)))
             (when (< (qc-n-from-oci qc) +n-buf-rows+)
               (setf (qc-oci-end-seen-p qc) t))
             (setf (qc-total-n-from-oci qc)
                   (uffi:deref-pointer rowcount :long)))))
    (values)))

;; the guts of the SQL function
;;
;; (like the SQL function, but with the QUERY argument hardwired to T, so
;; that the return value is always a cursor instead of a list)

;; Is this a SELECT statement?  SELECT statements are handled
;; specially by OCIStmtExecute().  (Non-SELECT statements absolutely
;; require a nonzero iteration count, while the ordinary choice for a
;; SELECT statement is a zero iteration count.

;; SELECT statements are the only statements which return tables.  We
;; don't free STMTHP in this case, but instead give it to the new
;; QUERY-CURSOR, and the new QUERY-CURSOR becomes responsible for
;; freeing the STMTHP when it is no longer needed.

(defun sql-stmt-exec (sql-stmt-string db result-types field-names)
  (with-slots (envhp svchp errhp)
    db
    (let ((stmthp (uffi:allocate-foreign-object :pointer-void)))
      (uffi:with-foreign-object (stmttype :unsigned-short)
        
        (oci-handle-alloc (deref-vp envhp)
			  stmthp
			  +oci-htype-stmt+ 0 +null-void-pointer-pointer+)
        (oci-stmt-prepare (deref-vp stmthp)
			  (deref-vp errhp)
                          (uffi:convert-to-cstring sql-stmt-string)
			  (length sql-stmt-string)
                          +oci-ntv-syntax+ +oci-default+ :database db)
        (oci-attr-get (deref-vp stmthp) 
		      +oci-htype-stmt+ 
                      stmttype
		      (uffi:make-null-pointer :unsigned-int)
		      +oci-attr-stmt-type+ 
                      (deref-vp errhp)
		      :database db)
        (let* ((select-p (= (uffi:deref-pointer stmttype :unsigned-short) 1)) 
               (iters (if select-p 0 1)))
          
          (oci-stmt-execute (deref-vp svchp)
			    (deref-vp stmthp)
			    (deref-vp errhp)
                            iters 0 +null-void-pointer+ +null-void-pointer+ +oci-default+
			    :database db)
          (cond (select-p
                 (make-query-cursor db stmthp result-types field-names))
                (t
                 (oci-handle-free (deref-vp stmthp) +oci-htype-stmt+)
                 nil)))))))


;; Return a QUERY-CURSOR representing the table returned from the OCI
;; operation done through STMTHP.  TYPES is the argument of the same
;; name from the external SQL function, controlling type conversion
;; of the returned arguments.

(defun make-query-cursor (db stmthp result-types field-names)
  (let ((qc (%make-query-cursor :db db
				:stmthp stmthp
				:cds (make-query-cursor-cds db stmthp 
							    result-types
							    field-names))))
    (refill-qc-buffers qc)
    qc))


;; the hairy part of MAKE-QUERY-CURSOR: Ask OCI for information
;; about table columns, translate the information into a Lisp
;; vector of column descriptors, and return it.

;; Allegro defines several flavors of type conversion, but this
;; implementation only supports the :AUTO flavor.

;; A note of explanation: OCI's internal number format uses 21
;; bytes (42 decimal digits). 2 separate (?) one-byte fields,
;; scale and precision, are used to deduce the nature of these
;; 21 bytes. See pp. 3-10, 3-26, and 6-13 of OCI documentation
;; for more details.

;; When calling OCI C code to handle the conversion, we have
;; only two numeric types available to pass the return value:
;; double-float and signed-long. It would be possible to
;; bypass the OCI conversion functions and write Lisp code
;; which reads the 21-byte field directly and decodes
;; it. However this is left as an exercise for the reader. :-)

;; The following table describes the mapping, based on the implicit
;; assumption that C's "signed long" type is a 32-bit integer.
;;
;;   Internal Values                     SQL Type        C Return Type
;;   ===============                     ========        =============
;;   Precision > 0        SCALE = -127   FLOAT       --> double-float
;;   Precision > 0 && <=9 SCALE = 0      INTEGER     --> signed-long
;;   Precision = 0 || > 9 SCALE = 0      BIG INTEGER --> double-float
;;   Precision > 0        SCALE > 0      DECIMAL     --> double-float

;; (OCI uses 1-based indexing here.)

;; KLUDGE: This should work for all other data types except those
;; which don't actually fit in their fixed-width field (BLOBs and the
;; like). As Winton says, we (Cadabra) don't need to worry much about
;; those, since we can't reason with them, so we don't use them. But
;; for a more general application it'd be good to have a more
;; selective and rigorously correct test here for whether we can
;; actually handle the given DEREF-DTYPE value. -- WHN 20000106

;; Note: The OCI documentation doesn't seem to say whether the COLNAME
;; value returned here is a newly-allocated copy which we're
;; responsible for freeing, or a pointer into some system copy which
;; will be freed when the system itself is shut down.  But judging
;; from the way that the result is used in the cdemodsa.c example
;; program, it looks like the latter: we should make our own copy of
;; the value, but not try to free it.

;; WORKAROUND: OCI seems to return ub2 values for the
;; +oci-attr-data-size+ attribute even though its documentation claims
;; that it returns a ub4, and even though the associated "sizep" value
;; is 4, not 2.  In order to make the code here work reliably, without
;; having to patch it later if OCI is ever fixed to match its
;; documentation, we pre-zero COLSIZE before making the call into OCI.

;; To exercise the weird OCI behavior (thereby blowing up the code
;; below, beware!) try setting this value into COLSIZE, calling OCI,
;; then looking at the value in COLSIZE.  (setf colsize #x12345678)
;; debugging only
            

(defun make-query-cursor-cds (database stmthp result-types field-names)
  (declare (optimize (safety 3) #+nil (speed 3))
	   (type oracle-database database)
	   (type pointer-pointer-void stmthp))
  (with-slots (errhp) database
    (uffi:with-foreign-objects ((dtype-foreign :unsigned-short)
			   (parmdp ':pointer-void)
			   (precision :byte)
			   (scale :byte)
			   (colname '(* :unsigned-char))
			   (colnamelen :unsigned-long)
			   (colsize :unsigned-long)
			   (colsizesize :unsigned-long)
			   (defnp ':pointer-void))
      (let ((buffer nil)
	    (sizeof nil))
	(do ((icolumn 0 (1+ icolumn))
	     (cds-as-reversed-list nil))
	    ((not (eql (oci-param-get (deref-vp stmthp) 
				      +oci-htype-stmt+
				      (deref-vp errhp)
				      parmdp
				      (1+ icolumn) :database database)
		       +oci-success+))
	     (coerce (reverse cds-as-reversed-list) 'simple-vector))
	  ;; Decode type of ICOLUMNth column into a type we're prepared to
	  ;; handle in Lisp.
	  (oci-attr-get (deref-vp parmdp)
			+oci-dtype-param+ 
			dtype-foreign
			(uffi:make-null-pointer :unsigned-int)
			+oci-attr-data-type+
			(deref-vp errhp))
	  (let ((dtype (uffi:deref-pointer dtype-foreign :unsigned-short)))
	    (case dtype
	      (#.SQLT-DATE
	       (setf buffer (acquire-foreign-resource :unsigned-char
						      (* 32 +n-buf-rows+)))
	       (setf sizeof 32 dtype #.SQLT-STR))
	      (#.SQLT-NUMBER
	       (oci-attr-get (deref-vp parmdp)
			     +oci-dtype-param+
			     precision
			     (uffi:make-null-pointer :unsigned-int)
			     +oci-attr-precision+
			     (deref-vp errhp))
	       (oci-attr-get (deref-vp parmdp)
			     +oci-dtype-param+
			     scale
			     (uffi:make-null-pointer :unsigned-int)
			     +oci-attr-scale+
			     (deref-vp errhp))
	       (let ((*scale (uffi:deref-pointer scale :byte))
		     (*precision (uffi:deref-pointer precision :byte)))
		 ;;(format t "scale=~d, precision=~d~%" *scale *precision)
		 (cond
		  ((or (zerop *scale)
		       (and (minusp *scale) (< *precision 10)))
		   (setf buffer (acquire-foreign-resource :int +n-buf-rows+)
			 sizeof 4			;; sizeof(int)
			 dtype #.SQLT-INT))
		  (t
		   (setf buffer (acquire-foreign-resource :double +n-buf-rows+)
			 sizeof 8                   ;; sizeof(double)
			 dtype #.SQLT-FLT))))          )
	      ;; Default to SQL-STR
	      (t		
	       (setf (uffi:deref-pointer colsize :unsigned-long) 0
		     dtype #.SQLT-STR)
	       (oci-attr-get (deref-vp parmdp)
			     +oci-dtype-param+ 
			     colsize
			     (uffi:make-null-pointer :unsigned-int) ;;  (uffi:pointer-address colsizesize) 
			     +oci-attr-data-size+
			     (deref-vp errhp))
	       (let ((colsize-including-null (1+ (uffi:deref-pointer colsize :unsigned-long))))
		 (setf buffer (acquire-foreign-resource
			       :unsigned-char (* +n-buf-rows+ colsize-including-null)))
		 (setf sizeof colsize-including-null))))
	    (let ((retcodes (acquire-foreign-resource :unsigned-short +n-buf-rows+))
		  (indicators (acquire-foreign-resource :short +n-buf-rows+))
		  (colname-string ""))
	      (when field-names
		(oci-attr-get (deref-vp parmdp)
			      +oci-dtype-param+
			      colname
			      colnamelen
			      +oci-attr-name+
			      (deref-vp errhp))
		(setq colname-string (uffi:convert-from-foreign-string
				      (uffi:deref-pointer colname '(* :unsigned-char))
				      :length (uffi:deref-pointer colnamelen :unsigned-long))))
	      (push (make-cd :name colname-string
			     :sizeof sizeof
			     :buffer buffer
			     :oci-data-type dtype
			     :retcodes retcodes
			     :indicators indicators
			     :result-type (cond
					   ((consp result-types)
					    (nth icolumn result-types))
					   ((null result-types)
					    :string)
					   (t
					    result-types)))
		    cds-as-reversed-list)
	      (oci-define-by-pos (deref-vp stmthp)
				 defnp
				 (deref-vp errhp)
				 (1+ icolumn) ; OCI 1-based indexing again
				 (uffi:with-cast-pointer (vp (foreign-resource-buffer buffer) :void)
				   vp)
				 sizeof
				 dtype
				 (uffi:with-cast-pointer (vp (foreign-resource-buffer indicators) :void)
				   vp)
				 (uffi:make-null-pointer :unsigned-short)
				 (uffi:with-cast-pointer (vp (foreign-resource-buffer retcodes) :unsigned-short)
				   vp)
				 +oci-default+))))))))
  
;; Release the resources associated with a QUERY-CURSOR.

(defun close-query (qc)
  (oci-handle-free (deref-vp (qc-stmthp qc)) +oci-htype-stmt+)
  (let ((cds (qc-cds qc)))
    (dotimes (i (length cds))
      (release-cd-resources (aref cds i))))
  (values))


;; Release the resources associated with a column description.

(defun release-cd-resources (cd)
  (free-foreign-resource (cd-buffer cd))
  (free-foreign-resource (cd-retcodes cd))
  (free-foreign-resource (cd-indicators cd))
  (values))


(defmethod database-name-from-spec (connection-spec (database-type (eql :oracle)))
  (check-connection-spec connection-spec database-type (dsn user password))
  (destructuring-bind (dsn user password) connection-spec
    (declare (ignore password))
    (concatenate 'string  dsn "/" user)))


(defmethod database-connect (connection-spec (database-type (eql :oracle)))
  (check-connection-spec connection-spec database-type (dsn user password))
  (destructuring-bind (data-source-name user password)
      connection-spec
    (let ((envhp (uffi:allocate-foreign-object :pointer-void))
          (errhp (uffi:allocate-foreign-object :pointer-void))
          (svchp (uffi:allocate-foreign-object :pointer-void))
          (srvhp (uffi:allocate-foreign-object :pointer-void)))
      ;; Requests to allocate environments and handles should never
      ;; fail in normal operation, and they're done too early to
      ;; handle errors very gracefully (since they're part of the
      ;; error-handling mechanism themselves) so we just assert they
      ;; work.
      (setf (deref-vp envhp) +null-void-pointer+)
      #+oci-8-1-5
      (progn
        (oci-env-create envhp +oci-default+  +null-void-pointer+
			+null-void-pointer+  +null-void-pointer+ 
			+null-void-pointer+ 0 +null-void-pointer-pointer+)
	(oci-handle-alloc envhp
			  (deref-vp errhp)
			  +oci-htype-error+ 0 
			  +null-void-pointer-pointer+))
      #-oci-8-1-5
      (progn
	(oci-initialize +oci-object+ +null-void-pointer+ +null-void-pointer+
			+null-void-pointer+ +null-void-pointer-pointer+)
        (ignore-errors (oci-handle-alloc +null-void-pointer+ envhp
					 +oci-htype-env+ 0
					 +null-void-pointer-pointer+)) ;no testing return
        (oci-env-init envhp +oci-default+ 0 +null-void-pointer-pointer+)
        (oci-handle-alloc (deref-vp envhp) errhp
			  +oci-htype-error+ 0 +null-void-pointer-pointer+)
        (oci-handle-alloc (deref-vp envhp) srvhp
			  +oci-htype-server+ 0 +null-void-pointer-pointer+)
	(uffi:with-cstring (dblink nil)
	  (oci-server-attach (deref-vp srvhp)
			     (deref-vp errhp)
			     dblink
			     0 +oci-default+))
        (oci-handle-alloc (deref-vp envhp) svchp
			  +oci-htype-svcctx+ 0 +null-void-pointer-pointer+)
        (oci-attr-set (deref-vp svchp)
		      +oci-htype-svcctx+
		      (deref-vp srvhp) 0 +oci-attr-server+ 
		      (deref-vp errhp))
        ;; oci-handle-alloc((dvoid *)encvhp, (dvoid **)&stmthp, OCI_HTYPE_STMT, 0, 0);
        ;;#+nil
	)
      (let (db server-version)
	(uffi:with-foreign-object (buf '(:array :unsigned-char #.+errbuf-len+))
	  (oci-server-version (deref-vp svchp)
			      (deref-vp errhp)
			      (uffi:char-array-to-pointer buf)
			      +errbuf-len+ +oci-htype-svcctx+)
	  (setf server-version (uffi:convert-from-foreign-string buf)))
	(setq db (make-instance 'oracle-database
				:name (database-name-from-spec connection-spec
							       database-type)
				:envhp envhp
				:errhp errhp
				:database-type :oracle
				:svchp svchp
				:dsn data-source-name
				:user user
				:server-version server-version
				:major-version-number (major-version-from-string
						       server-version)))

	(oci-logon (deref-vp envhp)
		   (deref-vp errhp) 
		   svchp
		   (uffi:convert-to-cstring user) (length user)
		   (uffi:convert-to-cstring password) (length password)
		   (uffi:convert-to-cstring data-source-name) (length data-source-name)
		   :database db)
	;; :date-format-length (1+ (length date-format)))))
	(setf (slot-value db 'clsql-sys::state) :open)
        (database-execute-command
	 (format nil "alter session set NLS_DATE_FORMAT='~A'" (date-format db)) db)
        db))))


(defun major-version-from-string (str)
  (cond 
    ((search " 10g " str)
     10)
    ((search " 9g " str)
     10)))


;; Close a database connection.

(defmethod database-disconnect ((database oracle-database))
  (osucc (oci-logoff (deref-vp (svchp database))
		     (deref-vp (errhp database))))
  (osucc (oci-handle-free (deref-vp (envhp database)) +oci-htype-env+))
  ;; Note: It's neither required nor allowed to explicitly deallocate the
  ;; ERRHP handle here, since it's owned by the ENVHP deallocated above,
  ;; and was therefore automatically deallocated at the same time.
  t)

;;; Do the database operation described in SQL-STMT-STRING on database
;;; DB and, if the command is a SELECT, return a representation of the
;;; resulting table. The representation of the table is controlled by the
;;; QUERY argument:
;;;   * If QUERY is NIL, the table is returned as a list of rows, with
;;;     each row represented by a list.
;;;   * If QUERY is non-NIL, the result is returned as a QUERY-CURSOR
;;;     suitable for FETCH-ROW and CLOSE-QUERY
;;; The TYPES argument controls the type conversion method used
;;; to construct the table. The Allegro version supports several possible
;;; values for this argument, but we only support :AUTO.

(defmethod database-query (query-expression (database oracle-database) result-types field-names)
  (let ((cursor (sql-stmt-exec query-expression database result-types field-names)))
    ;; (declare (type (or query-cursor null) cursor))
    (if (null cursor) ; No table was returned.
	(values)
      (do ((reversed-result nil))
	  (nil)
	(let* ((eof-value :eof)
	       (row (fetch-row cursor nil eof-value)))
	  (when (eq row eof-value)
	    (close-query cursor)
	    (if field-names
		(return (values (nreverse reversed-result)
				(loop for cd across (qc-cds cursor)
				    collect (cd-name cd))))
	      (return (nreverse reversed-result))))
	  (push row reversed-result))))))


(defmethod database-create-sequence
  (sequence-name (database oracle-database))
  (execute-command
   (concatenate 'string "CREATE SEQUENCE "
		(sql-escape sequence-name))
   :database database))

(defmethod database-drop-sequence
  (sequence-name (database oracle-database))
  (execute-command
   (concatenate 'string "DROP SEQUENCE "
		(sql-escape sequence-name))
   :database database))

(defmethod database-sequence-next (sequence-name (database oracle-database))
  (caar
   (query
    (concatenate 'string "SELECT "
		 (sql-escape sequence-name)
		 ".NEXTVAL FROM dual"
		 ) :database database)))

(defmethod database-list-sequences ((database oracle-database) &key owner)
  (mapcar #'car (database-query "select sequence_name from user_sequences" 
				database nil nil)))

(defmethod database-execute-command (sql-expression (database oracle-database))
  (database-query sql-expression database nil nil)
  ;; HACK HACK HACK
  (database-query "commit" database nil nil)
  t)


(defstruct (cd (:constructor make-cd)
	       (:print-function print-cd))
  "a column descriptor: metadata about the data in a table"

  ;; name of this column
  (name (error "missing NAME") :type simple-string :read-only t)
  ;; the size in bytes of a single element
  (sizeof (error "missing SIZE") :type fixnum :read-only t)
  ;; an array of +N-BUF-ROWS+ elements in C representation
  (buffer (error "Missing BUFFER")
	  :type foreign-resource
	  :read-only t)
  ;; an array of +N-BUF-ROWS+ OCI return codes in C representation.
  ;; (There must be one return code for every element of every
  ;; row in order to be able to represent nullness.)
  (retcodes (error "Missing RETCODES")
	    :type foreign-resource
	    :read-only t)
  (indicators (error "Missing INDICATORS")
	      :type foreign-resource
	      :read-only t)
  ;; the OCI code for the data type of a single element
  (oci-data-type (error "missing OCI-DATA-TYPE")
		 :type fixnum
		 :read-only t)
  (result-type (error "missing RESULT-TYPE")
	       :read-only t))


(defun print-cd (cd stream depth)
  (declare (ignore depth))
  (print-unreadable-object (cd stream :type t)
    (format stream
	    ":NAME ~S :OCI-DATA-TYPE ~S :OCI-DATA-SIZE ~S"
	    (cd-name cd)
	    (cd-oci-data-type cd)
	    (cd-sizeof cd))))

(defun print-query-cursor (qc stream depth)
  (declare (ignore depth))
  (print-unreadable-object (qc stream :type t :identity t)
    (prin1 (qc-db qc) stream)))


(defmethod database-query-result-set ((query-expression string)
				      (database oracle-database) 
				      &key full-set result-types)
  (let ((cursor (sql-stmt-exec query-expression database result-types nil)))
    (if full-set
	(values cursor (length (qc-cds cursor)) nil)
	(values cursor (length (qc-cds cursor))))))


(defmethod database-dump-result-set (result-set (database oracle-database))
  (close-query result-set)) 

(defmethod database-store-next-row (result-set (database oracle-database) list)
  (let* ((eof-value :eof)
	 (row (fetch-row result-set nil eof-value)))
    (unless (eq eof-value row)
      (loop for i from 0 below (length row)
	  do (setf (nth i list) (nth i row)))
      list)))

(defmethod clsql-sys:database-start-transaction ((database oracle-database))
  (call-next-method))

;;(with-slots (svchp errhp) database
;;    (osucc (oci-trans-start (uffi:deref-pointer svchp)
;;			    (uffi:deref-pointer errhp)
;;			    60
;;			    +oci-trans-new+)))
;;  t)
  

(defmethod clsql-sys:database-commit-transaction ((database oracle-database))
  (call-next-method)
  (with-slots (svchp errhp) database
	      (osucc (oci-trans-commit (deref-vp svchp)
				       (deref-vp errhp)
				       0)))
  t)

(defmethod clsql-sys:database-abort-transaction ((database oracle-database))
  (call-next-method)
  (osucc (oci-trans-rollback (deref-vp (svchp database))
			     (deref-vp (errhp database))
			     0))
  t)

(defparameter *constraint-types*
  '(("NOT-NULL" . "NOT NULL")))

(defmethod database-output-sql ((str string) (database oracle-database))
  (if (and (null (position #\' str))
	   (null (position #\\ str)))
      (format nil "'~A'" str)
    (let* ((l (length str))
	   (buf (make-string (+ l 3))))
      (setf (aref buf 0) #\')
      (do ((i 0 (incf i))
	   (j 1 (incf j)))
	  ((= i l) (setf (aref buf j) #\'))
	(if (= j (- (length buf) 1))
	    (setf buf (adjust-array buf (+ (length buf) 1))))
	(cond ((eql (aref str i) #\')
	       (setf (aref buf j) #\')
	       (incf j)))
	(setf (aref buf j) (aref str i)))
      buf)))


;; Specifications

(defmethod db-type-has-bigint? ((type (eql :oracle)))
  nil)

(defmethod db-type-has-fancy-math? ((db-type (eql :oracle)))
  t)

(defmethod db-type-has-boolean-where? ((db-type (eql :oracle)))
  nil)

(when (clsql-sys:database-type-library-loaded :oracle)
  (clsql-sys:initialize-database-type :database-type :oracle))
