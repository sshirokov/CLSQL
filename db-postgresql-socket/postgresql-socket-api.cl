;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          postgresql-socket.cl
;;;; Purpose:       Low-level PostgreSQL interface using sockets
;;;; Programmers:   Kevin M. Rosenberg based on
;;;;                Original code by Pierre R. Mai 
;;;;                
;;;; Date Started:  Feb 2002
;;;;
;;;; $Id: postgresql-socket-api.cl,v 1.2 2002/09/29 18:54:17 kevin Exp $
;;;;
;;;; This file, part of CLSQL, is Copyright (c) 2002 by Kevin M. Rosenberg
;;;; and Copyright (c) 1999-2001 by Pierre R. Mai
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************


;;;; Changes by Kevin Rosenberg
;;;;  - Added socket open functions for Allegro and Lispworks
;;;;  - Changed CMUCL FFI to UFFI
;;;;  - Added necessary (force-output) for socket streams on 
;;;;     Allegro and Lispworks
;;;;  - Added initialization variable
;;;;  - Added field type processing

 
(declaim (optimize (debug 3) (speed 3) (safety 1) (compilation-speed 0)))
(in-package :postgresql-socket)

(uffi:def-enum pgsql-ftype
    ((:bytea 17)
     (:int2 21)
     (:int4 23)
     (:int8 20)
     (:float4 700)
     (:float8 701)))

(defmethod database-type-library-loaded ((database-type
					  (eql :postgresql-socket)))
  "T if foreign library was able to be loaded successfully. Always true for
socket interface"
  t)
				      

;;; Message I/O stuff

(defmacro define-message-constants (description &rest clauses)
  (assert (evenp (length clauses)))
  (loop with seen-characters = nil
	for (name char) on clauses by #'cddr
	for char-code = (char-code char)
	for doc-string = (format nil "~A (~:C): ~A" description char name)
	if (member char seen-characters)
	do (error "Duplicate message type ~@C for group ~A" char description)
	else
	collect
	`(defconstant ,name ,char-code ,doc-string)
	into result-clauses
	and do (push char seen-characters)
      finally
	(return `(progn ,@result-clauses))))

(eval-when (:compile-toplevel :load-toplevel :execute)
(define-message-constants "Backend Message Constants"
  +ascii-row-message+ #\D
  +authentication-message+ #\R
  +backend-key-message+ #\K
  +binary-row-message+ #\B
  +completed-response-message+ #\C
  +copy-in-response-message+ #\G
  +copy-out-response-message+ #\H
  +cursor-response-message+ #\P
  +empty-query-response-message+ #\I
  +error-response-message+ #\E
  +function-response-message+ #\V
  +notice-response-message+ #\N
  +notification-response-message+ #\A
  +ready-for-query-message+ #\Z
  +row-description-message+ #\T))

(defgeneric send-socket-value (type socket value))

(defmethod send-socket-value ((type (eql 'int32)) socket (value integer))
  (write-byte (ldb (byte 8 24) value) socket)
  (write-byte (ldb (byte 8 16) value) socket)
  (write-byte (ldb (byte 8 8) value) socket)
  (write-byte (ldb (byte 8 0) value) socket))

(defmethod send-socket-value ((type (eql 'int16)) socket (value integer))
  (write-byte (ldb (byte 8 8) value) socket)
  (write-byte (ldb (byte 8 0) value) socket))

(defmethod send-socket-value ((type (eql 'int8)) socket (value integer))
  (write-byte (ldb (byte 8 0) value) socket))

(defmethod send-socket-value ((type (eql 'string)) socket (value string))
  (loop for char across value
	for code = (char-code char)
	do (write-byte code socket)
	finally (write-byte 0 socket)))

(defmethod send-socket-value ((type (eql 'limstring)) socket (value string))
  (loop for char across value
	for code = (char-code char)
	do (write-byte code socket)))

(defmethod send-socket-value ((type (eql 'byte)) socket (value integer))
  (write-byte value socket))

(defmethod send-socket-value ((type (eql 'byte)) socket (value character))
  (write-byte (char-code value) socket))

(defmethod send-socket-value ((type (eql 'byte)) socket value)
  (write-sequence value socket))

(defgeneric read-socket-value (type socket))

(defmethod read-socket-value ((type (eql 'int32)) socket)
  (let ((result 0))
    (setf (ldb (byte 8 24) result) (read-byte socket))
    (setf (ldb (byte 8 16) result) (read-byte socket))
    (setf (ldb (byte 8 8) result) (read-byte socket))
    (setf (ldb (byte 8 0) result) (read-byte socket))
    result))

(defmethod read-socket-value ((type (eql 'int16)) socket)
  (let ((result 0))
    (setf (ldb (byte 8 8) result) (read-byte socket))
    (setf (ldb (byte 8 0) result) (read-byte socket))
    result))

(defmethod read-socket-value ((type (eql 'int8)) socket)
  (read-byte socket))

(defmethod read-socket-value ((type (eql 'string)) socket)
  (with-output-to-string (out)
    (loop for code = (read-byte socket)
	  until (zerop code)
	  do (write-char (code-char code) out))))

(defgeneric skip-socket-value (type socket))

(defmethod skip-socket-value ((type (eql 'int32)) socket)
  (dotimes (i 4) (read-byte socket)))

(defmethod skip-socket-value ((type (eql 'int16)) socket)
  (dotimes (i 2) (read-byte socket)))

(defmethod skip-socket-value ((type (eql 'int8)) socket)
  (read-byte socket))

(defmethod skip-socket-value ((type (eql 'string)) socket)
  (loop until (zerop (read-byte socket))))

(defmacro define-message-sender (name (&rest args) &rest clauses)
  (loop with socket-var = (gensym)
	for (type value) in clauses
	collect
	`(send-socket-value ',type ,socket-var ,value)
	into body
      finally
	(return
	  `(defun ,name (,socket-var ,@args)
	     ,@body))))

(defun pad-limstring (string limit)
  (let ((result (make-string limit :initial-element #\NULL)))
    (loop for char across string
	  for index from 0 below limit
	  do (setf (char result index) char))
    result))

(define-message-sender send-startup-message
    (database user &optional (command-line "") (backend-tty ""))
  (int32 296)                           ; Length
  (int32 #x00020000)                    ; Version 2.0
  (limstring (pad-limstring database 64))
  (limstring (pad-limstring user 32))
  (limstring (pad-limstring command-line 64))
  (limstring (pad-limstring "" 64))     ; Unused
  (limstring (pad-limstring backend-tty 64)))

(define-message-sender send-terminate-message ()
  (byte #\X))

(define-message-sender send-unencrypted-password-message (password)
  (int32 (+ 5 (length password)))
  (string password))

(define-message-sender send-query-message (query)
  (byte #\Q)
  (string query))

(define-message-sender send-encrypted-password-message (crypted-password)
  (int32 (+ 5 (length crypted-password)))
  (string crypted-password))

(define-message-sender send-cancel-request (pid key)
  (int32 16)                            ; Length
  (int32 80877102)                      ; Magic
  (int32 pid)
  (int32 key))


(defun read-socket-sequence (string stream)
"KMR -- Added to support reading from binary stream into a string"
  (declare (optimize (speed 3) (safety 0))
	   (string string))
  (dotimes (i (length string))
    (declare (fixnum i))
    (setf (char string i) (code-char (read-byte stream))))
  string)


;;; Support for encrypted password transmission

(defvar *crypt-library-loaded* nil)

(defun crypt-password (password salt)
  "Encrypt a password for transmission to a PostgreSQL server."
  (unless *crypt-library-loaded*
    (uffi:load-foreign-library 
     (uffi:find-foreign-library "libcrypt"
			   '("/usr/lib/" "/usr/local/lib/" "/lib/"))
     :supporting-libraries '("c"))
    (eval '(uffi:def-function "crypt" 
	    ((key :cstring)
	     (salt :cstring))
	    :returning :cstring))
    (setq *crypt-library-loaded* t))
   (uffi:with-cstring (password-cstring password)
     (uffi:with-cstring (salt-cstring salt)
       (uffi:convert-from-cstring 
	(funcall (fdefinition 'crypt) password-cstring salt-cstring)))))
;;; Condition hierarchy

(define-condition postgresql-condition (condition)
  ((connection :initarg :connection :reader postgresql-condition-connection)
   (message :initarg :message :reader postgresql-condition-message))
  (:report
   (lambda (c stream)
     (format stream "~@<~A occurred on connection ~A. ~:@_Reason: ~A~:@>"
	     (type-of c)
	     (postgresql-condition-connection c)
	     (postgresql-condition-message c)))))

(define-condition postgresql-error (error postgresql-condition)
  ())

(define-condition postgresql-fatal-error (postgresql-error)
  ())

(define-condition postgresql-login-error (postgresql-fatal-error)
  ())

(define-condition postgresql-warning (warning postgresql-condition)
  ())

(define-condition postgresql-notification (postgresql-condition)
  ()
  (:report
   (lambda (c stream)
     (format stream "~@<Asynchronous notification on connection ~A: ~:@_~A~:@>"
	     (postgresql-condition-connection c)
	     (postgresql-condition-message c)))))

;;; Structures

(defstruct postgresql-connection
  host
  port
  database
  user
  password
  options
  tty
  socket
  pid
  key)

(defstruct postgresql-cursor
  connection
  name
  fields)

;;; Socket stuff

(defconstant +postgresql-server-default-port+ 5432
  "Default port of PostgreSQL server.")

(defvar *postgresql-server-socket-timeout* 60
  "Timeout in seconds for reads from the PostgreSQL server.")


#+cmu
(defun open-postgresql-socket (host port)
  (etypecase host
    (pathname
     ;; Directory to unix-domain socket
     (ext:connect-to-unix-socket
      (namestring
       (make-pathname :name ".s.PGSQL" :type (princ-to-string port)
		      :defaults host))))
    (string
     (ext:connect-to-inet-socket host port))))

#+cmu
(defun open-postgresql-socket-stream (host port)
  (system:make-fd-stream
   (open-postgresql-socket host port)
   :input t :output t :element-type '(unsigned-byte 8)
   :buffering :none
   :timeout *postgresql-server-socket-timeout*))

#+allegro
(defun open-postgresql-socket-stream (host port)
  (etypecase host
    (pathname
     (let ((path (namestring
		  (make-pathname :name ".s.PGSQL" :type (princ-to-string port)
				 :defaults host))))
       (socket:make-socket :type :stream :address-family :file
			   :connect :active
			   :remote-filename path :local-filename path)))
    (string
     (socket:with-pending-connect
	 (mp:with-timeout (*postgresql-server-socket-timeout* (error "connect failed"))
	   (socket:make-socket :type :stream :address-family :internet
			       :remote-port port :remote-host host
			       :connect :active :nodelay t))))
    ))

#+lispworks
(defun open-postgresql-socket-stream (host port)
  (etypecase host
    (pathname
     (error "File sockets not supported on Lispworks."))
    (string
     (comm:open-tcp-stream host port :direction :io :element-type '(unsigned-byte 8)
			   :read-timeout *postgresql-server-socket-timeout*))
    ))

;;; Interface Functions

(defun open-postgresql-connection (&key (host (cmucl-compat:required-argument))
					(port +postgresql-server-default-port+)
					(database (cmucl-compat:required-argument))
					(user (cmucl-compat:required-argument))
					options tty password)
  "Open a connection to a PostgreSQL server with the given parameters.
Note that host, database and user arguments must be supplied.

If host is a pathname, it is assumed to name a directory containing
the local unix-domain sockets of the server, with port selecting which
of those sockets to open.  If host is a string, it is assumed to be
the name of the host running the PostgreSQL server.  In that case a
TCP connection to the given port on that host is opened in order to
communicate with the server.  In either case the port argument
defaults to `+postgresql-server-default-port+'.

Password is the clear-text password to be passed in the authentication
phase to the server.  Depending on the server set-up, it is either
passed in the clear, or encrypted via crypt and a server-supplied
salt.  In that case the alien function specified by `*crypt-library*'
and `*crypt-function-name*' is used for encryption.

Note that all the arguments (including the clear-text password
argument) are stored in the `postgresql-connection' structure, in
order to facilitate automatic reconnection in case of communication
troubles."
  (reopen-postgresql-connection
   (make-postgresql-connection :host host :port port
			       :options (or options "") :tty (or tty "")
			       :database database :user user
			       :password (or password ""))))

(defun reopen-postgresql-connection (connection)
  "Reopen the given PostgreSQL connection.  Closes any existing
connection, if it is still open."
  (when (postgresql-connection-open-p connection)
    (close-postgresql-connection connection))
  (let ((socket (open-postgresql-socket-stream 
		  (postgresql-connection-host connection)
		  (postgresql-connection-port connection))))
    (unwind-protect
	 (progn
	   (setf (postgresql-connection-socket connection) socket)
	   (send-startup-message socket
				 (postgresql-connection-database connection)
				 (postgresql-connection-user connection)
				 (postgresql-connection-options connection)
				 (postgresql-connection-tty connection))
	   (force-output socket)
	   (loop
	       (case (read-socket-value 'int8 socket)
		 (#.+authentication-message+
		  (case (read-socket-value 'int32 socket)
		    (0 (return))
		    ((1 2)
		     (error 'postgresql-login-error
			    :connection connection
			    :message
			    "Postmaster expects unsupported Kerberos authentication."))
		    (3
		     (send-unencrypted-password-message
		      socket
		      (postgresql-connection-password connection)))
		    (4
		     (let ((salt (make-string 2)))
		       (read-socket-sequence salt socket)
		       (send-encrypted-password-message
			socket
			(crypt-password
			 (postgresql-connection-password connection) salt))))
		    (t
		     (error 'postgresql-login-error
			    :connection connection
			    :message
			    "Postmaster expects unknown authentication method."))))
		 (#.+error-response-message+
		  (let ((message (read-socket-value 'string socket)))
		    (error 'postgresql-login-error
			   :connection connection :message message)))
		 (t
		  (error 'postgresql-login-error
			 :connection connection
			 :message
			 "Received garbled message from Postmaster"))))
	   ;; Start backend communication
	   (force-output socket)
	   (loop
	       (case (read-socket-value 'int8 socket)
		 (#.+backend-key-message+
		  (setf (postgresql-connection-pid connection)
			(read-socket-value 'int32 socket)
			(postgresql-connection-key connection)
			(read-socket-value 'int32 socket)))
		 (#.+ready-for-query-message+
		  (setq socket nil)
		  (return connection))
		 (#.+error-response-message+
		  (let ((message (read-socket-value 'string socket)))
		    (error 'postgresql-login-error
			   :connection connection
			   :message message)))
		 (#.+notice-response-message+
		  (let ((message (read-socket-value 'string socket)))
		    (warn 'postgresql-warning :connection connection
			  :message message)))
		 (t
		  (error 'postgresql-login-error
			 :connection connection
			 :message
			 "Received garbled message from Postmaster")))))
      (when socket
	(close socket)))))

(defun close-postgresql-connection (connection &optional abort)
  (unless abort
    (ignore-errors
      (send-terminate-message (postgresql-connection-socket connection))))
  (close (postgresql-connection-socket connection)))

(defun postgresql-connection-open-p (connection)
  (let ((socket (postgresql-connection-socket connection)))
    (and socket (streamp socket) (open-stream-p socket))))

(defun ensure-open-postgresql-connection (connection)
  (unless (postgresql-connection-open-p connection)
    (reopen-postgresql-connection connection)))

(defun process-async-messages (connection)
  (assert (postgresql-connection-open-p connection))
  ;; Process any asnychronous messages
  (loop with socket = (postgresql-connection-socket connection)
	while (listen socket)
	do
	(case (read-socket-value 'int8 socket)
	  (#.+notice-response-message+
	   (let ((message (read-socket-value 'string socket)))
	     (warn 'postgresql-warning :connection connection
		   :message message)))
	  (#.+notification-response-message+
	   (let ((pid (read-socket-value 'int32 socket))
		 (message (read-socket-value 'string socket)))
	     (when (= pid (postgresql-connection-pid connection))
	       (signal 'postgresql-notification :connection connection
		       :message message))))
	  (t
	   (close-postgresql-connection connection)
	   (error 'postgresql-fatal-error :connection connection
		  :message "Received garbled message from backend")))))

(defun start-query-execution (connection query)
  (ensure-open-postgresql-connection connection)
  (process-async-messages connection)
  (send-query-message (postgresql-connection-socket connection) query)
  (force-output (postgresql-connection-socket connection)))

(defun wait-for-query-results (connection)
  (assert (postgresql-connection-open-p connection))
  (let ((socket (postgresql-connection-socket connection))
	(cursor-name nil)
	(error nil))
    (loop
	(case (read-socket-value 'int8 socket)
	  (#.+completed-response-message+
	   (return (values :completed (read-socket-value 'string socket))))
	  (#.+cursor-response-message+
	   (setq cursor-name (read-socket-value 'string socket)))
	  (#.+row-description-message+
	   (let* ((count (read-socket-value 'int16 socket))
		  (fields
		   (loop repeat count
		     collect
		     (list
		      (read-socket-value 'string socket)
		      (read-socket-value 'int32 socket)
		      (read-socket-value 'int16 socket)
		      (read-socket-value 'int32 socket)))))
	     (return
	       (values :cursor
		       (make-postgresql-cursor :connection connection
					       :name cursor-name
					       :fields fields)))))
	  (#.+copy-in-response-message+
	   (return :copy-in))
	  (#.+copy-out-response-message+
	   (return :copy-out))
	  (#.+ready-for-query-message+
	   (when error
	     (error error))
	   (return nil))
	  (#.+error-response-message+
	   (let ((message (read-socket-value 'string socket)))
	     (setq error
		   (make-condition 'postgresql-error
				   :connection connection :message message))))
	  (#.+notice-response-message+
	   (let ((message (read-socket-value 'string socket)))
	     (warn 'postgresql-warning
		   :connection connection :message message)))
	  (#.+notification-response-message+
	   (let ((pid (read-socket-value 'int32 socket))
		 (message (read-socket-value 'string socket)))
	     (when (= pid (postgresql-connection-pid connection))
	       (signal 'postgresql-notification :connection connection
		       :message message))))
	  (t
	   (close-postgresql-connection connection)
	   (error 'postgresql-fatal-error :connection connection
		  :message "Received garbled message from backend"))))))

(defun read-null-bit-vector (socket count)
  (let ((result (make-array count :element-type 'bit)))
    (dotimes (offset (ceiling count 8))
      (loop with byte = (read-byte socket)
	    for index from (* offset 8) below (min count (* (1+ offset) 8))
	    for weight downfrom 7
	    do (setf (aref result index) (ldb (byte 1 weight) byte))))
    result))


(defun read-field (socket type)
  (let ((length (- (read-socket-value 'int32 socket) 4)))
    (case type
      ((:int32 :int64)
       (read-integer-from-socket socket length))
      (:double
       (read-double-from-socket socket length))
      (t
       (let ((result (make-string length)))
	 (read-socket-sequence result socket)
	 result)))))

(uffi:def-constant +char-code-zero+ (char-code #\0))
(uffi:def-constant +char-code-minus+ (char-code #\-))
(uffi:def-constant +char-code-plus+ (char-code #\+))
(uffi:def-constant +char-code-period+ (char-code #\.))
(uffi:def-constant +char-code-lower-e+ (char-code #\e))
(uffi:def-constant +char-code-upper-e+ (char-code #\E))

(defun read-integer-from-socket (socket length)
  (declare (fixnum length))
  (if (zerop length)
      nil
    (let ((val 0)
	  (first-char (read-byte socket))
	  (minusp nil))
      (declare (fixnum first-char))
      (decf length) ;; read first char
      (cond
       ((= first-char +char-code-minus+)
	(setq minusp t))
       ((= first-char +char-code-plus+)
	)		;; nothing to do
       (t
	(setq val (- first-char +char-code-zero+))))
      
      (dotimes (i length)
	(declare (fixnum i))
	(setq val (+
		   (* 10 val)
		   (- (read-byte socket) +char-code-zero+))))
      (if minusp
	  (- val)
	val))))

(defmacro ascii-digit (int)
  (let ((offset (gensym)))
    `(let ((,offset (- ,int +char-code-zero+)))
      (declare (fixnum ,int ,offset))
      (if (and (>= ,offset 0)
	       (< ,offset 10))
	  ,offset
	  nil))))
      
(defun read-double-from-socket (socket length)
  (declare (fixnum length))
  (let ((before-decimal 0)
	(after-decimal 0)
	(decimal-count 0)
	(exponent 0)
	(decimalp nil)
	(minusp nil)
	(result nil)
	(char (read-byte socket)))
    (declare (fixnum char exponent decimal-count))
    (decf length) ;; already read first character
    (cond
      ((= char +char-code-minus+)
       (setq minusp t))
      ((= char +char-code-plus+)
       )
      ((= char +char-code-period+)
       (setq decimalp t))
      (t
       (setq before-decimal (ascii-digit char))
       (unless before-decimal
	 (error "Unexpected value"))))
    
    (block loop
      (dotimes (i length)
	(setq char (read-byte socket))
	;;      (format t "~&len:~D, i:~D, char:~D, minusp:~A, decimalp:~A" length i char minusp decimalp)
	(let ((weight (ascii-digit char)))
	  (cond 
	   ((and weight (not decimalp)) ;; before decimal point
	    (setq before-decimal (+ weight (* 10 before-decimal))))
	   ((and weight decimalp) ;; after decimal point
	    (setq after-decimal (+ weight (* 10 after-decimal)))
	    (incf decimal-count))
	   ((and (= char +char-code-period+))
	    (setq decimalp t))
	   ((or (= char +char-code-lower-e+) 	      ;; E is for exponent
		(= char +char-code-upper-e+))
	    (setq exponent (read-integer-from-socket socket (- length i 1)))
	    (setq exponent (or exponent 0))
	    (return-from loop))
	  (t 
	   (break "Unexpected value"))
	  )
	)))
    (setq result (* (+ (coerce before-decimal 'double-float)
		       (* after-decimal 
			  (expt 10 (- decimal-count))))
		    (expt 10 exponent)))
    (if minusp
	(- result)
	result)))
	
      
#+ignore
(defun read-double-from-socket (socket length)
  (let ((result (make-string length)))
    (read-socket-sequence result socket)
    (let ((*read-default-float-format* 'double-float))
      (read-from-string result))))

(defun read-cursor-row (cursor types)
  (let* ((connection (postgresql-cursor-connection cursor))
	 (socket (postgresql-connection-socket connection))
	 (fields (postgresql-cursor-fields cursor)))
    (assert (postgresql-connection-open-p connection))
    (loop
	(let ((code (read-socket-value 'int8 socket)))
	  (case code
	    (#.+ascii-row-message+
	     (return
	       (loop with count = (length fields)
		     with null-vector = (read-null-bit-vector socket count)
		     repeat count
		     for null-bit across null-vector
		     for i from 0
		     for null-p = (zerop null-bit)
		     if null-p
		     collect nil
		     else
		     collect
		     (read-field socket (nth i types)))))
	    (#.+binary-row-message+
	     (error "NYI"))
	    (#.+completed-response-message+
	     (return (values nil (read-socket-value 'string socket))))
	    (#.+error-response-message+
	     (let ((message (read-socket-value 'string socket)))
	       (error 'postgresql-error
		      :connection connection :message message)))
	    (#.+notice-response-message+
	     (let ((message (read-socket-value 'string socket)))
	       (warn 'postgresql-warning
		     :connection connection :message message)))
	    (#.+notification-response-message+
	     (let ((pid (read-socket-value 'int32 socket))
		   (message (read-socket-value 'string socket)))
	       (when (= pid (postgresql-connection-pid connection))
		 (signal 'postgresql-notification :connection connection
			 :message message))))
	    (t
	     (close-postgresql-connection connection)
	     (error 'postgresql-fatal-error :connection connection
		    :message "Received garbled message from backend")))))))

(defun map-into-indexed (result-seq func seq)
  (dotimes (i (length seq))
    (declare (fixnum i))
    (setf (elt result-seq i)
	  (funcall func (elt seq i) i)))
  result-seq)

(defun copy-cursor-row (cursor sequence types)
  (let* ((connection (postgresql-cursor-connection cursor))
	 (socket (postgresql-connection-socket connection))
	 (fields (postgresql-cursor-fields cursor)))
    (assert (= (length fields) (length sequence)))
    (loop
	(let ((code (read-socket-value 'int8 socket)))
	  (case code
	    (#.+ascii-row-message+
	     (return
	       #+ignore
	       (let* ((count (length sequence))
		      (null-vector (read-null-bit-vector socket count)))
		 (dotimes (i count)
		   (declare (fixnum i))
		   (if (zerop (elt null-vector i))
		       (setf (elt sequence i) nil)
		       (let ((value (read-field socket (nth i types))))
			 (setf (elt sequence i) value)))))
	       (map-into-indexed
		sequence
		#'(lambda (null-bit i)
		    (if (zerop null-bit)
			nil
			(read-field socket (nth i types))))
		(read-null-bit-vector socket (length sequence)))))
	    (#.+binary-row-message+
	     (error "NYI"))
	    (#.+completed-response-message+
	     (return (values nil (read-socket-value 'string socket))))
	    (#.+error-response-message+
	     (let ((message (read-socket-value 'string socket)))
	       (error 'postgresql-error
		      :connection connection :message message)))
	    (#.+notice-response-message+
	     (let ((message (read-socket-value 'string socket)))
	       (warn 'postgresql-warning
		     :connection connection :message message)))
	    (#.+notification-response-message+
	     (let ((pid (read-socket-value 'int32 socket))
		   (message (read-socket-value 'string socket)))
	       (when (= pid (postgresql-connection-pid connection))
		 (signal 'postgresql-notification :connection connection
			 :message message))))
	    (t
	     (close-postgresql-connection connection)
	     (error 'postgresql-fatal-error :connection connection
		    :message "Received garbled message from backend")))))))

(defun skip-cursor-row (cursor)
  (let* ((connection (postgresql-cursor-connection cursor))
	 (socket (postgresql-connection-socket connection))
	 (fields (postgresql-cursor-fields cursor)))
    (loop
	(let ((code (read-socket-value 'int8 socket)))
	  (case code
	    (#.+ascii-row-message+
	     (loop for null-bit across
		   (read-null-bit-vector socket (length fields))
		   do
		   (unless (zerop null-bit)
		     (let* ((length (read-socket-value 'int32 socket)))
		       (loop repeat (- length 4) do (read-byte socket)))))
	     (return t))
	    (#.+binary-row-message+
	     (error "NYI"))
	    (#.+completed-response-message+
	     (return (values nil (read-socket-value 'string socket))))
	    (#.+error-response-message+
	     (let ((message (read-socket-value 'string socket)))
	       (error 'postgresql-error
		      :connection connection :message message)))
	    (#.+notice-response-message+
	     (let ((message (read-socket-value 'string socket)))
	       (warn 'postgresql-warning
		     :connection connection :message message)))
	    (#.+notification-response-message+
	     (let ((pid (read-socket-value 'int32 socket))
		   (message (read-socket-value 'string socket)))
	       (when (= pid (postgresql-connection-pid connection))
		 (signal 'postgresql-notification :connection connection
			 :message message))))
	    (t
	     (close-postgresql-connection connection)
	     (error 'postgresql-fatal-error :connection connection
		    :message "Received garbled message from backend")))))))

(defun run-query (connection query &optional (types nil))
  (start-query-execution connection query)
  (multiple-value-bind (status cursor)
      (wait-for-query-results connection)
    (assert (eq status :cursor))
    (loop for row = (read-cursor-row cursor types)
	  while row
	  collect row
	  finally
	  (wait-for-query-results connection))))
