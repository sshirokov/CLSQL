;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:         utils.lisp
;;;; Purpose:      SQL utility functions
;;;; Programmer:   Kevin M. Rosenberg
;;;; Date Started: Mar 2002
;;;;
;;;; $Id$
;;;;
;;;; This file, part of CLSQL, is Copyright (c) 2002-2004 by Kevin M. Rosenberg
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(in-package #:clsql-base-sys)

(defun number-to-sql-string (num)
  (etypecase num
    (integer
     num)
    (rational
     (float-to-sql-string (coerce num 'double-float)))
    (number
     (float-to-sql-string num))))

(defun float-to-sql-string (num)
  "Convert exponent character for SQL"
  (let ((str (write-to-string num :readably t)))
    (cond
     ((find #\f str)
      (substitute #\e #\f str))
     ((find #\d str)
      (substitute #\e #\d str))
     ((find #\l str)
      (substitute #\e #\l str))
     ((find #\s str)
      (substitute #\e #\S str))
     ((find #\F str)
      (substitute #\e #\F str))
     ((find #\D str)
      (substitute #\e #\D str))
     ((find #\L str)
      (substitute #\e #\L str))
     ((find #\S str)
      (substitute #\e #\S str))
     (t
      str))))

(defun sql-escape (identifier)
  "Change hyphens to underscores, ensure string"
  (let* ((unescaped (etypecase identifier
                      (symbol (symbol-name identifier))
                      (string identifier)))
         (escaped (make-string (length unescaped))))
    (dotimes (i (length unescaped))
      (setf (char escaped i)
            (cond ((equal (char unescaped i) #\-)
                   #\_)
                  ;; ...
                  (t
                   (char unescaped i)))))
    escaped))


(defun sql-escape-quotes (s)
  "Escape quotes for SQL string writing"
  (substitute-string-for-char s #\' "''"))

(defun substitute-string-for-char (procstr match-char subst-str) 
"Substitutes a string for a single matching character of a string"
  (let ((pos (position match-char procstr)))
    (if pos
	(concatenate 'string
	  (subseq procstr 0 pos) subst-str
	  (substitute-string-for-char 
	   (subseq procstr (1+ pos)) match-char subst-str))
      procstr)))


(defun position-char (char string start max)
  "From KMRCL."
  (declare (optimize (speed 3) (safety 0) (space 0))
	   (fixnum start max) (simple-string string))
  (do* ((i start (1+ i)))
       ((= i max) nil)
    (declare (fixnum i))
    (when (char= char (schar string i)) (return i))))

(defun delimited-string-to-list (string &optional (separator #\space) 
						  skip-terminal)
  "Split a string with delimiter, from KMRCL."
  (declare (optimize (speed 3) (safety 0) (space 0) (compilation-speed 0))
	   (type string string)
	   (type character separator))
  (do* ((len (length string))
	(output '())
	(pos 0)
	(end (position-char separator string pos len)
	     (position-char separator string pos len)))
       ((null end)
	(if (< pos len)
	    (push (subseq string pos) output)
	    (when (or (not skip-terminal) (zerop len))
	      (push "" output)))
	(nreverse output))
    (declare (type fixnum pos len)
	     (type (or null fixnum) end))
    (push (subseq string pos end) output)
    (setq pos (1+ end))))

(defun string-to-list-connection-spec (str)
  (let ((at-pos (position-char #\@ str 0 (length str))))
    (cond
      ((and at-pos (> (length str) at-pos))
       ;; Connection spec is SQL*NET format
       (cons (subseq str (1+ at-pos))
	     (delimited-string-to-list (subseq str 0 at-pos) #\/)))
      (t
       (delimited-string-to-list str #\/)))))

#+allegro
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package '#:excl.osi)
    (require 'osi)))

(defun command-output (control-string &rest args)
  ;; Concatenates output and error since Lispworks combines
  ;; these, thus CLSQL can't depend upon separate results
  (multiple-value-bind (output error status)
      (apply #'%command-output control-string args)
    (values
     (concatenate 'string (if output output "") 
		  (if error error ""))
     status)))

(defun read-stream-to-string (in)
  (with-output-to-string (out)
    (let ((eof (gensym)))		    
      (do ((line (read-line in nil eof) 
		 (read-line in nil eof)))
	  ((eq line eof))
	(format out "~A~%" line)))))
	
;; From KMRCL
(defun %command-output (control-string &rest args)
  "Interpolate ARGS into CONTROL-STRING as if by FORMAT, and
synchronously execute the result using a Bourne-compatible shell, 
returns (VALUES string-output error-output exit-status)"
  (let ((command (apply #'format nil control-string args)))
    #+sbcl
    (let ((process (sb-ext:run-program  
		    "/bin/sh"
		    (list "-c" command)
		    :input nil :output :stream :error :stream)))
      (values
       (sb-impl::process-output process)
       (sb-impl::process-error process)
       (sb-impl::process-exit-code process)))
    
    #+(or cmu scl)
    (let ((process (ext:run-program  
		    "/bin/sh"
		    (list "-c" command)
		    :input nil :output :stream :error :stream)))
      (values
       (ext::process-output process)
       (ext::process-error process)
       (ext::process-exit-code process)))    

    #+allegro
    (multiple-value-bind (output error status)
	(excl.osi:command-output command :whole t)
      (values output error status))
    
    #+lispworks
    ;; BUG: Lispworks combines output and error streams
    (let ((output (make-output-string-stream)))
      (unwind-protect
	  (let ((status 
		 (system:call-system-showing-output
		  command
		  :shell-type "/bin/sh"
		  :output-stream output)))
	    (values (get-output-string output) nil status))
	(close output)))
    
    #+clisp		
    ;; BUG: CLisp doesn't allow output to user-specified stream
    (values
     nil
     nil
     (ext:run-shell-command  command :output :terminal :wait t))
    
    #+openmcl
    (let* ((process (ccl:run-program  
		     "/bin/sh"
		     (list "-c" command)
		     :input nil :output :stream :error :stream
		     :wait t))
	   (output (read-stream-to-string (ccl::external-process-output-stream process)))
	   (error (read-stream-to-string (ccl::external-process-error-stream process))))
      (close (ccl::external-process-output-stream process))
      (close (ccl::external-process-error-stream process))
      (values output
	      error
	      (nth-value 1 (ccl::external-process-status process))))
  
    #-(or openmcl clisp lispworks allegro scl cmu sbcl)
    (error "COMMAND-OUTPUT not implemented for this Lisp")

    ))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (char= #\a (schar (symbol-name '#:a) 0))
    (pushnew :lowercase-reader *features*)))

(defun string-default-case (str)
  #-lowercase-reader
  (string-upcase str)
  #+lowercase-reader
  (string-downcase str))

;; From KMRCL
(defun ensure-keyword (name)
  "Returns keyword for a name"
  (etypecase name
    (keyword name)
    (string (nth-value 0 (intern (string-default-case name) :keyword)))
    (symbol (nth-value 0 (intern (symbol-name name) :keyword)))))
