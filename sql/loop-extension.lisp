;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:    loop-extension.lisp
;;;; Purpose: Extensions to the Loop macro for CLSQL
;;;;
;;;; Copyright (c) 2001-2004 Kevin Rosenberg and (c) 1999-2001 Pierre R. Mai
;;;;
;;;; $Id$
;;;; *************************************************************************

(in-package #:cl-user)

#+(or allegro sbcl)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defpackage #:ansi-loop 
    (:import-from #+sbcl #:sb-loop #+allegro #:excl
		  #:*loop-epilogue*
		  #:*loop-ansi-universe* 
		  #:add-loop-path)))

#+(or allegro sbcl)
(defun ansi-loop::loop-gentemp (&optional (pref 'loopva-))
  (gensym (string pref)))

#+(or cmu scl sbcl openmcl allegro)
(defun loop-record-iteration-path (variable data-type prep-phrases)
  (let ((in-phrase nil)
	(from-phrase nil))
    (loop for (prep . rest) in prep-phrases
	  do
	  (case prep
	    ((:in :of)
	     (when in-phrase
	       (error 'clsql:sql-user-error
		      :message
		      (format nil
			      "Duplicate OF or IN iteration path: ~S."
			      (cons prep rest))))
	     (setq in-phrase rest))
	    ((:from)
	     (when from-phrase
	       (error 'clsql:sql-user-error
		      :message
		      (format nil
			      "Duplicate FROM iteration path: ~S."
			      (cons prep rest))))
	     (setq from-phrase rest))
	    (t
	     (error 'clsql:sql-user-error
		    :message
		    (format nil"Unknown preposition: ~S." prep)))))
    (unless in-phrase
      (error 'clsql:sql-user-error 
	     :message "Missing OF or IN iteration path."))
    (unless from-phrase
      (setq from-phrase '(clsql-sys:*default-database*)))

    (unless (consp variable)
      (setq variable (list variable)))

    (cond
     ;; object query
     ((and (consp (first in-phrase))
	   (string-equal "sql-query" (symbol-name (caar in-phrase)))
	   (consp (second (first in-phrase)))
	   (eq 'quote (first (second (first in-phrase))))
	   (symbolp (second (second (first in-phrase)))))

       (let ((result-var (ansi-loop::loop-gentemp
			      'loop-record-result-))
	     (step-var (ansi-loop::loop-gentemp 'loop-record-step-)))
	 `(((,variable nil ,@(and data-type (list data-type)))
	    (,result-var (clsql-sys:query ,(first in-phrase)))
	    (,step-var nil))
	   ()
	   ()
	   ()
	   (if (null ,result-var)
	       t
	       (progn
		 (setq ,step-var (first ,result-var))
		 (setq ,result-var (rest ,result-var))
		 nil))
	   (,variable ,step-var)
	   (null ,result-var)
	   ()
	   (if (null ,result-var)
	       t
	       (progn
		 (setq ,step-var (first ,result-var))
		 (setq ,result-var (rest ,result-var))
		 nil))
	   (,variable ,step-var))))
      
      ((consp variable)
       (let ((query-var (ansi-loop::loop-gentemp 'loop-record-))
	     (db-var (ansi-loop::loop-gentemp 'loop-record-database-))
	     (result-set-var (ansi-loop::loop-gentemp
			      'loop-record-result-set-))
	     (step-var (ansi-loop::loop-gentemp 'loop-record-step-)))
	 (push `(when ,result-set-var
		  (clsql-sys:database-dump-result-set ,result-set-var ,db-var))
	       ansi-loop::*loop-epilogue*)
	 `(((,variable nil ,@(and data-type (list data-type)))
	    (,query-var ,(first in-phrase))
	    (,db-var ,(first from-phrase))
	    (,result-set-var nil)
	    (,step-var nil))
	   ((multiple-value-bind (%rs %cols)
		(clsql-sys:database-query-result-set ,query-var ,db-var :result-types :auto)
	      (setq ,result-set-var %rs ,step-var (make-list %cols))))
	   ()
	   ()
	   (not (clsql-sys:database-store-next-row ,result-set-var ,db-var ,step-var))
	   (,variable ,step-var)
	   (not ,result-set-var)
	   ()
	   (not (clsql-sys:database-store-next-row ,result-set-var ,db-var ,step-var))
	   (,variable ,step-var)))))))

#+(or cmu scl sbcl openmcl allegro)
(ansi-loop::add-loop-path '(record records tuple tuples)
			  'loop-record-iteration-path
			  ansi-loop::*loop-ansi-universe*
			  :preposition-groups '((:of :in) (:from))
			  :inclusive-permitted nil)

#+lispworks 
(eval-when (:compile-toplevel :load-toplevel :execute)
  (in-package loop))

#+lispworks
(cl-user::define-loop-method (record records tuple tuples) clsql-loop-method 
  (in of from))

#+lispworks
(defun clsql-loop-method (method-name iter-var iter-var-data-type 
			  prep-phrases inclusive? allowed-preps 
			  method-specific-data)
  (declare (ignore method-name inclusive? allowed-preps method-specific-data))
  (let ((in-phrase nil)
	(from-phrase nil))
    (loop for (prep . rest) in prep-phrases
	  do
	  (cond
	    ((or (eq prep 'in) (eq prep 'of))
	     (when in-phrase
	       (error 'clsql:sql-user-error
		      :message
		      (format nil "Duplicate OF or IN iteration path: ~S."
			      (cons prep rest))))
	     (setq in-phrase rest))
	    ((eq prep 'from)
	     (when from-phrase
	       (error 'clsql:sql-user-error
		      :message
		      (format nil "Duplicate FROM iteration path: ~S."
			      (cons prep rest))))
	     (setq from-phrase rest))
	    (t
	     (error 'clsql:sql-user-error
		    :message (format nil "Unknown preposition: ~S." prep)))))
    (unless in-phrase
      (error 'clsql:sql-user-error 
	     :message "Missing OF or IN iteration path."))
    (unless from-phrase
      (setq from-phrase '(clsql:*default-database*)))

    (unless (consp iter-var)
      (setq iter-var (list iter-var)))

    (cond
     ;; object query
     ((and (consp in-phrase)
	   (string-equal "sql-query" (symbol-name (car in-phrase)))
	   (consp (second in-phrase))
	   (eq 'quote (first (second in-phrase)))
	   (symbolp (second (second in-phrase))))

       (let ((result-var (gensym "LOOP-RECORD-RESULT-"))
	     (step-var (gensym "LOOP-RECORD-STEP-")))
	 (values
	  t
	  nil
	  `(,@(mapcar (lambda (v) `(,v nil)) iter-var)
	      (,result-var (clsql:query ,in-phrase))
	      (,step-var nil))
	  ()
	  ()
	  ()
	  `((if (null ,result-var)
		t
		(progn
		  (setq ,step-var (first ,result-var))
		  (setq ,result-var (rest ,result-var))
		  nil)))
	  `(,iter-var ,step-var)
	  `((if (null ,result-var)
		t
		(progn
		  (setq ,step-var (first ,result-var))
		  (setq ,result-var (rest ,result-var))
		  nil)))
	   `(,iter-var ,step-var)
	   ()
	   ()
	   )))
      
      ((consp iter-var)
       (let ((query-var (gensym "LOOP-RECORD-"))
	     (db-var (gensym "LOOP-RECORD-DATABASE-"))
	     (result-set-var (gensym "LOOP-RECORD-RESULT-SET-"))
	     (step-var (gensym "LOOP-RECORD-STEP-")))
	 (values
	  t
	  nil
	  `(,@(mapcar (lambda (v) `(,v nil)) iter-var)
	    (,query-var ,in-phrase)
	    (,db-var ,(first from-phrase))
	    (,result-set-var nil)
	    (,step-var nil))
	  `((multiple-value-bind (%rs %cols)
		(clsql-sys:database-query-result-set ,query-var ,db-var :result-types :auto)
	      (setq ,result-set-var %rs ,step-var (make-list %cols))))
	  ()
	  ()
	  `((unless (clsql-sys:database-store-next-row ,result-set-var ,db-var ,step-var)
	      (when ,result-set-var
		(clsql-sys:database-dump-result-set ,result-set-var ,db-var))
	      t))
	  `(,iter-var ,step-var)
	  `((unless (clsql-sys:database-store-next-row ,result-set-var ,db-var ,step-var)
	      (when ,result-set-var
		(clsql-sys:database-dump-result-set ,result-set-var ,db-var))
	      t))
	  `(,iter-var ,step-var)
	  ()
	  ()))))))

