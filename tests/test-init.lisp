;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; ======================================================================
;;;; File:    test-init.lisp
;;;; Authors: Marcus Pearce <m.t.pearce@city.ac.uk>, Kevin Rosenberg
;;;; Created: 30/03/2004
;;;; Updated: $Id$
;;;;
;;;; Initialisation utilities for running regression tests on CLSQL. 
;;;;
;;;; This file is part of CLSQL.
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; ======================================================================

(in-package #:clsql-tests)

(defvar *report-stream* nil "Stream to send text report.")
(defvar *sexp-report-stream* nil "Stream to send sexp report.")
(defvar *rt-connection*)
(defvar *rt-fddl*)
(defvar *rt-fdml*)
(defvar *rt-ooddl*)
(defvar *rt-oodml*)
(defvar *rt-syntax*)
(defvar *rt-time*)

(defvar *test-database-type* nil)
(defvar *test-database-underlying-type* nil)
(defvar *test-database-user* nil)

(defclass thing ()
  ((extraterrestrial :initform nil :initarg :extraterrestrial)))

(def-view-class person (thing)
  ((height :db-kind :base :accessor height :type float
           :initarg :height)
   (married :db-kind :base :accessor married :type boolean
            :initarg :married)
   (birthday :type clsql-base:wall-time :initarg :birthday)
   (hobby :db-kind :virtual :initarg :hobby :initform nil)))
  
(def-view-class employee (person)
  ((emplid
    :db-kind :key
    :db-constraints :not-null
    :type integer
    :initarg :emplid)
   (groupid
    :db-kind :key
    :db-constraints :not-null
    :type integer
    :initarg :groupid)
   (first-name
    :accessor first-name
    :type (string 30)
    :initarg :first-name)
   (last-name
    :accessor last-name
    :type (string 30)
    :initarg :last-name)
   (email
    :accessor employee-email
    :type (string 100)
    :initarg :email)
   (companyid
    :type integer)
   (company
    :accessor employee-company
    :db-kind :join
    :db-info (:join-class company
			  :home-key companyid
			  :foreign-key companyid
			  :set nil))
   (managerid
    :type integer)
   (manager
    :accessor employee-manager
    :db-kind :join
    :db-info (:join-class employee
			  :home-key managerid
			  :foreign-key emplid
			  :set nil)))
  (:base-table employee))

(def-view-class company ()
  ((companyid
    :db-kind :key
    :db-constraints :not-null
    :type integer
    :initarg :companyid)
   (groupid
    :db-kind :key
    :db-constraints :not-null
    :type integer
    :initarg :groupid)
   (name
    :type (string 100)
    :initarg :name)
   (presidentid
    :type integer)
   (president
    :reader president
    :db-kind :join
    :db-info (:join-class employee
			  :home-key presidentid
			  :foreign-key emplid
			  :set nil))
   (employees
    :reader company-employees
    :db-kind :join
    :db-info (:join-class employee
			  :home-key (companyid groupid)
			  :foreign-key (companyid groupid)
			  :set t)))
  (:base-table company))


(def-view-class address ()
  ((emplid
    :db-kind :key
    :db-constraints :not-null
    :type integer
    :initarg :emplid)
   (street-number
    :type integer
    :initarg :street-number)
   (street-name
    :type (string 30)
    :void-value ""
    :initarg :street-name)
   (city
    :column "city_field"
    :void-value "no city"
    :type (string 30)
    :initarg :city)
   (postal-code
    :column "zip"
    :type integer
    :void-value 0
    :initarg :postal-code)))

(defun test-connect-to-database (db-type spec)
  (when (db-backend-has-create/destroy-db? db-type)
    (ignore-errors (destroy-database spec :database-type db-type))
    (ignore-errors (create-database spec :database-type db-type)))
  
  (setf *test-database-type* db-type)
  (when (>= (length spec) 3)
    (setq *test-database-user* (third spec)))
  
  ;; Connect to the database
  (clsql:connect spec
		 :database-type db-type
		 :make-default t
		 :if-exists :old)
  
  ;; Ensure database is empty
  (truncate-database :database *default-database*)
  
  (setf *test-database-underlying-type*
	(clsql-sys:database-underlying-type *default-database*))
  
  *default-database*)

(defparameter company1 nil)
(defparameter employee1 nil)
(defparameter employee2 nil)
(defparameter employee3 nil)
(defparameter employee4 nil)
(defparameter employee5 nil)
(defparameter employee6 nil)
(defparameter employee7 nil)
(defparameter employee8 nil)
(defparameter employee9 nil)
(defparameter employee10 nil)
(defparameter address1 nil)
(defparameter address2 nil)

(defun test-initialise-database ()
  (test-basic-initialize)
  
  (let ((*backend-warning-behavior*
	 (if (member *test-database-type* '(:postgresql :postgresql-socket))
	     :ignore
	   :warn)))
    (clsql:create-view-from-class 'employee)
    (clsql:create-view-from-class 'company)
    (clsql:create-view-from-class 'address))

  (setf company1 (make-instance 'company
		   :companyid 1
		   :groupid 1
		   :name "Widgets Inc.")
	employee1 (make-instance 'employee
		    :emplid 1
		    :groupid 1
		    :married t 
		    :height (1+ (random 1.00))
		    :birthday (clsql-base:get-time)
		    :first-name "Vladamir"
		    :last-name "Lenin"
		    :email "lenin@soviet.org")
	employee2 (make-instance 'employee
		    :emplid 2
		    :groupid 1
		    :height (1+ (random 1.00))
		    :married t 
		    :birthday (clsql-base:get-time)
		    :first-name "Josef"
		    :last-name "Stalin"
		    :email "stalin@soviet.org")
	employee3 (make-instance 'employee
		    :emplid 3
		    :groupid 1
		    :height (1+ (random 1.00))
		    :married t 
		    :birthday (clsql-base:get-time)
		    :first-name "Leon"
		    :last-name "Trotsky"
		    :email "trotsky@soviet.org")
	employee4 (make-instance 'employee
		    :emplid 4
		    :groupid 1
		    :height (1+ (random 1.00))
		    :married nil
		    :birthday (clsql-base:get-time)
		    :first-name "Nikita"
		    :last-name "Kruschev"
		    :email "kruschev@soviet.org")
	
	employee5 (make-instance 'employee
		    :emplid 5
		    :groupid 1
		    :married nil
		    :height (1+ (random 1.00))
		    :birthday (clsql-base:get-time)
		    :first-name "Leonid"
		    :last-name "Brezhnev"
		    :email "brezhnev@soviet.org")

	employee6 (make-instance 'employee
		    :emplid 6
		    :groupid 1
		    :married nil
		    :height (1+ (random 1.00))
		    :birthday (clsql-base:get-time)
		    :first-name "Yuri"
		    :last-name "Andropov"
		    :email "andropov@soviet.org")
	employee7 (make-instance 'employee
		    :emplid 7
		    :groupid 1
		    :height (1+ (random 1.00))
		    :married nil
		    :birthday (clsql-base:get-time)
		    :first-name "Konstantin"
		    :last-name "Chernenko"
		    :email "chernenko@soviet.org")
	employee8 (make-instance 'employee
		    :emplid 8
		    :groupid 1
		    :height (1+ (random 1.00))
		    :married nil
		    :birthday (clsql-base:get-time)
		    :first-name "Mikhail"
		    :last-name "Gorbachev"
		    :email "gorbachev@soviet.org")
	employee9 (make-instance 'employee
		    :emplid 9
		    :groupid 1 
		    :married nil
		    :height (1+ (random 1.00))
		    :birthday (clsql-base:get-time)
		    :first-name "Boris"
		    :last-name "Yeltsin"
		    :email "yeltsin@soviet.org")
	employee10 (make-instance 'employee
		     :emplid 10
		     :groupid 1
		     :married nil
		     :height (1+ (random 1.00))
		     :birthday (clsql-base:get-time)
		     :first-name "Vladamir"
		     :last-name "Putin"
		     :email "putin@soviet.org")

	address1 (make-instance 'address
				:emplid 1
				:street-number 10
				:street-name "Park Place"
				:city "Leningrad"
				:postal-code 123)

	address2 (make-instance 'address
				:emplid 2))
  
  ;; sleep to ensure birthdays are no longer at current time
  (sleep 2) 

  ;; Lenin manages everyone
  (clsql:add-to-relation employee2 'manager employee1)
  (clsql:add-to-relation employee3 'manager employee1)
  (clsql:add-to-relation employee4 'manager employee1)
  (clsql:add-to-relation employee5 'manager employee1)
  (clsql:add-to-relation employee6 'manager employee1)
  (clsql:add-to-relation employee7 'manager employee1)
  (clsql:add-to-relation employee8 'manager employee1)
  (clsql:add-to-relation employee9 'manager employee1)
  (clsql:add-to-relation employee10 'manager employee1)
  ;; Everyone works for Widgets Inc.
  (clsql:add-to-relation company1 'employees employee1)
  (clsql:add-to-relation company1 'employees employee2)
  (clsql:add-to-relation company1 'employees employee3)
  (clsql:add-to-relation company1 'employees employee4)
  (clsql:add-to-relation company1 'employees employee5)
  (clsql:add-to-relation company1 'employees employee6)
  (clsql:add-to-relation company1 'employees employee7)
  (clsql:add-to-relation company1 'employees employee8)
  (clsql:add-to-relation company1 'employees employee9)
  (clsql:add-to-relation company1 'employees employee10)
  ;; Lenin is president of Widgets Inc.
  (clsql:add-to-relation company1 'president employee1)
  ;; store these instances 
  (clsql:update-records-from-instance employee1)
  (clsql:update-records-from-instance employee2)
  (clsql:update-records-from-instance employee3)
  (clsql:update-records-from-instance employee4)
  (clsql:update-records-from-instance employee5)
  (clsql:update-records-from-instance employee6)
  (clsql:update-records-from-instance employee7)
  (clsql:update-records-from-instance employee8)
  (clsql:update-records-from-instance employee9)
  (clsql:update-records-from-instance employee10)
  (clsql:update-records-from-instance company1)
  (clsql:update-records-from-instance address1)
  (clsql:update-records-from-instance address2))

(defvar *error-count* 0)
(defvar *error-list* nil)

(defun run-function-append-report-file (function report-file)
    (let* ((report-path (etypecase report-file
			(pathname report-file)
			(string (parse-namestring report-file))))
	 (sexp-report-path (make-pathname :defaults report-path
					  :type "sexp")))
      (with-open-file (rs report-path :direction :output
			  :if-exists :append
		      :if-does-not-exist :create)
	(with-open-file (srs sexp-report-path :direction :output
			     :if-exists :append
			     :if-does-not-exist :create)
	  (funcall function :report-stream rs :sexp-report-stream srs)))))

(defun run-tests-append-report-file (report-file)
  (run-function-append-report-file 'run-tests report-file))


(defun run-tests (&key (report-stream *standard-output*) (sexp-report-stream nil))
  (let ((specs (read-specs))
	(*report-stream* report-stream)
	(*sexp-report-stream* sexp-report-stream)
	(*error-count* 0)
	(*error-list* nil))
    (unless specs
      (warn "Not running tests because test configuration file is missing")
      (return-from run-tests :skipped))
    (load-necessary-systems specs)
    (dolist (db-type +all-db-types+)
      (dolist (spec (db-type-spec db-type specs))
	(do-tests-for-backend db-type spec))))
  (zerop *error-count*))

(defun load-necessary-systems (specs)
  (dolist (db-type +all-db-types+)
    (when (db-type-spec db-type specs)
      (clsql:initialize-database-type :database-type db-type))))

(defun write-report-banner (report-type db-type stream)
  (format *report-stream* 
	  "~&
******************************************************************************
***     CLSQL ~A begun at ~A
***     ~A
***     ~A on ~A
***     Database ~A backend~A.
******************************************************************************
"
	  report-type
	  (clsql-base:format-time 
	   nil 
	   (clsql-base:utime->time (get-universal-time)))
	  (lisp-implementation-type)
	  (lisp-implementation-version)
	  (machine-type)
	  db-type
	  (if (not (eq db-type *test-database-underlying-type*))
	      (format nil " with underlying type ~A" 
		      *test-database-underlying-type*)
	      "")
	  ))

(defun do-tests-for-backend (db-type spec)
  (test-connect-to-database db-type spec)
  
  (unwind-protect
       (multiple-value-bind (test-forms skip-tests)
	  (compute-tests-for-backend db-type *test-database-underlying-type*)
	 
	 (write-report-banner "Test Suite" db-type *report-stream*)
	 
	(test-initialise-database)
	
	(regression-test:rem-all-tests)
	(dolist (test-form test-forms)
	  (eval test-form))
	
	(let ((remaining (regression-test:do-tests *report-stream*)))
	  (when (regression-test:pending-tests)
	    (incf *error-count* (length remaining))))
	
	(let ((sexp-error (list db-type 
				*test-database-underlying-type* 
				(get-universal-time)
				(length test-forms)
				(regression-test:pending-tests)
				(lisp-implementation-type) 
				(lisp-implementation-version)
				(machine-type))))
	  (when *sexp-report-stream*
	    (write sexp-error :stream *sexp-report-stream*)) 
	  (push sexp-error *error-list*))
	
	(format *report-stream* "~&Tests skipped:")
	(if skip-tests
	    (dolist (skipped skip-tests)
	      (format *report-stream*
		      "~&   ~20A ~A~%" (car skipped) (cdr skipped)))
	  (format *report-stream* " None~%")))
    (disconnect)))


(defun compute-tests-for-backend (db-type db-underlying-type)
  (let ((test-forms '())
	(skip-tests '()))
    (dolist (test-form (append (test-basic-forms)
			       *rt-connection* *rt-fddl* *rt-fdml*
			       *rt-ooddl* *rt-oodml* *rt-syntax*))
      (let ((test (second test-form)))
	(cond
	  ((and (null (db-type-has-views? db-underlying-type))
		(clsql-base::in test :fddl/view/1 :fddl/view/2 :fddl/view/3 :fddl/view/4))
	   (push (cons test "views not supported") skip-tests))
	  ((and (null (db-type-has-boolean-where? db-underlying-type))
		(clsql-base::in test :fdml/select/11 :oodml/select/5))
	   (push (cons test "boolean where not supported") skip-tests))
	  ((and (null (db-type-has-subqueries? db-underlying-type))
		(clsql-base::in test :fdml/select/5 :fdml/select/10))
	   (push (cons test "subqueries not supported") skip-tests))
	  ((and (null (db-type-transaction-capable? db-underlying-type
						    *default-database*))
		(clsql-base::in test :fdml/transaction/1 :fdml/transaction/2 :fdml/transaction/3 :fdml/transaction/4))
	   (push (cons test "transactions not supported") skip-tests))
	  ((and (null (db-type-has-fancy-math? db-underlying-type))
		(clsql-base::in test :fdml/select/1))
	   (push (cons test "fancy math not supported") skip-tests))
	  ((and (eql *test-database-type* :sqlite)
		(clsql-base::in test :fddl/view/4 :fdml/select/10))
	   (push (cons test "not supported by sqlite") skip-tests))
	  (t
	   (push test-form test-forms)))))
    (values (nreverse test-forms) (nreverse skip-tests))))

