;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; ======================================================================
;;;; File:    test-oodml.lisp
;;;; Author:  Marcus Pearce <m.t.pearce@city.ac.uk>
;;;; Created: 01/04/2004
;;;; Updated: $Id$
;;;;
;;;; Tests for the CLSQL Object Oriented Data Definition Language
;;;; (OODML).
;;;;
;;;; This file is part of CLSQL.
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; ======================================================================

(in-package #:clsql-tests)

#.(clsql:locally-enable-sql-reader-syntax)

(setq *rt-oodml*
      '(
	
	(deftest :oodml/select/1
	    (mapcar #'(lambda (e) (slot-value e 'last-name))
	     (clsql:select 'employee :order-by [last-name] :flatp t))
	  ("Andropov" "Brezhnev" "Chernenko" "Gorbachev" "Kruschev" "Lenin" "Putin"
	   "Stalin" "Trotsky" "Yeltsin"))

	(deftest :oodml/select/2
	    (mapcar #'(lambda (e) (slot-value e 'name))
	     (clsql:select 'company :flatp t))
	  ("Widgets Inc."))

	(deftest :oodml/select/3
	    (mapcar #'(lambda (e) (slot-value e 'ecompanyid))
	     (clsql:select 'employee
			   :where [and [= [slot-value 'employee 'ecompanyid]
					  [slot-value 'company 'companyid]]
				       [= [slot-value 'company 'name]
					  "Widgets Inc."]]
			   :flatp t))
	  (1 1 1 1 1 1 1 1 1 1))

	(deftest :oodml/select/4
	    (mapcar #'(lambda (e)
			(concatenate 'string (slot-value e 'first-name)
				     " "
				     (slot-value e 'last-name)))
	     (clsql:select 'employee :where [= [slot-value 'employee 'first-name]
					       "Vladamir"]
			   :flatp t		     
			   :order-by [last-name]))
	  ("Vladamir Lenin" "Vladamir Putin"))

	(deftest :oodml/select/5
	    (length (clsql:select 'employee :where [married] :flatp t))
	  3)

	(deftest :oodml/select/6
	    (let ((a (caar (clsql:select 'address :where [= 1 [addressid]]))))
	      (values
	       (slot-value a 'street-number)
	       (slot-value a 'street-name)
	       (slot-value a 'city)
	       (slot-value a 'postal-code)))
	  10 "Park Place" "Leningrad" 123)

	(deftest :oodml/select/7
	    (let ((a (caar (clsql:select 'address :where [= 2 [addressid]]))))
	      (values
	       (slot-value a 'street-number)
	       (slot-value a 'street-name)
	       (slot-value a 'city)
	       (slot-value a 'postal-code)))
	  nil "" "no city" 0)

	(deftest :oodml/select/8 
	    (mapcar #'(lambda (e) (slot-value e 'married)) 
	     (clsql:select 'employee :flatp t :order-by [emplid]))
	  (t t t nil nil nil nil nil nil nil))

	(deftest :oodml/select/9
	    (mapcar #'(lambda (pair)
			(list
			 (typep (car pair) 'address)
			 (typep (second pair) 'employee-address)
			 (slot-value (car pair) 'addressid)
			 (slot-value (second pair) 'aaddressid)
			 (slot-value (second pair) 'aemplid)))
	     (employee-addresses employee1))
	  ((t t 1 1 1) (t t 2 2 1)))

	(deftest :oodml/select/10
	    (mapcar #'(lambda (pair)
			(list
			 (typep (car pair) 'address)
			 (typep (second pair) 'employee-address)
			 (slot-value (car pair) 'addressid)
			 (slot-value (second pair) 'aaddressid)
			 (slot-value (second pair) 'aemplid)))
	     (employee-addresses employee2))
	  ((t t 2 2 2)))

	;; :retrieval :immediate should be boundp before accessed
	(deftest :oodm/retrieval/1
	    (mapcar #'(lambda (ea) (slot-boundp ea 'address))
	     (select 'employee-address :flatp t))
	  (t t t t t))

	(deftest :oodm/retrieval/2
	    (mapcar #'(lambda (ea) (typep (slot-value ea 'address) 'address))
	     (select 'employee-address :flatp t))
	  (t t t t t))

	;; tests update-records-from-instance 
	(deftest :oodml/update-records/1
	    (values
	     (progn
	       (let ((lenin (car (clsql:select 'employee
					       :where [= [slot-value 'employee 'emplid]
							 1]
					       :flatp t))))
		 (concatenate 'string
			      (first-name lenin)
			      " "
			      (last-name lenin)
			      ": "
			      (employee-email lenin))))
	     (progn
	       (setf (slot-value employee1 'first-name) "Dimitriy" 
		     (slot-value employee1 'last-name) "Ivanovich"
		     (slot-value employee1 'email) "ivanovich@soviet.org")
	       (clsql:update-records-from-instance employee1)
	       (let ((lenin (car (clsql:select 'employee
					       :where [= [slot-value 'employee 'emplid]
							 1]
					       :flatp t))))
		 (concatenate 'string
			      (first-name lenin)
			      " "
			      (last-name lenin)
			      ": "
			      (employee-email lenin))))
	     (progn 
	       (setf (slot-value employee1 'first-name) "Vladamir" 
		     (slot-value employee1 'last-name) "Lenin"
		     (slot-value employee1 'email) "lenin@soviet.org")
	       (clsql:update-records-from-instance employee1)
	       (let ((lenin (car (clsql:select 'employee
					       :where [= [slot-value 'employee 'emplid]
							 1]
					       :flatp t))))
		 (concatenate 'string
			      (first-name lenin)
			      " "
			      (last-name lenin)
			      ": "
			      (employee-email lenin)))))
	  "Vladamir Lenin: lenin@soviet.org"
	  "Dimitriy Ivanovich: ivanovich@soviet.org"
	  "Vladamir Lenin: lenin@soviet.org")

	;; tests update-record-from-slot 
	(deftest :oodml/update-records/2
	    (values
	     (employee-email
	      (car (clsql:select 'employee
				 :where [= [slot-value 'employee 'emplid] 1]
				 :flatp t)))
	     (progn
	       (setf (slot-value employee1 'email) "lenin-nospam@soviet.org")
	       (clsql:update-record-from-slot employee1 'email)
	       (employee-email
		(car (clsql:select 'employee
				   :where [= [slot-value 'employee 'emplid] 1]
				   :flatp t))))
	     (progn 
	       (setf (slot-value employee1 'email) "lenin@soviet.org")
	       (clsql:update-record-from-slot employee1 'email)
	       (employee-email
		(car (clsql:select 'employee
				   :where [= [slot-value 'employee 'emplid] 1]
				   :flatp t)))))
	  "lenin@soviet.org" "lenin-nospam@soviet.org" "lenin@soviet.org")

	;; tests update-record-from-slots
	(deftest :oodml/update-records/3
	    (values
	     (let ((lenin (car (clsql:select 'employee
					     :where [= [slot-value 'employee 'emplid]
						       1]
					     :flatp t))))
	       (concatenate 'string
			    (first-name lenin)
			    " "
			    (last-name lenin)
			    ": "
			    (employee-email lenin)))
	     (progn
	       (setf (slot-value employee1 'first-name) "Dimitriy" 
		     (slot-value employee1 'last-name) "Ivanovich"
		     (slot-value employee1 'email) "ivanovich@soviet.org")
	       (clsql:update-record-from-slots employee1 '(first-name last-name email))
	       (let ((lenin (car (clsql:select 'employee
					       :where [= [slot-value 'employee 'emplid]
							 1]
					       :flatp t))))
		 (concatenate 'string
			      (first-name lenin)
			      " "
			      (last-name lenin)
			      ": "
			      (employee-email lenin))))
	     (progn 
	       (setf (slot-value employee1 'first-name) "Vladamir" 
		     (slot-value employee1 'last-name) "Lenin"
		     (slot-value employee1 'email) "lenin@soviet.org")
	       (clsql:update-record-from-slots employee1 '(first-name last-name email))
	       (let ((lenin (car (clsql:select 'employee
					       :where [= [slot-value 'employee 'emplid]
							 1]
					       :flatp t))))
		 (concatenate 'string
			      (first-name lenin)
			      " "
			      (last-name lenin)
			      ": "
			      (employee-email lenin)))))
	  "Vladamir Lenin: lenin@soviet.org"
	  "Dimitriy Ivanovich: ivanovich@soviet.org"
	  "Vladamir Lenin: lenin@soviet.org")

	;; tests update-instance-from-records 
	(deftest :oodml/update-instance/1
	    (values
	     (concatenate 'string
			  (slot-value employee1 'first-name)
			  " "
			  (slot-value employee1 'last-name)
			  ": "
			  (slot-value employee1 'email))
	     (progn
	       (clsql:update-records [employee] 
				     :av-pairs '(([first-name] "Ivan")
						 ([last-name] "Petrov")
						 ([email] "petrov@soviet.org"))
				     :where [= [emplid] 1])
	       (clsql:update-instance-from-records employee1)
	       (concatenate 'string
			    (slot-value employee1 'first-name)
			    " "
			    (slot-value employee1 'last-name)
			    ": "
			    (slot-value employee1 'email)))
	     (progn 
	       (clsql:update-records [employee] 
				     :av-pairs '(([first-name] "Vladamir")
						 ([last-name] "Lenin")
						 ([email] "lenin@soviet.org"))
				     :where [= [emplid] 1])
	       (clsql:update-instance-from-records employee1)
	       (concatenate 'string
			    (slot-value employee1 'first-name)
			    " "
			    (slot-value employee1 'last-name)
			    ": "
			    (slot-value employee1 'email))))
	  "Vladamir Lenin: lenin@soviet.org"
	  "Ivan Petrov: petrov@soviet.org"
	  "Vladamir Lenin: lenin@soviet.org")

	;; tests update-slot-from-record 
	(deftest :oodml/update-instance/2
	    (values
	     (slot-value employee1 'email)
	     (progn
	       (clsql:update-records [employee] 
				     :av-pairs '(([email] "lenin-nospam@soviet.org"))
				     :where [= [emplid] 1])
	       (clsql:update-slot-from-record employee1 'email)
	       (slot-value employee1 'email))
	     (progn 
	       (clsql:update-records [employee] 
				     :av-pairs '(([email] "lenin@soviet.org"))
				     :where [= [emplid] 1])
	       (clsql:update-slot-from-record employee1 'email)
	       (slot-value employee1 'email)))
	  "lenin@soviet.org" "lenin-nospam@soviet.org" "lenin@soviet.org")


	(deftest :oodml/do-query/1
	    (let ((result '()))
	      (clsql:do-query ((e) [select 'employee :order-by [emplid]])
		(push (slot-value e 'last-name) result))
	      result)
	  ("Putin" "Yeltsin" "Gorbachev" "Chernenko" "Andropov" "Brezhnev" "Kruschev"
	   "Trotsky" "Stalin" "Lenin"))

	(deftest :oodml/do-query/2
	    (let ((result '()))
	      (clsql:do-query ((e c) [select 'employee 'company 
					     :where [= [slot-value 'employee 'last-name] 
						       "Lenin"]])
		(push (list (slot-value e 'last-name) (slot-value c 'name))
		      result))
	      result)
	  (("Lenin" "Widgets Inc.")))

	(deftest :oodml/map-query/1
	    (clsql:map-query 'list #'last-name [select 'employee :order-by [emplid]])
	  ("Lenin" "Stalin" "Trotsky" "Kruschev" "Brezhnev" "Andropov" "Chernenko"
	   "Gorbachev" "Yeltsin" "Putin"))

	(deftest :oodml/map-query/2 
	    (clsql:map-query 'list #'(lambda (e c) (list (slot-value e 'last-name)
							 (slot-value c 'name)))
	     [select 'employee 'company :where [= [slot-value 'employee 'last-name] 
						  "Lenin"]])
	  (("Lenin" "Widgets Inc.")))

	(deftest :oodml/iteration/3
	    (loop for (e) being the records in 
	     [select 'employee :where [< [emplid] 4] :order-by [emplid]]
	     collect (slot-value e 'last-name))
	  ("Lenin" "Stalin" "Trotsky"))

	))

#.(clsql:restore-sql-reader-syntax-state)
