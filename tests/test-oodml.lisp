;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; ======================================================================
;;;; File:    test-oodml.lisp
;;;; Author:  Marcus Pearce <m.t.pearce@city.ac.uk>
;;;; Created: 01/04/2004
;;;; Updated: $Id: $
;;;; ======================================================================
;;;;
;;;; Description ==========================================================
;;;; ======================================================================
;;;;
;;;; Tests for the CLSQL Object Oriented Data Definition Language
;;;; (OODML).
;;;;
;;;; ======================================================================

(in-package #:clsql-tests)

#.(clsql:locally-enable-sql-reader-syntax)

(deftest :oodml/select/1
    (mapcar #'(lambda (e) (slot-value e 'last-name))
            (clsql:select 'employee :order-by [last-name]))
  ("Andropov" "Brezhnev" "Chernenko" "Gorbachev" "Kruschev" "Lenin" "Putin"
              "Stalin" "Trotsky" "Yeltsin"))

(deftest :oodml/select/2
    (mapcar #'(lambda (e) (slot-value e 'name))
            (clsql:select 'company))
  ("Widgets Inc."))

(deftest :oodml/select/3
    (mapcar #'(lambda (e) (slot-value e 'companyid))
            (clsql:select 'employee
                         :where [and [= [slot-value 'employee 'companyid]
                                        [slot-value 'company 'companyid]]
                                     [= [slot-value 'company 'name]
                                        "Widgets Inc."]]))
  (1 1 1 1 1 1 1 1 1 1))

(deftest :oodml/select/4
    (mapcar #'(lambda (e)
                (concatenate 'string (slot-value e 'first-name)
                             " "
                             (slot-value e 'last-name)))
            (clsql:select 'employee :where [= [slot-value 'employee 'first-name]
                                             "Vladamir"]
                         :order-by [last-name]))
  ("Vladamir Lenin" "Vladamir Putin"))

;; sqlite fails this because it is typeless 
(deftest :oodml/select/5
    (length (sql:select 'employee :where [married]))
  3)

;; tests update-records-from-instance 
(deftest :oodml/update-records/1
    (values
     (progn
       (let ((lenin (car (clsql:select 'employee
                                      :where [= [slot-value 'employee 'emplid]
                                                1]))))
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
                                                1]))))
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
                                                1]))))
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
                        :where [= [slot-value 'employee 'emplid] 1])))
     (progn
       (setf (slot-value employee1 'email) "lenin-nospam@soviet.org")
       (clsql:update-record-from-slot employee1 'email)
       (employee-email
        (car (clsql:select 'employee
                          :where [= [slot-value 'employee 'emplid] 1]))))
     (progn 
       (setf (slot-value employee1 'email) "lenin@soviet.org")
       (clsql:update-record-from-slot employee1 'email)
       (employee-email
        (car (clsql:select 'employee
                          :where [= [slot-value 'employee 'emplid] 1])))))
  "lenin@soviet.org" "lenin-nospam@soviet.org" "lenin@soviet.org")

;; tests update-record-from-slots
(deftest :oodml/update-records/3
    (values
     (let ((lenin (car (clsql:select 'employee
                                    :where [= [slot-value 'employee 'emplid]
                                              1]))))
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
                                                1]))))
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
                                                1]))))
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


;(deftest :oodml/iteration/1
;    (clsql:do-query ((e) [select 'clsql-tests::employee :where [married]
;                                :order-by [emplid]])
;      (slot-value e last-name))
;  ("Lenin" "Stalin" "Trotsky"))

;(deftest :oodml/iteration/2
;    (clsql:map-query 'list #'last-name [select 'employee :where [married]
;                                              :order-by [emplid]])
;  ("Lenin" "Stalin" "Trotsky"))

;(deftest :oodml/iteration/3
;    (loop for (e) being the tuples in 
;          [select 'employee :where [married] :order-by [emplid]]
;          collect (slot-value e 'last-name))
;  ("Lenin" "Stalin" "Trotsky"))


#.(clsql:restore-sql-reader-syntax-state)
