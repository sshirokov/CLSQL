;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; ======================================================================
;;;; File:    test-ooddl.lisp
;;;; Author:  Marcus Pearce <m.t.pearce@city.ac.uk>
;;;; Created: 30/03/2004
;;;; Updated: <04/04/2004 11:52:11 marcusp>
;;;; ======================================================================
;;;;
;;;; Description ==========================================================
;;;; ======================================================================
;;;;
;;;; Tests for the CLSQL-USQL Object Oriented Data Definition Language
;;;; (OODDL).
;;;;
;;;; ======================================================================


(in-package :clsql-usql-tests)

#.(usql:locally-enable-sql-reader-syntax)

;; Ensure slots inherited from standard-classes are :virtual
(deftest :ooddl/metaclass/1
    (values 
     (usql-sys::view-class-slot-db-kind
      (usql-sys::slotdef-for-slot-with-class 'extraterrestrial
                                             (find-class 'person)))
     (usql-sys::view-class-slot-db-kind
      (usql-sys::slotdef-for-slot-with-class 'hobby (find-class 'person))))
  :virtual :virtual)

;; Ensure all slots in view-class are view-class-effective-slot-definition
(deftest :ooddl/metaclass/2
    (values
     (every #'(lambda (slotd)
                (typep slotd 'usql-sys::view-class-effective-slot-definition))
            (usql-sys::class-slots (find-class 'person)))
     (every #'(lambda (slotd)
                (typep slotd 'usql-sys::view-class-effective-slot-definition))
            (usql-sys::class-slots (find-class 'employee)))
     (every #'(lambda (slotd)
                (typep slotd 'usql-sys::view-class-effective-slot-definition))
            (usql-sys::class-slots (find-class 'company))))
  t t t)

(deftest :ooddl/join/1
    (mapcar #'(lambda (e)
                (slot-value e 'companyid))
            (company-employees company1))
  (1 1 1 1 1 1 1 1 1 1))

(deftest :ooddl/join/2
    (slot-value (president company1) 'last-name)
  "Lenin")

(deftest :ooddl/join/3
    (slot-value (employee-manager employee2) 'last-name)
  "Lenin")

(deftest :ooddl/time/1
    (let* ((now (usql:get-time)))
      (when (member *test-database-type* '(:postgresql :postgresql-socket))
        (usql:execute-command "set datestyle to 'iso'"))
      (usql:update-records [employee] :av-pairs `((birthday ,now))
                           :where [= [emplid] 1])
      (let ((dbobj (car (usql:select 'employee :where [= [birthday] now]))))
        (values
         (slot-value dbobj 'last-name)
         (usql:time= (slot-value dbobj 'birthday) now))))
  "Lenin" t)

(deftest :ooddl/time/2
    (let* ((now (usql:get-time))
           (fail-index -1))
      (when (member *test-database-type* '(:postgresql :postgresql-socket))
        (usql:execute-command "set datestyle to 'iso'"))
      (dotimes (x 40)
        (usql:update-records [employee] :av-pairs `((birthday ,now))
                             :where [= [emplid] 1])
        (let ((dbobj (car (usql:select 'employee :where [= [birthday] now]))))
          (unless (usql:time= (slot-value dbobj 'birthday) now)
            (setf fail-index x))
          (setf now (usql:roll now :day (* 10 x)))))
      fail-index)
  -1)

#.(usql:restore-sql-reader-syntax-state)