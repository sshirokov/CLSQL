;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; ======================================================================
;;;; File:    test-fddl.lisp
;;;; Author:  Marcus Pearce <m.t.pearce@city.ac.uk>
;;;; Created: 30/03/2004
;;;; Updated: <04/04/2004 11:53:29 marcusp>
;;;; ======================================================================
;;;;
;;;; Description ==========================================================
;;;; ======================================================================
;;;;
;;;; Tests for the CLSQL-USQL Functional Data Definition Language
;;;; (FDDL).
;;;; 
;;;; ======================================================================

(in-package :clsql-usql-tests)

#.(usql:locally-enable-sql-reader-syntax)

;; list current tables 
(deftest :fddl/table/1
    (apply #'values 
           (sort (mapcar #'string-downcase
                         (usql:list-tables :owner *test-database-user*))
                 #'string>))
  "usql_object_v" "employee" "company")

;; create a table, test for its existence, drop it and test again 
(deftest :fddl/table/2
    (progn (usql:create-table  [foo]
                               '(([id] integer)
                                 ([height] float)
                                 ([name] (string 24))
                                 ([comments] longchar)))
           (values
            (usql:table-exists-p [foo] :owner *test-database-user*)
            (progn
              (usql:drop-table [foo] :if-does-not-exist :ignore)
              (usql:table-exists-p [foo] :owner *test-database-user*))))
  t nil)

;; create a table, list its attributes and drop it 
(deftest :fddl/table/3
    (apply #'values 
           (progn (usql:create-table  [foo]
                                      '(([id] integer)
                                        ([height] float)
                                        ([name] (char 255))
                                        ([comments] longchar)))
                  (prog1
                      (sort (mapcar #'string-downcase
                                    (usql:list-attributes [foo]))
                            #'string<)
                    (usql:drop-table [foo] :if-does-not-exist :ignore))))
  "comments" "height" "id" "name")

(deftest :fddl/attributes/1
    (apply #'values
           (sort 
            (mapcar #'string-downcase
                    (usql:list-attributes [employee]
                                          :owner *test-database-user*))
            #'string<))
  "birthday" "companyid" "email" "emplid" "first_name" "groupid" "height"
  "last_name" "managerid" "married")

(deftest :fddl/attributes/2
    (apply #'values 
           (sort 
            (mapcar #'(lambda (a) (string-downcase (car a)))
                    (usql:list-attribute-types [employee]
                                               :owner *test-database-user*))
            #'string<))
  "birthday" "companyid" "email" "emplid" "first_name" "groupid" "height"
  "last_name" "managerid" "married")

;; create a view, test for existence, drop it and test again 
(deftest :fddl/view/1
    (progn (usql:create-view [lenins-group]
                             ;;not in sqlite 
                             ;;:column-list '([forename] [surname] [email])
                             :as [select [first-name] [last-name] [email]
                                         :from [employee]
                                         :where [= [managerid] 1]])
           (values  
            (usql:view-exists-p [lenins-group] :owner *test-database-user*)
            (progn
              (usql:drop-view [lenins-group] :if-does-not-exist :ignore)
              (usql:view-exists-p [lenins-group] :owner *test-database-user*))))
  t nil)

;; create a view, list its attributes and drop it 
(deftest :fddl/view/2
    (progn (usql:create-view [lenins-group]
                             ;;not in sqlite 
                             ;;:column-list '([forename] [surname] [email])
                              :as [select [first-name] [last-name] [email]
                                          :from [employee]
                                          :where [= [managerid] 1]])
           (prog1
	       (sort (mapcar #'string-downcase
			     (usql:list-attributes [lenins-group]))
		     #'string<)
	     (usql:drop-view [lenins-group] :if-does-not-exist :ignore)))
  ("email" "first_name" "last_name"))

;; create a view, select stuff from it and drop it 
(deftest :fddl/view/3
    (progn (usql:create-view [lenins-group]
                              :as [select [first-name] [last-name] [email]
                                          :from [employee]
                                          :where [= [managerid] 1]])
           (let ((result 
                  (list 
                   ;; Shouldn't exist 
                   (usql:select [first-name] [last-name] [email]
                                :from [lenins-group]
                                :where [= [last-name] "Lenin"])
                   ;; Should exist 
                   (car (usql:select [first-name] [last-name] [email]
                                     :from [lenins-group]
                                     :where [= [last-name] "Stalin"])))))
             (usql:drop-view [lenins-group] :if-does-not-exist :ignore)
             (apply #'values result)))
  nil ("Josef" "Stalin" "stalin@soviet.org"))

;; not in sqlite 
(deftest :fddl/view/4
    (if (eql *test-database-type* :sqlite)
        (values nil '(("Josef" "Stalin" "stalin@soviet.org")))
        (progn (usql:create-view [lenins-group]
                                 :column-list '([forename] [surname] [email])
                                 :as [select [first-name] [last-name] [email]
                                             :from [employee]
                                             :where [= [managerid] 1]])
               (let ((result 
                      (list
                       ;; Shouldn't exist 
                       (usql:select [forename] [surname] [email]
                                    :from [lenins-group]
                                    :where [= [surname] "Lenin"])
                       ;; Should exist 
                       (car (usql:select [forename] [surname] [email]
                                         :from [lenins-group]
                                         :where [= [surname] "Stalin"])))))
                 (usql:drop-view [lenins-group] :if-does-not-exist :ignore)
                 (apply #'values result))))
  nil ("Josef" "Stalin" "stalin@soviet.org"))

;; create an index, test for existence, drop it and test again 
(deftest :fddl/index/1
    (progn (usql:create-index [bar] :on [employee] :attributes
                              '([first-name] [last-name] [email]) :unique t)
           (values
            (usql:index-exists-p [bar] :owner *test-database-user*)
            (progn
              (case *test-database-type*
                (:mysql 
                 (usql:drop-index [bar] :on [employee]
                                  :if-does-not-exist :ignore))
                (t 
                 (usql:drop-index [bar]:if-does-not-exist :ignore)))
              (usql:view-exists-p [bar] :owner *test-database-user*))))
  t nil)

;; create indexes with names as strings, symbols and in square brackets 
(deftest :fddl/index/2
    (let ((names '("foo" foo [foo]))
          (result '()))
      (dolist (name names)
        (usql:create-index name :on [employee] :attributes '([emplid]))
        (push (usql:index-exists-p name :owner *test-database-user*) result)
        (case *test-database-type*
          (:mysql 
           (usql:drop-index name :on [employee] :if-does-not-exist :ignore))
          (t (usql:drop-index name :if-does-not-exist :ignore))))
      (apply #'values result))
  t t t)

;; create an sequence, test for existence, drop it and test again 
(deftest :fddl/sequence/1
    (progn (usql:create-sequence [foo])
           (values
            (usql:sequence-exists-p [foo] :owner *test-database-user*)
            (progn
              (usql:drop-sequence [foo] :if-does-not-exist :ignore)
              (usql:sequence-exists-p [foo] :owner *test-database-user*))))
  t nil)

;; create and increment a sequence
(deftest :fddl/sequence/2
    (let ((val1 nil))
      (usql:create-sequence [foo])
      (setf val1 (usql:sequence-next [foo]))
      (prog1
          (< val1 (usql:sequence-next [foo]))
        (usql:drop-sequence [foo] :if-does-not-exist :ignore)))
  t)

;; explicitly set the value of a sequence
(deftest :fddl/sequence/3
    (progn
      (usql:create-sequence [foo])
      (usql:set-sequence-position [foo] 5)
      (prog1
          (usql:sequence-next [foo])
        (usql:drop-sequence [foo] :if-does-not-exist :ignore)))
  6)

#.(usql:restore-sql-reader-syntax-state)