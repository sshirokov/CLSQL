;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; ======================================================================
;;;; File:    test-fdml.lisp
;;;; Author:  Marcus Pearce <m.t.pearce@city.ac.uk>
;;;; Created: 30/03/2004
;;;; Updated: $Id$
;;;; ======================================================================
;;;;
;;;; Description ==========================================================
;;;; ======================================================================
;;;;
;;;; Tests for the CLSQL Functional Data Manipulation Language
;;;; (FDML).
;;;; 
;;;; ======================================================================

(in-package #:clsql-tests)

#.(clsql:locally-enable-sql-reader-syntax)

;; inserts a record using all values only and then deletes it 
(deftest :fdml/insert/1
    (progn
      (clsql:insert-records :into [employee] 
                           :values `(11 1 "Yuri" "Gagarin" "gagarin@soviet.org"
                                     1 1 1.85 t ,(clsql-base:get-time)))
      (values 
       (clsql:select [first-name] [last-name] [email]
                    :from [employee] :where [= [emplid] 11])
       (progn (clsql:delete-records :from [employee] :where [= [emplid] 11])
              (clsql:select [*] :from [employee] :where [= [emplid] 11]))))
  (("Yuri" "Gagarin" "gagarin@soviet.org")) nil)

;; inserts a record using attributes and values and then deletes it
(deftest :fdml/insert/2
    (progn
      (clsql:insert-records :into [employee] 
                           :attributes '(emplid groupid first_name last_name
                                         email companyid managerid)
                           :values '(11 1 "Yuri" "Gagarin" "gagarin@soviet.org"
                                     1 1))
      (values 
       (clsql:select [first-name] [last-name] [email] :from [employee]
                    :where [= [emplid] 11])
       (progn (clsql:delete-records :from [employee] :where [= [emplid] 11])
              (clsql:select [*] :from [employee] :where [= [emplid] 11]))))
  (("Yuri" "Gagarin" "gagarin@soviet.org")) nil)

;; inserts a record using av-pairs and then deletes it
(deftest :fdml/insert/3
    (progn
      (clsql:insert-records :into [employee] 
                           :av-pairs'((emplid 11) (groupid 1)
                                      (first_name "Yuri")
                                      (last_name "Gagarin")
                                      (email "gagarin@soviet.org")
                                      (companyid 1) (managerid 1)))
      (values 
       (clsql:select [first-name] [last-name] [email] :from [employee]
                    :where [= [emplid] 11])
       (progn (clsql:delete-records :from [employee] :where [= [emplid] 11])
              (clsql:select [first-name] [last-name] [email] :from [employee]
                           :where [= [emplid] 11]))))
  (("Yuri" "Gagarin" "gagarin@soviet.org")) nil)

;; inserts a records using a query from another table 
(deftest :fdml/insert/4
    (progn
      (clsql:create-table [employee2] '(([forename] string)
				 ([surname] string)
				 ([email] string)))
      (clsql:insert-records :into [employee2] 
		     :query [select [first-name] [last-name] [email] 
				    :from [employee]]
		     :attributes '(forename surname email))
      (prog1
          (equal (clsql:select [*] :from [employee2])
                 (clsql:select [first-name] [last-name] [email]
                              :from [employee]))
        (clsql:drop-table [employee2] :if-does-not-exist :ignore)))
  t)

;; updates a record using attributes and values and then deletes it
(deftest :fdml/update/1
    (progn
      (clsql:update-records [employee] 
                           :attributes '(first_name last_name email)
                           :values '("Yuri" "Gagarin" "gagarin@soviet.org")
                           :where [= [emplid] 1])
      (values 
       (clsql:select [first-name] [last-name] [email] :from [employee]
                    :where [= [emplid] 1])
       (progn
         (clsql:update-records [employee] 
                              :av-pairs'((first_name "Vladamir")
                                         (last_name "Lenin")
                                         (email "lenin@soviet.org"))
                              :where [= [emplid] 1])
         (clsql:select [first-name] [last-name] [email] :from [employee]
                      :where [= [emplid] 1]))))
  (("Yuri" "Gagarin" "gagarin@soviet.org"))
  (("Vladamir" "Lenin" "lenin@soviet.org")))

;; updates a record using av-pairs and then deletes it
(deftest :fdml/update/2
    (progn
      (clsql:update-records [employee] 
                           :av-pairs'((first_name "Yuri")
                                      (last_name "Gagarin")
                                      (email "gagarin@soviet.org"))
                           :where [= [emplid] 1])
      (values 
       (clsql:select [first-name] [last-name] [email] :from [employee]
                    :where [= [emplid] 1])
       (progn
         (clsql:update-records [employee]
                              :av-pairs'((first_name "Vladamir")
                                         (last_name "Lenin")
                                         (email "lenin@soviet.org"))
                              :where [= [emplid] 1])
         (clsql:select [first-name] [last-name] [email]
                      :from [employee] :where [= [emplid] 1]))))
  (("Yuri" "Gagarin" "gagarin@soviet.org"))
  (("Vladamir" "Lenin" "lenin@soviet.org")))


(deftest :fdml/query/1
    (clsql:query "SELECT COUNT(*) FROM EMPLOYEE WHERE (EMAIL LIKE '%org')")
  (("10")))

(deftest :fdml/query/2
    (clsql:query
     "SELECT FIRST_NAME,LAST_NAME FROM EMPLOYEE WHERE (EMPLID <= 5) ORDER BY LAST_NAME")
  (("Leonid" "Brezhnev") ("Nikita" "Kruschev") ("Vladamir" "Lenin")
 ("Josef" "Stalin") ("Leon" "Trotsky")))

  
(deftest :fdml/execute-command/1
    (values
     (clsql:table-exists-p [foo] :owner *test-database-user*)
     (progn
       (clsql:execute-command "create table foo (bar integer)")
       (clsql:table-exists-p [foo] :owner *test-database-user*))
     (progn
       (clsql:execute-command "drop table foo")
       (clsql:table-exists-p [foo] :owner *test-database-user*)))
  nil t nil)


;; compare min, max and average hieghts in inches (they're quite short
;; these guys!) -- only works with pgsql 
(deftest :fdml/select/1
    (if (member *test-database-type* '(:postgresql-socket :postgresql))
        (let ((max (clsql:select [function "floor"
                                          [/ [* [max [height]] 100] 2.54]]
                                :from [employee]
                                :flatp t))
              (min (clsql:select [function "floor"
                                          [/ [* [min [height]] 100] 2.54]]
                                :from [employee]
                                :flatp t))
              (avg (clsql:select [function "floor"
                                          [avg [/ [* [height] 100] 2.54]]]
                                :from [employee]
                                :flatp t)))
          (apply #'< (mapcar #'parse-integer (append min avg max))))
        t)
  t)

(deftest :fdml/select/2
    (clsql:select [first-name] :from [employee] :flatp t :distinct t
                 :order-by [first-name])
  ("Boris" "Josef" "Konstantin" "Leon" "Leonid" "Mikhail" "Nikita" "Vladamir"
           "Yuri"))

(deftest :fdml/select/3
    (clsql:select [first-name] [count [*]] :from [employee]
                 :group-by [first-name]
                 :order-by [first-name])
  (("Boris" "1") ("Josef" "1") ("Konstantin" "1") ("Leon" "1") ("Leonid" "1")
   ("Mikhail" "1") ("Nikita" "1") ("Vladamir" "2") ("Yuri" "1")))

(deftest :fdml/select/4
    (clsql:select [last-name] :from [employee] :where [like [email] "%org"]
                 :order-by [last-name]
                 :flatp t)
  ("Andropov" "Brezhnev" "Chernenko" "Gorbachev" "Kruschev" "Lenin" "Putin"
              "Stalin" "Trotsky" "Yeltsin"))

(deftest :fdml/select/5
    (clsql:select [email] :from [employee] :flatp t 
                 :where [in [employee emplid]
                            [select [managerid] :from [employee]]])
  ("lenin@soviet.org"))

(deftest :fdml/select/6
    (if (member *test-database-type* '(:postgresql-socket :postgresql))
        (mapcar #'parse-integer
                (clsql:select [function "trunc" [height]] :from [employee]
                             :flatp t))
        (mapcar #'(lambda (s) (truncate (parse-integer s :junk-allowed t)))
                (clsql:select [height] :from [employee] :flatp t)))
  (1 1 1 1 1 1 1 1 1 1))

(deftest :fdml/select/7
    (sql:select [max [emplid]] :from [employee] :flatp t)
  ("10"))

(deftest :fdml/select/8
    (sql:select [min [emplid]] :from [employee] :flatp t)
  ("1"))

(deftest :fdml/select/9
    (subseq (car (sql:select [avg [emplid]] :from [employee] :flatp t)) 0 3)
  "5.5")

(deftest :fdml/select/10
    (sql:select [last-name] :from [employee]
                :where [not [in [emplid]
                                [select [managerid] :from  [company]]]]
                :flatp t
                :order-by [last-name])
  ("Andropov" "Brezhnev" "Chernenko" "Gorbachev" "Kruschev" "Putin" "Stalin"
              "Trotsky" "Yeltsin"))

(deftest :fdml/select/11
    (clsql:select [last-name] :from [employee] :where [married] :flatp t
                 :order-by [emplid])
  ("Lenin" "Stalin" "Trotsky"))

(deftest :fdml/select/12
    (let ((v 1))
      (clsql:select [last-name] :from [employee] :where [= [emplid] v]))
  (("Lenin")))

;(deftest :fdml/select/11
;    (sql:select [emplid] :from [employee]
;                :where [= [emplid] [any [select [companyid] :from [company]]]]
;                :flatp t)
;  ("1"))

(deftest :fdml/do-query/1
    (let ((result '()))
    (clsql:do-query ((name) [select [last-name] :from [employee]
                                   :order-by [last-name]])
      (push name result))
    result)
 ("Yeltsin" "Trotsky" "Stalin" "Putin" "Lenin" "Kruschev" "Gorbachev"
            "Chernenko" "Brezhnev" "Andropov")) 

(deftest :fdml/map-query/1
    (clsql:map-query 'list #'identity
                    [select [last-name] :from [employee] :flatp t
                            :order-by [last-name]])
  ("Andropov" "Brezhnev" "Chernenko" "Gorbachev" "Kruschev" "Lenin" "Putin"
              "Stalin" "Trotsky" "Yeltsin"))

(deftest :fdml/map-query/2
    (clsql:map-query 'vector #'identity
                    [select [last-name] :from [employee] :flatp t
                            :order-by [last-name]])
  #("Andropov" "Brezhnev" "Chernenko" "Gorbachev" "Kruschev" "Lenin" "Putin"
    "Stalin" "Trotsky" "Yeltsin"))
  
(deftest :fdml/loop/1
    (loop for (forename surname)
      being each tuple in
      [select [first-name] [last-name] :from [employee] :order-by [last-name]]
      collect (concatenate 'string forename " " surname))
  ("Yuri Andropov" "Leonid Brezhnev" "Konstantin Chernenko" "Mikhail Gorbachev"
                   "Nikita Kruschev" "Vladamir Lenin" "Vladamir Putin"
                   "Josef Stalin" "Leon Trotsky" "Boris Yeltsin"))

;; starts a transaction deletes a record and then rolls back the deletion 
(deftest :fdml/transaction/1
    (let ((results '()))
      ;; test if we are in a transaction
      (push (clsql:in-transaction-p) results)
      ;;start a transaction 
      (clsql:start-transaction)
      ;; test if we are in a transaction
      (push (clsql:in-transaction-p) results)
      ;;Putin has got to go
      (unless (eql *test-database-type* :mysql)
        (clsql:delete-records :from [employee] :where [= [last-name] "Putin"]))
      ;;Should be nil 
      (push 
       (clsql:select [*] :from [employee] :where [= [last-name] "Putin"])
       results)
      ;;Oh no, he's still there
      (clsql:rollback)
      ;; test that we are out of the transaction
      (push (clsql:in-transaction-p) results)
      ;; Check that we got him back alright 
      (push (clsql:select [email] :from [employee] :where [= [last-name] "Putin"]
                         :flatp t)
            results)
      (apply #'values (nreverse results)))
  nil t nil nil ("putin@soviet.org"))

;; starts a transaction, updates a record and then rolls back the update
(deftest :fdml/transaction/2
    (let ((results '()))
      ;; test if we are in a transaction
      (push (clsql:in-transaction-p) results)
      ;;start a transaction 
      (clsql:start-transaction)
      ;; test if we are in a transaction
      (push (clsql:in-transaction-p) results)
      ;;Putin has got to go
      (unless (eql *test-database-type* :mysql)
        (clsql:update-records [employee]
                             :av-pairs '((email "putin-nospam@soviet.org"))
                             :where [= [last-name] "Putin"]))
      ;;Should be new value  
      (push (clsql:select [email] :from [employee]
                         :where [= [last-name] "Putin"]
                         :flatp t)
            results)
      ;;Oh no, he's still there
      (clsql:rollback)
      ;; test that we are out of the transaction
      (push (clsql:in-transaction-p) results)
      ;; Check that we got him back alright 
      (push (clsql:select [email] :from [employee] :where [= [last-name] "Putin"]
                         :flatp t)
            results)
      (apply #'values (nreverse results)))
  nil t ("putin-nospam@soviet.org") nil ("putin@soviet.org")) 

;; runs an update within a transaction and checks it is committed
(deftest :fdml/transaction/3
    (let ((results '()))
      ;; check status 
      (push (clsql:in-transaction-p) results)
      ;; update records 
      (push
       (clsql:with-transaction () 
         (clsql:update-records [employee] 
                              :av-pairs '((email "lenin-nospam@soviet.org"))
                              :where [= [emplid] 1]))
       results)
      ;; check status 
      (push (clsql:in-transaction-p) results)
      ;; check that was committed 
      (push (clsql:select [email] :from [employee] :where [= [emplid] 1]
                         :flatp t)
            results)
      ;; undo the changes 
      (push
       (clsql:with-transaction () 
         (clsql:update-records [employee] 
                              :av-pairs '((email "lenin@soviet.org"))
                              :where [= [emplid] 1]))
       results)
      ;; and check status 
      (push (clsql:in-transaction-p) results)
      ;; check that was committed 
      (push (clsql:select [email] :from [employee] :where [= [emplid] 1]
                         :flatp t)
            results)
      (apply #'values (nreverse results)))
  nil :COMMITTED nil ("lenin-nospam@soviet.org") :COMMITTED
  nil ("lenin@soviet.org"))

;; runs a valid update and an invalid one within a transaction and checks
;; that the valid update is rolled back when the invalid one fails. 
(deftest :fdml/transaction/4
    (let ((results '()))
      ;; check status
      (push (clsql:in-transaction-p) results)
      (unless (eql *test-database-type* :mysql)
        (handler-case 
            (clsql:with-transaction () 
              ;; valid update
              (clsql:update-records [employee] 
                                   :av-pairs '((email "lenin-nospam@soviet.org"))
                                 :where [= [emplid] 1])
            ;; invalid update which generates an error 
            (clsql:update-records [employee] 
                                 :av-pairs
                                 '((emale "lenin-nospam@soviet.org"))
                                 :where [= [emplid] 1]))
        (clsql:clsql-sql-error ()
          (progn
            ;; check status 
            (push (clsql:in-transaction-p) results)
            ;; and check nothing done 
            (push (clsql:select [email] :from [employee] :where [= [emplid] 1]
                               :flatp t)
                  results)
            (apply #'values (nreverse results)))))))
  nil nil ("lenin@soviet.org"))

#.(clsql:restore-sql-reader-syntax-state)
