;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; ======================================================================
;;;; File:    test-syntax.lisp
;;;; Author:  Marcus Pearce <m.t.pearce@city.ac.uk>
;;;; Created: 30/03/2004
;;;; Updated: <04/04/2004 11:51:40 marcusp>
;;;; ======================================================================
;;;;
;;;; Description ==========================================================
;;;; ======================================================================
;;;;
;;;; Tests for the CLSQL-USQL Symbolic SQL syntax. 
;;;;
;;;; ======================================================================

(in-package :clsql-usql-tests)

#.(usql:locally-enable-sql-reader-syntax)


(deftest :syntax/generic/1
    (usql:sql "foo")
  "'foo'")

(deftest :syntax/generic/2
    (usql:sql 23)
  "23")

(deftest :syntax/generic/3
    (usql:sql 'bar)
  "BAR")

(deftest :syntax/generic/4
    (usql:sql '("ten" 10 ten))
  "('ten',10,TEN)")

(deftest :syntax/generic/5
    (usql:sql ["SELECT FOO,BAR FROM BAZ"])
  "SELECT FOO,BAR FROM BAZ")


(deftest :syntax/ident/1
    (usql:sql [foo])
  "FOO")

(deftest :syntax/ident/2
    (usql:sql [foo bar])
  "FOO.BAR")

;; not sure about this one 
(deftest :syntax/ident/3
    (usql:sql ["foo" bar])
  "foo.BAR")

;(deftest :syntax/ident/4
;    (usql:sql [foo "bar"])
;  "FOO \"bar\"")

(deftest :syntax/ident/5
    (usql:sql [foo :integer])
  "FOO INTEGER")

(deftest :syntax/ident/6
    (usql:sql [foo bar :integer])
  "FOO.BAR INTEGER")

;; not sure about this one 
(deftest :syntax/ident/7
    (usql:sql ["foo" bar :integer])
  "foo.BAR INTEGER")


(deftest :syntax/value/1
    (usql:sql [any '(3 4)])
  "(ANY ((3,4)))")

(deftest :syntax/value/2
    (usql:sql [* 2 3])
  "(2 * 3)")


(deftest :syntax/relational/1
    (usql:sql [> [baz] [beep]])
  "(BAZ > BEEP)")

(deftest :syntax/relational/2
    (let ((x 10))
      (usql:sql [> [foo] x]))
  "(FOO > 10)")


(deftest :syntax/function/1
    (usql:sql [function "COS" [age]])
  "COS(AGE)")

(deftest :syntax/function/2
    (usql:sql [function "TO_DATE" "02/06/99" "mm/DD/RR"])
  "TO_DATE('02/06/99','mm/DD/RR')")

(deftest :syntax/query/1
    (usql:sql [select [person_id] [surname] :from [person]])
  "SELECT PERSON_ID,SURNAME FROM PERSON")

(deftest :syntax/query/2 
    (usql:sql [select [foo] [bar *]
                      :from '([baz] [bar])
                      :where [or [= [foo] 3]
                                 [> [baz.quux] 10]]])
  "SELECT FOO,BAR.* FROM BAZ,BAR WHERE ((FOO = 3) OR (BAZ.QUUX > 10))")

(deftest :syntax/query/3
    (usql:sql [select [foo bar] [baz]
                      :from '([foo] [quux])
                      :where [or [> [baz] 3]
                                 [like [foo bar] "SU%"]]])
  "SELECT FOO.BAR,BAZ FROM FOO,QUUX WHERE ((BAZ > 3) OR (FOO.BAR LIKE 'SU%'))")

(deftest :syntax/query/4
    (usql:sql [select [count [*]] :from [emp]])
  "SELECT COUNT(*) FROM EMP")
  

(deftest :syntax/expression1
    (usql:sql
     (usql:sql-operation
      'select
      (usql:sql-expression :table 'foo :attribute 'bar)
      (usql:sql-expression :attribute 'baz)
      :from (list 
             (usql:sql-expression :table 'foo)
             (usql:sql-expression :table 'quux))
      :where
      (usql:sql-operation 'or 
                          (usql:sql-operation
                           '>
                           (usql:sql-expression :attribute 'baz)
                           3)
                          (usql:sql-operation
                           'like
                           (usql:sql-expression :table 'foo
                                                :attribute 'bar)
                           "SU%"))))
  "SELECT FOO.BAR,BAZ FROM FOO,QUUX WHERE ((BAZ > 3) OR (FOO.BAR LIKE 'SU%'))")
  
(deftest :syntax/expression/2
    (usql:sql
     (apply (usql:sql-operator 'and)
            (loop for table in '(thistime nexttime sometime never)
                  for count from 42
                  collect
                  [function "BETWEEN"
                            (usql:sql-expression :table table
                                                 :attribute 'bar)
                            (usql:sql-operation '* [hip] [hop])
                            count]
                  collect
                  [like (usql:sql-expression :table table
                                             :attribute 'baz)
                        (usql:sql table)])))
  "(BETWEEN(THISTIME.BAR,(HIP * HOP),42) AND (THISTIME.BAZ LIKE 'THISTIME') AND BETWEEN(NEXTTIME.BAR,(HIP * HOP),43) AND (NEXTTIME.BAZ LIKE 'NEXTTIME') AND BETWEEN(SOMETIME.BAR,(HIP * HOP),44) AND (SOMETIME.BAZ LIKE 'SOMETIME') AND BETWEEN(NEVER.BAR,(HIP * HOP),45) AND (NEVER.BAZ LIKE 'NEVER'))")
  
#.(usql:restore-sql-reader-syntax-state)