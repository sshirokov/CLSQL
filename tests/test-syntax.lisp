;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; ======================================================================
;;;; File:    test-syntax.lisp
;;;; Author:  Marcus Pearce <m.t.pearce@city.ac.uk>
;;;; Created: 30/03/2004
;;;; Updated: $Id$
;;;; ======================================================================
;;;;
;;;; Description ==========================================================
;;;; ======================================================================
;;;;
;;;; Tests for the CLSQL Symbolic SQL syntax. 
;;;;
;;;; ======================================================================

(in-package #:clsql-tests)

#.(clsql:locally-enable-sql-reader-syntax)

(setq *rt-syntax*
      '(
	
(deftest :syntax/generic/1
    (clsql:sql "foo")
  "'foo'")

(deftest :syntax/generic/2
    (clsql:sql 23)
  "23")

(deftest :syntax/generic/3
    (clsql:sql 'bar)
  "BAR")

(deftest :syntax/generic/4
    (clsql:sql '("ten" 10 ten))
  "('ten',10,TEN)")

(deftest :syntax/generic/5
    (clsql:sql ["SELECT FOO,BAR FROM BAZ"])
  "SELECT FOO,BAR FROM BAZ")


(deftest :syntax/ident/1
    (clsql:sql [foo])
  "FOO")

(deftest :syntax/ident/2
    (clsql:sql [foo bar])
  "FOO.BAR")

(deftest :syntax/ident/3
    (clsql:sql ["foo" bar])
  "FOO.BAR")

;(deftest :syntax/ident/4
;    (clsql:sql [foo "bar"])
;  "FOO \"bar\"")

(deftest :syntax/ident/5
    (clsql:sql [foo :integer])
  "FOO")

(deftest :syntax/ident/6
    (clsql:sql [foo bar :integer])
  "FOO.BAR")

(deftest :syntax/ident/7
    (clsql:sql ["foo" bar :integer])
  "FOO.BAR")

(deftest :syntax/value/1
    (clsql:sql [any '(3 4)])
  "(ANY ((3,4)))")

(deftest :syntax/value/2
    (clsql:sql [* 2 3])
  "(2 * 3)")


(deftest :syntax/relational/1
    (clsql:sql [> [baz] [beep]])
  "(BAZ > BEEP)")

(deftest :syntax/relational/2
    (let ((x 10))
      (clsql:sql [> [foo] x]))
  "(FOO > 10)")


(deftest :syntax/function/1
    (clsql:sql [function "COS" [age]])
  "COS(AGE)")

(deftest :syntax/function/2
    (clsql:sql [function "TO_DATE" "02/06/99" "mm/DD/RR"])
  "TO_DATE('02/06/99','mm/DD/RR')")

(deftest :syntax/query/1
    (clsql:sql [select [person_id] [surname] :from [person]])
  "SELECT PERSON_ID,SURNAME FROM PERSON")

(deftest :syntax/query/2 
    (clsql:sql [select [foo] [bar *]
                      :from '([baz] [bar])
                      :where [or [= [foo] 3]
                                 [> [baz.quux] 10]]])
  "SELECT FOO,BAR.* FROM BAZ,BAR WHERE ((FOO = 3) OR (BAZ.QUUX > 10))")

(deftest :syntax/query/3
    (clsql:sql [select [foo bar] [baz]
                      :from '([foo] [quux])
                      :where [or [> [baz] 3]
                                 [like [foo bar] "SU%"]]])
  "SELECT FOO.BAR,BAZ FROM FOO,QUUX WHERE ((BAZ > 3) OR (FOO.BAR LIKE 'SU%'))")

(deftest :syntax/query/4
    (clsql:sql [select [count [*]] :from [emp]])
  "SELECT COUNT(*) FROM EMP")
  

(deftest :syntax/expression1
    (clsql:sql
     (clsql:sql-operation
      'select
      (clsql:sql-expression :table 'foo :attribute 'bar)
      (clsql:sql-expression :attribute 'baz)
      :from (list 
             (clsql:sql-expression :table 'foo)
             (clsql:sql-expression :table 'quux))
      :where
      (clsql:sql-operation 'or 
                          (clsql:sql-operation
                           '>
                           (clsql:sql-expression :attribute 'baz)
                           3)
                          (clsql:sql-operation
                           'like
                           (clsql:sql-expression :table 'foo
                                                :attribute 'bar)
                           "SU%"))))
  "SELECT FOO.BAR,BAZ FROM FOO,QUUX WHERE ((BAZ > 3) OR (FOO.BAR LIKE 'SU%'))")
  
(deftest :syntax/expression/2
    (clsql:sql
     (apply (clsql:sql-operator 'and)
            (loop for table in '(thistime nexttime sometime never)
                  for count from 42
                  collect
                  [function "BETWEEN"
                            (clsql:sql-expression :table table
                                                 :attribute 'bar)
                            (clsql:sql-operation '* [hip] [hop])
                            count]
                  collect
                  [like (clsql:sql-expression :table table
                                             :attribute 'baz)
                        (clsql:sql table)])))
  "(BETWEEN(THISTIME.BAR,(HIP * HOP),42) AND (THISTIME.BAZ LIKE 'THISTIME') AND BETWEEN(NEXTTIME.BAR,(HIP * HOP),43) AND (NEXTTIME.BAZ LIKE 'NEXTTIME') AND BETWEEN(SOMETIME.BAR,(HIP * HOP),44) AND (SOMETIME.BAZ LIKE 'SOMETIME') AND BETWEEN(NEVER.BAR,(HIP * HOP),45) AND (NEVER.BAZ LIKE 'NEVER'))")

))

#.(clsql:restore-sql-reader-syntax-state)
