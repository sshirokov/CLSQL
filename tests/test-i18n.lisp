;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:    test-i18n.lisp
;;;; Purpose: Tests for passing non-ascii encoded strings to db and back
;;;; Author:  Nathan Bird
;;;; Created: Feb 2010
;;;;
;;;; This file, part of CLSQL, is Copyright (c) 2002-2010 by Kevin M. Rosenberg
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(in-package #:clsql-tests)

(setq *rt-i18n*
      '(

;;; The point of these two is to require proper encoding support
;;; UTF-8 for example can handle these easily.
;; I show this as a 20char string and 27 bytes in utf-8
(deftest :basic/i18n/1
    (first (query "SELECT 'Iñtërnâtiônàlizætiøn'"
		  :flatp t :field-names nil))
  "Iñtërnâtiônàlizætiøn")

;; the z in this one is even stronger
;; I show this as a 20char string and 28 bytes in utf-8
(deftest :basic/i18n/2
    (first (query "SELECT 'Iñtërnâtiônàližætiøn'"
		  :flatp t :field-names nil))
  "Iñtërnâtiônàližætiøn")

))
