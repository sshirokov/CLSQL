;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;;
;;;; $Id: objects.lisp 8963 2004-04-11 14:05:44Z kevin $
;;;;
;;;; Relations: This is not in CommonSQL API and may be removed
;;;;
;;;; This file is part of CLSQL.
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(in-package #:clsql-sys)


(defun synchronize-keys (src srckey dest destkey)
  (let ((skeys (if (listp srckey) srckey (list srckey)))
	(dkeys (if (listp destkey) destkey (list destkey))))
    (mapcar #'(lambda (sk dk)
		(setf (slot-value dest dk)
		      (typecase sk
			(symbol
			 (slot-value src sk))
			(t sk))))
	    skeys dkeys)))

(defun desynchronize-keys (dest destkey)
  (let ((dkeys (if (listp destkey) destkey (list destkey))))
    (mapcar #'(lambda (dk)
		(setf (slot-value dest dk) nil))
	    dkeys)))

(defmethod add-to-relation ((target standard-db-object)
			    slot-name
			    (value standard-db-object))
  (let* ((objclass (class-of target))
	 (sdef (or (slotdef-for-slot-with-class slot-name objclass)
                   (error "~s is not an known slot on ~s" slot-name target)))
	 (dbinfo (view-class-slot-db-info sdef))
	 (join-class (gethash :join-class dbinfo))
	 (homekey (gethash :home-key dbinfo))
	 (foreignkey (gethash :foreign-key dbinfo))
	 (to-many (gethash :set dbinfo)))
    (unless (equal (type-of value) join-class)
      (error 'clsql-type-error :slotname slot-name :typespec join-class
             :value value))
    (when (gethash :target-slot dbinfo)
      (error "add-to-relation does not work with many-to-many relations yet."))
    (if to-many
	(progn
	  (synchronize-keys target homekey value foreignkey)
	  (if (slot-boundp target slot-name)
              (unless (member value (slot-value target slot-name))
                (setf (slot-value target slot-name)
                      (append (slot-value target slot-name) (list value))))
              (setf (slot-value target slot-name) (list value))))
        (progn
          (synchronize-keys value foreignkey target homekey)
          (setf (slot-value target slot-name) value)))))

(defmethod remove-from-relation ((target standard-db-object)
			    slot-name (value standard-db-object))
  (let* ((objclass (class-of target))
	 (sdef (slotdef-for-slot-with-class slot-name objclass))
	 (dbinfo (view-class-slot-db-info sdef))
	 (homekey (gethash :home-key dbinfo))
	 (foreignkey (gethash :foreign-key dbinfo))
	 (to-many (gethash :set dbinfo)))
    (when (gethash :target-slot dbinfo)
      (error "remove-relation does not work with many-to-many relations yet."))
    (if to-many
	(progn
	  (desynchronize-keys value foreignkey)
	  (if (slot-boundp target slot-name)
	      (setf (slot-value target slot-name)
		    (remove value
			    (slot-value target slot-name)
                            :test #'equal))))
        (progn
          (desynchronize-keys target homekey)
          (setf (slot-value target slot-name)
                nil)))))

