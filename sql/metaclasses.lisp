;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;;
;;;; $Id$
;;;;
;;;; CLSQL metaclass for standard-db-objects created in the OODDL. 
;;;;
;;;; This file is part of CLSQL.
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(in-package #:clsql-sys)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (>= (length (generic-function-lambda-list
		     (ensure-generic-function
		      'compute-effective-slot-definition)))
	    3)
    (pushnew :kmr-normal-cesd cl:*features*))
  
  (when (>= (length (generic-function-lambda-list
		     (ensure-generic-function
		      'direct-slot-definition-class)))
	    3)
    (pushnew :kmr-normal-dsdc cl:*features*))
  
  (when (>= (length (generic-function-lambda-list
		     (ensure-generic-function
		      'effective-slot-definition-class)))
	    3)
    (pushnew :kmr-normal-esdc cl:*features*)))


;; ------------------------------------------------------------
;; metaclass: view-class

(defclass standard-db-class (standard-class)
  ((view-table
    :accessor view-table
    :initarg :view-table)
   (definition
    :accessor object-definition
    :initarg :definition
    :initform nil)
   (key-slots
    :accessor key-slots
    :initform nil)
   (class-qualifier
    :accessor view-class-qualifier
    :initarg :qualifier
    :initform nil))
  (:documentation "VIEW-CLASS metaclass."))

#+lispworks
(defmacro push-on-end (value location)
  `(setf ,location (nconc ,location (list ,value))))

;; As Heiko Kirscke (author of PLOB!) would say:  !@##^@%! Lispworks!
#+lispworks
(defconstant +extra-slot-options+ '(:column :db-kind :db-reader :nulls-ok
				    :db-writer :db-type :db-info))

#+lispworks 
(define-setf-expander assoc (key alist &environment env)
  (multiple-value-bind (temps vals stores store-form access-form)
      (get-setf-expansion alist env)
    (let ((new-value (gensym "NEW-VALUE-"))
          (keyed (gensym "KEYED-"))
          (accessed (gensym "ACCESSED-"))
          (store-new-value (car stores)))
      (values (cons keyed temps)
              (cons key vals)
              `(,new-value)
              `(let* ((,accessed ,access-form)
                      (,store-new-value (assoc ,keyed ,accessed)))
		(if ,store-new-value
		    (rplacd ,store-new-value ,new-value)
		    (progn
		      (setq ,store-new-value
                            (acons ,keyed ,new-value ,accessed))
		      ,store-form))
		,new-value)
              `(assoc ,new-value ,access-form)))))

#+lispworks 
(defmethod clos::canonicalize-defclass-slot :around
  ((prototype standard-db-class) slot)
 "\\lw\\ signals an error on unknown slot options; so this method
removes any extra allowed options before calling the default method
and returns the canonicalized extra options concatenated to the result
of the default method.  The extra allowed options are the value of the
\\fcite{+extra-slot-options+}."
  (let ((extra-slot-options ())
        (rest-options ())
        (result ()))
    (do ((olist (cdr slot) (cddr olist)))
        ((null olist))
      (let ((option (car olist)))
        (cond
         ((find option +extra-slot-options+)
          ;;(push (cons option (cadr olist)) extra-slot-options))
          (setf (assoc option extra-slot-options) (cadr olist)))
         (t
          (push (cadr olist) rest-options)
          (push (car olist) rest-options)))))
    (setf result (call-next-method prototype (cons (car slot) rest-options)))
    (dolist (option extra-slot-options)
      (push-on-end (car option) result)
      (push-on-end `(quote ,(cdr option)) result))
    result))

#+lispworks
(defconstant +extra-class-options+ '(:base-table))

#+lispworks 
(defmethod clos::canonicalize-class-options :around
    ((prototype standard-db-class) class-options)
  "\\lw\\ signals an error on unknown class options; so this method
removes any extra allowed options before calling the default method
and returns the canonicalized extra options concatenated to the result
of the default method.  The extra allowed options are the value of the
\\fcite{+extra-class-options+}."
  (let ((extra-class-options nil)
	(rest-options ())
	(result ()))
    (dolist (o class-options)
      (let ((option (car o)))
        (cond
         ((find option +extra-class-options+)
          ;;(push (cons option (cadr o)) extra-class-options))
          (setf (assoc option extra-class-options) (cadr o)))
         (t
	  (push o rest-options)))))
    (setf result (call-next-method prototype rest-options))
    (dolist (option extra-class-options)
      (push-on-end (car option) result)
      (push-on-end `(quote ,(cdr option)) result))
    result))


(defmethod validate-superclass ((class standard-db-class)
				(superclass standard-class))
  t)

(defun table-name-from-arg (arg)
  (cond ((symbolp arg)
	 arg)
	((typep arg 'sql-ident)
	 (slot-value arg 'name))
	((stringp arg)
	 (intern (symbol-name-default-case arg)))))

(defun column-name-from-arg (arg)
  (cond ((symbolp arg)
	 arg)
	((typep arg 'sql-ident)
	 (slot-value arg 'name))
	((stringp arg)
	 (intern (symbol-name-default-case arg)))))


(defun remove-keyword-arg (arglist akey)
  (let ((mylist arglist)
	(newlist ()))
    (labels ((pop-arg (alist)
	     (let ((arg (pop alist))
		   (val (pop alist)))
	       (unless (equal arg akey)
		 (setf newlist (append (list arg val) newlist)))
	       (when alist (pop-arg alist)))))
      (pop-arg mylist))
    newlist))

(defmethod initialize-instance :around ((class standard-db-class)
                                        &rest all-keys
					&key direct-superclasses base-table
                                        qualifier
					&allow-other-keys)
  (let ((root-class (find-class 'standard-db-object nil))
	(vmc (find-class 'standard-db-class)))
    (setf (view-class-qualifier class)
          (car qualifier))
    (if root-class
	(if (member-if #'(lambda (super)
			   (eq (class-of super) vmc)) direct-superclasses)
	    (call-next-method)
            (apply #'call-next-method
                   class
		   :direct-superclasses (append (list root-class)
                                                direct-superclasses)
		   (remove-keyword-arg all-keys :direct-superclasses)))
	(call-next-method))
    (setf (view-table class)
          (table-name-from-arg (sql-escape (or (and base-table
                                                    (if (listp base-table)
                                                        (car base-table)
                                                        base-table))
                                               (class-name class)))))
    (register-metaclass class (nth (1+ (position :direct-slots all-keys))
                                   all-keys))))

(defmethod reinitialize-instance :around ((class standard-db-class)
                                          &rest all-keys
                                          &key base-table 
                                          direct-superclasses qualifier
                                          &allow-other-keys)
  (let ((root-class (find-class 'standard-db-object nil))
	(vmc (find-class 'standard-db-class)))
    (setf (view-table class)
          (table-name-from-arg (sql-escape (or (and base-table
                                                    (if (listp base-table)
                                                        (car base-table)
                                                        base-table))
                                               (class-name class)))))
    (setf (view-class-qualifier class)
          (car qualifier))
    (if (and root-class (not (equal class root-class)))
	(if (member-if #'(lambda (super)
			   (eq (class-of super) vmc)) direct-superclasses)
	    (call-next-method)
            (apply #'call-next-method
                   class
                   :direct-superclasses (append (list root-class)
                                                direct-superclasses)
		   (remove-keyword-arg all-keys :direct-superclasses)))
        (call-next-method)))
  (register-metaclass class (nth (1+ (position :direct-slots all-keys))
                                 all-keys)))


(defun get-keywords (keys list)
  (flet ((extract (key)
           (let ((pos (position key list)))
             (when pos
               (nth (1+ pos) list)))))
    (mapcar #'extract keys)))

(defun describe-db-layout (class)
  (flet ((not-db-col (col)
           (not (member (nth 2 col)  '(nil :base :key))))
         (frob-slot (slot)
           (let ((type (slot-value slot 'type)))
             (if (eq type t)
                 (setq type nil))
             (list (slot-value slot 'name)
                   type
                   (slot-value slot 'db-kind)
                   (and (slot-boundp slot 'column)
                        (slot-value slot 'column))))))
    (let ((all-slots (mapcar #'frob-slot (ordered-class-slots class))))
      (setq all-slots (remove-if #'not-db-col all-slots))
      (setq all-slots (stable-sort all-slots #'string< :key #'car))
      ;;(mapcar #'dink-type all-slots)
      all-slots)))

(defun register-metaclass (class slots)
  (labels ((not-db-col (col)
             (not (member (nth 2 col)  '(nil :base :key))))
           (frob-slot (slot)
             (get-keywords '(:name :type :db-kind :column) slot)))
    (let ((all-slots (mapcar #'frob-slot slots)))
      (setq all-slots (remove-if #'not-db-col all-slots))
      (setq all-slots (stable-sort all-slots #'string< :key #'car))
      (setf (object-definition class) all-slots))
    #-(or allegro openmcl)
    (setf (key-slots class) (remove-if-not (lambda (slot)
					     (eql (slot-value slot 'db-kind)
						  :key))
					   (ordered-class-slots class)))))

#+(or allegro openmcl)
(defmethod finalize-inheritance :after ((class standard-db-class))
  ;; KMRL for slots without a type set, openmcl sets type-predicate to ccl:false
  ;; for standard-db-class
  #+openmcl
  (mapcar 
   #'(lambda (s)
       (if (eq 'ccl:false (slot-value s 'ccl::type-predicate))
	   (setf (slot-value s 'ccl::type-predicate) 'ccl:true)))
   (class-slots class))

  (setf (key-slots class) (remove-if-not (lambda (slot)
					   (eql (slot-value slot 'db-kind)
						:key))
					 (ordered-class-slots class))))

;; return the deepest view-class ancestor for a given view class

(defun base-db-class (classname)
  (let* ((class (find-class classname))
         (db-class (find-class 'standard-db-object)))
    (loop
     (let ((cds (class-direct-superclasses class)))
       (cond ((null cds)
              (error "not a db class"))
             ((member db-class cds)
              (return (class-name class))))
       (setq class (car cds))))))

(defun db-ancestors (classname)
  (let ((class (find-class classname))
        (db-class (find-class 'standard-db-object)))
    (labels ((ancestors (class)
             (let ((scs (class-direct-superclasses class)))
               (if (member db-class scs)
                   (list class)
                   (append (list class) (mapcar #'ancestors scs))))))
      (ancestors class))))

(defclass view-class-slot-definition-mixin ()
  ((column
    :accessor view-class-slot-column
    :initarg :column
    :documentation
    "The name of the SQL column this slot is stored in.  Defaults to
the slot name.")
   (db-kind
    :accessor view-class-slot-db-kind
    :initarg :db-kind
    :initform :base
    :type keyword
    :documentation
    "The kind of DB mapping which is performed for this slot.  :base
indicates the slot maps to an ordinary column of the DB view.  :key
indicates that this slot corresponds to part of the unique keys for
this view, :join indicates ... and :virtual indicates that this slot
is an ordinary CLOS slot.  Defaults to :base.")
   (db-reader
    :accessor view-class-slot-db-reader
    :initarg :db-reader
    :initform nil
    :documentation
    "If a string, then when reading values from the DB, the string
will be used for a format string, with the only value being the value
from the database.  The resulting string will be used as the slot
value.  If a function then it will take one argument, the value from
the database, and return the value that should be put into the slot.")
   (db-writer
    :accessor view-class-slot-db-writer
    :initarg :db-writer
    :initform nil
    :documentation
    "If a string, then when reading values from the slot for the DB,
the string will be used for a format string, with the only value being
the value of the slot.  The resulting string will be used as the
column value in the DB.  If a function then it will take one argument,
the value of the slot, and return the value that should be put into
the database.")
   (db-type
    :accessor view-class-slot-db-type
    :initarg :db-type
    :initform nil
    :documentation
    "A string which will be used as the type specifier for this slots
column definition in the database.")
   (db-constraints
    :accessor view-class-slot-db-constraints
    :initarg :db-constraints
    :initform nil
    :documentation
    "A single constraint or list of constraints for this column")
   (nulls-ok
    :accessor view-class-slot-nulls-ok
    :initarg :nulls-ok
    :initform nil
    :documentation
    "If t, all sql NULL values retrieved from the database become nil; if nil,
all NULL values retrieved are converted by DATABASE-NULL-VALUE")
   (db-info
    :accessor view-class-slot-db-info
    :initarg :db-info
    :documentation "Description of the join.")
   (specified-type
    :accessor specified-type
    :initform nil
    :documentation "KMR: Internal slot storing the :type specified by user.")))

(defparameter *db-info-lambda-list*
  '(&key join-class
	 home-key
	 foreign-key
         (key-join nil)
         (target-slot nil)
	 (retrieval :immmediate)
	 (set nil)))
	  
(defun parse-db-info (db-info-list)
  (destructuring-bind
	(&key join-class home-key key-join foreign-key (delete-rule nil)
	      (target-slot nil) (retrieval :deferred) (set nil))
      db-info-list
    (let ((ih (make-hash-table :size 6)))
      (if join-class
	  (setf (gethash :join-class ih) join-class)
	  (error "Must specify :join-class in :db-info"))
      (if home-key
	  (setf (gethash :home-key ih) home-key)
	  (error "Must specify :home-key in :db-info"))
      (when delete-rule
	(setf (gethash :delete-rule ih) delete-rule))
      (if foreign-key
	  (setf (gethash :foreign-key ih) foreign-key)
	  (error "Must specify :foreign-key in :db-info"))
      (when key-join
        (setf (gethash :key-join ih) t))
      (when target-slot
	(setf (gethash :target-slot ih) target-slot))
      (when set
	(setf (gethash :set ih) set))
      (when retrieval
	(progn
	  (setf (gethash :retrieval ih) retrieval)
	  (if (eql retrieval :immediate)
	      (setf (gethash :set ih) nil))))
      ih)))

(defclass view-class-direct-slot-definition (view-class-slot-definition-mixin
					     standard-direct-slot-definition)
  ())

(defclass view-class-effective-slot-definition (view-class-slot-definition-mixin
						standard-effective-slot-definition)
  ())

(defmethod direct-slot-definition-class ((class standard-db-class)
                                         #+kmr-normal-dsdc &rest
                                         initargs)
  (declare (ignore initargs))
  (find-class 'view-class-direct-slot-definition))

(defmethod effective-slot-definition-class ((class standard-db-class)
					    #+kmr-normal-esdc &rest
					    initargs)
  (declare (ignore initargs))
  (find-class 'view-class-effective-slot-definition))

#+openmcl
(defun compute-class-precedence-list (class)
  ;; safe to call this in openmcl
  (class-precedence-list class))

#-(or sbcl cmu)
(defmethod compute-slots ((class standard-db-class))
  "Need to sort order of class slots so they are the same across
implementations."
  (let ((slots (call-next-method))
	desired-sequence
	output-slots)
    (dolist (c (compute-class-precedence-list class))
      (dolist (s (class-direct-slots c))
	(let ((name (slot-definition-name s)))
	  (unless (find name desired-sequence)
	    (push name desired-sequence)))))
    (dolist (desired desired-sequence)
      (let ((slot (find desired slots :key #'slot-definition-name)))
	(assert slot)
	(push slot output-slots)))
    output-slots))

(defun compute-lisp-type-from-slot-specification (slotd specified-type)
  "Computes the Lisp type for a user-specified type. Needed for OpenMCL
which does type checking before storing a value in a slot."
  #-openmcl (declare (ignore slotd))
  ;; This function is called after the base compute-effective-slots is called.
  ;; OpenMCL sets the type-predicate based on the initial value of the slots type.
  ;; so we have to override the type-predicates here
  (cond
    ((consp specified-type)
     (cond
       ((and (symbolp (car specified-type))
	     (string-equal (symbol-name (car specified-type)) "string"))
	#+openmcl (setf (slot-value slotd 'ccl::type-predicate) 'stringp)
	'string)
       (t
	#+openmcl (setf (slot-value slotd 'ccl::type-predicate) 'ccl:true)
	specified-type)))
    #+openmcl
    ((null specified-type)
     ;; setting this here is not enough since openmcl later sets the
     ;; type-predicate to ccl:false. So, have to check slots again
     ;; in finalize-inheritance 
     #+openmcl (setf (slot-value slotd 'ccl::type-predicate) 'ccl:true)
     t)
    (t
     ;; This can be improved for OpenMCL to set a more specific type
     ;; predicate based on the value specified-type 
     #+openmcl (setf (slot-value slotd 'ccl::type-predicate) 'ccl:true)
     specified-type)))

;; Compute the slot definition for slots in a view-class.  Figures out
;; what kind of database value (if any) is stored there, generates and
;; verifies the column name.

(defmethod compute-effective-slot-definition ((class standard-db-class)
					      #+kmr-normal-cesd slot-name
					      direct-slots)
  #+kmr-normal-cesd (declare (ignore slot-name))

  (let ((slotd (call-next-method))
	(sd (car direct-slots)))
    
    (typecase sd
      (view-class-slot-definition-mixin
       ;; Use the specified :column argument if it is supplied, otherwise
       ;; the column slot is filled in with the slot-name,  but transformed
       ;; to be sql safe, - to _ and such.
       (setf (slot-value slotd 'column)
             (column-name-from-arg
              (if (slot-boundp sd 'column)
                  (view-class-slot-column sd)
                  (column-name-from-arg
                   (sql-escape (slot-definition-name sd))))))
       
       (setf (slot-value slotd 'db-type)
             (when (slot-boundp sd 'db-type)
               (view-class-slot-db-type sd)))
       
       (setf (slot-value slotd 'nulls-ok)
             (view-class-slot-nulls-ok sd))
       
       ;; :db-kind slot value defaults to :base (store slot value in
       ;; database)
       
       (setf (slot-value slotd 'db-kind)
             (if (slot-boundp sd 'db-kind)
                 (view-class-slot-db-kind sd)
                 :base))
       
       (setf (slot-value slotd 'db-writer)
             (when (slot-boundp sd 'db-writer)
               (view-class-slot-db-writer sd)))
       (setf (slot-value slotd 'db-constraints)
             (when (slot-boundp sd 'db-constraints)
               (view-class-slot-db-constraints sd)))
               
       ;; I wonder if this slot option and the previous could be merged,
       ;; so that :base and :key remain keyword options, but :db-kind
       ;; :join becomes :db-kind (:join <db info .... >)?
       
       (setf (slot-value slotd 'db-info)
             (when (slot-boundp sd 'db-info)
               (if (listp (view-class-slot-db-info sd))
                   (parse-db-info (view-class-slot-db-info sd))
                   (view-class-slot-db-info sd))))

       ;; KMR: store the user-specified type and then compute
       ;; real Lisp type and store it
       (setf (specified-type slotd)
	     (slot-definition-type slotd))
       (setf (slot-value slotd 'type)
	     (compute-lisp-type-from-slot-specification 
	      slotd (slot-definition-type slotd)))
       )
      ;; all other slots
      (t
       (change-class slotd 'view-class-effective-slot-definition
		     #+allegro :name 
		     #+allegro (slot-definition-name sd))
       (setf (slot-value slotd 'column)
             (column-name-from-arg
              (sql-escape (slot-definition-name sd))))

       (setf (slot-value slotd 'db-info) nil)
       (setf (slot-value slotd 'db-kind)
             :virtual)))
    slotd))

(defun slotdefs-for-slots-with-class (slots class)
  (let ((result nil))
    (dolist (s slots)
      (let ((c (slotdef-for-slot-with-class s class)))
	(if c (setf result (cons c result)))))
    result))

(defun slotdef-for-slot-with-class (slot class)
  (find-if #'(lambda (d) (eql slot (slot-definition-name d)))
	   (class-slots class)))

#+ignore
(eval-when (:compile-toplevel :load-toplevel :execute)
  #+kmr-normal-cesd
  (setq cl:*features* (delete :kmr-normal-cesd cl:*features*))
  #+kmr-normal-dsdc
  (setq cl:*features* (delete :kmr-normal-dsdc cl:*features*))
  #+kmr-normal-esdc
  (setq cl:*features* (delete :kmr-normal-esdc cl:*features*))
  )
