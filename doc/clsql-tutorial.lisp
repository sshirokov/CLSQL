
(in-package :cl-user)

;; You must set these variables to appropriate values. 
(defvar *tutorial-database-type* nil 
  "Possible values are :postgresql,:postgresql-socket :mysql or :sqlite")
(defvar *tutorial-database-name* ""
  "The name of the database we will work in.")
(defvar *tutorial-database-user* "" 
  "The name of the database user we will work as.")
(defvar *tutorial-database-server* ""
  "The name of the database server if required")
(defvar *tutorial-database-password* "" 
  "The password if required")

(sql:def-view-class employee ()
  ((emplid
    :db-kind :key
    :db-constraints :not-null
    :nulls-ok nil
    :type integer
    :initarg :emplid)
   (first-name
    :accessor first-name
    :type (string 30)
    :initarg :first-name)
   (last-name
    :accessor last-name
    :type (string 30)
    :initarg :last-name)
   (email
    :accessor employee-email
    :type (string 100)
    :nulls-ok t
    :initarg :email)
   (companyid
    :type integer)
   (company
    :accessor employee-company
    :db-kind :join
    :db-info (:join-class company
			  :home-key companyid
			  :foreign-key companyid
			  :set nil))
   (managerid
    :type integer
    :nulls-ok t)
   (manager
    :accessor employee-manager
    :db-kind :join
    :db-info (:join-class employee
			  :home-key managerid
			  :foreign-key emplid
			  :set nil)))
  (:base-table employee))

(sql:def-view-class company ()
  ((companyid
    :db-type :key
    :db-constraints :not-null
    :type integer
    :initarg :companyid)
   (name
    :type (string 100)
    :initarg :name)
   (presidentid
    :type integer)
   (president
    :reader president
    :db-kind :join
    :db-info (:join-class employee
			  :home-key presidentid
			  :foreign-key emplid
			  :set nil))
   (employees
    :reader company-employees
    :db-kind :join
    :db-info (:join-class employee
			  :home-key companyid
			  :foreign-key companyid
			  :set t)))
  (:base-table company))

;; Connect to the database (see the CLSQL documentation for vendor
;; specific connection specs).
(sql:connect `(,*tutorial-database-server* 
	       ,*tutorial-database-name*
	       ,*tutorial-database-user* 
	       ,*tutorial-database-password*)
	     :database-type *tutorial-database-type*)

;; Record the sql going out, helps us learn what is going
;; on behind the scenes
(sql:start-sql-recording)

;; Create the tables for our view classes
;; First we drop them, ignoring any errors
(ignore-errors
 (sql:drop-view-from-class 'employee)
 (sql:drop-view-from-class 'company))

(sql:create-view-from-class 'employee)
(sql:create-view-from-class 'company)


;; Create some instances of our view classes
(defvar employee1 (make-instance 'employee
			       :emplid 1
			       :first-name "Vladamir"
			       :last-name "Lenin"
			       :email "lenin@soviet.org"))

(defvar company1 (make-instance 'company
			      :companyid 1
			      :name "Widgets Inc."))
			      

(defvar employee2 (make-instance 'employee
			       :emplid 2
			       :first-name "Josef"
			       :last-name "Stalin"
			       :email "stalin@soviet.org"))

;; Lenin manages Stalin (for now)
(sql:add-to-relation employee2 'manager employee1)

;; Lenin and Stalin both work for Widgets Inc.
(sql:add-to-relation company1 'employees employee1)
(sql:add-to-relation company1 'employees employee2)

;; Lenin is president of Widgets Inc.
(sql:add-to-relation company1 'president employee1)

(sql:update-records-from-instance employee1)
(sql:update-records-from-instance employee2)
(sql:update-records-from-instance company1)

;; lets us use the functional
;; sql interface 
(sql:locally-enable-sql-reader-syntax)


(format t "The email address of ~A ~A is ~A"
	(first-name employee1)
	(last-name employee1)
	(employee-email employee1))

(setf (employee-email employee1) "lenin-nospam@soviets.org")

;; Update the database
(sql:update-records-from-instance employee1)

(let ((new-lenin (car
		  (sql:select 'employee
			      :where [= [slot-value 'employee 'emplid] 1]))))
  (format t "His new email is ~A"
	  (employee-email new-lenin)))


;; Some queries

;; all employees
(sql:select 'employee)
;; all companies
(sql:select 'company)

;; employees named Lenin
(sql:select 'employee :where [= [slot-value 'employee 'last-name]
				"Lenin"])

(sql:select 'company :where [= [slot-value 'company 'name]
			       "Widgets Inc."])

;; Employees of Widget's Inc.
(sql:select 'employee
	    :where [and [= [slot-value 'employee 'companyid]
			   [slot-value 'company 'companyid]]
			[= [slot-value 'company 'name]
			   "Widgets Inc."]])

;; Same thing, except that we are using the employee
;; relation in the company view class to do the join for us,
;; saving us the work of writing out the SQL!
(company-employees company1)

;; President of Widgets Inc.
(president company1)

;; Manager of Josef Stalin
(employee-manager employee2)
