;;;; Proposed new file in clsql-tests to enable abstracting datasets for reuse.
;;;;
;;;; The core is def-datset and with-dataset that respectively define a set,
;;;; and enable one for a dynamic scope. Datasets will normally be setup and
;;;; torn down in the scope of one test, which may impose a computation
;;;; overhead, but enables simpler tests by not worrying about side-effects
;;;; between tests.
;;;;
;;;; In general datsets should be database agnostic, but because the code
;;;; is only run in the scope of a test, if a test is excluded for a backend
;;;; or some other reason then it is never run hence doesn't cause problems.

(in-package #:clsql-tests)

(defmacro def-dataset (name &body body)
  "Define a dataset"
  ;;probably just shove this into a param, perhaps a marginal
  ;; bit of processing first.
  `(defparameter ,name ',body))

;;incomplete example taken from test-init
(def-dataset *employees*
  (:setup "CREATE TABLE EMPLOYEES (
emplid int not null,
groupid int not null,
married boolean not null,
first_name varchar (50),
last_name varchar (50))" ))
;;alternatively setup can still be done as
;;(:setup #'(lambda () (create-view-from-class ...)))
  (:data "employees"
   (:columns "emplid,groupid,married,height,first_name,last_name")
   (:values "1,1,false,1.5,'Napolean', 'Bonaparte'")
   (:values (format nil "1,1,true,~a,'Vladimir','Lenin'" (1+ (random 1.00)))))
  (:cleanup "DROP TABLE EMPLOYEES"))


(defmacro with-dataset (name &body body)
  "Use a dataset in a dynamic scope, e.g. a single test.
1. Before the body:
  * :setup is run
  * :data is loaded
2. Body
3. :cleanup always happens"
  `(unwind-protect
	(progn (%dataset-init ,name)
	       ,@body)
     (handler-case (%dataset-cleanup ,name)
       (error #'print-the-error)) ;;recursive error catch
     ))


(defun %dataset-dispatch (arg)
  "For use with def-dataset and with-dataset, tries to DWIM."
  (etypecase arg
    (string (clsql-sys:execute-command arg))  ;;treat it as a sql command.
    ((or function symbol) (funcall fn))  ;;run functions
    (list (mapc #'%dataset-dispatch arg)))) ;;lists are implicity progn

(defun %dataset-init (name)
  "Run initialization code and fill database for given dataset."
  (let ((setup (find :setup name :key #'first))
	(data (find :data name :key #'first)))
    (when setup
      (%dataset-dispatch setup))
    (when data
      ;;run raw sql insert statements,
      ;; other schemes can be done as a function in setup.
      ;; most likely implemented with something like
      (let (table-name columns values-list) ;gather components
	(dolist (v values-list)
	  (clsql-sys:execute-command
	   (format nil "INSERT INTO ~a (~a) VALUES (~a)"
		   table-name columns v)))))))

(defun %dataset-cleanup (name)
  "Run cleanup code associated with the given dataset."
  (%dataset-dispatch (find :cleanup name)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Example Test Code

;;example old test
(deftest :oodml/select/1
    (mapcar #'(lambda (e) (slot-value e 'last-name))
	    (clsql:select 'employee :order-by [last-name] :flatp t :caching nil))
  ("Andropov" "Brezhnev" "Chernenko" "Gorbachev" "Kruschev" "Lenin" "Putin"
	      "Stalin" "Trotsky" "Yeltsin"))

;;just wrap in the with-dataset
(deftest :oodml/select/1
    (with-dataset (*employees*)
      (mapcar #'(lambda (e) (slot-value e 'last-name))
	      (clsql:select 'employee :order-by [last-name]
			    :flatp t :caching nil)))
  ("Andropov" "Brezhnev" "Chernenko" "Gorbachev" "Kruschev" "Lenin" "Putin"
	      "Stalin" "Trotsky" "Yeltsin"))


;;old
(deftest :oodml/update-records/4
	  (flet ((print-fresh-node ()
		   (let ((base (car (clsql:select 'node
				      :where [= 1 [slot-value 'node 'node-id]]
				      :flatp t :caching nil))))
		     (format nil "~a ~a"
			     (slot-value base 'node-id)
			     (slot-value base 'title)))))
	    (values
	      (print-fresh-node) ;ensure that data is correct when we start
	      (let ((base (car (clsql:select 'node
				 :where [= 1 [slot-value 'node 'node-id]]
				 :flatp t :caching nil))))
		(setf (slot-value base 'title) "Altered title")
		;;modify and check
		(clsql:update-records-from-instance base)
		(print-fresh-node))
	      (let ((base (car (clsql:select 'node
				 :where [= 1 [slot-value 'node 'node-id]]
				 :flatp t :caching nil))))
		(setf (slot-value base 'title) "Bare node")
		;;modify back to the original and check
		(clsql:update-records-from-instance base)
		(print-fresh-node))))
	  "1 Bare node"
	  "1 Altered title"
	  "1 Bare node")

;;rewritten
(deftest :oodml/update-records/4
	  (flet ((get-node ()
		   (clsql:select 'node :where [= 1 [node-id]]
				 :flatp t :caching nil)))
	    (with-dataset (*nodes*)
	      (values
	      (let ((base (get-node)))
		(setf (slot-value base 'title) "Altered title")
		(clsql:update-records-from-instance base)
		(setf base (get-node))
		(format nil "~a ~a"
			     (slot-value base 'node-id)
			     (slot-value base 'title))))))
	  "1 Altered title")
