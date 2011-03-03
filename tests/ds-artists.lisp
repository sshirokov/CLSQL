(in-package #:clsql-tests)

#.(clsql:locally-enable-sql-reader-syntax)

(defparameter artist1 nil)

(def-view-class artist ()
  ((artist_id :accessor artist_id :initarg :id
	      :type integer :db-kind :key :db-constraints (:not-null :auto-increment)
	      :autoincrement-sequence 'artist_artist_id_seq)
   (name :accessor name :initarg :name :type (varchar 20))
   (genre :accessor genre :initarg :genre :type (varchar 10) :db-constraints (:default "'Unknown'"))))

(defun initialize-ds-artists ()
   ;   (start-sql-recording :type :both)
   ;   (let ((*backend-warning-behavior*
   ;          (if (member *test-database-type* '(:postgresql :postgresql-socket))
   ;              :ignore
   ; 	     :warn)))
  (mapc #'clsql:create-view-from-class
	  '(artist))

  (setq *test-start-utime* (get-universal-time))
  (let* ((*db-auto-sync* nil))
    (setf  artist1 (make-instance 'artist
				  :name "Mogwai"))))

(def-dataset *ds-artists*
  (:setup initialize-ds-artists)
  (:cleanup (lambda ()
 	      (mapc #'clsql-sys:drop-view-from-class
 		    '(artist))
 	      (ignore-errors
 		(mapc #'clsql-sys:drop-sequence
 		      (list "artist_artist_id_seq"))))))

#.(clsql:restore-sql-reader-syntax-state)
