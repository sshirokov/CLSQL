;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;;
;;;; $Id$
;;;;
;;;; CLSQL broadcast streams which can be used to monitor the
;;;; flow of commands to, and results from, a database.
;;;;
;;;; This file is part of CLSQL.
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(in-package #:clsql-base-sys)

(defun start-sql-recording (&key (type :commands) (database *default-database*))
  "Begin recording SQL command or result traffic. By default the
broadcast stream is just *STANDARD-OUTPUT* but this can be modified
using ADD-SQL-STREAM or DELETE-SQL-STREAM. TYPE determines whether SQL
command or result traffic is recorded, or both. It must be either
:commands, :results, :query, :both, or :all, and defaults to
:commands. DATABASE defaults to *default-database*."
  (when (in type :all :both :commands)
    (setf (command-recording-stream database)
          (make-broadcast-stream *standard-output*)))
  (when (in type :all :both :results)
    (setf (result-recording-stream database)
          (make-broadcast-stream *standard-output*)))
  (when (in type :all :query)
    (setf (query-recording-stream database)
	  (make-broadcast-stream
	   *standard-output*)))
  (values))

(defun stop-sql-recording (&key (type :commands) (database *default-database*))
  "Stops recording of SQL command or result traffic.  TYPE determines
whether to stop SQL command or result traffic, or both.  It must be
either :commands, :results, :both, or :all, defaulting to :commands. DATABASE
defaults to *default-database*."
  (when (in type :all :both :commands)
    (setf (command-recording-stream database) nil))
  (when (in type :all :both :results)
    (setf (result-recording-stream database) nil))
  (when (in type :all :query)
    (setf (query-recording-stream database) nil))
  (values))

(defun sql-recording-p (&key (type :commands) (database *default-database*))
  "Returns t if recording of TYPE of SQL interaction specified is
enabled.  TYPE must be either :commands, :results, :query, :all,
:both, :either, or :any.  DATABASE defaults to *default-database*."
  (when (or (and (eq type :commands)
                 (command-recording-stream database))
            (and (eq type :results)
                 (result-recording-stream database))
            (and (eq type :all)
                 (result-recording-stream database)
                 (query-recording-stream database)
                 (command-recording-stream database))
            (and (eq type :both)
                 (result-recording-stream database)
                 (command-recording-stream database))
            (and (eq type :either)
                 (or (result-recording-stream database)
                     (command-recording-stream database)))
            (and (eq type :any)
                 (or (result-recording-stream database)
                     (command-recording-stream database)
		     (query-recording-stream database))))
    t))

(defun add-sql-stream (stream &key (type :commands)
                              (database *default-database*))
  "Add the given STREAM as a component stream for the recording
broadcast stream for the given SQL interaction TYPE.  TYPE must be
either :commands, :results, :query, :all, or :both, defaulting to
:commands.  DATABASE defaults to *default-database*."
  (when (in type :all :both :commands)
    (unless (member stream
                    (list-sql-streams :type :commands :database database))
      (setf (command-recording-stream database)
            (apply #'make-broadcast-stream
                   (cons stream (list-sql-streams :type :commands
                                                  :database database))))))
  (when (in type :all :both :results)
    (unless (member stream (list-sql-streams :type :results :database database))
      (setf (result-recording-stream database)
            (apply #'make-broadcast-stream
                   (cons stream (list-sql-streams :type :results
                                                  :database database))))))
  (when (in type :all :query)
    (unless (member stream (list-sql-streams :type :query :database database))
      (setf (query-recording-stream database)
            (apply #'make-broadcast-stream
                   (cons stream (list-sql-streams :type :query
                                                  :database database))))))
  stream)
			      
(defun delete-sql-stream (stream &key (type :commands)
                                 (database *default-database*))
  "Removes the given STREAM from the recording broadcast stream for
the given TYPE of SQL interaction.  TYPE must be either :commands,
:results, :query, :both, or :all, defaulting to :commands.  DATABASE
defaults to *default-database*."
  (when (in type :all :both :commands)
    (setf (command-recording-stream database)
          (apply #'make-broadcast-stream
                 (remove stream (list-sql-streams :type :commands
                                                  :database database)))))
  (when (in type :all :both :results)
    (setf (result-recording-stream database)
          (apply #'make-broadcast-stream
                 (remove stream (list-sql-streams :type :results
                                                  :database database)))))
  (when (in type :all :query)
    (setf (query-recording-stream database)
          (apply #'make-broadcast-stream
                 (remove stream (list-sql-streams :type :commands
                                                  :database database)))))
  stream)

(defun list-sql-streams (&key (type :commands) (database *default-database*))
  "Returns the set of streams which the recording broadcast stream
send SQL interactions of the given TYPE sends data. TYPE must be
either :commands, :results, :query, :both, or :all, defaulting to :commands.
DATABASE defaults to *default-database*."
  (let ((crs (command-recording-stream database))
        (qrs (query-recording-stream database))
        (rrs (result-recording-stream database)))
    (cond
      ((eq type :commands)
       (when crs (broadcast-stream-streams crs)))
      ((eq type :results)
       (when rrs (broadcast-stream-streams rrs)))
      ((eq type :query)
       (when qrs (broadcast-stream-streams qrs)))
      ((eq type :both)
       (append (when crs (broadcast-stream-streams crs))
               (when rrs (broadcast-stream-streams rrs))))
      ((eq type :all)
       (append (when crs (broadcast-stream-streams crs))
               (when rrs (broadcast-stream-streams rrs))
               (when qrs (broadcast-stream-streams qrs))))
      (t
       (error "Unknown recording type. ~A" type)))))

(defun sql-stream (&key (type :commands) (database *default-database*))
  "Returns the broadcast streams used for recording SQL commands or
results traffic. TYPE must be either :commands, :query, or :results defaulting
to :commands while DATABASE defaults to *default-database*."
  (cond
    ((eq type :commands)
     (command-recording-stream database))
    ((eq type :results)
     (result-recording-stream database))
    ((eq type :query)
     (query-recording-stream database))
    (t
     (error "Unknown recording type. ~A" type))))
  
(defun record-sql-action (expr type database)
  (unless database
    (return-from record-sql-action))
  (with-slots (command-recording-stream
               query-recording-stream
               result-recording-stream)
      database
    (let ((stream
	   (ecase type
	     (:command command-recording-stream)
	     (:query query-recording-stream)
	     (:result result-recording-stream))))
      (when stream
        (format stream ";; ~A ~A => ~A~%"
                (iso-timestring (get-time))
 			      (database-name database)
 			      expr)))))
