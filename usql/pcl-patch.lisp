
;; Note that this will no longer required for cmucl as of version 19a. 

(in-package #+cmu :pcl #+sbcl :sb-pcl)

(defmacro pv-binding1 ((pv calls pv-table-symbol pv-parameters slot-vars) 
		       &body body)
  `(pv-env (,pv ,calls ,pv-table-symbol ,pv-parameters)
     (let (,@(mapcar #'(lambda (slot-var p) `(,slot-var (get-slots-or-nil ,p)))
	       slot-vars pv-parameters))
       ,@(mapcar #'(lambda (slot-var) `(declare (ignorable ,slot-var))) slot-vars)
       ,@body)))
