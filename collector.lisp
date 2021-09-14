(uiop:define-package #:com.andrewsoutar.matcher/collector
  (:use #:cl)
  (:export #:make-collector #:collect-all #:collect #:with-collectors))
(cl:in-package #:com.andrewsoutar.matcher/collector)

(defun make-collector (&rest things)
  (let ((collector (list* nil things)))
    (setf (car collector) (last collector))
    collector))

(defun collect-all (collector &rest collectors)
  (dolist (collector2 collectors)
    (when collector2
      (setf (cdr (car collector)) (cdr collector2)
            (car collector) (car collector2))))
  collector)

(defun collect (collector &rest things)
  (when (and things (not collector))
    (error "Cannot collect into collector ~A" collector))
  (cdr (collect-all collector (apply 'make-collector things))))

(defmacro with-collectors ((&rest bindings) &body body)
  `(let ,(mapcar (lambda (binding)
                   (multiple-value-bind (name things)
                       (if (consp binding) (values (first binding) (rest binding)) binding)
                     `(,name (make-collector ,@things))))
                 bindings)
     ,@body))
