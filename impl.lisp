(uiop:define-package #:com.andrewsoutar.matcher/impl
  (:use #:cl #:com.andrewsoutar.matcher/collector)
  (:export #:*actions* #:*temporaries* #:*bindings* #:*ignored-bindings* #:*helpers*)
  (:export #:with-matcher-env #:compile-pattern #:compile-run-with-bindings #:build-matcher-form))
(cl:in-package #:com.andrewsoutar.matcher/impl)

(defvar *actions*)
(defvar *temporaries*)
(defvar *bindings*)
(defvar *ignored-bindings*)
(defvar *helpers*)
(defvar *fail*)

(defmacro with-matcher-env (&body body)
  `(with-collectors (*actions* *temporaries* *bindings* *ignored-bindings* *helpers*) ,@body))

(defun ensure-cons (x) (if (consp x) x (list x)))

(defun gensym* (&rest things)
  (gensym (apply 'concatenate 'string (mapcar 'string things))))

(defun compile-run-with-bindings (forms)
  (let ((ignored (gensym "IGNORED")))
    (collect *bindings* `(,ignored (progn ,@forms (values))))
    (collect *ignored-bindings* ignored)
    ignored))

(defun compile-init-form-pattern (pattern init-form-stuff value &optional initialized-p-var)
  (destructuring-bind (&optional (init-form nil init-form-p) (supplied-p nil supplied-p-p)) init-form-stuff
    (if init-form-p
        (let ((submatch (gensym "SUBMATCH"))
              (submatch-value (gensym "SUBMATCH-VALUE")))
          (unless initialized-p-var
            (setf initialized-p-var (gensym "INITIALIZED-P"))
            (collect *temporaries* initialized-p-var))
          (compile-run-with-bindings `(unless ,initialized-p-var (,submatch ,init-form)))
          (with-collectors (*actions*)
            (%compile-pattern pattern submatch-value t)
            (collect *helpers* `(,submatch (,submatch-value) ,@(collect *actions*))))
          (when supplied-p-p (collect *bindings* `(,supplied-p ,initialized-p-var)))
          `((setq ,initialized-p-var t) (,submatch ,value)))
        (with-collectors (*actions*)
          (when initialized-p-var (collect *actions* `(setq ,initialized-p-var t)))
          (%compile-pattern pattern value)
          (collect *actions*)))))

(defun make-keyword-matcher (key-args-collector list-remaining no-match-form)
  ;; Compile into a raw tagbody because `do' and friends create implicit blocks
  (let ((loop-head (gensym "LOOP-HEAD")) (loop-end (gensym "LOOP-END")))
    `(tagbody ,loop-head
        (unless (consp ,list-remaining) (go ,loop-end))
        (unless (case (car ,list-remaining)
                  ,@(mapcar (lambda (key-arg)
                              (destructuring-bind (keyword subpattern init-form-stuff) key-arg
                                (let ((initialized-p (gensym* "INITIALIZED-P-" keyword)))
                                  (collect *temporaries* initialized-p)
                                  `(,keyword
                                    (unless ,initialized-p
                                      (pop ,list-remaining) ; pop keyword itself
                                      ,@(compile-init-form-pattern subpattern init-form-stuff `(pop ,list-remaining)
                                                                   initialized-p)
                                      t)))))
                     (collect key-args-collector)))
          ,no-match-form))))

(defun compile-cons (pattern list-remaining)
  (labels ((dispatch (default)
             (cond ((consp pattern)
                    (let ((next-fun (case (car pattern)
                                      (&optional #'optional-arg)
                                      (&key #'key-arg)
                                      (&rest #'rest-arg))))
                      (if next-fun
                          (progn (pop pattern) (dispatch next-fun))
                          default)))
                   ((not (null pattern)) #'end-rest)
                   ((eql default #'rest-arg) #'end-ignore)
                   (t #'end-null)))
           (required-arg ()
             (collect *actions* `(unless (consp ,list-remaining) ,*fail*))
             (%compile-pattern (pop pattern) `(pop ,list-remaining))
             (funcall (dispatch #'required-arg)))
           (optional-arg ()
             (collect *actions* `(unless (listp ,list-remaining) ,*fail*))
             (destructuring-bind (subpattern &rest init-form-stuff) (ensure-cons (pop pattern))
               (collect *actions*
                 `(unless (endp ,list-remaining)
                    ,@(compile-init-form-pattern subpattern init-form-stuff `(pop ,list-remaining)))))
             (funcall (dispatch #'optional-arg)))
           (key-arg ()
             (do ((key-args (make-collector))
                  (next-fun #'key-arg (dispatch next-fun)))
                 ((not (eql next-fun #'key-arg))
                  (collect *actions*
                    (cond ((eql next-fun #'end-null) (make-keyword-matcher key-args list-remaining *fail*))
                          ((eql next-fun #'end-ignore) (make-keyword-matcher key-args list-remaining
                                                                             `(pop ,list-remaining)))
                          (t (let ((collector (gensym "COLLECTOR")))
                               (collect *temporaries* collector)
                               `(with-collectors (,collector)
                                  ,@(make-keyword-matcher key-args list-remaining
                                                          `(collect ,collector (pop ,list-remaining)))
                                  (setq ,list-remaining (collect ,collector))))
                             (funcall next-fun)))))
               (destructuring-bind (kv-pattern &rest init-form-stuff) (ensure-cons (pop pattern))
                 (multiple-value-bind (keyword subpattern)
                     (etypecase kv-pattern
                       (symbol (values (intern (symbol-name kv-pattern) :keyword) kv-pattern))
                       ((cons symbol (cons * null)) (values-list kv-pattern)))
                   (collect key-args `(,keyword ,subpattern ,init-form-stuff))))))
           (rest-arg ()
             (%compile-pattern (pop pattern) list-remaining t)
             (when pattern (funcall (dispatch #'required-arg))))
           (end-rest () (%compile-pattern pattern list-remaining t)) 
          (end-ignore ())
           (end-null () (end-rest)))
    (funcall (dispatch #'required-arg))))

(defun %compile-pattern (pattern value &optional value-safe-p)
  (etypecase pattern
    ((eql t)
     (unless value-safe-p
       (collect *actions* value)))
    ((or null number keyword (cons (eql quote)))
     (collect *actions* `(unless (eql ,pattern ,value) ,*fail*)))
    (string
     (collect *actions* `(unless (string= ,pattern ,value) ,*fail*)))
    (symbol
     (let ((temp (gensym* "TEMP-" pattern)))
       (collect *temporaries* temp)
       (collect *actions* `(setq ,temp ,value))
       (collect *bindings* `(,pattern ,temp))))
    (cons
     (let ((matcher (when (symbolp (first pattern)) (get (first pattern) 'matcher))))
       (if matcher
           (funcall matcher (rest pattern) value value-safe-p)
           (let ((list-remaining (gensym "LIST-REMAINING")))
             (collect *temporaries* list-remaining)
             (collect *actions* `(setq ,list-remaining ,value))
             (compile-cons pattern list-remaining)))))))

(defun compile-pattern (pattern value *fail* &optional value-safe-p)
  ;; If `value-safe-p', `value' can be evaluated any number of times
  ;; (including zero); otherwise it must be evaluated only once
  (%compile-pattern pattern value value-safe-p))

(defun build-matcher-form (body)
  `(let ,(collect *temporaries*)
     (labels ,(collect *helpers*)
       (let* ,(collect *bindings*)
         (declare (ignore ,@(collect *ignored-bindings*)))
         ,@body))))

(defmacro defmatcher ((name &rest pattern-rest) (value-var &optional (value-safe-p-var nil vsp-p)) &body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (get ',name 'matcher)
           ,(let ((pattern-var (gensym "PATTERN")))
              `(lambda (,pattern-var ,value-var
                        &optional ,(if vsp-p value-safe-p-var (setf value-safe-p-var (gensym "VALUE-SAFE-P"))))
                 ,@(unless vsp-p `((declare (ignore ,value-safe-p-var))))
                 (destructuring-bind ,pattern-rest ,pattern-var ,@body))))))
