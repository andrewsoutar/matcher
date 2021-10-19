(uiop:define-package #:com.andrewsoutar.matcher
  (:nicknames #:com.andrewsoutar.matcher/main)
  (:use #:cl #:com.andrewsoutar.matcher/collector #:com.andrewsoutar.matcher/impl)
  (:export #:match-let* #:match-case #:match-ecase))
(cl:in-package #:com.andrewsoutar.matcher/main)

(defmacro match-let* ((&rest bindings) &body body)
  (with-matcher-env
    (dolist (binding bindings)
      (multiple-value-bind (actions bindings)
          (with-collectors (*actions* *bindings*)
            (destructuring-bind (pattern value) binding
              (compile-pattern pattern value `(error "Match failure")))
            (values *actions* *bindings*))
        (compile-run-with-bindings (collect actions))
        (collect-all *bindings* bindings)))
    (build-matcher-form body)))

(defmacro match-case (form &body cases)
  (let ((form-var (gensym "FORM"))
        (match-case-block (gensym "MATCH-CASE")))
    `(let ((,form-var ,form))
       (block ,match-case-block
         (tagbody
            ,@(mapcan (lambda (one-case)
                        (destructuring-bind (pattern &body body) one-case
                          (let ((next-tag (gensym "NEXT")))
                            (with-matcher-env
                              (compile-pattern pattern form-var `(go ,next-tag) t)
                              `((return-from ,match-case-block ,(build-matcher-form body))
                                ,next-tag)))))
                      cases))))))

(defmacro match-ecase (form &body cases)
  (let ((form-var (gensym "FORM")))
    `(let ((,form-var ,form))
       (match-case ,form-var ,@cases (t (error "Value ~A did not match any case" ,form-var))))))
