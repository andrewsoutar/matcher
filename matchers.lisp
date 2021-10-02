(uiop:define-package #:com.andrewsoutar.matcher/matchers
  (:use)
  (:import-from #:com.andrewsoutar.matcher/impl)
  (:import-from #:com.andrewsoutar.matcher/main)
  (:export #:and #:or #:satisfies #:type #:call #:eq #:eql #:equal #:equalp #:string=))
(cl:in-package #:com.andrewsoutar.matcher/impl)

(defmatcher (com.andrewsoutar.matcher/matchers:and &rest patterns) (value value-safe-p)
  (flet ((frob (value) (dolist (pattern patterns) (%compile-pattern pattern value t))))
    (if value-safe-p
        (frob value)
        (let ((value-var (gensym "AND-VALUE")))
          (collect *temporaries* value-var)
          (collect *actions* `(setq ,value-var ,value))
          (frob value-var)))))

(defmatcher (com.andrewsoutar.matcher/matchers:or &rest patterns) (value value-safe-p)
  (flet ((frob (value)
           (let ((up-actions *actions*)
                 (end-label (gensym "OR-END")))
             (with-collectors (*actions*)
               (dolist (pattern patterns)
                 (let ((next-label (gensym "OR-NEXT"))
                       (satisfied-var (gensym "OR-SATISFIED-P"))
                       (up-bindings *bindings*))
                   (collect *temporaries* satisfied-var)
                   (with-collectors (*bindings*)
                     ;; FIXME This will break any extracted helpers (for
                     ;; init-forms, mostly).  There isn't really a good
                     ;; way to handle that, because on a failed
                     ;; init-form match, we don't want to have to
                     ;; backtrack back in to match another or-branch. If
                     ;; this becomes an issue, I could have it throw a
                     ;; runtime error when it would backtrack instead,
                     ;; but init-forms inside an or-pattern is hairy
                     ;; anyway, so probably best not to do that.
                     (compile-pattern pattern value `(go ,next-label) t)
                     (collect *actions*
                       `(setq ,satisfied-var t)
                       `(go ,end-label)
                       next-label)
                     (dolist (binding (collect *bindings*))
                       (destructuring-bind (var value) binding
                         (collect up-bindings `(,var (when ,satisfied-var ,value))))))))
               (collect up-actions `(tagbody ,@(collect *actions*) ,*fail* ,end-label))))))
    (if value-safe-p
        (frob value)
        (let ((value-var (gensym "OR-VALUE")))
          (collect *temporaries* value-var)
          (collect *actions* `(setq ,value-var ,value))
          (frob value-var)))))

(macrolet ((def ((name &rest params) test-form)
             `(defmatcher (,(find-symbol (symbol-name name) '#:com.andrewsoutar.matcher/matchers) ,@params)
                  (value)
                (collect *actions* `(unless ,,test-form ,*fail*)))))
  (def (satisfies predicate) `(funcall ,predicate ,value))
  (def (type type-specifier) `(typep ,value ',type-specifier))
  (def (call call-form) `(,@call-form ,value))
  (macrolet ((defeq (name) `(def (,name thing) `(,',name ,thing ,value))))
    (defeq eq)
    (defeq eql)
    (defeq equal)
    (defeq equalp)
    (defeq string=)))
