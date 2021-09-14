(uiop:define-package #:com.andrewsoutar.matcher/impl
  (:use #:cl #:com.andrewsoutar.matcher/collector)
  (:export #:*actions* #:*temporaries* #:*bindings* #:*ignored-bindings* #:*helpers)
  (:export #:with-matcher-env))
(cl:in-package #:com.andrewsoutar.matcher/impl)

(defvar *actions*)
(defvar *temporaries*)
(defvar *bindings*)
(defvar *ignored-bindings*)
(defvar *helpers*)

(defmacro with-matcher-env (&body body)
  `(with-collectors (*actions* *temporaries* *bindings* *ignored-bindings* *helpers*) ,@body))
