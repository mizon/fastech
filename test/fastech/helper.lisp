(defpackage :test.fastech.helper
  (:use :cl)
  (:export :is-parsed))
(in-package :test.fastech.helper)

(defun is-parsed (parser input expected-value expected-remainder message)
  (multiple-value-bind (value remainder)
      (fastech:parse parser input)
    (cl-test-more:is (list value remainder)
                     (list expected-value expected-remainder)
                     message)))
