(defpackage :test.fastech.helper
  (:use :cl)
  (:import-from :cl-test-more
                :is
                :fail)
  (:import-from :fastech
                :parse-error
                :parse-error-remainder
                :parse-error-message)
  (:export :is-parsed
           :is-parse-error))
(in-package :test.fastech.helper)

(defun is-parsed (parser input expected-value expected-remainder message)
  (multiple-value-bind (value remainder)
      (fastech:parse parser input)
    (is (list value remainder)
        (list expected-value expected-remainder)
        message)))

(defun is-parse-error (parser input expected-remainder expected-message message)
  (handler-case (fastech:parse parser input)
    (parse-error (e)
      (is (list (parse-error-remainder e) (parse-error-message e))
          (list expected-remainder expected-message)
          message))
    (:no-error (value remainder)
      (declare (ignore value remainder))
      (fail "must fail parsing"))))
