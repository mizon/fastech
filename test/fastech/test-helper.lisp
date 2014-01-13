(defpackage :fastech.test-helper
  (:use :cl)
  (:import-from :cl-test-more
                :is
                :fail)
  (:import-from :fastech
                :parse-failed
                :parse-failed-remainder
                :parse-failed-message)
  (:export :is-parsed
           :is-parse-failed))
(in-package :fastech.test-helper)

(defun is-parsed (parser input expected-value expected-remainder message)
  (multiple-value-bind (value remainder)
      (fastech:parse-only parser input)
    (is (list value remainder)
        (list expected-value expected-remainder)
        message)))

(defun is-parse-failed (parser input expected-remainder expected-message message)
  (handler-case (fastech:parse-only parser input)
    (parse-failed (e)
      (is (list (parse-failed-remainder e) (parse-failed-message e))
          (list expected-remainder expected-message)
          message))
    (:no-error (value remainder)
      (declare (ignore value remainder))
      (fail "must fail parsing"))))
