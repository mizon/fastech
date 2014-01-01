(defpackage :test.fastech.primitive
  (:use :cl
        :cl-test-more)
  (:import-from :fastech
                :parse-error)
  (:import-from :test.fastech.helper
                :is-parsed))
(in-package :test.fastech.primitive)

(plan 1)

(diag "unexpected")
(is-error (fastech:parse (fastech:unexpected "message") "foobar")
          'parse-error
          "fails always")

(finalize)
