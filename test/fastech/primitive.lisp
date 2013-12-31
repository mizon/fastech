(defpackage :test.fastech.primitive
  (:use :cl
        :cl-test-more)
  (:import-from :fastech
                :parse-error))
(in-package :test.fastech.primitive)

(plan 1)

(diag "fail")
(is-error (fastech:parse (fastech:fail "message") "foobar")
          'parse-error
          "fails always")

(finalize)
