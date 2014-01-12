(defpackage :fastech.experimental-test
  (:use :cl
        :cl-test-more)
  (:import-from :fastech
                :str
                :always)
  (:import-from :fastech.experimental
                :parser)
  (:import-from :fastech.test-helper
                :is-parsed))
(in-package :fastech.experimental-test)

(plan 3)

(diag "parser")
(is-parsed (parser
            (:bind foo (str "foo"))
            (:bind bar (str "bar"))
            (always (list foo bar))) "foobar"
           (list "foo" "bar") ""
           "expands the bind clause properly")
(is-parsed (parser
            (:let x 100)
            (:let y 200)
            (always (list x y))) "foobar"
           (list 100 200) "foobar"
           "expands the let clause properly")
(is-parsed (parser
            (:bind foo (str "foo"))
            (:let foo "bar")
            (if (string= foo "bar")
                (always "ok")
                (always "not ok"))) "foobar"
           "ok" "bar"
           "expands the various clauses properly")

(finalize)
