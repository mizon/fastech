(defpackage :test.fastech.string
  (:use :cl
        :cl-test-more
        :fastech))
(in-package :test.fastech.string)

(plan 3)

(diag "str")
(is (parse (str "foo") "foobar")
    (values "foo" "bar")
    "parses a string")

(diag "satisfy")
(flet ((pred (char)
         (eq char #\a)))
  (is (parse (satisfy #'pred) "abc")
      (values #\a "bc")
      "parses the satisfied char")
  (is-error (parse (satisfy #'pred) "bac")
            'parse-error
            "failes parsing the satisfied char"))

(finalize)
