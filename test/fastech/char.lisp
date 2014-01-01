(defpackage :test.fastech.char
  (:use :cl
        :cl-test-more)
  (:import-from :fastech
                :parse
                :parse-error
                :str
                :satisfy))
(in-package :test.fastech.char)

(plan 5)

(diag "str")
(is (parse (str "foo") "foobar")
    (values "foo" "bar")
    "parses a string")
(is-error (parse (str "foo") "barfoo")
          'parse-error
          "fails parsing invalid input")
(is-error (parse (str "foo") "")
          'parse-error
          "fails parsing invalid input")

(diag "satisfy")
(flet ((pred (char)
         (eq char #\a)))
  (is (parse (satisfy #'pred) "abc")
      (values #\a "bc")
      "parses the satisfied char")
  (is-error (parse (satisfy #'pred) "bac")
            'parse-error
            "fails parsing the unsatisfied char"))

(finalize)
