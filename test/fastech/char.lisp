(defpackage :test.fastech.char
  (:use :cl
        :cl-test-more)
  (:import-from :fastech
                :parse
                :parse-error
                :chr
                :any-char
                :str
                :satisfy)
  (:import-from :test.fastech.helper
                :is-parsed))
(in-package :test.fastech.char)

(plan 9)

(diag "chr")
(is-parsed (chr #\a) "abc"
           #\a "bc"
           "parses a char")
(is-error (parse (chr #\a) "foo")
          'parse-error
          "fails parsing invalid input")

(diag "any-char")
(is-parsed (any-char) "foo"
           #\f "oo"
           "parses an any char")
(is-error (parse (any-char) "")
          'parse-error
          "fails parsing the empty input")

(diag "str")
(is-parsed (str "foo") "foobar"
           "foo" "bar"
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
  (is-parsed (satisfy #'pred) "abc"
             #\a "bc"
             "parses the satisfied char")
  (is-error (parse (satisfy #'pred) "bac")
            'parse-error
            "fails parsing the unsatisfied char"))

(finalize)
