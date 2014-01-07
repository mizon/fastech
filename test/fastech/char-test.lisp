(defpackage :fastech.char-test
  (:use :cl
        :cl-test-more)
  (:import-from :fastech
                :parse
                :parse-failed
                :chr
                :any-char
                :str
                :satisfy
                :take-while
                :take-while1)
  (:import-from :fastech.test-helper
                :is-parsed
                :is-parse-failed))
(in-package :fastech.char-test)

(plan 15)

(diag "chr")
(is-parsed (chr #\a) "abc"
           #\a "bc"
           "parses a char")
(is-parse-failed (chr #\a) "foo"
                 "foo" "chr"
                 "fails parsing invalid input")

(diag "any-char")
(is-parsed (any-char) "foo"
           #\f "oo"
           "parses an any char")
(is-parse-failed (any-char) ""
                 "" "ensure: end of input"
                 "fails parsing the empty input")

(diag "str")
(is-parsed (str "foo") "foobar"
           "foo" "bar"
           "parses a string")
(is-parse-failed (str "foo") "barfoo"
                 "barfoo" "str"
                 "fails parsing invalid input")
(is-parse-failed (str "foo") ""
                 "" "ensure: end of input"
                 "fails parsing invalid input")

(diag "satisfy")
(flet ((pred (char)
         (eq char #\a)))
  (is-parsed (satisfy #'pred) "abc"
             #\a "bc"
             "parses the satisfied char")
  (is-parse-failed (satisfy #'pred) "bac"
                   "bac" "satisfy"
                   "fails parsing the unsatisfied char"))

(diag "take-while")
(is-parsed (take-while (lambda (c) (eq c #\a))) "aaabc"
           "aaa" "bc"
           "parses characters while `pred' returns non-nil")
(is-parsed (take-while (lambda (c) (eq c #\b))) "aaabc"
           "" "aaabc"
           "doesn't advance input position but succeeds")
(is-parsed (take-while (lambda (c) (eq c #\b))) ""
           "" ""
           "succeeds with empty inputs")
(is-parsed (take-while (constantly t)) "foobar"
           "foobar" ""
           "parses the whole input")

(diag "take-while1")
(is-parsed (take-while1 (lambda (c) (eq c #\a))) "aaabcd"
           "aaa" "bcd"
           "parses characters while `pred' returns non-nil")
(is-parse-failed (take-while1 (lambda (c) (eq c #\a))) "bcd"
                 "bcd" "take-while1"
                 "fails if it couldn't consume any inputs")

(finalize)
