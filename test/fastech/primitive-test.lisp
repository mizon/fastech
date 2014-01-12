(defpackage :fastech.primitive-test
  (:use :cl
        :cl-test-more)
  (:import-from :fastech
                :parse-failed
                :bind
                :always
                :map-result
                :try
                :str
                :parse-failed-remainder
                :take-remainder
                :alternative)
  (:import-from :fastech.combinator
                :*>)
  (:import-from :fastech.test-helper
                :is-parsed
                :is-parse-failed))
(in-package :fastech.primitive-test)

(plan 14)

(diag "unexpected")
(is-parse-failed (fastech:unexpected "message") "foobar"
                 "foobar" "message"
                 "fails always")

(diag "bind")
(is-parsed (bind (always 'foo) (constantly (always 'bar)))
           "foobar"
           'bar "foobar"
           "binds two parsers")

(diag "map-result")
(is-parsed (map-result #'string-upcase (str "foo")) "foobar"
           "FOO" "bar"
           "applies a function to the result")
(is-parse-failed (map-result #'identity (str "bar")) "foobar"
                 "foobar" "str"
                 "fails with invalid inputs")

(diag "try")
(is-parse-failed (bind
                  (str "foo")
                  (constantly (str "bar")))
                 "foofoo"
                 "foo" "str"
                 "parses a fragment")
(is-parse-failed (try
                  (bind
                   (str "foo")
                   (constantly (str "bar"))))
                 "foofoo"
                 "foofoo" "str"
                 "parses a fragment but keeps the remainder")

(diag "take-remainder")
(is-parsed (take-remainder) "foobar"
           "foobar" ""
           "consumes the whole input")
(is-parsed (*> (str "foo") (take-remainder)) "foobar"
           "bar" ""
           "consumes the remaining input")

(diag "alternative")
(is-parsed (alternative (str "foo") (str "bar")) "foobar"
           "foo" "bar"
           "applies the first parser")
(is-parsed (alternative (str "foo") (str "bar")) "barfoo"
           "bar" "foo"
           "applies the second parser")
(is-parse-failed (alternative (str "foo") (str "bar")) "noo"
                 "noo" "str"
                 "fails if the both parsers failed")

(diag "get-position")
(is-parsed (fastech.primitive:get-position) "foobar"
           0 "foobar"
           "gets the current position")
(is-parsed (*> (setf (fastech.primitive:get-position) 4)
               (fastech.primitive:get-position))  "foobar"
           4 "ar"
           "sets the current position")

(diag "with-context")
(is-parsed (fastech.primitive:with-context (i p)
             (always (list p i ))) "foobar"
           (list 0 "foobar") "foobar"
           "makes an arbitrary parser with the context")

(finalize)
