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
                :parse-failed-remainder)
  (:import-from :fastech.test-helper
                :is-parsed
                :is-parse-failed))
(in-package :fastech.primitive-test)

(plan 6)

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

(finalize)
