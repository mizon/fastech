(defpackage :fastech.primitive-test
  (:use :cl
        :cl-test-more)
  (:import-from :fastech
                :parse-error
                :bind
                :always
                :map-result
                :try
                :str
                :parse-error-remainder)
  (:import-from :fastech.test-helper
                :is-parsed
                :is-parse-error))
(in-package :fastech.primitive-test)

(plan 6)

(diag "unexpected")
(is-parse-error (fastech:unexpected "message") "foobar"
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
(is-parse-error (map-result #'identity (str "bar")) "foobar"
                "foobar" "str"
                "fails with invalid inputs")

(diag "try")
(is-parse-error (bind
                 (str "foo")
                 (constantly (str "bar")))
                "foofoo"
                "foo" "str"
                "parses a fragment")
(is-parse-error (try
                 (bind
                  (str "foo")
                  (constantly (str "bar"))))
                "foofoo"
                "foofoo" "str"
                "parses a fragment but keeps the remainder")

(finalize)
