(defpackage :test.fastech.primitive
  (:use :cl
        :cl-test-more)
  (:import-from :fastech
                :parse-error
                :bind-parsers
                :always
                :try
                :str
                :parse-error-remainder)
  (:import-from :test.fastech.helper
                :is-parsed
                :is-parse-error))
(in-package :test.fastech.primitive)

(plan 4)

(diag "unexpected")
(is-parse-error (fastech:unexpected "message") "foobar"
                "foobar" "message"
                "fails always")

(diag "bind-parsers")
(is-parsed (bind-parsers (always 'foo) (constantly (always 'bar)))
           "foobar"
           'bar "foobar"
           "binds two parsers")

(diag "try")
(is-parse-error (bind-parsers
                 (str "foo")
                 (constantly (str "bar")))
                "foofoo"
                "foo" "str"
                "parses a fragment")
(is-parse-error (try
                 (bind-parsers
                  (str "foo")
                  (constantly (str "bar"))))
                "foofoo"
                "foofoo" "str"
                "parses a fragment but keeps the remainder")

(finalize)
