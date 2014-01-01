(defpackage :test.fastech.combinators
  (:use :cl
        :cl-test-more)
  (:import-from :fastech
                :parse
                :bind-parsers
                :always
                :not-followed-by
                :choice
                :optional
                :parse-error
                :str
                :unexpected
                :many
                :many1)
  (:import-from :test.fastech.helper
                :is-parsed))
(in-package :test.fastech.combinators)

(plan 13)

(diag "bind-parsers")
(is-parsed (bind-parsers (always 'foo) (constantly (always 'bar)))
           "foobar"
           'bar "foobar"
           "binds two parsers")

(diag "always")
(is-parsed (always :foo) "some string"
           :foo "some string"
           "makes an always parser")

(diag "not-followed-by")
(is-parsed (not-followed-by (str "foo")) "barfoo"
           nil "barfoo"
           "succeeds when the inner parser fails")
(is-error (parse (not-followed-by (always 100)) "foobar")
          parse-error
          "failes when the inner parser succeeds")

(diag "choice")
(is-parsed (choice (always :foo) (always :foobar)) "foobar"
           :foo "foobar"
           "accepts the succeeded parser")
(is-parsed (choice (unexpected "bar") (str "foo") (unexpected "bar"))
           "foobar"
           "foo" "bar"
           "accepts the succeeded parser")
(is-error (parse (choice (unexpected "foo") (unexpected "bar")) "foobar")
          'parse-error
          "fails when the all parsers fails")

(diag "optional")
(is-parsed (optional (str "foo")) "foobar"
           "foo" "bar"
           "runs the inner parser")
(is-parsed (optional (str "bar"))  "foobar"
           nil "foobar"
           "returns nil when the inner parser fails")

(diag "many")
(is-parsed (many (str "foo")) "bar"
           () "bar"
           "succeeds with the invalid input")
(is-parsed (many (str "foo")) "foofoofoo"
           '("foo" "foo" "foo") ""
           "parses many words")

(diag "many1")
(is-error (parse (many1 (str "foo")) "bar")
          'parse-error
          "fails with the invalid input")
(is-parsed (many1 (str "foo")) "foofoofoo"
           '("foo" "foo" "foo") ""
           "parses many words")

(finalize)
