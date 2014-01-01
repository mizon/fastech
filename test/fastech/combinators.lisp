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
                :many1))
(in-package :test.fastech.combinators)

(plan 11)

(diag "bind-parsers")
(is (parse (bind-parsers (always :foo)
                         (constantly (always :bar)))
           "foobar")
    :bar
    "binds two parsers")

(diag "always")
(is (parse (always :foo) "some string") :foo "makes an always parser")

(diag "not-followed-by")
(is (parse (not-followed-by (str "foo")) "barfoo")
    (values nil "barfoo")
    "succeeds when the inner parser fails")
(is-error (parse (not-followed-by (always 100)) "foobar")
          parse-error
          "failes when the inner parser succeeds")

(diag "choice")
(is (parse (choice (always :foo) (always :foobar)) "foobar")
    (values :foo "foobar")
    "accepts the succeeded parser")
(is (parse (choice (unexpected "bar")
                   (str "foo")
                   (unexpected "bar"))
           "foobar")
    (values "foo" "bar")
    "accepts the succeeded parser")
(is-error (parse (choice (unexpected "foo") (unexpected "bar")) "foobar")
          'parse-error
          "fails when the all parsers fails")

(diag "optional")
(is (parse (optional (str "foo")) "foobar")
    (values "foo" "bar")
    "runs the inner parser")
(is (parse (optional (str "bar"))  "foobar")
    (values nil "foobar")
    "returns nil when the inner parser fails")

(diag "many")
(is (parse (many (str "foo")) "bar")
    (values () "bar")
    "succeeds with the invalid input")
(is (parse (many (str "foo")) "foofoofoo")
    (values '("foo" "foo" "foo") "")
    "parses many words")

(diag "many1")
(is-error (parses (many1 (str "foo")) "bar")
          'parse-error
          "fails with the invalid input")
(is (parse (many1 (str "foo")) "foofoofoo")
    (values '("foo" "foo" "foo") "")
    "parses many words")

(finalize)
