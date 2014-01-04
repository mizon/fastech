(defpackage :fastech.combinators-test
  (:use :cl
        :cl-test-more)
  (:import-from :fastech
                :parse
                :always
                :not-followed-by
                :choice
                :optional
                :parse-failed
                :str
                :unexpected
                :many
                :many1
                :*>
                :<*)
  (:import-from :fastech.test-helper
                :is-parsed
                :is-parse-failed))
(in-package :fastech.combinators-test)

(plan 14)

(diag "always")
(is-parsed (always :foo) "some string"
           :foo "some string"
           "makes an always parser")

(diag "not-followed-by")
(is-parsed (not-followed-by (str "foo")) "barfoo"
           nil "barfoo"
           "succeeds when the inner parser fails")
(is-parse-failed (not-followed-by (always 100)) "foobar"
                 "foobar" "not followed by"
                 "failes when the inner parser succeeds")

(diag "choice")
(is-parsed (choice (always :foo) (always :foobar)) "foobar"
           :foo "foobar"
           "accepts the succeeded parser")
(is-parsed (choice (unexpected "bar") (str "foo") (unexpected "bar"))
           "foobar"
           "foo" "bar"
           "accepts the succeeded parser")
(is-parse-failed (choice (unexpected "foo") (unexpected "bar")) "foobar"
                 "foobar" "bar"
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
(is-parse-failed (many1 (str "foo")) "bar"
                 "bar" "str"
                 "fails with the invalid input")
(is-parsed (many1 (str "foo")) "foofoofoo"
           '("foo" "foo" "foo") ""
           "parses many words")

(diag "*>")
(is-parsed (*> (str "foo") (str "bar") (str "noo")) "foobarnoo"
           "noo" ""
           "keeps the last result")

(diag "<*")
(is-parsed (<* (str "foo") (str "bar") (str "noo")) "foobarnoo"
           "foo" ""
           "keeps the first result")

(finalize)
