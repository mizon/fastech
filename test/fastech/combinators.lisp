(defpackage :test.fastech.combinators
  (:use :cl
        :cl-test-more
        :fastech))
(in-package :test.fastech.combinators)

(plan 3)

(diag "bind-parsers")
(let ((parser (bind-parsers (always :foo)
                            (constantly (always :bar)))))
  (is (parse parser "foobar") :bar "binds two parsers"))

(diag "always")
(is (parse (always :foo) "some string") :foo "makes an always parser")

(diag "not-followed-by")
(let ((parser (not-followed-by (always 100))))
  (is-error (parse parser "foobar")
            parse-error
            "failes when the inner parser succeeds"))

(diag "many")

(diag "many1")

(finalize)
