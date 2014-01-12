(defpackage :fastech.experimental
  (:use :cl)
  (:import-from :fastech.primitive
                :bind)
  (:export :parser))
(in-package :fastech.experimental)

(defmacro parser (clause &body clauses)
  "[EXPERIMENTAL] A macro makes easy to write complicated parsers.

Syntax:
    parser {clause}* parser-expression

    clause ::= (:let var expression)
             | (:bind var parser-expression)
             | parser-expression

* `var' --- A symbol.
* `expression' --- An arbitrary expression.
* `parser-expression' --- An expression to be evaluated to a parser.

Examples:
    (parse (parser
            (:bind foo (str \"foo\"))
            (:let foobar (list foo \"bar\"))
            (always foobar))
           \"foobar\")
    ;=> (\"foo\" \"bar\")
    (parse (parser
            (:bind c (any-char))
            (if (eq c #\A)
                (always \"It is A.\")
                (unexpected \"It isn't A.\")))
           \"ABC\")
    ;=> \"It is A.\""
  (if clauses
      (optima:match clause
        ((list :let var expression)
         `(let ((,var ,expression))
            (parser ,@clauses)))
        ((list :bind var parser)
         `(bind ,parser
                (lambda (,var)
                  (parser ,@clauses))))
        (otherwise
         `(bind ,clause
                (constantly
                 (parser ,@clauses)))))
      clause))
