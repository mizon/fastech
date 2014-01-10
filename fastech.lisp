(defpackage :fastech
  (:use :cl
        :fastech.primitive
        :fastech.combinators
        :fastech.char)
  (:export :parse
           :always
           :unexpected
           :parse-failed
           :parse-failed-remainder
           :parse-failed-message
           :map-result
           :try
           :take-remainder

           :bind-parsers
           :not-followed-by
           :choice
           :optional
           :many
           :many1
           :*>
           :<*

           :chr
           :any-char
           :str
           :satisfy
           :take-while
           :take-while1
           :take-till))
(in-package :fastech)
