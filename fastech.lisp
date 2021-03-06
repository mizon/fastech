(defpackage :fastech
  (:use :cl
        :fastech.primitive
        :fastech.combinator
        :fastech.char)
  (:export :parse
           :parse-only
           :always
           :unexpected
           :parse-failed
           :parse-failed-remainder
           :parse-failed-message
           :map-result
           :try
           :take-remainder
           :alternative

           :bind-parsers
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
