(defpackage :fastech
  (:use :cl
        :fastech.primitive
        :fastech.combinators
        :fastech.char)
  (:export :parse
           :always
           :unexpected
           :parse-error
           :parse-error-remainder
           :parse-error-message
           :try

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
           :satisfy))
(in-package :fastech)
