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

           :bind-parsers
           :not-followed-by
           :choice
           :optional
           :many
           :many1

           :str
           :satisfy))
(in-package :fastech)
