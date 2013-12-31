(defpackage :fastech
  (:use :cl
        :fastech.primitive
        :fastech.combinators
        :fastech.string)
  (:export :parse
           :always
           :unexpected
           :parse-error
           :parse-error-remainder
           :parse-error-message

           :bind-parsers
           :not-followed-by

           :str
           :satisfy))
(in-package :fastech)
