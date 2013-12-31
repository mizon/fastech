(defpackage :fastech
  (:use :cl
        :fastech.combinators
        :fastech.string)
  (:export :parse
           :parse-error
           :parse-error-remainder
           :parse-error-message

           :bind-parsers
           :always
           :not-followed-by

           :str
           :satisfy))
(in-package :fastech)

(defun parse (parser input)
  (funcall parser input 0 #'success-fn #'failure-fn))

;; Default success function
(defun success-fn (input pos value)
  (values value (subseq input pos)))

;; Default failure function
(defun failure-fn (input pos message)
  (error 'parse-error :remainder (subseq input pos) :message message))

(define-condition parse-error ()
  ((remainder :initarg :remainder :reader parse-error-remainder)
   (message :initarg :message :reader parse-error-message)))
