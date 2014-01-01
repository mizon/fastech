(defpackage :fastech.primitive
  (:use :cl)
  (:export :parse
           :always
           :unexpected
           :parse-error))
(in-package :fastech.primitive)

(defun parse (parser input)
  (funcall parser input 0 #'success-fn #'failure-fn))

(defun always (value)
  (lambda (i p sf ff)
    (declare (ignore ff))
    (funcall sf i p value)))

(defun unexpected (message)
  (lambda (i p sf ff)
    (declare (ignore sf))
    (funcall ff i p message)))

(defun try (parser))

;; Default success function
(defun success-fn (input pos value)
  (values value (subseq input pos)))

;; Default failure function
(defun failure-fn (input pos message)
  (error 'parse-error :remainder (subseq input pos) :message message))

(define-condition parse-error ()
  ((remainder :initarg :remainder :reader parse-error-remainder)
   (message :initarg :message :reader parse-error-message)))
