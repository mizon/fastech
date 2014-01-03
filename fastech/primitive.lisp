(defpackage :fastech.primitive
  (:use :cl)
  (:export :parse
           :always
           :unexpected
           :bind
           :map-parser
           :try
           :parse-error
           :parse-error-remainder
           :parse-error-message))
(in-package :fastech.primitive)

(defun parse (parser input)
  "Runs `parser' with input. Throws `parse-error' when `parser' fails."
  (funcall parser input 0 #'success-fn #'failure-fn))

(defun always (value)
  "Succeeds always. `value' is this parser's result."
  (lambda (i p sf ff)
    (declare (ignore ff))
    (funcall sf i p value)))

(defun unexpected (message)
  "Fails always. The parse error message is `message'."
  (lambda (i p sf ff)
    (declare (ignore sf))
    (funcall ff i p message)))

(defun bind (parser f)
  "Binds a parser which `f' returns to `parser'. `f' takes the result of `parser'."
  (lambda (i p sf ff)
    (labels ((sf1 (i p v)
               (funcall (funcall f v) i p sf ff)))
      (funcall parser i p #'sf1 ff))))

(defun map-parser (f parser)
  "Applies `f' to `parser''s result and the result is this parser's result."
  (lambda (i p sf ff)
    (flet ((sf1 (i p v)
             (funcall sf i p (funcall f v))))
      (funcall parser i p #'sf1 ff))))

(defun try (parser)
  "Applies `parser'. Resets the input position when `parser' fails."
  (lambda (i p sf ff)
    (flet ((ff1 (i1 p1 message)
             (declare (ignore i1 p1))
             (funcall ff i p message)))
      (funcall parser i p sf #'ff1))))

;; Default success function
(defun success-fn (input pos value)
  (values value (subseq input pos)))

;; Default failure function
(defun failure-fn (input pos message)
  (error 'parse-error :remainder (subseq input pos) :message message))

(define-condition parse-error ()
  ((remainder :initarg :remainder :reader parse-error-remainder)
   (message :initarg :message :reader parse-error-message))
  (:documentation "A condition used when failed to parse."))
