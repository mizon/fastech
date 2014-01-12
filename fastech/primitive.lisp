(defpackage :fastech.primitive
  (:use :cl)
  (:export :parse
           :always
           :unexpected
           :bind
           :map-result
           :try
           :take-remainder
           :alternative
           :parse-failed
           :parse-failed-remainder
           :parse-failed-message))
(in-package :fastech.primitive)

(defun parse (parser input)
  "Runs `parser' with `input'. Throws `parse-failed' when `parser' fails."
  (funcall parser input 0 #'success-fn #'failure-fn))

(declaim (inline always))
(defun always (value)
  "Succeeds always. `value' is this parser's result."
  (lambda (i p sf ff)
    (declare (ignore ff))
    (funcall sf i p value)))

(declaim (inline unexpected))
(defun unexpected (message)
  "Fails always. The parse error message is `message'."
  (lambda (i p sf ff)
    (declare (ignore sf))
    (funcall ff i p message)))

(declaim (inline bind))
(defun bind (parser f)
  "Binds a parser which `f' returns to `parser'. `f' takes the result of `parser'."
  (lambda (i p sf0 ff)
    (labels ((sf1 (i p v)
               (funcall (funcall f v) i p sf0 ff)))
      (funcall parser i p #'sf1 ff))))

(declaim (inline map-result))
(defun map-result (f parser)
  "Applies `f' to the result of `parser'. The value `f' returns becomes mapped parser's result."
  (bind parser
        (lambda (r)
          (always (funcall f r)))))

(declaim (inline try))
(defun try (parser)
  "Applies `parser'. Resets the input position when `parser' fails."
  (lambda (i p sf ff0)
    (flet ((ff1 (i1 p1 message)
             (declare (ignore i1 p1))
             (funcall ff0 i p message)))
      (funcall parser i p sf #'ff1))))

(declaim (inline take-remainder))
(defun take-remainder ()
  "Consumes the whole remaining input."
  (lambda (i p sf ff)
    (declare (ignore ff))
    (funcall sf i (length i) (subseq i p))))

(declaim (inline alternative))
(defun alternative (left right)
  "Takes two parsers. Applies the first, and applies the second if the first failed."
  (lambda (i p sf ff0)
    (flet ((ff1 (i p msg)
             (declare (ignore msg))
             (funcall right i p sf ff0)))
      (funcall left i p sf #'ff1))))

;; Default success function
(defun success-fn (input pos value)
  (values value (subseq input pos)))

;; Default failure function
(defun failure-fn (input pos message)
  (error 'parse-failed :remainder (subseq input pos) :message message))

(define-condition parse-failed ()
  ((remainder :initarg :remainder :reader parse-failed-remainder)
   (message :initarg :message :reader parse-failed-message))
  (:documentation "A condition used when failed to parse."))
