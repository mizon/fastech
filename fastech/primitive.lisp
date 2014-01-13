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
           :parse-failed-message

           ;; Internals
           :get-position
           :with-context))
(in-package :fastech.primitive)

(defvar *input* nil)

(defun parse (parser input)
  "Runs `parser' with `input'. Throws `parse-failed' when `parser' fails."
  (let ((*input* input))
    (funcall parser 0 #'success-fn #'failure-fn)))

(declaim (inline always))
(defun always (value)
  "Succeeds always. `value' is this parser's result."
  (lambda (p sf ff)
    (declare (ignore ff))
    (funcall sf p value)))

(declaim (inline unexpected))
(defun unexpected (message)
  "Fails always. The parse error message is `message'."
  (lambda (p sf ff)
    (declare (ignore sf))
    (funcall ff p message)))

(declaim (inline bind))
(defun bind (parser f)
  "Binds a parser which `f' returns to `parser'. `f' takes the result of `parser'."
  (lambda (p sf0 ff)
    (labels ((sf1 (p v)
               (funcall (funcall f v) p sf0 ff)))
      (funcall parser p #'sf1 ff))))

(declaim (inline map-result))
(defun map-result (f parser)
  "Applies `f' to the result of `parser'. The value `f' returns becomes mapped parser's result."
  (bind parser
        (lambda (r)
          (always (funcall f r)))))

(declaim (inline try))
(defun try (parser)
  "Applies `parser'. Resets the input position when `parser' fails."
  (lambda (p sf ff0)
    (flet ((ff1 (p1 message)
             (declare (ignore p1))
             (funcall ff0 p message)))
      (funcall parser p sf #'ff1))))

(declaim (inline take-remainder))
(defun take-remainder ()
  "Consumes the whole remaining input."
  (lambda (p sf ff)
    (declare (ignore ff))
    (funcall sf (length *input*) (subseq *input* p))))

(declaim (inline alternative))
(defun alternative (left right)
  "Takes two parsers. Applies the first, and applies the second if the first failed."
  (lambda (p sf ff0)
    (flet ((ff1 (p msg)
             (declare (ignore msg))
             (funcall right p sf ff0)))
      (funcall left p sf #'ff1))))

;; Default success function
(defun success-fn (pos value)
  (values value (subseq *input* pos)))

;; Default failure function
(defun failure-fn (pos message)
  (error 'parse-failed :remainder (subseq *input* pos) :message message))

(define-condition parse-failed ()
  ((remainder :initarg :remainder :reader parse-failed-remainder)
   (message :initarg :message :reader parse-failed-message))
  (:documentation "A condition used when failed to parse."))

;;; Internals - Used by the fastech packages.

(declaim (inline get-position))
(defun get-position ()
  "Internal"
  (lambda (p sf ff)
    (declare (ignore ff))
    (funcall sf p p)))

(declaim (inline (setf get-position)))
(defun (setf get-position) (pos)
  "Internal"
  (lambda (p sf ff)
    (declare (ignore p ff))
    (funcall sf pos nil)))

(defmacro with-context (arg-list &body body)
  "Internal"
  (let ((p (gensym "P"))
        (sf (gensym "SF"))
        (ff (gensym "FF")))
    `(lambda (,p ,sf ,ff)
       (funcall ((lambda ,arg-list ,@body) *input* ,p)
                ,p
                ,sf
                ,ff))))
