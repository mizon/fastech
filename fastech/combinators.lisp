(defpackage :fastech.combinators
  (:use :cl)
  (:export :bind-parsers
           :always
           :not-followed-by))
(in-package :fastech.combinators)

(defun bind-parsers (parser f)
  (lambda (i p sf ff)
    (labels ((sf1 (i p v)
               (funcall (funcall f v) i p sf ff)))
      (funcall parser i p #'sf1 ff))))

(defun always (value)
  (lambda (i p sf ff)
    (declare (ignore ff))
    (funcall sf i p value)))

(defun not-followed-by (parser)
  (lambda (i p sf ff)
    (flet ((sf1 (i1 p1 v)
             (funcall ff i1 p1 "not followed by"))
           (ff1 (i1 p1 msg)
             (funcall sf i1 p1 nil)))
      (funcall parser i p #'sf1 #'ff1))))
