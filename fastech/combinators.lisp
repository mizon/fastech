(defpackage :fastech.combinators
  (:use :cl)
  (:export :bind-parsers
           :not-followed-by))
(in-package :fastech.combinators)

(defun bind-parsers (parser f)
  (lambda (i p sf ff)
    (labels ((sf1 (i p v)
               (funcall (funcall f v) i p sf ff)))
      (funcall parser i p #'sf1 ff))))

(defun not-followed-by (parser)
  (lambda (i p sf ff)
    (flet ((sf1 (i1 p1 v)
             (declare (ignore v))
             (funcall ff i1 p1 "not followed by"))
           (ff1 (i1 p1 msg)
             (declare (ignore msg))
             (funcall sf i1 p1 nil)))
      (funcall parser i p #'sf1 #'ff1))))
