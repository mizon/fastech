(defpackage :fastech.combinators
  (:use :cl)
  (:import-from :fastech.primitive
                :always)
  (:export :bind-parsers
           :not-followed-by
           :choice
           :optional
           :many
           :many1))
(in-package :fastech.combinators)

(defun bind-parsers (parser f)
  (lambda (i p sf ff)
    (labels ((sf1 (i p v)
               (funcall (funcall f v) i p sf ff)))
      (funcall parser i p #'sf1 ff))))

(defun not-followed-by (parser)
  (lambda (i p sf ff)
    (flet ((sf1 (i1 p1 v)
             (declare (ignore i1 p1 v))
             (funcall ff i p "not followed by"))
           (ff1 (i p msg)
             (declare (ignore msg))
             (funcall sf i p nil)))
      (funcall parser i p #'sf1 #'ff1))))

(defun choice (parser &rest parsers)
  (reduce (lambda (l r)
            (or-parser l r))
          (cons parser parsers)))

(defun optional (parser)
  (or-parser parser (always nil)))

(defun many (parser)
  (or-parser (many1 parser) (always ())))

(defun many1 (parser)
  (bind-parsers
   parser
   (lambda (v)
     (bind-parsers
      (many parser)
      (lambda (vs)
        (always (cons v vs)))))))

(defun or-parser (left right)
  (lambda (i p sf ff)
    (flet ((ff1 (i p msg)
             (declare (ignore msg))
             (funcall right i p sf ff)))
      (funcall left i p sf #'ff1))))
