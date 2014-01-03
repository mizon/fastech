(defpackage :fastech.combinators
  (:use :cl)
  (:import-from :fastech.primitive
                :always
                :bind-parsers)
  (:export :not-followed-by
           :choice
           :optional
           :many
           :many1
           :*>
           :<*))
(in-package :fastech.combinators)

(defun not-followed-by (parser)
  "Succeeds if parser fails, fails if parser succeeds."
  (lambda (i p sf ff)
    (flet ((sf1 (i1 p1 v)
             (declare (ignore i1 p1 v))
             (funcall ff i p "not followed by"))
           (ff1 (i p msg)
             (declare (ignore msg))
             (funcall sf i p nil)))
      (funcall parser i p #'sf1 #'ff1))))

(defun choice (parser &rest parsers)
  "Tries applying parsers in order. When the inner parser succeeds, halts and uses the value of the succeeding parser."
  (reduce (lambda (l r)
            (or-parser l r))
          (cons parser parsers)))

(defun optional (parser)
  "Tries to apply parser, succeeds whether parser succeeds or not."
  (or-parser parser (always nil)))

(defun many (parser)
  "Applies parser many times until parser fails. The list of parser values is this parser's value."
  (or-parser (many1 parser) (always ())))

(defun many1 (parser)
  "Applies parser many times as well as many but only succeeds when parser succeeds more once."
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

(defun *> (parser &rest parsers)
  "Applies parsers in order and keeps the value of the last parser."
  (reduce (lambda (l r)
            (bind-parsers l (constantly r)))
          parsers
          :initial-value parser))

(defun <* (parser &rest parsers)
  "Applies parsers in order and keeps the value of the first parser."
  (bind-parsers
   parser
   (lambda (v)
     (*> (apply #'*> parsers) (always v)))))
