(defpackage :fastech.combinators
  (:use :cl)
  (:import-from :fastech.primitive
                :always
                :bind)
  (:export :not-followed-by
           :choice
           :optional
           :many
           :many1
           :*>
           :<*))
(in-package :fastech.combinators)

(declaim (inline not-followed-by))
(defun not-followed-by (parser)
  "Succeeds if `parser' fails. Fails if `parser' succeeds."
  (lambda (i p sf ff)
    (flet ((sf1 (i1 p1 v)
             (declare (ignore i1 p1 v))
             (funcall ff i p "not followed by"))
           (ff1 (i p msg)
             (declare (ignore msg))
             (funcall sf i p nil)))
      (funcall parser i p #'sf1 #'ff1))))

(declaim (inline choice))
(defun choice (parser &rest parsers)
  "Tries applying `parsers' in order. When the inner parser succeeds, halts and uses the result of the succeeding parser."
  (reduce (lambda (l r)
            (or-parser l r))
          (cons parser parsers)))

(declaim (inline optional))
(defun optional (parser)
  "Tries to apply `parser'. Succeeds whether `parser' succeeds or not."
  (or-parser parser (always nil)))

(declaim (inline many))
(defun many (parser)
  "Applies `parser' many times until `parser' fails. The list of parser results is this parser's result."
  (labels ((many-p ()
             (or-parser (many1-p) (always ())))
           (many1-p ()
             (bind
              parser
              (lambda (v)
                (bind
                 (many-p)
                 (lambda (vs)
                   (always (cons v vs))))))))
    (many-p)))

(declaim (inline many1))
(defun many1 (parser)
  "Applies `parser' many times as well as `many' but only succeeds when `parser' succeeds more once."
  (bind
   parser
   (lambda (v)
     (bind
      (many parser)
      (lambda (vs)
        (always (cons v vs)))))))

(declaim (inline or-parser))
(defun or-parser (left right)
  (lambda (i p sf ff)
    (flet ((ff1 (i p msg)
             (declare (ignore msg))
             (funcall right i p sf ff)))
      (funcall left i p sf #'ff1))))

(declaim (inline *>))
(defun *> (parser &rest parsers)
  "Applies `parsers' in order and keeps the result of the last parser."
  (reduce (lambda (l r)
            (bind l (constantly r)))
          parsers
          :initial-value parser))

(declaim (inline <*))
(defun <* (parser &rest parsers)
  "Applies `parsers' in order and keeps the result of the first parser."
  (bind
   parser
   (lambda (v)
     (*> (apply #'*> parsers) (always v)))))
