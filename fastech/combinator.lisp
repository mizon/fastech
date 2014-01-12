(defpackage :fastech.combinator
  (:use :cl)
  (:import-from :fastech.primitive
                :always
                :bind
                :alternative)
  (:export :choice
           :optional
           :many
           :many1
           :*>
           :<*))
(in-package :fastech.combinator)

(declaim (inline choice))
(defun choice (parser &rest parsers)
  "Tries applying `parsers' in order. When the inner parser succeeds, halts and uses the result of the succeeding parser."
  (reduce (lambda (l r)
            (alternative l r))
          (cons parser parsers)))

(declaim (inline optional))
(defun optional (parser)
  "Tries to apply `parser'. Succeeds whether `parser' succeeds or not."
  (alternative parser (always nil)))

(declaim (inline many))
(defun many (parser)
  "Applies `parser' many times until `parser' fails. The list of parser results is this parser's result."
  (labels ((many-p ()
             (alternative (many1-p) (always ())))
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
