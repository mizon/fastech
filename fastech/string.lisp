(defpackage :fastech.string
  (:use :cl)
  (:import-from :fastech.combinators
                :bind-parsers)
  (:export :str
           :satisfy))
(in-package :fastech.string)

(defun str (string)
  (bind-parsers
   (ensure (length string))
   (constantly (lambda (i p sf ff)
                 (if (string= i string :start1 p :end1 (+ p (length string)))
                     (funcall sf i (+ p (length string)) string)
                     (funcall ff i p "str"))))))

(defun satisfy (pred)
  (bind-parsers
   (ensure 1)
   (constantly (lambda (i p sf ff)
                 (if (funcall pred (aref i p))
                     (funcall sf i (1+ p) (aref i p))
                     (funcall ff i p "satisfy"))))))

(defun ensure (length)
  (lambda (i p sf ff)
    (if (< (- (length i) p) length)
        (funcall ff i p "ensure: end of input")
        (funcall sf i p nil))))
