(defpackage :fastech.char
  (:use :cl)
  (:import-from :fastech.combinators
                :bind-parsers)
  (:export :chr
           :any-char
           :str
           :satisfy))
(in-package :fastech.char)

(defun chr (char)
  (bind-parsers
   (ensure 1)
   (constantly
    (lambda (i p sf ff)
      (if (eq (char i p) char)
          (funcall sf i (1+ p) (char i p))
          (funcall ff i p "chr"))))))

(defun str (string)
  (bind-parsers
   (ensure (length string))
   (constantly
    (lambda (i p sf ff)
      (if (string= i string :start1 p :end1 (+ p (length string)))
          (funcall sf i (+ p (length string)) string)
          (funcall ff i p "str"))))))

(defun satisfy (pred)
  (bind-parsers
   (ensure 1)
   (constantly
    (lambda (i p sf ff)
      (if (funcall pred (aref i p))
          (funcall sf i (1+ p) (aref i p))
          (funcall ff i p "satisfy"))))))

(defun ensure (length)
  (lambda (i p sf ff)
    (if (< (- (length i) p) length)
        (funcall ff i p "ensure: end of input")
        (funcall sf i p nil))))

(setf any-char
      (bind-parsers
       (ensure 1)
       (constantly
        (lambda (i p sf ff)
          (funcall sf i (1+ p) (char i p))))))
