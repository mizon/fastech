(defpackage :fastech.char
  (:use :cl)
  (:import-from :fastech.primitive
                :bind)
  (:export :chr
           :any-char
           :str
           :satisfy))
(in-package :fastech.char)

(declaim (inline chr))
(defun chr (char)
  "Parses a character."
  (bind
   (ensure 1)
   (constantly
    (lambda (i p sf ff)
      (if (eq (char i p) char)
          (funcall sf i (1+ p) (char i p))
          (funcall ff i p "chr"))))))

(declaim (inline any-char))
(defun any-char ()
  "Parses an arbitrary character."
  (bind
   (ensure 1)
   (constantly
    (lambda (i p sf ff)
      (funcall sf i (1+ p) (char i p))))))

(declaim (inline str))
(defun str (string)
  "Parses a string."
  (bind
   (ensure (length string))
   (constantly
    (lambda (i p sf ff)
      (if (string= i string :start1 p :end1 (+ p (length string)))
          (funcall sf i (+ p (length string)) string)
          (funcall ff i p "str"))))))

(declaim (inline satisfy))
(defun satisfy (pred)
  "Parses a character if `pred' returns non-nil, `pred' takes a head character of the current input."
  (bind
   (ensure 1)
   (constantly
    (lambda (i p sf ff)
      (if (funcall pred (aref i p))
          (funcall sf i (1+ p) (aref i p))
          (funcall ff i p "satisfy"))))))

;; Helper
(declaim (inline ensure))
(defun ensure (length)
  (lambda (i p sf ff)
    (if (< (- (length i) p) length)
        (funcall ff i p "ensure: end of input")
        (funcall sf i p nil))))
