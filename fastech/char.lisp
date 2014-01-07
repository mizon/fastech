(defpackage :fastech.char
  (:use :cl)
  (:import-from :iterate
                :iter
                :for
                :leave)
  (:import-from :fastech.primitive
                :bind)
  (:export :chr
           :any-char
           :str
           :satisfy
           :take-while))
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
      (declare (ignore ff))
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

(defun take-while (pred)
  "Parses characters while `pred' returns non-nil, and the result is a string. This parser is very faster than `(many (satisfy pred))'."
  (lambda (i p sf ff)
    (declare (ignore ff))
    (let* ((l (length i))
           (end (iter (for idx upfrom p)
                      (cond ((= idx l)
                             (leave idx))
                            ((not (funcall pred (aref i idx)))
                             (leave idx))))))
      (funcall sf i end (subseq i p end)))))

;; Helper
(declaim (inline ensure))
(defun ensure (length)
  (lambda (i p sf ff)
    (if (< (- (length i) p) length)
        (funcall ff i p "ensure: end of input")
        (funcall sf i p nil))))
