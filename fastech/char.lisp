(defpackage :fastech.char
  (:use :cl)
  (:import-from :iterate
                :iter
                :for
                :leave)
  (:import-from :fastech.primitive
                :bind
                :always
                :unexpected
                :get-position
                :with-context)
  (:import-from :fastech.combinator
                :choice
                :*>)
  (:export :chr
           :any-char
           :str
           :satisfy
           :take-while
           :take-while1
           :take-till))
(in-package :fastech.char)

(declaim (inline chr))
(defun chr (char)
  "Parses a character."
  (bind
   (ensure 1)
   (constantly
    (with-context (i p)
      (if (eq (char i p) char)
          (*> (setf (get-position) (1+ p))
              (always (char i p)))
          (unexpected "chr"))))))

(declaim (inline any-char))
(defun any-char ()
  "Parses an arbitrary character."
  (bind
   (ensure 1)
   (constantly
    (with-context (i p)
      (*> (setf (get-position) (1+ p))
          (always (char i p)))))))

(declaim (inline str))
(defun str (string)
  "Parses a string."
  (bind
   (ensure (length string))
   (constantly
    (with-context (i p)
      (if (string= i string :start1 p :end1 (+ p (length string)))
          (*> (setf (get-position) (+ p (length string)))
              (always string))
          (unexpected "str"))))))

(declaim (inline satisfy))
(defun satisfy (pred)
  "Parses a character if `pred' returns non-nil, `pred' takes a head character of the current input."
  (bind
   (ensure 1)
   (constantly
    (with-context (i p)
      (if (funcall pred (aref i p))
          (*> (setf (get-position) (1+ p))
              (always (aref i p)))
          (unexpected "satisfy"))))))

(declaim (inline take-while))
(defun take-while (pred)
  "Parses characters while `pred' returns non-nil, and the result is the string. This parser is very faster than `(many (satisfy pred))'."
  (choice (take-while1 pred) (always "")))

(declaim (inline take-while1))
(defun take-while1 (pred)
  "Same as `take-while' except that `take-while' fails if no characters were consumed."
  (with-context (i p)
    (let* ((l (length i))
           (end (iter (for idx upfrom p)
                      (cond ((= idx l)
                             (leave idx))
                            ((not (funcall pred (aref i idx)))
                             (leave idx))))))
      (if (= p end)
          (unexpected "take-while1")
          (*> (setf (get-position) end)
              (always (subseq i p end)))))))

(declaim (inline take-till))
(defun take-till (pred)
  "Parses characters till `pred' returns non-nil, and the result is the string."
  (take-while (lambda (c)
                (not (funcall pred c)))))

;; Helper
(declaim (inline ensure))
(defun ensure (length)
  (with-context (i p)
    (if (< (- (length i) p) length)
        (unexpected "ensure: end of input")
        (always nil))))
