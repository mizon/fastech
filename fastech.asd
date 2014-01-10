(defpackage :fastech-asd
  (:use :cl
        :asdf))
(in-package :fastech-asd)

(defsystem :fastech
  :description "Fast parser combinators"
  :license "BSD3"
  :version "0.1"
  :author "Keita Mizuochi <mizon9@gmail.com>"
  :components ((:file "fastech"
                :depends-on ("fastech/primitive"
                             "fastech/char"
                             "fastech/combinator"))
               (:file "fastech/combinator"
                :depends-on ("fastech/primitive"))
               (:file "fastech/char"
                :depends-on ("fastech/combinator"))
               (:file "fastech/primitive"))
  :depends-on (:iterate))

(defsystem :fastech-test
  :license "BSD3"
  :version "0.1"
  :author "Keita Mizuochi <mizon9@gmail.com>"
  :components ((:module "test/fastech"
                :components ((:file "combinator-test"
                              :depends-on ("test-helper"))
                             (:file "char-test"
                              :depends-on ("test-helper"))
                             (:file "primitive-test"
                              :depends-on ("test-helper"))
                             (:file "test-helper"))))
  :depends-on (:cl-test-more
               :fastech))
