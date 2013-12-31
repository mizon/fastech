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
                             "fastech/string"
                             "fastech/combinators"))
               (:file "fastech/combinators")
               (:file "fastech/string"
                :depends-on ("fastech/combinators"))
               (:file "fastech/primitive")))

(defsystem :fastech-test
  :license "BSD3"
  :version "0.1"
  :author "Keita Mizuochi <mizon9@gmail.com>"
  :components ((:module "test"
                :components ((:file "fastech/combinators")
                             (:file "fastech/string")
                             (:file "fastech/primitive"))))
  :depends-on (:cl-test-more
               :fastech))
