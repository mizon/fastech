(defpackage :fastech-asd
  (:use :cl
        :asdf))
(in-package :fastech-asd)

(defsystem :fastech
  :description "Fast parser combinators"
  :license "BSD3"
  :version "0.1"
  :author "Keita Mizuochi <mizon9@gmail.com>"
  :components ((:module "."
                :components ((:file "fastech"
                              :depends-on ("fastech/combinators"
                                           "fastech/string"))
                             (:file "fastech/combinators")
                             (:file "fastech/string"
                              :depends-on ("fastech/combinators"))))))

(defsystem :fastech-test
  :license "BSD3"
  :version "0.1"
  :author "Keita Mizuochi <mizon9@gmail.com>"
  :components ((:module "test"
                :components ((:file "fastech")
                             (:file "fastech/combinators")
                             (:file "fastech/string"))))
  :depends-on (:cl-test-more
               :fastech))
