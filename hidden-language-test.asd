(defpackage :hidden-language-test-asd
  (:use :cl :asdf))

(in-package :hidden-language-test-asd)

(defsystem hidden-language-test
  :license "MIT"
  :author "Kevin Galligan"
  :depends-on (:hidden-language :fiveam)
  :pathname "t"
  :serial t
  :components ((:file "package")
               (:file "gates")
               ))
