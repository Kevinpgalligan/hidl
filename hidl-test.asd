(defpackage :hidl-test-asd
  (:use :cl :asdf))

(in-package :hidl-test-asd)

(defsystem hidl-test
  :license "MIT"
  :author "Kevin Galligan"
  :depends-on (:hidl :fiveam)
  :pathname "t"
  :serial t
  :components ((:file "package")
               (:file "gates")
               ))
