(defpackage :hidl-asd
  (:use :cl :asdf))

(in-package :hidl-asd)

(defsystem hidl
  :license "MIT"
  :author "Kevin Galligan"
  :depends-on (:alexandria)
  :pathname "src"
  :serial t
  :components ((:file "package")
               (:file "schedule")
               (:file "gates")
               ;(:file "simulate")
               ))
