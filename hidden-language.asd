(defpackage :hidden-language-asd
  (:use :cl :asdf))

(in-package :hidden-language-asd)

(defsystem hidden-language
  :license "MIT"
  :author "Kevin Galligan"
  :depends-on (:alexandria)
  :pathname "src"
  :serial t
  :components ((:file "package")
               (:file "schedule")
               (:file "gates")
               (:file "simulate")))
