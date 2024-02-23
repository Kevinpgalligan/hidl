(defpackage :hidden-language
  (:use :cl)
  (:export
   :make-and-gate
   :make-or-gate
   :make-xor-gate
   :make-not-gate
   :lgate-compute-output
   :lgate-set-input!))
