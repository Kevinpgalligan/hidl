(defpackage :hidl
  (:use :cl)
  (:export
   :make-and-gate
   :make-or-gate
   :make-xor-gate
   :make-not-gate
   :gate-compute-output
   :gate-set-input!))
