(in-package hidl-test)

(def-suite gates
  :in hidl)

(in-suite gates)

(test and-gate
  (let ((g (make-and-gate)))
    (gate-set-input! g 0 0)
    (gate-set-input! g 1 1)
    (is (= 0 (gate-compute-output g)))
    (gate-set-input! g 0 1)
    (gate-set-input! g 1 1)
    (is (= 1 (gate-compute-output g)))
    (gate-set-input! g 0 0)
    (gate-set-input! g 1 0)
    (is (= 0 (gate-compute-output g)))))

(test or-gate
  (let ((g (make-or-gate)))
    (gate-set-input! g 0 0)
    (gate-set-input! g 1 1)
    (is (= 1 (gate-compute-output g)))
    (gate-set-input! g 0 1)
    (gate-set-input! g 1 1)
    (is (= 1 (gate-compute-output g)))
    (gate-set-input! g 0 0)
    (gate-set-input! g 1 0)
    (is (= 0 (gate-compute-output g)))))

(test xor-gate
  (let ((g (make-xor-gate)))
    (gate-set-input! g 0 0)
    (gate-set-input! g 1 1)
    (is (= 1 (gate-compute-output g)))
    (gate-set-input! g 0 1)
    (gate-set-input! g 1 0)
    (is (= 1 (gate-compute-output g)))
    (gate-set-input! g 0 1)
    (gate-set-input! g 1 1)
    (is (= 0 (gate-compute-output g)))
    (gate-set-input! g 0 0)
    (gate-set-input! g 1 0)
    (is (= 0 (gate-compute-output g)))))

(test not-gate
  (let ((g (make-not-gate)))
    (gate-set-input! g 0 0)
    (is (= 1 (gate-compute-output g)))
    (gate-set-input! g 0 1)
    (is (= 0 (gate-compute-output g)))))
