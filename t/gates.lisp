(in-package hidden-language-test)

(def-suite gates
  :in hidden-language)

(in-suite gates)

(test and-gate
  (let ((g (make-and-gate)))
    (lgate-set-input! g 0 0)
    (lgate-set-input! g 1 1)
    (is (= 0 (lgate-compute-output g)))
    (lgate-set-input! g 0 1)
    (lgate-set-input! g 1 1)
    (is (= 1 (lgate-compute-output g)))
    (lgate-set-input! g 0 0)
    (lgate-set-input! g 1 0)
    (is (= 0 (lgate-compute-output g)))))

(test or-gate
  (let ((g (make-or-gate)))
    (lgate-set-input! g 0 0)
    (lgate-set-input! g 1 1)
    (is (= 1 (lgate-compute-output g)))
    (lgate-set-input! g 0 1)
    (lgate-set-input! g 1 1)
    (is (= 1 (lgate-compute-output g)))
    (lgate-set-input! g 0 0)
    (lgate-set-input! g 1 0)
    (is (= 0 (lgate-compute-output g)))))

(test xor-gate
  (let ((g (make-xor-gate)))
    (lgate-set-input! g 0 0)
    (lgate-set-input! g 1 1)
    (is (= 1 (lgate-compute-output g)))
    (lgate-set-input! g 0 1)
    (lgate-set-input! g 1 0)
    (is (= 1 (lgate-compute-output g)))
    (lgate-set-input! g 0 1)
    (lgate-set-input! g 1 1)
    (is (= 0 (lgate-compute-output g)))
    (lgate-set-input! g 0 0)
    (lgate-set-input! g 1 0)
    (is (= 0 (lgate-compute-output g)))))

(test not-gate
  (let ((g (make-not-gate)))
    (lgate-set-input! g 0 0)
    (is (= 1 (lgate-compute-output g)))
    (lgate-set-input! g 0 1)
    (is (= 0 (lgate-compute-output g)))))
