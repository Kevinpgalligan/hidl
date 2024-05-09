(in-package hidl)

(defparameter *gate-properties* (make-hash-table))

(defclass gate ()
  ((inputs :initarg :inputs)
   (output :initarg :output)
   (observers :initarg :observers)
   (properties :initarg :properties)))

(defun make-gate (type num-inputs)
  (make-instance 'gate
                 :inputs (make-inputs num-inputs)
                 :output 0
                 :observers nil
                 :properties (or (gethash type *gate-properties*)
                                 (error "Unknown logic gate type"))))

(defun gate-inputs (gate) (slot-value gate 'inputs))
(defun gate-output (gate) (slot-value gate 'output))
(defun gate-observers (gate) (slot-value gate 'observers))
(defun gate-type (gate) (slot-value (slot-value gate 'properties) 'name))
(defun gate-compute-fn (gate) (slot-value (slot-value gate 'properties) 'compute))
(defun gate-delay (gate) (slot-value (slot-value gate 'properties) 'delay))

(defun gate-add-observer! (gate observer)
  (setf (slot-value gate 'observers)
        (cons observer (slot-value gate 'observers))))

(defun gate-ith-input (gate i)
  (inputs-get (gate-inputs gate) i))

(defun gate-set-input! (gate i value)
  (setf (aref (gate-inputs gate) i) value))

(defun gate-compute-output (gate)
  (funcall (gate-compute-fn gate) (gate-inputs gate)))

;; Should be treated as a singleton that is shared between all instances
;; of a particular type of gate.
(defclass gate-properties ()
  ((name :initarg :name)
   (compute :initarg :compute)
   (delay :initarg :delay)))

(defun register-gate (type function propagation-delay)
  (setf (gethash type *gate-properties*)
        (make-instance 'gate-properties :name type :compute function :delay propagation-delay)))

(defmacro def-gate (name function propagation-delay
                     &key default-inputs fixed-inputs)
  (let ((args
          (cond
            (fixed-inputs (list))
            (default-inputs `(&key (n ,default-inputs)))
            (t (list 'n)))))
    `(progn
       (defun ,(alexandria:symbolicate 'make- name '-gate)
           (,@args)
         (make-gate ',name
                     ,@(cond
                         (fixed-inputs (list fixed-inputs))
                         (default-inputs (list 'n))
                         (t args))))
       (register-gate ',name ,function ,propagation-delay))))

(defun make-inputs (size)
  (make-array size :initial-element 0 :element-type 'bit))

(defun inputs-size (inputs)
  (length inputs))

(defun inputs-get (inputs i)
  (aref inputs i))

(defun inputs-set! (inputs i new-value)
  (declare (bit new-value))
  (setf (aref inputs i) new-value))

(defmacro iter-inputs ((inputs ref) &body body)
  (alexandria:once-only (inputs)
    `(loop for i from 0 upto (1- (inputs-size ,inputs))
           for ,ref = (inputs-get ,inputs i)
           do ,@body)))

(defun compute-gate-or (inputs)
  (or (iter-inputs (inputs x)
        (when (= x 1)
          (return 1)))
      0))

(defun compute-gate-and (inputs)
  (or (iter-inputs (inputs x)
        (when (= x 0)
          (return 0)))
      1))

(defun compute-gate-xor (inputs)
  (let ((result 0))
    (iter-inputs (inputs x)
      (setf result (logxor result x)))
    result))

(defun compute-gate-const (inputs)
  (iter-inputs (inputs x)
    (return x)))

(defun compute-gate-not (inputs)
  ;; There should only be 1 input.
  (logxor 1 (inputs-get inputs 0)))

;;;; Gate definitions! They're the building blocks of circuits.
;;;; I may or may not implement NAND in terms of other gates.
(def-gate and #'compute-gate-and 4 :default-inputs 2)
(def-gate or #'compute-gate-or 4 :default-inputs 2)
(def-gate xor #'compute-gate-xor 8 :default-inputs 2)
(def-gate not #'compute-gate-not 2 :fixed-inputs 1)

(defclass observing-gate ()
  ((gate
    :initarg :gate
    :reader gate)
   (input-index
    :initarg :input-index
    :reader input-index)))

(defun make-observing-gate (gate input-index)
  (make-instance 'observing-gate :gate gate :input-index input-index))
