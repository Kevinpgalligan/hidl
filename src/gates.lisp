(in-package hidden-language)

(defparameter *lgate-properties* (make-hash-table))

(defclass lgate ()
  ((inputs :initarg :inputs)
   (output :initarg :output)
   (observers :initarg :observers)
   (properties :initarg :properties)))

(defun make-lgate (type num-inputs)
  (make-instance 'lgate
                 :inputs (make-inputs num-inputs)
                 :output 0
                 :observers nil
                 :properties (or (gethash type *lgate-properties*)
                                 (error "Unknown logic gate type"))))

(defun lgate-inputs (lgate) (slot-value lgate 'inputs))
(defun lgate-output (lgate) (slot-value lgate 'output))
(defun lgate-observers (lgate) (slot-value lgate 'observers))
(defun lgate-type (lgate) (slot-value (slot-value lgate 'properties) 'name))
(defun lgate-compute-fn (lgate) (slot-value (slot-value lgate 'properties) 'compute))
(defun lgate-delay (lgate) (slot-value (slot-value lgate 'properties) 'delay))

(defun lgate-add-observer! (lgate observer)
  (setf (slot-value lgate 'observers)
        (cons observer (slot-value lgate 'observers))))

(defun lgate-ith-input (lgate i)
  (inputs-get (lgate-inputs lgate) i))

(defun lgate-set-input! (lgate i value)
  (setf (aref (lgate-inputs lgate) i) value))

(defun lgate-compute-output (lgate)
  (funcall (lgate-compute-fn lgate) (lgate-inputs lgate)))

;; Should be treated as a singleton that is shared between all instances
;; of a particular type of lgate.
(defclass lgate-properties ()
  ((name :initarg :name)
   (compute :initarg :compute)
   (delay :initarg :delay)))

(defun register-lgate (type function propagation-delay)
  (setf (gethash type *lgate-properties*)
        (make-instance 'lgate-properties :name type :compute function :delay propagation-delay)))

(defmacro def-lgate (name function propagation-delay
                     &key default-inputs fixed-inputs)
  (let ((args
          (cond
            (fixed-inputs (list))
            (default-inputs `(&key (n ,default-inputs)))
            (t (list 'n)))))
    `(progn
       (defun ,(alexandria:symbolicate 'make- name '-gate)
           (,@args)
         (make-lgate ',name
                     ,@(cond
                         (fixed-inputs (list fixed-inputs))
                         (default-inputs (list 'n))
                         (t args))))
       (register-lgate ',name ,function ,propagation-delay))))

(defun make-inputs (size)
  (make-array size :initial-element 0 :element-type 'bit))

(defun inputs-size (inputs)
  (length inputs))

(defun inputs-get (inputs i)
  (aref inputs i))

(defun inputs-set! (inputs i new-value)
  "This overrides the value of an input, even if other wires are sending
power to it. So use with caution, preferably only for gates w/ a single input."
  (declare (bit new-value))
  (setf (aref inputs i) new-value))

(defmacro iter-inputs ((inputs ref) &body body)
  (alexandria:once-only (inputs)
    `(loop for i from 0 upto (1- (inputs-size ,inputs))
           for ,ref = (inputs-get ,inputs i)
           do ,@body)))

(defun compute-lgate-or (inputs)
  (or (iter-inputs (inputs x)
        (when (= x 1)
          (return 1)))
      0))

(defun compute-lgate-and (inputs)
  (or (iter-inputs (inputs x)
        (when (= x 0)
          (return 0)))
      1))

(defun compute-lgate-xor (inputs)
  (let ((result 0))
    (iter-inputs (inputs x)
      (setf result (logxor result x)))
    result))

(defun compute-lgate-const (inputs)
  (iter-inputs (inputs x)
    (return x)))

(defun compute-lgate-not (inputs)
  ;; There should only be 1 input.
  (logxor 1 (inputs-get inputs 0)))

;;;; Gate definitions! They're the building blocks of circuits.
;;;; I may or may not implement NAND in terms of other gates.
(def-lgate and #'compute-lgate-and 4 :default-inputs 2)
(def-lgate or #'compute-lgate-or 4 :default-inputs 2)
(def-lgate xor #'compute-lgate-xor 8 :default-inputs 2)
(def-lgate not #'compute-lgate-not 2 :fixed-inputs 1)

(defclass observing-lgate ()
  ((lgate
    :initarg :lgate
    :reader lgate)
   (input-index
    :initarg :input-index
    :reader input-index)))

(defun make-observing-lgate (lgate input-index)
  (make-instance 'observing-lgate :lgate lgate :input-index input-index))
