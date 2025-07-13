(in-package hidl)

(deftype stack () 'vector)

(defun make-stack (&rest elements)
  (make-array (length elements)
              :adjustable t
              :fill-pointer t
              :initial-contents elements))

(defun stack-push! (s value)
  (vector-push-extend value s))

(defun stack-empty-p (s)
  (zerop (fill-pointer s)))

(defun stack-pop! (s)
  (vector-pop s))

(defun stack-size (s)
  (fill-pointer s))

(defun stack= (s1 s2)
  (equalp s1 s2))
