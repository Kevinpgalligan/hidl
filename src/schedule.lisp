(in-package hidden-language)

(defstruct node prev next value)

;; Need to override default print behaviour because otherwise cyclic
;; references will kill us at the REPL.
(defmethod print-object ((obj node) out)
  (with-slots (node-value) obj
    (print-unreadable-object (obj out :type t)
      (format out "value = ~a" (node-value obj)))))

;; A stack that doesn't free memory once it has been allocated, even
;; if the stack is emptied. The idea is to avoid garbage collection.
(defclass stack ()
  ((bottom-node
    :initarg :bottom-node
    :accessor bottom-node)
   (top-node
    :initarg :top-node
    :accessor top-node)
   ;; This stores a reference to the base node which is created
   ;; on initialisation of the stack, even before any elements
   ;; have been added.
   (base
    :initarg :base
    :accessor base)))

(defun make-stack ()
  (make-instance 'stack
                 :bottom-node nil
                 :top-node nil
                 :base (make-node :prev nil :next nil :value nil)))

(defun stack-push! (s value)
  (let ((next-node (or (and (null (bottom-node s))
                            (base s))
                       (and (top-node s)
                            (node-next (top-node s)))
                       (make-node :prev (top-node s)
                                  :next nil
                                  :value value))))
    (if (null (bottom-node s))
        (setf (bottom-node s) next-node
              (top-node s) next-node
              (node-value next-node) value)
        (progn
          (when (node-next (top-node s))
            ;; There was an existing node that we can reuse to store the value!
            (setf (node-value next-node) value))
          (setf (node-next (top-node s)) next-node)
          (setf (top-node s) next-node)))))

(defun stack-empty-p (s)
  (null (bottom-node s)))

(defmethod print-object ((obj stack) out)
  (with-slots (value) obj
    (print-unreadable-object (obj out :type t)
      (format out "~{~a ~}" (loop for node = (bottom-node obj) then (node-next node)
                                  while node
                                  collect (node-value node)
                                  ;; This ensures that we don't try to print beyond the top.
                                  while (not (eq node (top-node obj))))))))

(defun stack-pop! (s)
  (when (null (top-node s))
    (error "Tried to pop from empty stack."))
  (let ((value (node-value (top-node s)))
        (prev (node-prev (top-node s))))
    (if (null prev)
        (setf (bottom-node s) nil
              (top-node s) nil)
        (progn
          ;; Overwrite the value of this node so we're not hanging on to
          ;; a reference that's no longer needed.
          (setf (node-value (top-node s)) nil)
          (setf (top-node s) prev)))
    value))

(defun stack-capacity (s)
  (loop for node = (base s) then (node-next node)
        while node
        sum 1))

(defun stack-size (s)
  (loop for node = (bottom-node s) then (node-next node)
        while node
        sum 1
        while (not (eq node (top-node s)))))

(defclass schedule ()
  ((timetable
    :initarg :timetable
    :reader timetable)
   (index
    :initarg :index
    :accessor index)
   (size
    :initarg :size
    :reader size)))

(defun make-schedule (size)
  (make-instance 'schedule
                 :timetable (make-array size
                                        :element-type 'stack
                                        :initial-contents
                                        (loop repeat size
                                              collect (make-stack)))
                 :index 0
                 :size size))

(defun schedule-add-event! (schedule event time-from-now)
  (when (>= time-from-now (size schedule))
    (error "Tried to add an event too far in the future."))
  (stack-push! (aref (timetable schedule) (mod (+ (index schedule) time-from-now)
                                              (size schedule)))
              event))

(defun schedule-empty-current-period! (schedule)
  (loop while (not (stack-empty-p (schedule-current-period schedule)))
        do (stack-pop! (schedule-current-period schedule))))

(defun schedule-advance! (schedule)
  (schedule-empty-current-period! schedule)
  (setf (index schedule) (mod (1+ (index schedule)) (size schedule))))

(defun schedule-current-period (schedule)
  (aref (timetable schedule) (index schedule)))

(defmethod print-object ((obj schedule) out)
  (format out "~a" (timetable obj)))
