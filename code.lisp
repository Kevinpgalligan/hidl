(defun make-schedule (capacity)
  (make-instance 'schedule
                 ;; May need to use a different data structure to contain
                 ;; the events at each timeslot, don't want to have to constantly
                 ;; create & destroy cons cells all the time.
                 :timetable (make-array capacity :element-type 'list :initial-element nil)
                 :capacity capacity))

(defclass schedule ()
  ((timetable
    :initarg :timetable
    :reader timetable)
   (index
    :initform 0
    :accessor index)
   (capacity
    :initarg :capacity
    :reader capacity)))

(defun add-event (schedule event time-from-now)
  (let ((add-index (+ (index schedule) time-from-now)))
    (when (>= add-index (capacity schedule))
      (setf add-index (mod add-index (capacity schedule)))
      (when (>= add-index (index schedule))
        (error "Tried to add an event too far in the future.")))
    (push event (aref (timetable schedule) add-index))))

(defun advance-schedule (schedule)
  (setf (aref (timetable schedule) (index schedule)) nil)
  (incf (index schedule))
  (when (>= (index schedule) (capacity schedule))
    (setf (index schedule) 0)))

(defstruct node prev next value)

;; A stack that doesn't free memory once it has been allocated, even
;; if the stack is emptied.
(defclass stack ()
  ((bottom-node
    :initarg :bottom-node
    :accessor bottom-node)
   (top-node
    :initarg :top-node
    :accessor top-node)))

(defun make-stack ()
  (make-instance 'stack :bottom-node nil :top-node nil))

(defun stack-push (s value)
  (let ((next-node (or (and (top-node s)
                            (node-next (top-node s)))
                       (make-node :prev (top-node s)
                                  :next nil
                                  :value value))))
    (when (not (bottom-node s))
      (setf (bottom-node s) next-node))
    (when (top-node s)
      (setf (slot-value (top-node s) 'next) next-node))
    (setf (top-node s) next-node)))

;; Thoughts:
;;   1. Finish implementing the memory-saving stack.
;;   2. Refactor schedule to use stack data structure at each time slot.
;;   3. Create a simulation: at each cycle, execute all the events at the appropriate
;;      time slot and create random future events. Should be able to control the
;;      frequency, i.e. how many cycles per minute. Will need to refamiliarise myself
;;      with the time step thingy used in games. If there is no frequency limit, then
;;      run as fast as possible.
;;   4. Maybe make a simple circuit simulator using this framework, but obviously I
;;      won't know how to make anything complex until I've gone through the Code book again.
