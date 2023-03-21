;;;; THOUGHTS
;;;; I have the data structures in place for a scheduling
;;;; system. I've also implemented a sample piece of code
;;;; that uses the schedule to run a simulation, see RUN-SIMULATION.

;;;; I should make the interface of the schedule a bit more opaque;
;;;; instead of fetching the stack used by the current slot of the
;;;; schedule and operating on that, the schedule should offer an
;;;; interface POP and EMPTY-P for the current time period.
;;;; On efficiency: may be better to remove generic functions, unsure
;;;; if that will be a bottleneck.
;;;; On the loop in RUN-SIMULATION: I might need to review the articles
;;;; I read on how to construct a game loop, mine seems to do the job
;;;; but I'm unsure how it will function when there are many many ticks
;;;; per second or when the work done at each tick takes a long time.

;;;; Beyond that, the next step will be to design the actual circuit
;;;; simulator. The 'events' in the schedule can be a reference to
;;;; a component of the circuit and its new output, maybe? And this
;;;; must be propagated to all the other components that are connected.
;;;; Need to refer to SICP for the design.

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
    :initarg 0
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

;;;; TESTING FOR THE SCHEDULE DATA STRUCTURE, can be used
;;;; as the basis for unit tests.
;; CL-USER> (defparameter *sched* (make-schedule 3))
;; *SCHED*
;; CL-USER> *sched*
;; #(#<STACK > #<STACK > #<STACK >)
;; CL-USER> (schedule-add-event! *sched* 1 0)
;; 1
;; CL-USER> *sched*
;; #(#<STACK 1 > #<STACK > #<STACK >)
;; CL-USER> (schedule-add-event! *sched* 1 2)
;; 1
;; CL-USER> *sched*
;; #(#<STACK 1 > #<STACK > #<STACK 1 >)
;; CL-USER> (schedule-add-event! *sched* 2 1)
;; 2
;; CL-USER> *sched*
;; #(#<STACK 1 > #<STACK 2 > #<STACK 1 >)
;; CL-USER> (schedule-add-event! *sched* 3 0)
;; #<NODE value = 3>
;; CL-USER> *sched*
;; #(#<STACK 1 3 > #<STACK 2 > #<STACK 1 >)
;; CL-USER> (schedule-empty-current-period! *sched*)
;; NIL
;; CL-USER> *sched*
;; #(#<STACK > #<STACK 2 > #<STACK 1 >)
;; CL-USER> (schedule-current-period *sched*)
;; #<STACK >
;; CL-USER> (schedule-advance! *sched*)
;; 1
;; CL-USER> *sched*
;; #(#<STACK > #<STACK 2 > #<STACK 1 >)
;; CL-USER> (schedule-add-event! *sched* 4 2)
;; 4
;; CL-USER> *sched*
;; #(#<STACK 4 > #<STACK 2 > #<STACK 1 >)
;; CL-USER> (schedule-add-event! *sched* 5 2)
;; #<NODE value = 5>
;; CL-USER> *sched*
;; #(#<STACK 4 5 > #<STACK 2 > #<STACK 1 >)
;; CL-USER> (schedule-advance! *sched*)
;; 2
;; CL-USER> *sched*
;; #(#<STACK 4 5 > #<STACK > #<STACK 1 >)
;; CL-USER> (index *sched*)
;; 2
;; CL-USER> (schedule-advance! *sched*)
;; 0
;; CL-USER> *sched*
;; #(#<STACK 4 5 > #<STACK > #<STACK >)
;; CL-USER> (schedule-current-period *sched*)
;; #<STACK 4 5 >

(defun run-simulation (schedule-size frequency)
  (let ((seconds-per-tick (/ 1 frequency))
        (ticks 0)
        (schedule (make-schedule schedule-size))
        (next-tick-time (get-float-time-seconds)))
    (loop do (wait-until next-tick-time)
          do (incf ticks)
          do (execute-step schedule ticks)
          do (incf next-tick-time seconds-per-tick))))

(defun execute-step (schedule ticks)
  ;; Do all the 'tasks' at the current time period in the
  ;; schedule, add some random tasks in the future, advance
  ;; the schedule, and print it out.
  (format t "Schedule is at: ~a~%" (index schedule))
  (let ((current-period (schedule-current-period schedule)))
    (loop while (not (stack-empty-p current-period))
          do (let ((task (stack-pop current-period)))
               (format t "   Executing task ~a~%" task))))
  (loop repeat 3
        do (schedule-add-event! schedule
                                ticks
                                (1+ (random (1- (size schedule))))))
  (schedule-advance! schedule)
  (format t "The new schedule: ~a~%" schedule))

(defun wait-until (time)
  (sleep (max 0 (- time (get-float-time-seconds)))))

(defun get-float-time-seconds ()
  (float (/ (get-internal-real-time) internal-time-units-per-second)))
