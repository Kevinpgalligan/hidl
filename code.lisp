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

(defclass lgate ()
  ((inputs :initarg :inputs)
   (output :initarg :output)
   (observers :initarg :observers)
   (properties :initarg :type)))

(defun lgate-inputs (lgate) (slot-value lgate 'inputs))
(defun lgate-output (lgate) (slot-value lgate 'output))
(defun lgate-observers (lgate) (slot-value lgate 'observers))
(defun lgate-type (lgate) (slot-value (slot-value lgate 'properties) 'name))
(defun lgate-compute-fn (lgate) (slot-value (slot-value lgate 'properties) 'compute))
(defun lgate-delay (lgate) (slot-value (slot-value lgate 'properties) 'delay))
(defun lgate-add-observer! (lgate observer)
  (push (slot-value lgate 'observers) observer))

(defparameter *lgate-properties* (make-hash-table))

;; Should be treated as a singleton that is shared between all instances
;; of a particular type of lgate.
(defclass lgate-properties ()
  ((name :initarg :name)
   (compute :initarg :compute)
   (delay :initarg :delay)))

(defun register-lgate (type function propagation-delay)
  `(setf (gethash ,type *lgate-properties*)
         (make-instance 'lgate-properties :name ,type :compute ,function :delay ,propagation-delay)))

;; Create map from name->function, to be used in LGATE-UPDATE-STATE!.
;; An lgate has some number of inputs and a single output.
;; The function takes all of the inputs (maybe an array?) and computes
;; the output (0 or 1).
;; Keep running until there are no more events in the schedule (i.e. circuit
;; has reached stable state).
;; A nice metric: how many events per cycle are being executed.
;; Another cool idea: calculate the 'breath' of the circuit graph, what's the
;; longest number of cycles it could take a signal to propagate from 1 end to another?
;; Nodes are gates, edges are inputs/outputs, weights on edges are the prop delay.
(defmacro def-lgate (name function propagation-delay)
  `(register-lgate ,name ,function ,propagation-delay))

(defun make-inputs (size)
  (make-array size :initial-element 0 :element-type 'integer))

(defun inputs-size (inputs)
  (array-dimension inputs 0))

(defun inputs-get (inputs i)
  (if (zerop (aref inputs i))
      0
      1))

(defun inputs-register-change! (inputs i new-value)
  (declare (bit new-value))
  (if (zerop new-value)
      (decf (aref inputs i))
      (incf (aref inputs i))))

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

;;;; Gate definitions! They're the building blocks of circuits.
;;;; I may or may not implement NAND in terms of other gates.
(def-lgate and compute-lgate-and 4)
(def-lgate or compute-lgate-or 4)
(def-lgate xor compute-lgate-xor 5)
;; This one just passes on whatever value is handed to it.
;; Careful with propagation-delay=0, it could result in an
;; infinite loop if 2 const lgates are connected together.
;; Maybe the delay should be 1?
(def-lgate const compute-lgate-const 0)

(defun propagate-to-lgate! (lgate changed-input-index schedule)
  (inputs-register-change! (lgate-inputs lgate) changed-input-index)
  ;; 1. compute the new value using its compute function
  ;; 2. add an event to set the new output value of this gate (just reference
  ;;    to gate + new output value)
  ;; 3. add events for each observer; depends on observer type, one type
  ;;    would be lgate+input index, another type would be arbitrary function dispatch.
  ;; Event instances should be drawn from a pool so that we don't
  ;; create them gratuitously. They should contain the new output value as well
  ;; as any data necessary for the execution of the event.
  ;; Design question for later: it's possible that
  ;; multiple inputs of a gate will be updated in the same cycle, so how we should
  ;; execute each time period is to go through all the events and set new input values,
  ;; then go back through all the affected lgates and propagate them.
  ;; In that case... the INPUTS-REGISTER-CHANGE! call should not take place in this function.
  ;; Optimisation: 2 wires connected to an lgate input, 1 turns on and 1 turns off. End result
  ;; is that the lgate input hasn't changed. So in this case we don't need to propagate.
  ;; After that's done, need an outer loop that executes events in the schedule, propagates
  ;; changes, and so on. And the only other thing is a way to wire the gates together. Then
  ;; I can test it.
  ;; Execution should take place within the context of a circuit; when executing a circuit,
  ;; I should check the gate with the max delay and that determines the schedule size. Really, there
  ;; will be a very small schedule with lots and lots of events, as the circuit scales up.
  )

