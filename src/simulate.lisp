(in-package hidl)

;; This should be calculated...
(defparameter *max-delay* 9)

(defun simulate-circuit (circuit frequency max-propagation-delay)
  (let ((seconds-per-tick (/ 1 frequency))
        (ticks 0)
        (schedule (make-schedule *max-delay*))
        (event-pool (make-event-pool))
        (next-tick-time (get-float-time-seconds)))
    (loop do (wait-until next-tick-time)
          do (incf ticks)
          do (incf next-tick-time seconds-per-tick)
          do (execute-step schedule ticks))))

(defun wait-until (time)
  (sleep (max 0 (- time (get-float-time-seconds)))))

(defun get-float-time-seconds ()
  (float (/ (get-internal-real-time) internal-time-units-per-second)))

(defun execute-step (schedule ticks event-pool)
  (while (not (schedule-current-period-empty-p schedule))
         do (let ((event (schedule-remove-next-event! schedule)))
              (with-slots (gate new-value)
                  event
                (when (not (= new-value (gate-output gate)))
                  (setf (slot-value gate 'output) new-value)
                  (propagate-to-observers! gate schedule)))
              (release-event event-pool event)))
  (schedule-advance! schedule))

(defstruct event gate new-value)

(defun make-event-pool ()
  "Use this data structure to avoid garbage churn, event data
structures are reused rather than being allocated constantly."
  (make-stack))

(defun acquire-event (event-pool gate new-value)
  (if (stack-empty-p *event-pool*)
      (make-event :gate gate :new-value new-value)
      (let ((event (stack-pop! event-pool)))
        (setf (slot-value event 'gate) gate
              (slot-value event 'new-value) new-value)
        event)))

(defun release-event (event-pool event)
  (with-slots (gate new-value)
      event
    (setf gate nil
          new-value nil)
    (stack-push! event-pool event)))

(defun propagate-to-observers! (gate schedule)
  (loop for observer in (gate-observers gate)
        ;; For now, assume that all observers are of type OBSERVING-GATE.
        ;; This type of observer receives the new output value of one gate.
        ;; It updates one of the inputs of the receiving gate. If there's
        ;; a change in the input, it [[future Kevin: recomputes the output and
        ;; schedules that change in output for the future. Also need to consider
        ;; how to deduplicate]].
        ;; Later, we can generalise to other types of observers.
        do (with-accessors ((receiving-gate gate)
                            (input-index input-index))
               observer
             (let ((old-value (gate-ith-input receiving-gate input-index)))
               (inputs-register-change! (gate-inputs receiving-gate)
                                        input-index
                                        (gate-output gate))
               (let ((new-value (gate-ith-input receiving-gate input-index)))
                 (when (not (= old-value new-value))
                   (schedule-add-event!
                    schedule
                    (acquire-event event-pool receiving-gate (gate-compute-output receiving-gate))
                    (gate-delay receiving-gate))))))))
