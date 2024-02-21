(in-package hidden-language)

(defun simulate-circuit (circuit frequency max-propagation-delay)
  ;; TODO: calculate max prop delay from circuit
  (let ((seconds-per-tick (/ 1 frequency))
        (ticks 0)
        (schedule (make-schedule (max-possible-propagation-delay)))
        (event-pool (make-event-pool))
        (next-tick-time (get-float-time-seconds)))
    (loop do (wait-until next-tick-time)
          do (incf ticks)
          do (execute-step schedule ticks)
          do (incf next-tick-time seconds-per-tick))))

(defun max-possible-propagation-delay ()
  ;; TODO not sure how to do this
  )

(defun wait-until (time)
  (sleep (max 0 (- time (get-float-time-seconds)))))

(defun get-float-time-seconds ()
  (float (/ (get-internal-real-time) internal-time-units-per-second)))

(defun execute-step (schedule ticks event-pool)
  (while (not (schedule-current-period-empty-p schedule))
         do (let ((event (schedule-current-period-remove-next-event! schedule)))
              (with-slots (lgate new-value)
                  event
                (when (not (= new-value (lgate-output lgate)))
                  (setf (slot-value lgate 'output) new-value)
                  (propagate-to-observers! lgate schedule)))
              (release-event event-pool event)))
  (schedule-advance! schedule))

(defstruct event lgate new-value)

(defun make-event-pool ()
  "Use this data structure to avoid garbage churn, event data
structures are reused rather than being allocated constantly."
  (make-stack))

(defun acquire-event (event-pool lgate new-value)
  (if (stack-empty-p *event-pool*)
      (make-event :lgate lgate :new-value new-value)
      (let ((event (stack-pop! event-pool)))
        (setf (slot-value event 'lgate) lgate
              (slot-value event 'new-value) new-value)
        event)))

(defun release-event (event-pool event)
  ;; Shouldn't we add it back to the event pool here?
  (with-slots (lgate new-value)
      event
    (setf lgate nil
          new-value nil)))

(defun propagate-to-observers! (lgate schedule)
  (loop for observer in (lgate-observers lgate)
        ;; For now, assume that all observers are of type OBSERVING-LGATE.
        ;; This type of observer receives the new output value of one lgate.
        ;; It updates one of the inputs of the receiving lgate. If there's
        ;; a change in the input, it 
        ;; Later, we can generalise this.
        do (with-accessors ((receiving-lgate lgate)
                            (input-index input-index))
               observer
             (let ((old-value (lgate-ith-input receiving-lgate input-index)))
               (inputs-register-change! (lgate-inputs receiving-lgate)
                                        input-index
                                        (lgate-output lgate))
               (let ((new-value (lgate-ith-input receiving-lgate input-index)))
                 (when (not (= old-value new-value))
                   (schedule-add-event!
                    schedule
                    (acquire-event event-pool receiving-lgate (lgate-compute-output receiving-lgate))
                    (lgate-delay receiving-lgate))))))))
