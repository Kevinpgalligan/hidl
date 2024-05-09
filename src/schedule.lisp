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

(defun schedule-clear-current-period! (schedule)
  (loop while (not (stack-empty-p (schedule-current-period schedule)))
        do (stack-pop! (schedule-current-period schedule))))

(defun schedule-advance! (schedule)
  (schedule-clear-current-period! schedule)
  (setf (index schedule) (mod (1+ (index schedule)) (size schedule))))

(defun schedule-current-period (schedule)
  (aref (timetable schedule) (index schedule)))

(defmethod print-object ((obj schedule) out)
  (format out "~a" (timetable obj)))
