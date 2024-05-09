(in-package hidl-test)

(def-suite schedule
  :in hidl)

(in-suite schedule)

(test stack
 (let ((s (hidl::make-stack)))
   (is-true (hidl::stack-empty-p s))
   (hidl::stack-push! s 1)
   (is-false (hidl::stack-empty-p s))
   (hidl::stack-push! s 2)
   (is (= 2 (hidl::stack-size s)))
   (is (= 2 (hidl::stack-pop! s)))
   (is (= 1 (hidl::stack-pop! s)))
   (is-true (hidl::stack-empty-p s))))

(test add-event-too-far
  (let ((sch (hidl::make-schedule 3)))
    (signals error
      (hidl::schedule-add-event! sch 1 3))))

(test add-some-events
  (let ((sch (hidl::make-schedule 4)))
    (hidl::schedule-add-event! sch 1 1)
    (hidl::schedule-add-event! sch 2 1)
    (hidl::schedule-add-event! sch 3 2)
    (is (hidl::stack= (hidl::make-stack) (hidl::schedule-current-period sch)))
    (hidl::schedule-advance! sch)
    (is (hidl::stack= (hidl::make-stack 1 2) (hidl::schedule-current-period sch)))
    (hidl::schedule-clear-current-period! sch)
    (is (hidl::stack= (hidl::make-stack) (hidl::schedule-current-period sch)))
    (hidl::schedule-advance! sch)
    (is (hidl::stack= (hidl::make-stack 3) (hidl::schedule-current-period sch)))
    (hidl::schedule-clear-current-period! sch)
    (is (hidl::stack= (hidl::make-stack) (hidl::schedule-current-period sch)))
    (hidl::schedule-advance! sch)
    (hidl::schedule-clear-current-period! sch)))

(test loops-around
  (let ((sch (hidl::make-schedule 3)))
    (loop repeat 3 do (hidl::schedule-advance! sch))
    (hidl::schedule-add-event! sch 1 2)
    (loop repeat 2 do (hidl::schedule-advance! sch))
    (is (hidl::stack= (hidl::make-stack 1) (hidl::schedule-current-period sch)))))

(test advancing-clears-period
  (let ((sch (hidl::make-schedule 3)))
    (hidl::schedule-add-event! sch 1 1)
    (loop repeat 4 do (hidl::schedule-advance! sch))
    (is (hidl::stack= (hidl::make-stack) (hidl::schedule-current-period sch)))))

