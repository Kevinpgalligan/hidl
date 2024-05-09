;; Design dilemma: need a way to label inputs and
;; to allow a variable number of inputs
;; I should completely define the API and write
;; sample components using it before I implement
;; the actual macros.
;; Change name to 'defcircuit'? Sounds better. Circuits and subcircuits.
(defcomponent half-adder (arguments-for-half-adder-go-here)
  ;; This is a form that defines the labels for the inputs,
  ;; if any, and the number of inputs. Can make use of the arguments.
  ;; Need to consider what this will look like.
  (declare-inputs (list a num-bits)) ;; labels can look like (a 0), (a 1), ..., (a (1- num-bits))
  (declare-inputs (list b num-bits))
  ;; and this is the code that is executed to create an
  ;; instance of the component.
  ;; with-components automatically stores the resulting sub-components
  ;; in the component instance.
  ;; Hmmm, need to allow loops when creating components.
  (with-components ((x (gate and))
                    (y (gate or)))
    ;; WIRE maps from a transmitter to a receiver.
    ;; if the transmitter is one of the inputs for the component,
    ;; then we store the receiver in a list of all receivers for
    ;; that input. Then, when we wire something to the input
    ;; labelled 'a', we recursively wire it to all the sub-components
    ;; associated with 'a', until we get to the gate level.
    ;; If that makes sense.
    ;; If the transmitter is a gate, the receiver must be added
    ;; as an observer.
    (wire a (x 0))
    (wire x (y 0))
    (wire a (y 1))
    (loop repeat N ; N could be the number of inputs, for example
          for i = 0 then (1+ i)
          ;; The inputs are (a 0), (a 1), ...
          ;; Hmm, this is quite ugly. Or is it?
          ;; What if inputs are labelled 0 - N and we
          ;; want to combine them in some way, how do we
          ;; reference them? loop for i = 0 to N, but
          ;; (wire i (+ i 1)) would not work.
          ;; I think it's best to evaluate the arguments of
          ;; WIRE and treat the result as a label. Even if
          ;; it looks slightly less pleasant.
          do (wire (a i) ((half-adder 0) 0))))
  ;; Possible alternative syntax for creating subcomponents
  (loop for i = 0 then (1+ i)
        while (< i N)
        ;; First arg is the subcomponent label, e.g. (ha 0)
        ;; Unsure how to define it so that ha is treated as
        ;; a symbol while i is evaluated.
        ;; Maybe: if it's a list, the first item is a symbol
        ;; but the remaining things are evaluated. This could also
        ;; work for the WIRE macro. But then wiring (a i) to the j-th
        ;; input of the i-th half-adder could get ugly (see above),
        ;;    (wire (a i) ((ha i) j))
        ;; If it's a symbol then it's not evaluated, e.g.
        ;;    (wire a (x 0))
        ;; Or is there a better way to express this? Not to mention,
        ;; we may not want to evaluate i! What if there is a half adder
        ;; with the label 'i' and we want to use its j-th input? Maybe
        ;; that could be
        ;;    (wire (a i) ((ha 'i) j))
        ;; defcomponent defines a function to create the component.
        ;; Within that function there is always a local binding for a
        ;; global called *current-component*. SUBCOMPONENT adds the resulting
        ;; subcomponent to the list of subcomponents for *current-component*.
        do (subcomponent (ha i) (make-component 'half-adder))))
