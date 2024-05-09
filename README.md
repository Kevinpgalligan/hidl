### hidl
A computer emulation framework based on Code: The Hidden Language of Computer Hardware.

### TODO
- Add unit tests for existing code.
- Finish the schedule interface so that the half-adder test works (see 'test-session.txt').
- Last touches to simulator (add all gates to schedule or whatever at the beginning).
- Somehow hide interface of schedule & lgates better, so it's obvious how to use them.
- Design macro DSL for components and constructing circuits.

### Dumping brainstorming
A nice metric: how many events per cycle are being executed.

Another cool idea: calculate the 'breadth' of the circuit graph, what's the longest number of cycles it could take a signal to propagate from 1 end to another?

Nodes are gates, edges are inputs/outputs, weights on edges are the prop delay.

### Planning
Basic circuit to test: a single adder (2 lgates for setting input, print the output).

From circuit to CPU:
   * ALU.
   * Memory cells used to store 'instructions',
   * multiplexer thing maps instruction in memory cell to appropriate circuitry for that instruction.
   * I'm unsure how instruction arguments are read; instruction pointer is stored somewhere, add 1 to it for each expected argument.
   * Assembler should be pretty easy? Base the instruction set on 6502, perhaps. There are also instructions to read/write to/from the memory cells, of course.
   * Assembler needs to be able to define data.

FINALLY: BASIC in assembly language.

I can't imagine where to start. First define what dialect I'm implementing, prob the earliest BASIC implemented by Microsoft.

Tokens stored as string data, implement string comparison, store the current 'state' in a variable, how tokens are processed depends on state. Oh, there can be nested states? Maybe use the stack for that.

Altair BASIC functions & statements:
  * LIST, NEW, PRINT, INPUT, IF...THEN, FOR...NEXT, SQR, RND, SIN, LET, USR, DATA, READ, REM, CLEAR, STOP, TAB, RESTORE, ABS, END, INT, RETURN, STEP, GOTO, and GOSUB. 

...as per: https://en.wikipedia.org/wiki/Microsoft_BASIC

Also: https://en.wikipedia.org/wiki/Altair_BASIC

Assuming terminal graphics, it should be easier. Each group of memory cells defines a character, which I somehow render to a fixed-size rectangle on the screen.
    
    * PRINT -> if string, write directly to memory
             if number, convert to character & then write to memory
             if variable, evaluate it
    * IF...THEN -> on processing IF, enter IF state. In IF state, evaluate an
                 expression and leave its result in some register; then we expect
                 THEN, at which point we check the result. If result is true, execute
                 code up to END and then pop out of the IF state. If not, skip until END
                 and pop out of the IF state (careful to avoid the END from nested IF...ENDs).
    * FOR -> hmmm, maybe need some method of storing the condition so that it can be rechecked.
    * RND, SQR, SIN -> function execution state, save function pointer somewhere, evaluate expression
                     and leave its result in a certain register, then apply numerical function using
                     pointer. Maybe 'evaluate expression' should be its own state.
    * LET -> I think this is for assigning variables, I suppose we would have a table of variables somewhere
           and their values.


### Dumping this test code here
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
