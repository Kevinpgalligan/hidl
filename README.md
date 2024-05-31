### hidl
A computer emulation framework based on Code: The Hidden Language of Computer Hardware.

### TODO
- Maybe refactor schedule to use less verbose names.
- Review simulate.lisp and add unit tests. One possible addition: add all gates to the schedule at the start.
- Add half-adder test for simulator.
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