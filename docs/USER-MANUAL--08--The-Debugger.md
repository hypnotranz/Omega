# OmegaLLM User Manual

[← Table of Contents](USER-MANUAL--00--Table-Of-Contents.md)

---

## Chapter 8: The Debugger
### Time-Travel Debugging

OmegaLLM includes a step debugger with time-travel capabilities.

### Loading an Expression

```lisp
Ω> :debug (+ (* 2 3) (* 4 5))
Debug session started. Use :step to begin stepping.

─── Step 0 ───
Control: Expr: Begin(1 exprs)
Stack depth: 0
```

### Stepping Through Execution

```lisp
Ω> :step 5
Step 5: Expr: App(...)
Stack depth: 2

Ω> :step 10
Step 15: Value: 26
DONE at step 15: 26
```

### Viewing the Trace

```lisp
Ω> :trace
Trace (16 steps recorded):
  [0] Expr: Begin(1 exprs) | stack=0
  [1] Expr: App(...) | stack=1
  [2] Expr: App(...) | stack=2
  ...
  [15] Value: 26 | stack=0 <-- current
```

### Time Travel

Jump to any previous state:

```lisp
Ω> :goto 5
Jumped to step 5.

─── Step 5 ───
Control: Expr: App(...)
Stack depth: 2
```

### Breakpoints

Set breakpoints on steps, expression types, or effects:

```lisp
Ω> :break step 10
Breakpoint 1 added: step = 10

Ω> :break expr If
Breakpoint 2 added: expr = If

Ω> :break effect infer.op
Breakpoint 3 added: effect = infer.op

Ω> :breaks
Breakpoints:
  1: step = 10 [enabled]
  2: expr = If [enabled]
  3: effect = infer.op [enabled]
```