# OmegaLLM User Manual

[← Table of Contents](USER-MANUAL--00--Table-Of-Contents.md)

---

## Chapter 1: Getting Started
### Running the REPL

The simplest way to interact with OmegaLLM is through its REPL (Read-Eval-Print Loop):

```bash
cd OmegaLLM
npx tsx bin/omega-repl.ts
```

You'll see:
```
═══════════════════════════════════════════════════════════
Ω REPL — Omega Lisp with Oracle Protocol & Debugger
═══════════════════════════════════════════════════════════
Type Lisp expressions to evaluate.
Commands: :help :debug :step :run :goto :trace :break :quit

Ω>
```

### Your First Expression

Try some basic Lisp:

```lisp
Ω> (+ 1 2 3)
=> 6

Ω> (define (square x) (* x x))
=> square

Ω> (square 5)
=> 25
```

### Sessions: Persistent State Across Calls

For scripting and automation, use sessions. A session preserves your definitions across separate invocations:

```bash
# Define a function
npx tsx bin/omega-repl.ts --session mywork --cmd "(define (double x) (* x 2))"
=> double

# Use it later (in a separate call)
npx tsx bin/omega-repl.ts --session mywork --cmd "(double 21)"
=> 42

# Check what's defined
npx tsx bin/omega-repl.ts --session mywork --cmd ":defs"
(define (double x) (* x 2))
```

Sessions are stored in `~/.omega-sessions/` and persist until you delete them.