# OmegaLLM

**A programming language for LLM calls.**

Write programs that include prompts and process results. LLM calls are just expressions in the language.

```lisp
; Call an LLM
(effect infer.op "What is the capital of France?")
=> "Paris"

; Process the result
(define answer (effect infer.op "What is 2+2?"))
(if (equal? answer "4") "correct" "wrong")
=> "correct"

; Loop over LLM calls
(map (lambda (x) (effect infer.op (list "Translate to French: " x)))
     (list "hello" "goodbye"))
=> ("bonjour" "au revoir")
```

---

## Install

```bash
cd OmegaLLM
npm install
```

Set API key in `.env`:
```
OPENAI_API_KEY=sk-...
```

---

## Syntax

### LLM Calls

```lisp
(effect infer.op "prompt")
```

That's an LLM call. It returns the response as a string.

```lisp
(effect infer.op "What color is the sky?")
=> "blue"

(effect infer.op (list "Summarize: " some-text))
=> "A summary of the text..."
```

### Variables and Functions

```lisp
; Variable
(define x 10)

; Function
(define (double x) (* x 2))

; Function that calls LLM
(define (translate text lang)
  (effect infer.op (list "Translate to " lang ": " text)))

(translate "hello" "Spanish")
=> "hola"
```

### Control Flow

```lisp
; Conditional
(if (> x 5) "big" "small")

; LLM result in conditional
(if (equal? (effect infer.op "Is the sky blue? yes/no") "yes")
    "correct"
    "wrong")
```

### Lists and Loops

```lisp
; Create list
(list 1 2 3)

; Map function over list
(map double (list 1 2 3))
=> (2 4 6)

; Map LLM calls over list
(map (lambda (word) (effect infer.op (list "Define: " word)))
     (list "algorithm" "recursion"))
=> ("A step-by-step procedure..." "A function that calls itself...")
```

### Strings

```lisp
; Concatenate (using list creates a prompt)
(effect infer.op (list "Explain " topic " in one sentence"))

; Or use string-append for actual concatenation
(string-append "Hello " "World")
=> "Hello World"
```

---

## Core Pattern

The whole point: **LLM calls are expressions**. Use them like any other value.

```lisp
; Get LLM response
(define response (effect infer.op "List 3 programming languages"))

; Process it
(define languages (parse-list response))

; Use each one
(map (lambda (lang)
       (effect infer.op (list "What is " lang " used for?")))
     languages)
```

This is a **program** that:
1. Asks the LLM for a list
2. Parses the response
3. Asks follow-up questions for each item

You can't do this with a single prompt. You need code.

---

## Examples

### Chatbot Loop

```lisp
(define (chat history message)
  (let ((prompt (list history "\nUser: " message "\nAssistant:")))
    (let ((response (effect infer.op prompt)))
      (list history "\nUser: " message "\nAssistant: " response))))

; Build up conversation
(define h1 (chat "" "Hello"))
(define h2 (chat h1 "What's your name?"))
(define h3 (chat h2 "Tell me a joke"))
```

### Semantic Filter

```lisp
(define (is-food? item)
  (equal? "yes"
    (effect infer.op (list "Is " item " food? Answer only yes or no"))))

(filter is-food? (list "apple" "car" "pizza" "laptop"))
=> ("apple" "pizza")
```

### Chain of Thought

```lisp
(define (solve-step-by-step problem)
  (let ((steps (effect infer.op (list "Break this into steps: " problem))))
    (let ((solutions (map (lambda (step) (effect infer.op step))
                          (parse-steps steps))))
      (effect infer.op (list "Combine: " solutions)))))
```

### Retry Until Valid

```lisp
(define (get-valid-json prompt)
  (let ((response (effect infer.op prompt)))
    (if (valid-json? response)
        response
        (get-valid-json (list prompt "\nReturn valid JSON only.")))))
```

### Nested LLM Calls (Reentrancy)

The LLM can call code that calls another LLM:

```lisp
(define (research topic)
  (let ((questions (effect infer.op (list "Generate 3 questions about " topic))))
    (map (lambda (q) (effect infer.op q))
         (parse-questions questions))))
```

The outer LLM generates questions. Each question triggers an inner LLM call.

---

## Nondeterministic Search

For trying multiple options with backtracking:

```lisp
; Choose from alternatives (backtracks on failure)
(effect amb.choose (list (lambda () "a") (lambda () "b") (lambda () "c")))

; Fail and backtrack
(effect amb.fail "reason")

; Require condition (fails if false)
(define (require cond)
  (if cond #t (effect amb.fail "constraint failed")))
```

Example: Find a word that satisfies semantic constraints

```lisp
(let ((word (effect amb.choose
              (list (lambda () "apple")
                    (lambda () "car")
                    (lambda () "pizza")))))
  (require (equal? "yes"
    (effect infer.op (list "Is " word " food? yes/no"))))
  word)
=> "apple"  ; Tried "apple", LLM said yes, done
            ; (Would have backtracked to "pizza" if "apple" failed)
```

---

## API

### TypeScript

```typescript
import { OmegaRuntime } from "./src/runtime";
import { createOpenAIAdapter } from "./src/core/oracle/adapters/openaiAdapter";

const runtime = new OmegaRuntime({
  adapter: createOpenAIAdapter({
    apiKey: process.env.OPENAI_API_KEY,
    model: "gpt-4",
  })
});

const result = await runtime.eval('(effect infer.op "Hello")');
```

### REPL

```bash
# Development mode (slower startup ~7s, TypeScript on-the-fly)
npx tsx bin/omega-repl.ts

# Fast mode (0.3s startup, requires npm run build)
npm run omega-fast
# or directly:
node dist/omega-repl.mjs
```

```
Ω> (+ 1 2)
=> 3

Ω> (effect infer.op "What is 2+2?")
=> "4"
```

### Build

```bash
# Build everything (TypeScript + bundled fast version)
npm run build

# TypeScript only
npm run build:tsc

# Bundle only (requires TypeScript build first)
npm run bundle
```

The `npm run build` creates both the regular dist files AND the bundled `dist/omega-repl.mjs` which starts in ~0.3 seconds vs ~7 seconds for tsx.

### Environment Variables

| Variable | Description |
|----------|-------------|
| `OPENAI_API_KEY` | OpenAI API key |
| `ANTHROPIC_API_KEY` | Anthropic API key |
| `OMEGA_ADAPTER` | `openai` (default) or `anthropic` |
| `OMEGA_MODEL` | Override model (e.g., `gpt-4o`, `claude-sonnet-4-20250514`) |
| `OMEGA_SESSION_DIR` | Directory for session files |
| `OMEGA_SCRIPTED_ORACLE` | `1` to use scripted oracle (testing) |
| `OMEGA_MOCK_EFFECTS` | `1` to mock effects (testing) |

---

## Reference

### Effects

| Syntax | Description |
|--------|-------------|
| `(effect infer.op "prompt")` | Call LLM, return response |
| `(effect infer.op (list "a" "b"))` | Call LLM with concatenated prompt |
| `(effect amb.choose (list thunks...))` | Pick one, backtrack on fail |
| `(effect amb.fail "reason")` | Fail and backtrack |
| `(effect shell.op "command")` | Execute shell command, return stdout |
| `(effect file.read.op "path")` | Read file contents |
| `(effect file.write.op "path" "content")` | Write content to file |

### System Effects (Orchestration)

Execute shell commands and perform file I/O:

```lisp
;; Run a shell command
(effect shell.op "echo hello")
=> "hello"

;; Read a file
(effect file.read.op "config.json")
=> "{\"key\": \"value\"}"

;; Write a file
(effect file.write.op "output.txt" "result data")
=> ()
```

### Reentrant Orchestration

Omega programs can spawn other Omega programs:

```lisp
;; orchestrator.scm - spawns workers
(begin
  ;; Write task for worker
  (effect file.write.op "task.txt" "process this data")

  ;; Spawn worker (uses fast bundled mode)
  (effect shell.op "node dist/omega-repl.mjs -c \"(load \\\"worker.scm\\\")\"")

  ;; Read result
  (effect file.read.op "result.txt"))

;; worker.scm - processes task
(begin
  (define task (effect file.read.op "task.txt"))
  (define result (effect infer.op (list "Process: " task)))
  (effect file.write.op "result.txt" result))
```

**Trace visibility:**
- Spawn via `shell.op`: Child trace is separate (opaque to parent)
- Call via `infer.op`: Unified trace (LLM sees full context)

### Primitives

| Syntax | Description |
|--------|-------------|
| `(define name value)` | Bind variable |
| `(define (name args) body)` | Define function |
| `(lambda (args) body)` | Anonymous function |
| `(if cond then else)` | Conditional |
| `(let ((x val)) body)` | Local binding |
| `(begin expr1 expr2 ...)` | Sequence |
| `(list a b c)` | Create list |
| `(car list)` | First element |
| `(cdr list)` | Rest of list |
| `(cons x list)` | Prepend to list |
| `(null? list)` | Is list empty? |
| `(map fn list)` | Apply to each |
| `(filter pred list)` | Keep matching |
| `(equal? a b)` | Deep equality |
| `(+ - * /)` | Arithmetic |
| `(< > = <= >=)` | Comparison |
| `(and or not)` | Logic |
| `(string-append a b)` | Join strings |

### Files

```
src/
├── runtime.ts                    # OmegaRuntime entry point
├── core/
│   ├── prims.ts                  # All primitives
│   ├── eval/
│   │   ├── machine.ts            # Execution engine
│   │   └── values.ts             # Value types
│   └── oracle/
│       ├── portalImpl.ts         # LLM request handling
│       └── adapters/
│           ├── openaiAdapter.ts  # OpenAI
│           └── anthropicAdapter.ts # Anthropic
```

---

## That's It

OmegaLLM lets you write programs that include LLM calls.

- `(effect infer.op "prompt")` — call the LLM
- Process results with code
- Loop, branch, compose
- LLM calls are just expressions
