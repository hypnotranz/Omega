# OmegaLLM

A Lisp where you can call LLMs.

```lisp
(ask-llm "What is the capital of France?")
=> "Paris"
```

---

## Install

```bash
cd OmegaLLM
npm install
```

Set your API key in `.env`:
```
OPENAI_API_KEY=sk-...
```

---

## Usage

### TypeScript

```typescript
import { OmegaRuntime, createOpenAIAdapter } from "omegallm";

const omega = new OmegaRuntime({
  adapter: createOpenAIAdapter({
    apiKey: process.env.OPENAI_API_KEY
  })
});

// Simple eval
await omega.eval("(+ 1 2)");  // => 3

// LLM call
await omega.eval('(ask-llm "What is 2+2?")');  // => "4"
```

### REPL

```bash
npx tsx bin/omega-repl.ts
```

```
Ω> (+ 1 2)
=> 3

Ω> (ask-llm "Hello!")
=> "Hello! How can I help you?"
```

---

## The Language

### Basics

```lisp
; Math
(+ 1 2 3)           ; => 6
(* 2 3)             ; => 6
(- 10 3)            ; => 7
(/ 10 2)            ; => 5

; Strings
"hello"             ; => "hello"
(concat "a" "b")    ; => "ab"

; Booleans
#t                  ; true
#f                  ; false
(> 5 3)             ; => #t
(= 2 2)             ; => #t

; Variables
(define x 10)
x                   ; => 10

; Functions
(define (square x) (* x x))
(square 5)          ; => 25

; Lambda
((lambda (x) (* x 2)) 5)  ; => 10

; Conditionals
(if (> x 5) "big" "small")

; Lists
(list 1 2 3)        ; => (1 2 3)
(car (list 1 2 3))  ; => 1
(cdr (list 1 2 3))  ; => (2 3)
(cons 0 (list 1 2)) ; => (0 1 2)
```

---

## LLM Calls

```lisp
; Basic call
(ask-llm "What is the capital of France?")
=> "Paris"

; With variables
(define topic "quantum physics")
(ask-llm (concat "Explain " topic " simply"))

; In a function
(define (translate text lang)
  (ask-llm (concat "Translate to " lang ": " text)))

(translate "Hello" "Spanish")
=> "Hola"
```

### Composing LLM Calls

```lisp
; Sequential calls
(define (analyze text)
  (list
    (ask-llm (concat "Summarize: " text))
    (ask-llm (concat "Sentiment: " text))))

; LLM as predicate
(define (is-positive? text)
  (equal? "yes"
    (ask-llm (concat "Is this positive? yes/no: " text))))

(is-positive? "I love this!")  ; => #t
```

---

## Nondeterministic Choice

```lisp
; Pick one from list (can backtrack on failure)
(choose (list "a" "b" "c"))
=> "a"

; With constraint - backtracks until one works
(define x (choose (list 1 2 3 4 5)))
(if (< x 3) x (effect amb.fail "too big"))
=> 1  ; tries 1, succeeds
```

---

## Primitives Reference

| Name | Args | Description |
|------|------|-------------|
| `ask-llm` | prompt | Call the LLM |
| `choose` | list | Pick from list, backtrack on fail |
| `+` `-` `*` `/` | nums | Arithmetic |
| `=` `<` `>` | a b | Comparison |
| `and` `or` `not` | bools | Logic |
| `if` | cond then else | Conditional |
| `define` | name val | Bind variable |
| `lambda` | params body | Anonymous function |
| `let` | bindings body | Local scope |
| `list` | items... | Create list |
| `car` | list | First element |
| `cdr` | list | Rest of list |
| `cons` | item list | Prepend to list |
| `concat` | strs... | Join strings |
| `equal?` | a b | Deep equality |
| `map` | fn list | Apply to each |
| `filter` | pred list | Keep matching |

---

## Advanced: Raw Effects

For fine-grained control, use effects directly:

```lisp
; LLM inference (same as ask-llm)
(effect infer.op "prompt")

; Nondeterministic choice (lower-level than choose)
(effect amb.choose (list (lambda () 1) (lambda () 2)))

; Backtrack/fail
(effect amb.fail "reason")
```

---

## API

### OmegaRuntime

```typescript
new OmegaRuntime({
  adapter: LLMAdapter,       // Required
  maxSteps?: number,         // Default: 100000
})

omega.eval(code: string): Promise<Val>
omega.eval(code: string, bindings: Record<string, any>): Promise<Val>
```

### Adapters

```typescript
// OpenAI
createOpenAIAdapter({
  apiKey: string,
  model?: string,           // Default: "gpt-4"
  streaming?: boolean,
})

// Anthropic
createAnthropicAdapter({
  apiKey: string,
  model?: string,           // Default: "claude-3-5-sonnet-latest"
  streaming?: boolean,
})
```

---

## Files

```
src/
├── index.ts           # Exports
├── runtime.ts         # OmegaRuntime class
└── core/
    ├── prims.ts       # Primitives: ask-llm, choose, +, -, etc.
    ├── eval/
    │   ├── machine.ts # Execution engine
    │   └── values.ts  # Value types
    └── oracle/
        └── adapters/  # OpenAI, Anthropic integrations
```

---

## Examples

### Chatbot

```lisp
(define (chat message)
  (ask-llm (concat "User: " message "\nAssistant:")))

(chat "What's the weather?")
```

### Code Generator

```lisp
(define (codegen desc)
  (ask-llm (concat "Write Python for: " desc "\nCode only:")))

(codegen "fibonacci")
```

### Semantic Filter

```lisp
(define (food? x)
  (equal? "yes" (ask-llm (concat "Is " x " food? yes/no"))))

(filter food? (list "apple" "car" "pizza"))
=> ("apple" "pizza")
```

---

## That's It

- `(ask-llm "prompt")` - call the LLM
- `(choose (list ...))` - pick one, backtrack on fail
- Everything else is Lisp

Source: [src/core/prims.ts](../src/core/prims.ts)
