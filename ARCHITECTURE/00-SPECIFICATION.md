# 00: LambdaLLM Language Specification

## Overview

LambdaLLM is a Lisp dialect designed for AI agent orchestration with first-class debugging support.

**Key characteristics:**
- Homoiconic (code is data)
- Lexically scoped
- First-class continuations
- Non-unwinding condition system
- Pure evaluator with FFI for effects
- Serializable closures (for time-travel)

## Notation

This specification uses:
- `<name>` for non-terminals
- `|` for alternatives
- `*` for zero or more
- `+` for one or more
- `?` for optional
- `"text"` for literal tokens

---

## 1. Lexical Structure

### 1.1 Character Set

LambdaLLM source code is Unicode (UTF-8).

### 1.2 Whitespace and Comments

```ebnf
<whitespace> ::= <space> | <tab> | <newline> | <carriage-return>
<comment>    ::= ";" <any-char-except-newline>* <newline>
```

Whitespace and comments are ignored except as token separators.

### 1.3 Tokens

```ebnf
<token> ::= <delimiter> | <atom>

<delimiter> ::= "(" | ")" | "[" | "]" | "{" | "}"
              | "'" | "`" | "," | ",@"

<atom> ::= <number> | <string> | <symbol> | <boolean> | <keyword>
```

### 1.4 Numbers

```ebnf
<number>  ::= <integer> | <float>
<integer> ::= <sign>? <digit>+
<float>   ::= <sign>? <digit>+ "." <digit>+
<sign>    ::= "+" | "-"
<digit>   ::= "0" | "1" | ... | "9"
```

Examples: `42`, `-17`, `3.14`, `-0.5`

### 1.5 Strings

```ebnf
<string>      ::= '"' <string-char>* '"'
<string-char> ::= <any-char-except-quote-or-backslash>
                | <escape-sequence>
<escape-sequence> ::= "\\" | "\"" | "\n" | "\t" | "\r"
```

Examples: `"hello"`, `"line1\nline2"`, `"say \"hi\""`

### 1.6 Symbols

```ebnf
<symbol>       ::= <symbol-start> <symbol-cont>*
<symbol-start> ::= <letter> | <special-initial>
<symbol-cont>  ::= <symbol-start> | <digit> | <special-subsequent>
<special-initial>    ::= "!" | "$" | "%" | "&" | "*" | "/" | ":"
                       | "<" | "=" | ">" | "?" | "^" | "_" | "~"
<special-subsequent> ::= "+" | "-" | "." | "@"
```

Examples: `foo`, `+`, `string->number`, `file-not-found?`

### 1.7 Booleans

```ebnf
<boolean> ::= "#t" | "#f" | "true" | "false"
```

### 1.8 Keywords

```ebnf
<keyword> ::= ":" <symbol>
```

Examples: `:name`, `:type`, `:value`

---

## 2. Data Types

### 2.1 Type Hierarchy

```
Value
├── Atom
│   ├── Number (integer or float)
│   ├── String
│   ├── Boolean (#t or #f)
│   ├── Nil (null, '())
│   └── Keyword
├── Symbol
├── Pair (cons cell)
├── List (proper list of pairs ending in nil)
├── Vector (indexable sequence)
├── Map (key-value mapping)
└── Procedure
    ├── Primitive (built-in)
    ├── Closure (user-defined lambda)
    ├── Macro (compile-time transformer)
    └── Continuation (captured call stack)
```

### 2.2 Equivalence

Two values are `eq?` if they are the same object (identity).

Two values are `equal?` if:
- Both atoms with same value
- Both symbols with same name
- Both pairs with `equal?` car and cdr
- Both vectors with `equal?` elements
- Both maps with `equal?` key-value pairs

### 2.3 Truthiness

All values are truthy except:
- `#f` (false)
- `nil` / `null` / `'()`

Note: `0`, `""`, and `'()` are truthy (unlike some languages).

---

## 3. Expressions

### 3.1 Syntax

```ebnf
<expression> ::= <atom>
               | <symbol>
               | <list-expression>
               | <quoted-expression>

<list-expression> ::= "(" <expression>* ")"

<quoted-expression> ::= "'" <expression>
                      | "`" <expression>
                      | "," <expression>
                      | ",@" <expression>
```

### 3.2 Evaluation Rules

1. **Atoms** evaluate to themselves
2. **Symbols** evaluate to their bound value in the current environment
3. **Lists** are evaluated as either:
   - Special forms (if head is a special form keyword)
   - Procedure applications (otherwise)
4. **Quoted expressions** are not evaluated

### 3.3 Special Forms

| Form | Syntax | Description |
|------|--------|-------------|
| `quote` | `(quote <datum>)` | Return datum unevaluated |
| `if` | `(if <test> <then> <else>?)` | Conditional |
| `cond` | `(cond (<test> <expr>*)...)` | Multi-way conditional |
| `and` | `(and <expr>*)` | Short-circuit and |
| `or` | `(or <expr>*)` | Short-circuit or |
| `lambda` | `(lambda <params> <body>+)` | Create closure |
| `define` | `(define <name> <value>)` | Bind in current scope |
| `set!` | `(set! <name> <value>)` | Mutate existing binding |
| `let` | `(let ((<n> <v>)*) <body>+)` | Parallel local bindings |
| `let*` | `(let* ((<n> <v>)*) <body>+)` | Sequential local bindings |
| `letrec` | `(letrec ((<n> <v>)*) <body>+)` | Recursive local bindings |
| `begin` | `(begin <expr>+)` | Sequence, return last |
| `call/cc` | `(call/cc <proc>)` | Capture continuation |
| `signal` | `(signal <condition>)` | Signal condition |
| `handler-bind` | `(handler-bind <handlers> <body>+)` | Install handlers |
| `restart-case` | `(restart-case <body> <restarts>+)` | Define restarts |

### 3.4 Procedure Application

```
(<operator> <operand>*)
```

1. Evaluate `<operator>` to get procedure
2. Evaluate `<operand>`s left-to-right to get arguments
3. Apply procedure to arguments

---

## 4. Environments and Scope

### 4.1 Lexical Scope

Names are resolved in the environment where they are **defined**, not where they are **used**.

```lisp
(define x 10)
(define (get-x) x)     ; Captures x=10
(let ((x 20))
  (get-x))             ; Returns 10, not 20
```

### 4.2 Environment Structure

An environment is a chain of frames:

```
[current-frame] → [parent-frame] → ... → [global-frame]
```

Each frame contains:
- Bindings (name → value)
- Parent pointer (or null for global)

### 4.3 Lookup Rules

To look up name `n` in environment `E`:
1. If `n` is bound in current frame of `E`, return its value
2. Otherwise, look up `n` in parent of `E`
3. If no parent, signal `unbound-variable` condition

### 4.4 Binding Rules

- `define` creates binding in current frame
- `set!` modifies existing binding (searches up chain)
- `let`/`let*`/`letrec` create new frame with bindings

---

## 5. Procedures

### 5.1 Lambda Expressions

```lisp
(lambda (<param>*) <body>+)
(lambda (<param>* . <rest>) <body>+)  ; Rest parameter
```

Creates a closure capturing:
- Parameter list
- Body expressions
- Defining environment (snapshot)

### 5.2 Application

```lisp
(<proc> <arg>*)
```

For closure application:
1. Create new frame with parent = closure's captured environment
2. Bind parameters to arguments in new frame
3. Evaluate body in new frame
4. Return value of last expression

### 5.3 Primitives

Primitives are built-in procedures implemented in the host language (TypeScript). They:
- Have fixed arity (or variadic)
- Execute synchronously
- Cannot be serialized directly (referenced by name)

### 5.4 Continuations

A continuation is a first-class representation of "what happens next."

```lisp
(call/cc (lambda (k)
           ...
           (k value)   ; Return value to continuation
           ...))
```

Invoking a continuation:
- Abandons current computation
- Returns value to captured point

---

## 6. Condition System

### 6.1 Conditions

A condition represents an exceptional situation without immediately unwinding the stack.

```lisp
(signal <condition-value>)
```

### 6.2 Handlers

Handlers are established dynamically and invoked when matching conditions are signaled.

```lisp
(handler-bind
  ((<type> <handler-fn>)
   ...)
  <body>+)
```

When condition is signaled:
1. Search handler stack for matching handler
2. Call handler **with stack intact**
3. Handler can: invoke restart, decline, or transfer

### 6.3 Restarts

Restarts define recovery options at the point where condition originates.

```lisp
(restart-case
  <body>
  (<restart-name> (<params>) <restart-body>+)
  ...)
```

Handler invokes restart:
```lisp
(invoke-restart '<restart-name> <args>*)
```

---

## 7. Namespaces and Modules

### 7.1 Namespaces

A namespace is a named container for definitions.

```lisp
(ns myapp.core)                    ; Declare namespace
(in-ns 'myapp.core)               ; Switch to namespace
```

### 7.2 Require/Import

```lisp
(require 'myapp.utils)             ; Load namespace
(require '[myapp.utils :as u])     ; Load with alias
(require '[myapp.utils :refer [foo bar]]) ; Import specific names
```

### 7.3 Module Files

One file = one module. File `myapp/core.lisp` defines namespace `myapp.core`.

---

## 8. Foreign Function Interface (FFI)

### 8.1 World Operations

All side effects go through the World interface:

```lisp
(world.read <path>)                ; Read file
(world.write <path> <content>)     ; Write file
(world.list <pattern>)             ; List files
(world.run <cmd> <args>)           ; Execute command
```

### 8.2 LLM Operations

LLM calls are FFI operations:

```lisp
(llm.complete <prompt> <system>?)  ; Get completion
(llm.chat <messages> <tools>?)     ; Chat with tools
```

### 8.3 Registering FFI Functions

From TypeScript:
```typescript
ffi.register('my.operation', (arg1, arg2) => {
  // Implementation
  return result;
});
```

From Lisp:
```lisp
(my.operation arg1 arg2)
```

---

## 9. Standard Library

See [14-STDLIB.md](14-STDLIB.md) for complete reference.

### 9.1 Core

| Function | Description |
|----------|-------------|
| `(+ n...)` | Addition |
| `(- n n?)` | Subtraction |
| `(* n...)` | Multiplication |
| `(/ n n)` | Division |
| `(= a b)` | Equality |
| `(< a b)` | Less than |
| `(> a b)` | Greater than |
| `(not x)` | Logical not |

### 9.2 Lists

| Function | Description |
|----------|-------------|
| `(cons h t)` | Construct pair |
| `(car p)` | First of pair |
| `(cdr p)` | Rest of pair |
| `(list x...)` | Create list |
| `(length l)` | List length |
| `(map f l)` | Map over list |
| `(filter p l)` | Filter list |
| `(reduce f init l)` | Reduce list |

### 9.3 Strings

| Function | Description |
|----------|-------------|
| `(str x...)` | Concatenate to string |
| `(string-length s)` | String length |
| `(substring s start end?)` | Extract substring |
| `(string-split s sep)` | Split string |

---

## 10. Concurrency Model

### 10.1 Single-Threaded Core

The evaluator is single-threaded. Concurrency is achieved through:
- Async FFI calls (return promises)
- Continuation-based coroutines

### 10.2 Async/Await Pattern

```lisp
(await (llm.complete "prompt"))    ; Wait for async result
```

### 10.3 Future Work

- Actor model for parallelism
- STM for shared state
- Channels for communication

---

## 11. Memory Model

### 11.1 Values

Values are immutable except:
- Environment bindings (via `set!`)
- World state (via FFI)

### 11.2 Garbage Collection

Handled by host language (TypeScript/JavaScript GC).

### 11.3 Object Identity

- Atoms: identity by value
- Symbols: interned (same name = same object)
- Lists: identity by reference
- Closures: identity by reference

---

## 12. Execution Model

### 12.1 Entry Point

```lisp
;; File: main.lisp
(ns main)

(define (main args)
  (println "Hello, world!"))
```

### 12.2 REPL

Interactive read-eval-print loop:
```
λ> (+ 1 2)
3
λ> (define x 10)
10
λ> x
10
```

### 12.3 Script Mode

```bash
lambdallm run script.lisp
lambdallm eval "(+ 1 2)"
```

---

## Appendix A: Grammar (Complete EBNF)

```ebnf
program     ::= expression*

expression  ::= atom
              | symbol
              | "(" expression* ")"
              | "'" expression
              | "`" expression
              | "," expression
              | ",@" expression

atom        ::= number | string | boolean | keyword | nil

number      ::= integer | float
integer     ::= sign? digit+
float       ::= sign? digit+ "." digit+
sign        ::= "+" | "-"
digit       ::= "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"

string      ::= '"' string-char* '"'
string-char ::= <any char except " or \> | escape
escape      ::= "\\" | "\"" | "\n" | "\t" | "\r"

boolean     ::= "#t" | "#f" | "true" | "false"

keyword     ::= ":" symbol

nil         ::= "nil" | "null"

symbol      ::= symbol-start symbol-cont*
symbol-start ::= letter | special-initial
symbol-cont  ::= symbol-start | digit | special-subsequent
letter       ::= "a" | ... | "z" | "A" | ... | "Z"
special-initial    ::= "!" | "$" | "%" | "&" | "*" | "/" | ":"
                     | "<" | "=" | ">" | "?" | "^" | "_" | "~"
special-subsequent ::= "+" | "-" | "." | "@"
```

---

## Appendix B: Reserved Words

These symbols have special meaning:

```
quote if cond and or lambda define set!
let let* letrec begin
call/cc signal handler-bind restart-case invoke-restart
ns in-ns require import
defmacro
```

---

## Appendix C: Differences from Scheme/Common Lisp

| Feature | Scheme | Common Lisp | LambdaLLM |
|---------|--------|-------------|-----------|
| Booleans | `#t`/`#f` | `t`/`nil` | `#t`/`#f` |
| False value | `#f` | `nil` | `#f` and `nil` |
| Continuations | `call/cc` | None | `call/cc` |
| Conditions | None | `signal`/`handler-bind` | `signal`/`handler-bind` |
| Namespaces | Libraries | Packages | Clojure-style `ns` |
| FFI | Foreign | CFFI | Built-in `world.*`, `llm.*` |
