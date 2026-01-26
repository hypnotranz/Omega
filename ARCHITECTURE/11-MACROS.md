# ⚠️ SUPERSEDED BY 32-10 HYGIENIC SYNTAX-RULES

> **THIS SPEC DESCRIBES UNHYGIENIC MACROS - 32-10 ADDS HYGIENIC SYSTEM**
>
> ## What Changed
>
> This spec describes basic `defmacro`-style macros that can accidentally
> capture variables (unhygienic). The 32-series adds a proper **hygienic
> macro system** via `syntax-rules`:
>
> ```lisp
> ;; THIS SPEC (unhygienic defmacro):
> (defmacro my-or (a b)
>   `(let ((tmp ,a))      ; 'tmp' could clash with user's 'tmp'!
>      (if tmp tmp ,b)))
>
> ;; 32-10 (hygienic syntax-rules):
> (define-syntax my-or
>   (syntax-rules ()
>     ((my-or a b)
>      (let ((tmp a))      ; 'tmp' is automatically renamed - NO CLASH
>        (if tmp tmp b)))))
> ```
>
> ## What 32-10 Adds
>
> - **syntax-rules**: Pattern-based hygienic macros (R5RS compatible)
> - **syntax-case**: Procedural hygienic macros (R6RS compatible)
> - **Phase separation**: Clear compile-time vs runtime distinction
> - **Module integration**: Macros respect module boundaries
>
> ## What's Still Valid
>
> The *concepts* of compile-time transformation are still valid.
> `defmacro` can still exist for backwards compatibility, but `syntax-rules`
> should be preferred for new code.
>
> ## References
> - [32-10 amb + syntax-rules](32-10-AMB-SYNTAX.md)
> - [ARCHITECTURE-REDESIGN-ASSESSMENT.md](../docs/ARCHITECTURE-REDESIGN-ASSESSMENT.md)

---

# 11: Macros (Compile-Time Transformations)

## What Are Macros?

Macros are **compile-time code transformations**. They receive unevaluated syntax and return new syntax to evaluate.

```
┌─────────────────────────────────────────────────────────────────┐
│  Normal function:  (f (+ 1 2))  →  f receives 3 (evaluated)    │
│  Macro:            (m (+ 1 2))  →  m receives '(+ 1 2) (syntax)│
└─────────────────────────────────────────────────────────────────┘
```

---

## Why Macros?

| Use Case | Example |
|----------|---------|
| New control flow | `(when test body...)` |
| Domain-specific syntax | `(html [:div "hello"])` |
| Compile-time computation | `(static-assert (> x 0))` |
| Code generation | `(defstruct Point x y)` |
| Debugging wrappers | `(trace (expensive-call))` |

---

## Defining Macros

```lisp
(defmacro when (test &rest body)
  `(if ,test
       (begin ,@body)
       nil))

;; Usage:
(when (> x 0)
  (print "positive")
  (process x))

;; Expands to:
(if (> x 0)
    (begin (print "positive")
           (process x))
    nil)
```

### Syntax

```ebnf
<defmacro> ::= "(defmacro" <name> <params> <body>+ ")"

<params> ::= "(" <param>* <rest-param>? ")"
<rest-param> ::= "&rest" <symbol>
```

---

## Quasiquote

Quasiquote (`) makes template construction easier:

| Syntax | Meaning |
|--------|---------|
| `` `expr `` | Quote, but allow unquoting |
| `,expr` | Unquote: evaluate and insert |
| `,@expr` | Splice: evaluate and spread |

```lisp
(define x 10)
(define items '(a b c))

`(list ,x)        ; → (list 10)
`(items: ,@items) ; → (items: a b c)
`(,x ,@items ,x)  ; → (10 a b c 10)
```

---

## Macro Expansion

### Expansion Process

1. **Check**: Is head a macro?
2. **Call**: Invoke macro with unevaluated arguments
3. **Replace**: Substitute result for original form
4. **Recurse**: Expand any macros in result
5. **Evaluate**: Evaluate fully-expanded form

### Explicit Expansion

```lisp
(macroexpand-1 '(when test body))
; → (if test (begin body) nil)

(macroexpand '(when test (when nested x)))
; → (if test (begin (if nested (begin x) nil)) nil)
```

---

## Hygiene

### The Problem

Macros can accidentally capture variables:

```lisp
;; BAD: captures 'temp' from user code
(defmacro swap! (a b)
  `(let ((temp ,a))
     (set! ,a ,b)
     (set! ,b temp)))

;; User code:
(define temp 100)
(swap! x temp)  ; BUG: 'temp' shadowed!
```

### Solution: Gensym

```lisp
;; GOOD: use gensym for internal variables
(defmacro swap! (a b)
  (let ((temp (gensym "temp")))
    `(let ((,temp ,a))
       (set! ,a ,b)
       (set! ,b ,temp))))
```

`gensym` creates a unique symbol that can't conflict.

---

## Common Macro Patterns

### 1. With-Pattern (Resource Management)

```lisp
(defmacro with-open-file ((var path) &rest body)
  (let ((result (gensym)))
    `(let ((,var (open-file ,path)))
       (let ((,result (begin ,@body)))
         (close-file ,var)
         ,result))))

;; Usage:
(with-open-file (f "data.txt")
  (read-all f))
```

### 2. Anaphoric Macros

```lisp
(defmacro aif (test then &optional else)
  `(let ((it ,test))
     (if it ,then ,else)))

;; Usage:
(aif (find-user id)
  (greet it)    ; 'it' is the found user
  (error "not found"))
```

### 3. Debugging/Tracing

```lisp
(defmacro trace (expr)
  `(let ((result ,expr))
     (println (str "TRACE: " ',expr " = " result))
     result))

;; Usage:
(trace (+ 1 2))
;; Prints: TRACE: (+ 1 2) = 3
;; Returns: 3
```

### 4. Loop Constructs

```lisp
(defmacro dotimes ((var n) &rest body)
  (let ((limit (gensym)))
    `(let ((,limit ,n))
       (let loop ((,var 0))
         (when (< ,var ,limit)
           ,@body
           (loop (+ ,var 1)))))))

;; Usage:
(dotimes (i 5)
  (println i))
```

### 5. DSL Definition

```lisp
(defmacro html (form)
  (cond
    ((string? form) form)
    ((keyword? form) (str "<" (keyword-name form) "/>"))
    ((list? form)
     (let ((tag (car form))
           (children (cdr form)))
       `(str "<" ,(keyword-name tag) ">"
             ,@(map (lambda (c) `(html ,c)) children)
             "</" ,(keyword-name tag) ">")))))

;; Usage:
(html [:div [:p "Hello"] [:p "World"]])
;; → "<div><p>Hello</p><p>World</p></div>"
```

---

## Implementation

```typescript
// macros.ts

interface Macro {
  type: 'macro';
  name: string;
  params: string[];
  restParam?: string;
  body: Value;
  env: EnvSnapshot;
}

function defineMacro(name: string, params: Value, body: Value, env: Environment): Macro {
  const { regular, rest } = parseParams(params);
  return {
    type: 'macro',
    name,
    params: regular,
    restParam: rest,
    body,
    env: env.snapshot()
  };
}

function expandMacro(macro: Macro, args: Value[]): Value {
  // Create environment with macro's captured env
  const env = Environment.fromSnapshot(macro.env);

  // Bind parameters to UNEVALUATED arguments
  for (let i = 0; i < macro.params.length; i++) {
    env.define(macro.params[i], args[i]);
  }

  // Bind rest parameter if present
  if (macro.restParam) {
    env.define(macro.restParam, args.slice(macro.params.length));
  }

  // Evaluate macro body (this produces the expansion)
  return evalExpr(macro.body, env, identityCont, ffi);
}

function macroexpand1(expr: Value, env: Environment): Value {
  if (!isList(expr) || expr.length === 0) return expr;

  const head = expr[0];
  if (!isSymbol(head)) return expr;

  const value = env.lookupOrNull(symbolName(head));
  if (!isMacro(value)) return expr;

  return expandMacro(value, expr.slice(1));
}

function macroexpand(expr: Value, env: Environment): Value {
  let current = expr;
  let expanded = macroexpand1(current, env);

  while (!equal(current, expanded)) {
    current = expanded;
    expanded = macroexpand1(current, env);
  }

  // Recursively expand in subforms
  if (isList(expanded)) {
    return expanded.map(sub => macroexpand(sub, env));
  }

  return expanded;
}
```

---

## Evaluation Order

```
Source Code
    │
    ▼
┌──────────────┐
│  READER      │  Parse text → AST
└──────┬───────┘
       │
       ▼
┌──────────────┐
│  MACRO       │  Expand macros (recursive)
│  EXPANSION   │
└──────┬───────┘
       │
       ▼
┌──────────────┐
│  EVALUATOR   │  Evaluate expanded code
└──────────────┘
```

---

## Debugging Macros

```lisp
;; See expansion
(macroexpand-1 '(when test body))

;; Step-by-step expansion
(macroexpand-all '(complex nested (macros here)))

;; Trace macro expansion
(with-macro-trace
  (eval '(my-macro args)))
;; Prints each expansion step
```

---

## Comparison

| Language | Macro Type | Hygiene |
|----------|------------|---------|
| Common Lisp | Unhygienic | Manual (gensym) |
| Scheme | Hygienic (syntax-rules) | Automatic |
| Clojure | Unhygienic | Manual (gensym) |
| Racket | Hygienic (syntax) | Automatic |
| **LambdaLLM** | Unhygienic | Manual (gensym) |

We use the Common Lisp/Clojure style for simplicity. Gensym provides hygiene when needed.
