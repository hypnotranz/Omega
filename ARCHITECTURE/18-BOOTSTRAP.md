# 18: Bootstrap (Self-Hosting Strategy)

## Overview

Bootstrap defines how LambdaLLM is built from source. Following Lisp tradition, the runtime is partially written in itself.

---

## Bootstrap Layers

```
┌─────────────────────────────────────────────────────────────────┐
│                       Bootstrap Sequence                        │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  Layer 0: TypeScript Foundation (the "metacircular base")       │
│  ├── reader.ts          - S-expression parser                   │
│  ├── types.ts           - Core type definitions                 │
│  ├── continuation.ts    - Continuation machinery                │
│  ├── env.ts             - Environment implementation            │
│  └── eval.ts            - Pure evaluator (~200 lines)           │
│                                                                 │
│  Layer 1: TypeScript Primitives                                 │
│  ├── stdlib/base.ts     - Arithmetic, comparison, predicates    │
│  ├── stdlib/list.ts     - cons, car, cdr, map, filter           │
│  └── stdlib/string.ts   - String operations                     │
│                                                                 │
│  Layer 2: Lisp Foundation (minimal runtime can load these)      │
│  ├── core.lisp          - when, unless, cond, case              │
│  ├── list.lisp          - reduce, flatten, zip, partition       │
│  ├── macro.lisp         - defmacro, quasiquote implementation   │
│  └── condition.lisp     - handler-bind, restart-case            │
│                                                                 │
│  Layer 3: Lisp Libraries (loaded on demand)                     │
│  ├── io.lisp            - File operations wrappers              │
│  ├── llm.lisp           - LLM interaction helpers               │
│  └── test.lisp          - Testing framework                     │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

---

## Layer 0: TypeScript Foundation

These files are hand-written TypeScript. They form the irreducible kernel.

### Build Order

```typescript
// build/layer0.ts

const LAYER0_FILES = [
  // Order matters - dependencies first
  'src/types.ts',         // No dependencies
  'src/reader.ts',        // Depends on types
  'src/env.ts',           // Depends on types
  'src/continuation.ts',  // Depends on types
  'src/condition.ts',     // Depends on types, continuation
  'src/eval.ts',          // Depends on all above
  'src/ffi.ts',           // Depends on types
];

async function buildLayer0(): Promise<void> {
  for (const file of LAYER0_FILES) {
    await transpile(file);
  }
}
```

### Minimal API

Layer 0 exports exactly these functions:

```typescript
// The public API from Layer 0
export {
  // Reader
  read,
  readAll,

  // Types
  Value,
  List,
  Closure,
  isSymbol,
  isList,

  // Environment
  Environment,
  createEnvironment,

  // Evaluator
  evalExpr,

  // Continuation
  Continuation,
  applyCont,

  // FFI
  FFI,
  registerFFI,
};
```

---

## Layer 1: TypeScript Primitives

Primitives that are easier to write in TypeScript than Lisp.

```typescript
// stdlib/base.ts

export const primitives: Map<string, Primitive> = new Map([
  // Arithmetic
  ['+', prim((...args) => args.reduce((a, b) => a + b, 0))],
  ['-', prim((a, b) => b === undefined ? -a : a - b)],
  ['*', prim((...args) => args.reduce((a, b) => a * b, 1))],
  ['/', prim((a, b) => a / b)],
  ['mod', prim((a, b) => a % b)],

  // Comparison
  ['=', prim((a, b) => a === b)],
  ['<', prim((a, b) => a < b)],
  ['>', prim((a, b) => a > b)],
  ['<=', prim((a, b) => a <= b)],
  ['>=', prim((a, b) => a >= b)],

  // Predicates
  ['null?', prim(x => x === null || (Array.isArray(x) && x.length === 0))],
  ['number?', prim(x => typeof x === 'number')],
  ['string?', prim(x => typeof x === 'string')],
  ['symbol?', prim(x => typeof x === 'symbol')],
  ['list?', prim(x => Array.isArray(x))],
  ['procedure?', prim(x => isProcedure(x))],

  // List primitives
  ['cons', prim((h, t) => [h, ...(t || [])])],
  ['car', prim(l => l[0])],
  ['cdr', prim(l => l.slice(1))],
  ['list', prim((...args) => args)],
  ['length', prim(l => l.length)],
  ['nth', prim((l, n) => l[n])],
  ['append', prim((...lists) => lists.flat())],

  // String primitives
  ['str', prim((...args) => args.map(String).join(''))],
  ['string-length', prim(s => s.length)],
  ['substring', prim((s, start, end) => s.substring(start, end))],
  ['string-split', prim((s, sep) => s.split(sep))],
  ['string-join', prim((parts, sep) => parts.join(sep))],

  // Control
  ['apply', prim((fn, args) => applyFn(fn, args))],
  ['error', prim(msg => { throw new Error(msg); })],

  // Meta
  ['gensym', prim((prefix = 'g') => Symbol(prefix + gensymCounter++))],
  ['typeof', prim(x => typeOf(x))],
]);
```

---

## Layer 2: Lisp Foundation

Once Layer 0 and 1 are loaded, we can load Lisp code.

### core.lisp

```lisp
;; core.lisp - Loaded at boot time

;; Control flow macros
(defmacro when (test . body)
  `(if ,test (begin ,@body) nil))

(defmacro unless (test . body)
  `(if ,test nil (begin ,@body)))

(defmacro cond clauses
  (if (null? clauses)
      nil
      (let ((clause (car clauses)))
        (if (eq? (car clause) 'else)
            `(begin ,@(cdr clause))
            `(if ,(car clause)
                 (begin ,@(cdr clause))
                 (cond ,@(cdr clauses)))))))

(defmacro case (key . clauses)
  (let ((keyvar (gensym 'key)))
    `(let ((,keyvar ,key))
       (cond ,@(map (lambda (clause)
                      (if (eq? (car clause) 'else)
                          clause
                          `((member ,keyvar ',(car clause))
                            ,@(cdr clause))))
                    clauses)))))

;; Boolean operations
(defmacro and args
  (cond
    ((null? args) #t)
    ((null? (cdr args)) (car args))
    (else `(if ,(car args) (and ,@(cdr args)) #f))))

(defmacro or args
  (cond
    ((null? args) #f)
    ((null? (cdr args)) (car args))
    (else (let ((temp (gensym 'or)))
            `(let ((,temp ,(car args)))
               (if ,temp ,temp (or ,@(cdr args))))))))

;; Let variants
(defmacro let* (bindings . body)
  (if (null? bindings)
      `(begin ,@body)
      `(let (,(car bindings))
         (let* ,(cdr bindings) ,@body))))

(defmacro letrec (bindings . body)
  `(let ,(map (lambda (b) (list (car b) nil)) bindings)
     ,@(map (lambda (b) `(set! ,(car b) ,(cadr b))) bindings)
     ,@body))
```

### list.lisp

```lisp
;; list.lisp - Higher-order list operations

(define (map fn lst)
  (if (null? lst)
      '()
      (cons (fn (car lst))
            (map fn (cdr lst)))))

(define (filter pred lst)
  (cond
    ((null? lst) '())
    ((pred (car lst))
     (cons (car lst) (filter pred (cdr lst))))
    (else (filter pred (cdr lst)))))

(define (reduce fn init lst)
  (if (null? lst)
      init
      (reduce fn (fn init (car lst)) (cdr lst))))

(define (fold-right fn init lst)
  (if (null? lst)
      init
      (fn (car lst) (fold-right fn init (cdr lst)))))

(define (zip . lists)
  (if (any? null? lists)
      '()
      (cons (map car lists)
            (apply zip (map cdr lists)))))

(define (partition pred lst)
  (list (filter pred lst)
        (filter (lambda (x) (not (pred x))) lst)))

(define (take n lst)
  (if (or (= n 0) (null? lst))
      '()
      (cons (car lst) (take (- n 1) (cdr lst)))))

(define (drop n lst)
  (if (or (= n 0) (null? lst))
      lst
      (drop (- n 1) (cdr lst))))

(define (flatten lst)
  (cond
    ((null? lst) '())
    ((list? (car lst))
     (append (flatten (car lst)) (flatten (cdr lst))))
    (else (cons (car lst) (flatten (cdr lst))))))
```

### macro.lisp

```lisp
;; macro.lisp - Macro system implementation

;; Quasiquote expansion (compile-time)
;; This is implemented in the reader/expander, but here's the logic:

(define (expand-quasiquote x depth)
  (cond
    ;; Base atoms
    ((not (list? x)) `(quote ,x))

    ;; Empty list
    ((null? x) '(quote ()))

    ;; Unquote at depth 1
    ((and (= depth 1) (eq? (car x) 'unquote))
     (cadr x))

    ;; Unquote-splicing at depth 1
    ((and (= depth 1) (list? (car x)) (eq? (caar x) 'unquote-splicing))
     `(append ,(cadar x) ,(expand-quasiquote (cdr x) depth)))

    ;; Nested quasiquote
    ((eq? (car x) 'quasiquote)
     `(list 'quasiquote ,(expand-quasiquote (cadr x) (+ depth 1))))

    ;; Nested unquote
    ((eq? (car x) 'unquote)
     `(list 'unquote ,(expand-quasiquote (cadr x) (- depth 1))))

    ;; General list
    (else
     `(cons ,(expand-quasiquote (car x) depth)
            ,(expand-quasiquote (cdr x) depth)))))
```

---

## Layer 3: Libraries

Loaded on demand via `require`.

```lisp
;; Example: llm.lisp

(ns lambdallm.llm
  (:require [lambdallm.core :refer [when unless]]
            [lambdallm.condition :refer [handler-bind invoke-restart]]))

(define (complete prompt #:system nil #:model nil)
  "Complete prompt using LLM."
  (llm.complete prompt system model))

(define (complete-with-retry prompt #:max-retries 3)
  "Complete with automatic retry on failure."
  (handler-bind
    ((llm-error
      (lambda (c)
        (if (< (condition-attempts c) max-retries)
            (invoke-restart 'retry)
            (invoke-restart 'use-fallback nil)))))
    (complete prompt)))
```

---

## Build Process

### Development Build

```bash
# Install dependencies
bun install

# Build Layer 0 (TypeScript → JavaScript)
bun run build:layer0

# Run tests for Layer 0
bun test src/

# Build full runtime (includes loading Layer 2 Lisp)
bun run build

# Result: dist/lambdallm.js (single file)
```

### Production Build

```bash
# Optimized build
bun run build:prod

# Creates:
# - dist/lambdallm.min.js (browser, ~80KB gzipped)
# - dist/lambdallm-cli (standalone binary)
# - dist/lambdallm-node.mjs (Node.js ESM)
```

### Build Script

```typescript
// build/index.ts

import { build } from 'bun';

async function buildAll() {
  // Layer 0: TypeScript
  await buildLayer0();

  // Layer 1: Bundle primitives
  await bundlePrimitives();

  // Layer 2: Compile Lisp to embedded strings
  const lispSources = await compileLispSources([
    'lib/core.lisp',
    'lib/list.lisp',
    'lib/macro.lisp',
    'lib/condition.lisp',
  ]);

  // Bundle everything
  await build({
    entrypoints: ['src/index.ts'],
    outdir: 'dist',
    define: {
      'LISP_SOURCES': JSON.stringify(lispSources),
    },
    minify: process.env.NODE_ENV === 'production',
  });
}
```

---

## Runtime Initialization

```typescript
// src/runtime.ts

export async function createRuntime(config: RuntimeConfig = {}): Promise<Runtime> {
  // Layer 0: Create evaluator infrastructure
  const ffi = new FFI();
  const env = createBaseEnvironment();

  // Layer 1: Install TypeScript primitives
  for (const [name, prim] of basePrimitives) {
    env.define(name, prim);
  }

  // Layer 2: Load Lisp foundation
  const foundationSources = [
    LISP_SOURCES.core,
    LISP_SOURCES.list,
    LISP_SOURCES.macro,
    LISP_SOURCES.condition,
  ];

  for (const source of foundationSources) {
    const exprs = readAll(source);
    for (const expr of exprs) {
      await evalExpr(expr, env, initialContinuation(), ffi);
    }
  }

  // Register FFI handlers
  setupFFI(ffi, config.world, config.llm);

  return new Runtime(env, ffi, config);
}
```

---

## Self-Hosting Path

Long-term goal: More of the system written in LambdaLLM itself.

```
Current:    TypeScript 70% / Lisp 30%
Target:     TypeScript 30% / Lisp 70%

What moves to Lisp:
- Macro expander (currently TypeScript)
- Module loader
- REPL implementation
- Debugger
- Formatter
- Linter

What stays in TypeScript:
- Reader (performance critical)
- Evaluator (small, stable)
- Continuation machinery
- FFI bridge
```

---

## Development Workflow

```
┌─────────────────────────────────────────────────────────────────┐
│  Edit                                                           │
│  ├── .ts files → Rebuild Layer 0 → Hot reload                  │
│  └── .lisp files → Just reload file → Instant                  │
│                                                                 │
│  Test                                                           │
│  ├── bun test        → TypeScript tests                        │
│  └── llm test        → Lisp tests (test.lisp framework)        │
│                                                                 │
│  Debug                                                          │
│  ├── VS Code         → Full debugger                           │
│  └── REPL            → (debug) mode with time-travel           │
└─────────────────────────────────────────────────────────────────┘
```

---

## Bootstrapping New Features

Adding a new feature follows this pattern:

```
1. Prototype in Lisp (fast iteration)
   (define (new-feature x)
     ...)

2. Test thoroughly in REPL
   > (new-feature 'test)

3. Decide: Keep in Lisp or move to TypeScript?
   - Performance critical → TypeScript primitive
   - Needs JS interop → TypeScript primitive
   - Everything else → Keep in Lisp

4. If moving to TypeScript:
   - Write tests first
   - Implement in stdlib/*.ts
   - Export from primitives
   - Verify Lisp code still works
```
