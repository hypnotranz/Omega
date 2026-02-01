# 200: Macro Manager

## Status: COMPLETE ✅
- **Completed**: 2026-01-29
- **Location**: OmegaLLM-3/src/runtime/subsystems/MacroManager.ts (312 lines)

## Purpose
Handles macro definition, expansion, and hygiene (gensym) for compile-time code transformations.

## Dependencies
- 100-types.md ✅

## Source References
- ARCHITECTURE/11-MACROS.md
- SICP Chapter 4 (metacircular evaluator)

---

## Deliverables

```
src/runtime/subsystems/
├── MacroManager.ts          # Main macro manager class
└── macros/
    ├── Macro.ts             # Macro type definition
    ├── Quasiquote.ts        # Quasiquote expansion
    └── Gensym.ts            # Hygienic symbol generation
```

---

## Key Interface

```typescript
export interface Macro {
  type: 'macro';
  name: string;
  params: string[];
  restParam?: string;
  body: Val;
  env: EnvSnapshot;
}

export interface MacroManager {
  /**
   * Define a new macro in the environment.
   */
  defineMacro(name: string, params: Val, body: Val, env: Environment): Macro;

  /**
   * Expand a macro call once (macroexpand-1).
   */
  expandOnce(expr: Val, env: Environment): Val;

  /**
   * Fully expand all macros recursively (macroexpand).
   */
  expandAll(expr: Val, env: Environment): Val;

  /**
   * Check if a value is a macro.
   */
  isMacro(value: Val): value is Macro;

  /**
   * Generate a unique symbol for hygiene.
   */
  gensym(prefix?: string): Sym;

  /**
   * Process quasiquote expression.
   * Handles `, (unquote) and ,@ (splice-unquote).
   */
  processQuasiquote(expr: Val, env: Environment): Val;
}
```

---

## Quasiquote Expansion

```typescript
/**
 * Quasiquote syntax:
 * `expr        - quote, but allow unquoting
 * ,expr        - unquote: evaluate and insert
 * ,@expr       - splice: evaluate and spread
 */
export function expandQuasiquote(expr: Val, env: Environment): Val {
  if (!isList(expr)) {
    return expr; // Atoms are unchanged
  }

  if (isUnquote(expr)) {
    return evalExpr(unquoteValue(expr), env);
  }

  if (isUnquoteSplice(expr)) {
    throw new Error('Splice ,@ not allowed outside list');
  }

  // Process list elements, handling splice
  const result: Val[] = [];
  for (const element of expr) {
    if (isUnquoteSplice(element)) {
      const spliced = evalExpr(unquoteSpliceValue(element), env);
      if (!isList(spliced)) {
        throw new Error('Splice ,@ must produce a list');
      }
      result.push(...spliced);
    } else {
      result.push(expandQuasiquote(element, env));
    }
  }
  return result;
}
```

---

## Gensym Implementation

```typescript
let gensymCounter = 0;

export function gensym(prefix: string = 'g'): Sym {
  return Symbol.for(`#:${prefix}${++gensymCounter}`);
}

// Reset for testing
export function resetGensym(): void {
  gensymCounter = 0;
}
```

---

## Macro Expansion Order

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
│  EXPANSION   │  ← MacroManager.expandAll()
└──────┬───────┘
       │
       ▼
┌──────────────┐
│  EVALUATOR   │  Evaluate expanded code
└──────────────┘
```

---

## Test Requirements

### Unit Tests: `tests/runtime/subsystems/MacroManager.test.ts`
- [ ] defineMacro() creates valid macro structure
- [ ] expandOnce() expands single macro call
- [ ] expandAll() recursively expands nested macros
- [ ] gensym() generates unique symbols
- [ ] gensym() resets correctly for testing
- [ ] processQuasiquote() handles unquote correctly
- [ ] processQuasiquote() handles splice-unquote correctly
- [ ] Macro body captures closure environment
- [ ] Rest params (&rest) work correctly
- [ ] Hygiene via gensym prevents variable capture

### Integration Tests
- [ ] `(defmacro when ...)` defines and expands correctly
- [ ] Nested macros expand in correct order
- [ ] Anaphoric macros (aif, awhen) work with gensym

---

## Common Macro Patterns

```lisp
;; Control flow
(defmacro when (test &rest body)
  `(if ,test (begin ,@body) nil))

(defmacro unless (test &rest body)
  `(if ,test nil (begin ,@body)))

;; Anaphoric (uses 'it')
(defmacro aif (test then &optional else)
  `(let ((it ,test))
     (if it ,then ,else)))

;; Resource management
(defmacro with-open-file ((var path) &rest body)
  (let ((result (gensym "result")))
    `(let ((,var (open-file ,path)))
       (let ((,result (begin ,@body)))
         (close-file ,var)
         ,result))))
```

---

## Acceptance Criteria
1. All standard macros (when, unless, and, or) work correctly
2. Quasiquote handles nested unquoting
3. Gensym prevents accidental variable capture
4. Macro expansion completes without infinite loops
5. Error messages identify macro expansion source
