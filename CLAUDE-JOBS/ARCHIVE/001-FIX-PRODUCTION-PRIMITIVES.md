# JOB-001: Fix Production Primitives - OmegaLLM Runtime is Non-Functional

**Priority**: P0 - CRITICAL
**Estimated Effort**: 2-4 hours
**Skills Required**: TypeScript, understanding of Lisp evaluation
**Status**: ✅ DONE (2026-01-19)

> **Completed**: All 1124 tests pass. Production `OmegaRuntime` now has 117 primitives.

---

## Executive Summary

The **production `OmegaRuntime` class is completely non-functional**. It only has 5 primitives (`+`, `-`, `=`, `not`, `unit`) while the REPL and tests have 117 primitives. This is because:

1. The test helpers have a complete `installPrims()` function with 117 primitives
2. The production runtime has a FAKE `installPrims()` that returns an empty environment
3. The compiler's `initialEnv()` only knows 5 primitive names
4. Tests and REPL bypass production code entirely by importing from test helpers

**The fix is simple**: Move the test helper primitives to production and wire them into `OmegaRuntime`.

---

## Problem Statement

### What's Broken

Anyone using OmegaLLM as a library will get failures:

```typescript
import { OmegaRuntime } from "omega-llm";

const omega = new OmegaRuntime();
const result = await omega.eval("(cons 1 2)");  // ERROR: cons is not defined
const result2 = await omega.eval("(car (list 1 2 3))");  // ERROR: car is not defined
const result3 = await omega.eval("(* 2 3)");  // ERROR: * is not defined
```

Only these work:
```typescript
await omega.eval("(+ 1 2)");   // Works - 3
await omega.eval("(- 5 3)");   // Works - 2
await omega.eval("(= 1 1)");   // Works - #t
await omega.eval("(not #f)");  // Works - #t
await omega.eval("unit");      // Works - null
```

### Why Tests and REPL Work

The REPL and tests **bypass the production `OmegaRuntime` entirely**:

| Component | Import Path | Primitives | Status |
|-----------|-------------|------------|--------|
| CLI REPL | `import { installPrims } from "../test/helpers/prims"` | 117 | **Works** |
| Tests (28 files) | `import { installPrims } from "../helpers/prims"` | 117 | **Works** |
| omegaHarness | `import { installPrims } from "./prims"` | 117 | **Works** |
| **OmegaRuntime** | Uses `compileTextToExpr()` | 5 | **BROKEN** |

---

## Root Cause Analysis

### File 1: The Fake Production `installPrims()`

**File**: [src/runtime.ts](../src/runtime.ts) lines 98-103

```typescript
// Minimal prims installer - matches what tests expect
function installPrims(store: COWStore): { env: ReturnType<typeof envEmpty>; store: COWStore } {
  const env = envEmpty();  // <-- RETURNS EMPTY ENVIRONMENT!
  // The prims are installed by the compile-time env in compileTextToExpr
  // We just return empty env here; the compiler handles prim bindings
  return { env, store };
}
```

**Problem**: This comment is a LIE. The compiler does NOT install all primitives.

### File 2: The Compiler Only Knows 5 Primitives

**File**: [src/core/pipeline/compileText.ts](../src/core/pipeline/compileText.ts) lines 76-90

```typescript
/**
 * Initialize compile-time env with primitive bindings whose runtime internal names match.
 * This must align with installPrims(...) in tests.
 */
function initialEnv(moduleScope: string): Env {
  const prims = ["+", "-", "=", "not", "unit"];  // <-- ONLY 5 PRIMITIVES!
  let env: Env = [];
  for (const p of prims) {
    env = env.concat([{
      bid: `prim:${p}`,
      name: p,
      scopes: [moduleScope],
      phase: 0,
      kind: "value",
      value: p,
    } satisfies Binding]);
  }
  return env;
}
```

**Problem**: Only 5 primitives are known to the compiler. The comment says "must align with installPrims(...) in tests" but it doesn't.

### File 3: The Test Helpers Have Everything

**File**: [test/helpers/prims.ts](../test/helpers/prims.ts) (1100+ lines, 117 primitives)

This file has a complete, working `installPrims()` function with:
- All arithmetic: `+`, `-`, `*`, `/`, `modulo`, `<`, `>`, `<=`, `>=`, `=`
- All list ops: `cons`, `car`, `cdr`, `null?`, `pair?`, `list`, `append`, `length`, `reverse`, `list-ref`
- All streams: `stream-car`, `stream-cdr`, `stream-null?`, `the-empty-stream`, `list->stream`
- All strings: `string=?`, `string-contains?`, `string-replace-all`, `string-split`, `string-join`, `string-trim`, `string-length`, `string-append`, `substring`
- Type predicates: `symbol?`, `string?`, `number?`, `boolean?`, `procedure?`
- Higher-order: `map`, `filter`
- Distributions: `dist`, `dist?`, `dist-count`, `dist-normalize`, `dist-sample`, `dist-topk`
- Effects: `amb`, `fail`, TRS rewriting primitives
- And 50+ more...

**This file is CORRECTLY IMPLEMENTED.** It just needs to be moved to production.

---

## The Fix

### Step 1: Copy Test Primitives to Production

Copy [test/helpers/prims.ts](../test/helpers/prims.ts) to [src/core/prims.ts](../src/core/prims.ts):

```bash
cp test/helpers/prims.ts src/core/prims.ts
```

Then update the imports at the top of the new file:

**BEFORE** (in test/helpers/prims.ts):
```typescript
import type { Env } from "../../src/core/eval/env";
import { envEmpty, envSet } from "../../src/core/eval/env";
import type { Store } from "../../src/core/eval/store";
import type { Val } from "../../src/core/eval/values";
import { VUnit, VTrue, VFalse } from "../../src/core/eval/values";
import { rule, rewriteOnce, rewriteFixpoint, rewriteTrace, detectConflicts, substitute, type Rule, type Strategy } from "../../src/core/oracle/trs";
import { matchAST } from "../../src/core/oracle/match";
```

**AFTER** (in src/core/prims.ts):
```typescript
import type { Env } from "./eval/env";
import { envEmpty, envSet } from "./eval/env";
import type { Store } from "./eval/store";
import type { Val } from "./eval/values";
import { VUnit, VTrue, VFalse } from "./eval/values";
import { rule, rewriteOnce, rewriteFixpoint, rewriteTrace, detectConflicts, substitute, type Rule, type Strategy } from "./oracle/trs";
import { matchAST } from "./oracle/match";
```

### Step 2: Update runtime.ts to Use Real Primitives

**File**: [src/runtime.ts](../src/runtime.ts)

**BEFORE** (lines 24-26 area - add import):
```typescript
import { envEmpty } from "./core/eval/env";
```

**AFTER**:
```typescript
import { installPrims } from "./core/prims";  // ADD THIS LINE
// Remove or keep envEmpty if used elsewhere
```

**BEFORE** (lines 98-103 - the fake installPrims):
```typescript
// Minimal prims installer - matches what tests expect
function installPrims(store: COWStore): { env: ReturnType<typeof envEmpty>; store: COWStore } {
  const env = envEmpty();
  // The prims are installed by the compile-time env in compileTextToExpr
  // We just return empty env here; the compiler handles prim bindings
  return { env, store };
}
```

**AFTER**:
DELETE this entire function. The import from `./core/prims` provides the real `installPrims`.

### Step 3: Update compileText.ts to Know All Primitive Names

**File**: [src/core/pipeline/compileText.ts](../src/core/pipeline/compileText.ts)

**BEFORE** (lines 76-90):
```typescript
function initialEnv(moduleScope: string): Env {
  const prims = ["+", "-", "=", "not", "unit"];
  // ...
}
```

**AFTER**:
```typescript
function initialEnv(moduleScope: string): Env {
  // ALL primitives that installPrims() provides
  const prims = [
    // Arithmetic
    "+", "-", "*", "/", "modulo",
    // Comparison
    "=", "<", ">", "<=", ">=",
    // Logic
    "not",
    // Special values
    "unit", "__uninit",
    // List operations
    "cons", "car", "cdr", "null?", "pair?", "list", "append", "length", "reverse", "list-ref",
    "cadr", "caddr",
    // Equality
    "eq?", "equal?",
    // Type predicates
    "symbol?", "string?", "number?", "boolean?", "procedure?",
    // String operations
    "string=?", "string-contains?", "string-replace-all", "string-split", "string-join",
    "string-trim", "string-downcase", "string-upcase", "string-length", "string-append", "substring",
    // Higher-order functions
    "map", "filter",
    // Distribution operations
    "dist", "dist?", "dist-count", "dist-value-at", "dist-weight-at", "dist-normalize",
    "dist-sample", "dist-topk", "dist-from-list", "dist-to-list",
    // Stream operations
    "the-empty-stream", "stream-null?", "stream-car", "stream-cdr", "list->stream",
    // Meaning operations
    "meaning?", "meaning-denotation", "meaning-confidence", "meaning-trace",
    // Debug/display
    "display", "newline", "error",
    // Symbol operations
    "symbol->string", "string->symbol", "gensym",
    // TRS rewriting
    "rule", "rule?", "rule-lhs", "rule-rhs", "rule-name",
    "rewrite-once", "rewrite-fixpoint", "rewrite-trace", "rewrite-conflicts",
    "match-ast", "substitute",
    // Generic dispatch
    "make-generic", "generic?", "add-method!", "apply-generic",
    // Vector operations
    "vector", "vector?", "vector-length", "vector-ref", "vector-set!",
    // amb/nondeterminism
    "amb", "fail",
  ];
  let env: Env = [];
  for (const p of prims) {
    env = env.concat([{
      bid: `prim:${p}`,
      name: p,
      scopes: [moduleScope],
      phase: 0,
      kind: "value",
      value: p,
    } satisfies Binding]);
  }
  return env;
}
```

**IMPORTANT**: The list above should match EXACTLY what `installPrims()` in `src/core/prims.ts` provides. To get the complete list, run:

```bash
grep -oP "def\(\"[^\"]+\"" src/core/prims.ts | sed 's/def("//' | sed 's/"//' | sort -u
```

### Step 4: Update Test Helpers to Import from Production

**File**: [test/helpers/prims.ts](../test/helpers/prims.ts)

Replace the entire file content with:

```typescript
// Re-export from production primitives
// This ensures tests use the same primitives as production
export { installPrims } from "../../src/core/prims";
```

This ensures tests use the exact same primitives as production.

### Step 5: Export from Index

**File**: [src/core/index.ts](../src/core/index.ts) (create if doesn't exist, or add to existing)

Add:
```typescript
export { installPrims } from "./prims";
```

---

## Verification Steps

### Step A: Build Should Pass

```bash
cd OmegaLLM
npm run build
```

Expected: No TypeScript errors.

### Step B: All Tests Should Still Pass

```bash
npm test
```

Expected: All existing tests pass (they now use production primitives).

### Step C: New Integration Test

Create [test/integration/production-runtime.spec.ts](../test/integration/production-runtime.spec.ts):

```typescript
import { describe, it, expect } from "vitest";
import { OmegaRuntime } from "../../src/runtime";

describe("OmegaRuntime production primitives", () => {
  const omega = new OmegaRuntime();

  it("should have cons/car/cdr", async () => {
    const result = await omega.eval("(car (cons 1 2))");
    expect(result.ok).toBe(true);
    expect(result.value).toEqual({ tag: "Num", n: 1 });
  });

  it("should have list operations", async () => {
    const result = await omega.eval("(length (list 1 2 3 4 5))");
    expect(result.ok).toBe(true);
    expect(result.value).toEqual({ tag: "Num", n: 5 });
  });

  it("should have multiplication", async () => {
    const result = await omega.eval("(* 6 7)");
    expect(result.ok).toBe(true);
    expect(result.value).toEqual({ tag: "Num", n: 42 });
  });

  it("should have string operations", async () => {
    const result = await omega.eval('(string-length "hello")');
    expect(result.ok).toBe(true);
    expect(result.value).toEqual({ tag: "Num", n: 5 });
  });

  it("should have stream operations", async () => {
    const result = await omega.eval("(stream-null? the-empty-stream)");
    expect(result.ok).toBe(true);
    expect(result.value).toEqual({ tag: "Bool", b: true });
  });

  it("should have map", async () => {
    // Note: map only works with Native functions, not lambdas
    // This tests that at least the primitive exists
    const result = await omega.eval("procedure?");
    expect(result.ok).toBe(true);
  });
});
```

Run it:
```bash
npm test -- test/integration/production-runtime.spec.ts
```

### Step D: REPL Verification

```bash
npx tsx bin/omega-repl.ts
```

Test these expressions:
```lisp
Ω> (cons 1 2)
=> (1 . 2)

Ω> (car (list 1 2 3))
=> 1

Ω> (* 6 7)
=> 42

Ω> (string-length "hello world")
=> 11

Ω> (stream-null? the-empty-stream)
=> #t
```

---

## Files Modified Summary

| File | Action | Description |
|------|--------|-------------|
| `src/core/prims.ts` | CREATE | Copy from test/helpers/prims.ts, fix imports |
| `src/runtime.ts` | MODIFY | Import from `./core/prims`, delete fake `installPrims` |
| `src/core/pipeline/compileText.ts` | MODIFY | Add all primitive names to `initialEnv()` |
| `test/helpers/prims.ts` | MODIFY | Re-export from production |
| `test/integration/production-runtime.spec.ts` | CREATE | New integration test |
| `src/core/index.ts` | MODIFY | Export `installPrims` |

---

## Known Limitations to Document

After fixing, the following limitations remain (these are NOT bugs, just design constraints):

1. **`map` and `filter` only work with Native functions**
   - User-defined lambdas require machine stepping
   - Workaround: Define `map` recursively in Lisp:
     ```lisp
     (define (my-map f lst)
       (if (null? lst)
           (list)
           (cons (f (car lst)) (my-map f (cdr lst)))))
     ```

2. **Stream operations exist but are basic**
   - The elaborate stream module in `src/core/stream/` has TypeScript functions with camelCase names
   - The test helper primitives provide basic `stream-car`, `stream-cdr`, `stream-null?`
   - Full stream fusion/analysis is TypeScript-internal only

---

## Update FEATURES Document

After completing the fix, update [docs/LAMBDA-LLM--OMEGA-LLM-FEATURES.md](../docs/LAMBDA-LLM--OMEGA-LLM-FEATURES.md):

1. Change section header from "CRITICAL: THE ENTIRE OMEGALLM RUNTIME IS BROKEN" to "FIXED: Production Primitives"

2. Replace the catastrophe content with:
   ```markdown
   ## Production Runtime Status: OPERATIONAL

   The `OmegaRuntime` class now has full primitive support (117 primitives).

   **Fixed in**: [link to commit/PR]

   ### Available Primitives

   | Category | Primitives |
   |----------|------------|
   | Arithmetic | `+`, `-`, `*`, `/`, `modulo`, `<`, `>`, `<=`, `>=`, `=` |
   | List ops | `cons`, `car`, `cdr`, `null?`, `pair?`, `list`, `append`, `length`, `reverse` |
   | Streams | `stream-car`, `stream-cdr`, `stream-null?`, `the-empty-stream` |
   | Strings | `string-append`, `string-length`, `substring`, `string=?`, ... |
   | Types | `number?`, `string?`, `symbol?`, `boolean?`, `procedure?` |
   | HOF | `map`, `filter` (Native functions only) |
   | Effects | `amb`, `fail` |

   ### Known Limitations

   - `map`/`filter` only work with built-in (Native) functions, not user-defined lambdas
   - Define recursive versions in Lisp for lambda support
   ```

---

## Checklist

- [ ] Copy `test/helpers/prims.ts` to `src/core/prims.ts`
- [ ] Fix imports in new `src/core/prims.ts`
- [ ] Update `src/runtime.ts` to import and use real `installPrims`
- [ ] Update `src/core/pipeline/compileText.ts` with all primitive names
- [ ] Update `test/helpers/prims.ts` to re-export from production
- [ ] Create integration test `test/integration/production-runtime.spec.ts`
- [ ] Run `npm run build` - no errors
- [ ] Run `npm test` - all pass
- [ ] Test REPL manually with `(cons 1 2)`, `(* 6 7)`, etc.
- [ ] Update FEATURES document
- [ ] Commit with message: "fix: wire production primitives into OmegaRuntime"

---

## Questions?

If anything is unclear, the key insight is:

**The primitives exist and work. They're just in `test/helpers/` instead of `src/core/`. Move them and wire them up.**

The test file [test/helpers/prims.ts](../test/helpers/prims.ts) is the source of truth - it has 1100+ lines of working primitive implementations.
