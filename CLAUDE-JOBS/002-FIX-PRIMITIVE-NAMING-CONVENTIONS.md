# JOB-002: Fix Primitive Naming Conventions - Non-SICP Compliant Names

**Priority**: P1 - Important
**Estimated Effort**: 1-2 hours
**Skills Required**: TypeScript, grep/sed familiarity
**Status**: ✅ DONE (2026-01-19)

> **Completed**: All 17 non-compliant primitives renamed to Scheme-standard hyphenated names.
> All 1124 tests pass.

---

## Executive Summary

17 primitives used `/` in their names (e.g., `machine/step`, `rewrite/once`), which is **NOT valid in standard Scheme/SICP**. In Scheme, `/` is the division operator, so `machine/step` would parse as `(/ machine step)`.

The fact that these worked reveals OmegaLLM's reader was **Clojure-influenced** (where `namespace/symbol` is valid), not strict SICP. They have now been renamed to use hyphens for SICP compliance.

---

## Root Cause Analysis

### Why `/` Was Used

The developers used `/` to **mirror directory structure**:
```
src/machine/step.ts    → primitive "machine/step"
src/machine/state.ts   → primitive "machine/new"
src/rewrite/...        → primitives "rewrite/*"
```

This is documented in [ARCHITECTURE/32-LANGUAGE-OFFICIAL-6.md](../ARCHITECTURE/32-LANGUAGE-OFFICIAL-6.md):
```
* `machine/*` = **Interpreter** (GoF) realized as an abstract machine
```

### The Problem

| Language | How `machine/step` Parses | Valid? |
|----------|---------------------------|--------|
| **Scheme/SICP** | `(/ machine step)` - division | ❌ NO |
| **Clojure** | Symbol `machine/step` | ✅ YES |
| **Common Lisp** | Symbol `machine/step` | ⚠️ Usually yes |
| **OmegaLLM** | Symbol `machine/step` | ✅ Works (Clojure-style) |

OmegaLLM claims SICP heritage but has Clojure-style reader.

### Non-Compliant Primitives (17 total)

**Using `/` instead of `-`** (16 primitives):
```
machine/new          → machine-new
machine?             → (OK - predicate)
machine/step         → machine-step
machine/run          → machine-run
machine/done?        → machine-done?
machine/value        → machine-value
machine/control      → machine-control
machine/stack        → machine-stack
machine/step-count   → machine-step-count
machine/fork         → machine-fork
machine/resume       → machine-resume
machine/add-breakpoint → machine-add-breakpoint
machine/last-op      → machine-last-op
rewrite/once         → rewrite-once
rewrite/fixpoint     → rewrite-fixpoint
rewrite/trace        → rewrite-trace
rewrite/conflicts    → rewrite-conflicts
```

**Using double underscore** (1 primitive):
```
__uninit             → *uninit* or %uninit
```

---

## The Fix

### Step 1: Update Primitive Definitions in `src/core/prims.ts`

**File**: [src/core/prims.ts](../src/core/prims.ts)

Find and replace all `/` with `-` in primitive names:

```typescript
// BEFORE
def("machine/new", { ... });
def("machine/step", { ... });
def("rewrite/once", { ... });
def("rewrite/fixpoint", { ... });

// AFTER
def("machine-new", { ... });
def("machine-step", { ... });
def("rewrite-once", { ... });
def("rewrite-fixpoint", { ... });
```

Also fix `__uninit`:
```typescript
// BEFORE
def("__uninit", { ... });

// AFTER
def("*uninit*", { ... });  // Common Lisp style for special values
// OR
def("%uninit", { ... });   // Alternative convention
```

### Step 2: Update Compiler Primitive List in `compileText.ts`

**File**: [src/core/pipeline/compileText.ts](../src/core/pipeline/compileText.ts)

Update the `prims` array in `initialEnv()`:

```typescript
// BEFORE (lines 117-122)
"rewrite/once", "rewrite/fixpoint", "rewrite/trace", "rewrite/conflicts",
"machine/new", "machine?", "machine/step", "machine/run", "machine/done?",
"machine/value", "machine/control", "machine/stack", "machine/step-count",
"machine/fork", "machine/resume", "machine/add-breakpoint", "machine/last-op",

// AFTER
"rewrite-once", "rewrite-fixpoint", "rewrite-trace", "rewrite-conflicts",
"machine-new", "machine?", "machine-step", "machine-run", "machine-done?",
"machine-value", "machine-control", "machine-stack", "machine-step-count",
"machine-fork", "machine-resume", "machine-add-breakpoint", "machine-last-op",
```

### Step 3: Update Any Tests Using These Primitives

Search for usages:
```bash
grep -r "machine/" test/ --include="*.ts" --include="*.lisp"
grep -r "rewrite/" test/ --include="*.ts" --include="*.lisp"
```

Update test files to use new names.

### Step 4: Update Documentation

Files that reference these primitives:
- [INTEGRATION-ARCHITECTURE.md](../INTEGRATION-ARCHITECTURE.md) - mentions `machine/step`, `rewrite/once`
- [docs/LAMBDA-LLM--OMEGA-LLM-FEATURES.md](../docs/LAMBDA-LLM--OMEGA-LLM-FEATURES.md)
- Any ARCHITECTURE docs

---

## Files to Modify

| File | Changes |
|------|---------|
| [src/core/prims.ts](../src/core/prims.ts) | Rename 17 primitives |
| [src/core/pipeline/compileText.ts](../src/core/pipeline/compileText.ts) | Update prims array |
| [test/helpers/prims.ts](../test/helpers/prims.ts) | Re-exports, should auto-update |
| [INTEGRATION-ARCHITECTURE.md](../INTEGRATION-ARCHITECTURE.md) | Update docs |
| Test files (TBD) | Update any usages |

---

## Verification Steps

1. **Run all tests**:
   ```bash
   npm test
   ```
   All 1124 tests should pass.

2. **Verify primitives are renamed**:
   ```bash
   grep "machine/" src/core/prims.ts      # Should return nothing
   grep "rewrite/" src/core/prims.ts      # Should return nothing
   grep "machine-" src/core/prims.ts      # Should show new names
   ```

3. **Test in REPL**:
   ```lisp
   (define m (machine-new '(+ 1 2)))
   (machine-step m)
   (machine-done? m)
   ```

---

## Naming Convention Reference

### SICP/Scheme Conventions
- **Kebab-case**: `stream-car`, `string-append`, `call-with-current-continuation`
- **Predicates**: end with `?` - `null?`, `pair?`, `list?`
- **Mutators**: end with `!` - `set!`, `set-car!`
- **Conversions**: use `->` - `list->vector`, `string->symbol`
- **Special values**: varies - `#f`, `#t`, `'()`, `the-empty-stream`

### What's NOT Valid in Scheme
- `/` in names (it's division)
- CamelCase
- Double underscores (more C/Python)

### Recommended Renames

| Current (Invalid) | Recommended (SICP-style) |
|-------------------|--------------------------|
| `machine/new` | `machine-new` or `make-machine` |
| `machine/step` | `machine-step` |
| `machine/run` | `machine-run` or `run-machine` |
| `machine/done?` | `machine-done?` ✓ |
| `machine/value` | `machine-value` |
| `machine/control` | `machine-control` |
| `machine/stack` | `machine-stack` |
| `machine/step-count` | `machine-step-count` |
| `machine/fork` | `machine-fork` or `fork-machine` |
| `machine/resume` | `machine-resume` |
| `machine/add-breakpoint` | `machine-add-breakpoint` |
| `machine/last-op` | `machine-last-op` |
| `rewrite/once` | `rewrite-once` |
| `rewrite/fixpoint` | `rewrite-fixpoint` |
| `rewrite/trace` | `rewrite-trace` |
| `rewrite/conflicts` | `rewrite-conflicts` |
| `__uninit` | `*uninit*` or `%uninit` |

---

## Checklist

- [ ] Update `src/core/prims.ts` - rename all 17 primitives
- [ ] Update `src/core/pipeline/compileText.ts` - update prims array
- [ ] Search and update test files
- [ ] Update INTEGRATION-ARCHITECTURE.md
- [ ] Update any other docs with old names
- [ ] Run full test suite
- [ ] Test in REPL manually
- [ ] Commit changes

---

## Notes

### Why Not Keep `/` Style?

1. **SICP Compatibility**: OmegaLLM spec references SICP heavily
2. **Parser Confusion**: `/` is division in standard Lisps
3. **Portability**: Code using `machine/step` won't run in real Scheme
4. **Consistency**: Rest of codebase uses hyphen-style

### Alternative: Implement Real Namespaces

Instead of renaming, could implement Clojure-style namespaces:
```lisp
(ns machine)
(define (step m) ...)

;; Then use as:
(machine/step m)  ; Clojure-style qualified reference
```

But this is a larger change. Simple renaming is the minimal fix.

---

*Created: 2026-01-19*
*Related: [001-FIX-PRODUCTION-PRIMITIVES.md](./001-FIX-PRODUCTION-PRIMITIVES.md)*
