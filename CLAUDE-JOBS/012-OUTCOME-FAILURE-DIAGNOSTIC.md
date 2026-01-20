# JOB-012: Implement Canonical Outcome/Failure/Diagnostic ADTs

**Priority**: P0 - Foundation (unifies error handling across all layers)
**Estimated Effort**: 2 days
**Skills Required**: TypeScript, algebraic data types, error handling patterns
**Status**: NOT STARTED
**Depends On**: [009-FRAMEIR-PACKAGE](./009-FRAMEIR-PACKAGE.md)

> **MUST READ**: [LOGISTICS.md](LOGISTICS.md) before starting - covers testing, file locations, and proof of completion requirements.

---

## Quick Start

```bash
# 1. Read the architecture spec
cat docs/ARCHITECTURE-LANGUAGES-2.md | grep -A 50 "§20"
cat docs/ARCHITECTURE-LANGUAGES-3.md | grep -A 50 "§33"

# 2. Create outcome files
mkdir -p src/outcome
touch src/outcome/{outcome,failure,diagnostic,codes,constructors,matchers,index}.ts

# 3. Run tests as you implement
npx vitest run test/outcome/

# 4. Verify type safety
npx tsc --noEmit
```

---

## Dependencies

| Type | Job | Reason |
|------|-----|--------|
| **REQUIRES** | [009-FRAMEIR-PACKAGE](./009-FRAMEIR-PACKAGE.md) | Needs Span type for diagnostic locations |
| **BLOCKS** | [011-PORT-ABSTRACTIONS](./011-PORT-ABSTRACTIONS.md) | Ports return Outcome types |
| **BLOCKS** | [013-LINT-PASSES](./013-LINT-PASSES.md) | Lint passes emit Diagnostics |
| **BLOCKS** | [014-COMPILATION-PIPELINE](./014-COMPILATION-PIPELINE.md) | Compiler returns Outcome |

---

## What Already Exists

### Current StepOutcome (machine-specific)
```typescript
// src/core/eval/machine.ts - existing outcome-like type
export type StepOutcome =
  | { type: 'continue' }
  | { type: 'done'; value: Value }
  | { type: 'suspend'; reason: string }
  | { type: 'error'; message: string };
```

### Current OracleResp (oracle-specific)
```typescript
// src/core/oracle/adapter.ts - existing result type
export type InferResult =
  | { ok: true; response: OracleResponse }
  | { ok: false; error: string };
```

### Thrown Errors (unstructured)
```typescript
// Various files - thrown errors with no structure
throw new Error("Budget exceeded");
throw new Error("Tool not found");
```

### Gap Analysis
| Existing | Missing |
|----------|---------|
| StepOutcome | Only used in machine, no Failure structure |
| InferResult | Ok/Error pattern, not canonical Outcome |
| Thrown errors | No diagnostic codes, no span, no cause chain |
| No Diagnostic type | Errors are just strings |
| No unified codes | Each layer invents its own error messages |

---

## Reference Documents

This job implements specifications from:

| Document | Sections | Topics |
|----------|----------|--------|
| [ARCHITECTURE-LANGUAGES-2.md](../docs/ARCHITECTURE-LANGUAGES-2.md) | §20-22 | Outcome type, Failure normalization |
| [ARCHITECTURE-LANGUAGES-3.md](../docs/ARCHITECTURE-LANGUAGES-3.md) | §33-34 | Effect kinds, diagnostic codes |

**Key Quote from §20**:
> "Outcome[A] = Done { value: A, meta: Meta } | Fail { failure: Failure, meta: Meta } | Pause { suspended: Suspended, meta: Meta }"

---

## Executive Summary

Implement the **canonical Outcome/Failure/Diagnostic** types that unify error handling across all layers:

1. **Outcome[A]** - The universal result type (Done | Fail | Pause)
2. **Failure** - Structured failure with reason, context, diagnostics
3. **Diagnostic** - Error/warning codes with structured data

Currently, OmegaLLM has implicit error handling scattered across:
- `OracleResp` (outcome-like but not canonical)
- `StepOutcome` (machine-specific)
- Thrown errors (unstructured)

This job unifies them into one canonical shape.

---

## Output Files

| File | Action | Description |
|------|--------|-------------|
| `src/outcome/index.ts` | **CREATE** | Package exports |
| `src/outcome/outcome.ts` | **CREATE** | Outcome ADT |
| `src/outcome/failure.ts` | **CREATE** | Failure type |
| `src/outcome/diagnostic.ts` | **CREATE** | Diagnostic type + code registry |
| `src/outcome/codes.ts` | **CREATE** | Error/warning code definitions |
| `src/outcome/constructors.ts` | **CREATE** | Helper constructors |
| `src/outcome/matchers.ts` | **CREATE** | Pattern matching utilities |
| `test/outcome/outcome.spec.ts` | **CREATE** | Outcome tests |

---

## Task 1: Define Outcome ADT

**Reference**: [ARCHITECTURE-LANGUAGES-2.md §20](../docs/ARCHITECTURE-LANGUAGES-2.md)

### 1.1 Create `src/outcome/outcome.ts`

```typescript
import { Failure } from "./failure";
import { Span } from "../frameir/meta";

/**
 * Metadata attached to outcomes.
 */
export interface OutcomeMeta {
  /** Span where outcome was produced */
  span?: Span;

  /** Execution time in ms */
  durationMs?: number;

  /** Budget consumed */
  budgetUsed?: {
    llmCalls?: number;
    tokens?: number;
    timeMs?: number;
    toolCalls?: number;
  };

  /** Provenance references */
  evidenceIds?: string[];
}

/**
 * Successful completion.
 */
export interface Done<A> {
  readonly tag: "Done";
  readonly value: A;
  readonly meta: OutcomeMeta;
}

/**
 * Failed with structured failure.
 */
export interface Fail {
  readonly tag: "Fail";
  readonly failure: Failure;
  readonly meta: OutcomeMeta;
}

/**
 * Suspended (for multi-turn, human-in-the-loop, etc.).
 */
export interface Pause<A> {
  readonly tag: "Pause";
  readonly suspended: Suspended<A>;
  readonly meta: OutcomeMeta;
}

/**
 * Suspension state for resumable computations.
 */
export interface Suspended<A> {
  /** Why computation paused */
  reason: SuspendReason;

  /** Continuation to resume with input */
  resume: (input: unknown) => Promise<Outcome<A>>;

  /** Optional state for serialization */
  state?: unknown;
}

export type SuspendReason =
  | { tag: "AwaitingHumanInput"; prompt: string }
  | { tag: "AwaitingApproval"; action: string }
  | { tag: "ResourceExhausted"; resource: string }
  | { tag: "Custom"; reason: string; data?: unknown };

/**
 * Universal result type.
 * All effectful computations return Outcome.
 */
export type Outcome<A> = Done<A> | Fail | Pause<A>;

// Type guards
export function isDone<A>(o: Outcome<A>): o is Done<A> {
  return o.tag === "Done";
}

export function isFail<A>(o: Outcome<A>): o is Fail {
  return o.tag === "Fail";
}

export function isPause<A>(o: Outcome<A>): o is Pause<A> {
  return o.tag === "Pause";
}
```

---

## Task 2: Define Failure Type

### 2.1 Create `src/outcome/failure.ts`

```typescript
import { Diagnostic } from "./diagnostic";

/**
 * Failure reason categories.
 */
export type FailureReason =
  // Resource failures
  | "budget-exceeded"
  | "timeout"
  | "rate-limited"

  // Validation failures
  | "validation-failed"
  | "schema-mismatch"
  | "type-error"

  // Oracle failures
  | "oracle-error"
  | "oracle-timeout"
  | "invalid-response"

  // Tool failures
  | "tool-error"
  | "tool-not-found"
  | "tool-contract-violation"

  // Control flow
  | "user-cancelled"
  | "precondition-failed"
  | "invariant-violated"

  // System
  | "internal-error"
  | "not-implemented"

  // Custom
  | `custom:${string}`;

/**
 * Structured failure.
 */
export interface Failure {
  /** Primary failure reason */
  reason: FailureReason;

  /** Human-readable message */
  message: string;

  /** Structured context data */
  context?: Record<string, unknown>;

  /** Associated diagnostics (errors/warnings) */
  diagnostics: Diagnostic[];

  /** Nested cause (for wrapping) */
  cause?: Failure;

  /** Whether this failure is recoverable */
  recoverable: boolean;

  /** Suggested recovery actions */
  suggestions?: string[];
}

/**
 * Create a failure.
 */
export function failure(
  reason: FailureReason,
  message: string,
  opts?: Partial<Omit<Failure, "reason" | "message">>
): Failure {
  return {
    reason,
    message,
    diagnostics: opts?.diagnostics ?? [],
    recoverable: opts?.recoverable ?? false,
    context: opts?.context,
    cause: opts?.cause,
    suggestions: opts?.suggestions
  };
}

/**
 * Wrap a failure with additional context.
 */
export function wrapFailure(inner: Failure, message: string, context?: Record<string, unknown>): Failure {
  return {
    ...inner,
    message,
    context: { ...inner.context, ...context },
    cause: inner
  };
}

/**
 * Check if failure is of a specific reason.
 */
export function isFailureReason(f: Failure, reason: FailureReason): boolean {
  return f.reason === reason;
}

/**
 * Get all diagnostics including from cause chain.
 */
export function allDiagnostics(f: Failure): Diagnostic[] {
  const diags = [...f.diagnostics];
  if (f.cause) {
    diags.push(...allDiagnostics(f.cause));
  }
  return diags;
}
```

---

## Task 3: Define Diagnostic Type

**Reference**: [ARCHITECTURE-LANGUAGES-3.md §33](../docs/ARCHITECTURE-LANGUAGES-3.md)

### 3.1 Create `src/outcome/diagnostic.ts`

```typescript
import { Span } from "../frameir/meta";

/**
 * Diagnostic severity levels.
 */
export type DiagnosticSeverity = "error" | "warning" | "info" | "hint";

/**
 * Structured diagnostic (error or warning).
 */
export interface Diagnostic {
  /** Diagnostic code (e.g., "E0301") */
  code: string;

  /** Severity level */
  severity: DiagnosticSeverity;

  /** Human-readable message */
  message: string;

  /** Source location */
  span?: Span;

  /** Structured data */
  data?: Record<string, unknown>;

  /** Related diagnostics */
  related?: Diagnostic[];

  /** Suggested fixes */
  fixes?: DiagnosticFix[];
}

/**
 * Suggested fix for a diagnostic.
 */
export interface DiagnosticFix {
  /** Description of the fix */
  description: string;

  /** Replacement text (if applicable) */
  replacement?: string;

  /** Span to replace */
  span?: Span;
}

/**
 * Create an error diagnostic.
 */
export function errorDiag(
  code: string,
  message: string,
  opts?: Partial<Omit<Diagnostic, "code" | "message" | "severity">>
): Diagnostic {
  return {
    code,
    message,
    severity: "error",
    ...opts
  };
}

/**
 * Create a warning diagnostic.
 */
export function warnDiag(
  code: string,
  message: string,
  opts?: Partial<Omit<Diagnostic, "code" | "message" | "severity">>
): Diagnostic {
  return {
    code,
    message,
    severity: "warning",
    ...opts
  };
}
```

---

## Task 4: Define Diagnostic Codes

**Reference**: [LambdaLLM ARCHITECTURE/15-DIAGNOSTICS.md](../../LambdaLLM/ARCHITECTURE/15-DIAGNOSTICS.md)

### 4.1 Create `src/outcome/codes.ts`

```typescript
import { Diagnostic, DiagnosticSeverity } from "./diagnostic";

/**
 * Diagnostic code definition.
 */
interface DiagCodeDef {
  code: string;
  severity: DiagnosticSeverity;
  category: string;
  template: string;
}

/**
 * All diagnostic codes.
 */
export const DIAGNOSTIC_CODES: Record<string, DiagCodeDef> = {
  // Syntax errors (E00xx)
  E0001: { code: "E0001", severity: "error", category: "Syntax", template: "Malformed expression" },
  E0002: { code: "E0002", severity: "error", category: "Syntax", template: "Unbalanced parentheses" },
  E0003: { code: "E0003", severity: "error", category: "Syntax", template: "Invalid string literal" },

  // Type errors (E01xx)
  E0100: { code: "E0100", severity: "error", category: "Type", template: "Type mismatch: expected {expected}, got {actual}" },
  E0101: { code: "E0101", severity: "error", category: "Type", template: "Undefined variable: {name}" },
  E0102: { code: "E0102", severity: "error", category: "Type", template: "Wrong number of arguments: expected {expected}, got {actual}" },

  // Runtime errors (E02xx)
  E0200: { code: "E0200", severity: "error", category: "Runtime", template: "Division by zero" },
  E0201: { code: "E0201", severity: "error", category: "Runtime", template: "Index out of bounds: {index}" },
  E0202: { code: "E0202", severity: "error", category: "Runtime", template: "Null pointer dereference" },

  // Oracle/LLM errors (E03xx)
  E0300: { code: "E0300", severity: "error", category: "Oracle", template: "Oracle timeout after {ms}ms" },
  E0301: { code: "E0301", severity: "error", category: "Oracle", template: "Budget exhausted: {resource}" },
  E0302: { code: "E0302", severity: "error", category: "Oracle", template: "Invalid response format" },
  E0303: { code: "E0303", severity: "error", category: "Oracle", template: "Tool call failed: {tool}" },

  // Validation errors (E04xx)
  E0400: { code: "E0400", severity: "error", category: "Validation", template: "Schema validation failed" },
  E0401: { code: "E0401", severity: "error", category: "Validation", template: "Required field missing: {field}" },
  E0402: { code: "E0402", severity: "error", category: "Validation", template: "Invalid value for field: {field}" },

  // Capability errors (E05xx)
  E0500: { code: "E0500", severity: "error", category: "Capability", template: "Missing capability: {cap}" },
  E0501: { code: "E0501", severity: "error", category: "Capability", template: "Tool not allowed: {tool}" },
  E0502: { code: "E0502", severity: "error", category: "Capability", template: "Model not allowed: {model}" },

  // Warnings (W0xxx)
  W0001: { code: "W0001", severity: "warning", category: "Performance", template: "Large context: {tokens} tokens" },
  W0002: { code: "W0002", severity: "warning", category: "Performance", template: "Deep recursion: depth {depth}" },
  W0003: { code: "W0003", severity: "warning", category: "Style", template: "Unused variable: {name}" },
  W0004: { code: "W0004", severity: "warning", category: "Style", template: "Unreachable code" },
  W0005: { code: "W0005", severity: "warning", category: "Oracle", template: "Low confidence response: {confidence}" },
};

/**
 * Create diagnostic from code with interpolation.
 */
export function makeDiagnostic(
  code: keyof typeof DIAGNOSTIC_CODES,
  params?: Record<string, string | number>,
  span?: import("../frameir/meta").Span
): Diagnostic {
  const def = DIAGNOSTIC_CODES[code];
  let message = def.template;

  if (params) {
    for (const [key, value] of Object.entries(params)) {
      message = message.replace(`{${key}}`, String(value));
    }
  }

  return {
    code: def.code,
    severity: def.severity,
    message,
    span,
    data: params as Record<string, unknown>
  };
}
```

---

## Task 5: Implement Constructors and Matchers

### 5.1 Create `src/outcome/constructors.ts`

```typescript
import { Outcome, Done, Fail, Pause, OutcomeMeta, Suspended, SuspendReason } from "./outcome";
import { Failure, failure } from "./failure";
import { makeDiagnostic, DIAGNOSTIC_CODES } from "./codes";

/**
 * Create a successful outcome.
 */
export function done<A>(value: A, meta: OutcomeMeta = {}): Done<A> {
  return { tag: "Done", value, meta };
}

/**
 * Create a failed outcome.
 */
export function fail(f: Failure, meta: OutcomeMeta = {}): Fail {
  return { tag: "Fail", failure: f, meta };
}

/**
 * Create a paused outcome.
 */
export function pause<A>(suspended: Suspended<A>, meta: OutcomeMeta = {}): Pause<A> {
  return { tag: "Pause", suspended, meta };
}

// Common failure constructors

export function budgetExceeded(resource: string, meta: OutcomeMeta = {}): Fail {
  return fail(
    failure("budget-exceeded", `Budget exceeded: ${resource}`, {
      diagnostics: [makeDiagnostic("E0301", { resource })],
      recoverable: false
    }),
    meta
  );
}

export function timeout(ms: number, meta: OutcomeMeta = {}): Fail {
  return fail(
    failure("timeout", `Timeout after ${ms}ms`, {
      diagnostics: [makeDiagnostic("E0300", { ms })],
      recoverable: true
    }),
    meta
  );
}

export function validationFailed(message: string, context?: Record<string, unknown>, meta: OutcomeMeta = {}): Fail {
  return fail(
    failure("validation-failed", message, {
      diagnostics: [makeDiagnostic("E0400")],
      context,
      recoverable: true
    }),
    meta
  );
}

export function toolError(tool: string, error: string, meta: OutcomeMeta = {}): Fail {
  return fail(
    failure("tool-error", `Tool ${tool} failed: ${error}`, {
      diagnostics: [makeDiagnostic("E0303", { tool })],
      recoverable: true
    }),
    meta
  );
}

export function capabilityDenied(cap: string, meta: OutcomeMeta = {}): Fail {
  return fail(
    failure("precondition-failed", `Missing capability: ${cap}`, {
      diagnostics: [makeDiagnostic("E0500", { cap })],
      recoverable: false
    }),
    meta
  );
}
```

### 5.2 Create `src/outcome/matchers.ts`

```typescript
import { Outcome, Done, Fail, Pause, isDone, isFail, isPause } from "./outcome";

/**
 * Pattern match on outcome.
 */
export function match<A, R>(
  outcome: Outcome<A>,
  handlers: {
    done: (d: Done<A>) => R;
    fail: (f: Fail) => R;
    pause: (p: Pause<A>) => R;
  }
): R {
  switch (outcome.tag) {
    case "Done": return handlers.done(outcome);
    case "Fail": return handlers.fail(outcome);
    case "Pause": return handlers.pause(outcome);
  }
}

/**
 * Map over successful outcome.
 */
export function mapOutcome<A, B>(o: Outcome<A>, f: (a: A) => B): Outcome<B> {
  if (isDone(o)) {
    return { ...o, value: f(o.value) };
  }
  return o as unknown as Outcome<B>;
}

/**
 * FlatMap over outcome.
 */
export async function flatMapOutcome<A, B>(
  o: Outcome<A>,
  f: (a: A) => Promise<Outcome<B>>
): Promise<Outcome<B>> {
  if (isDone(o)) {
    return f(o.value);
  }
  return o as unknown as Outcome<B>;
}

/**
 * Get value or throw.
 */
export function unwrap<A>(o: Outcome<A>): A {
  if (isDone(o)) return o.value;
  if (isFail(o)) throw new Error(o.failure.message);
  throw new Error("Cannot unwrap paused outcome");
}

/**
 * Get value or default.
 */
export function unwrapOr<A>(o: Outcome<A>, defaultValue: A): A {
  if (isDone(o)) return o.value;
  return defaultValue;
}
```

---

## Verification Steps

### 1. Type Checking
```bash
npx tsc --noEmit src/outcome/**/*.ts
```

### 2. Unit Tests
```bash
npx vitest run test/outcome/
```

Tests must verify:
- Type guards work correctly
- Constructors produce valid outcomes
- Pattern matching is exhaustive
- Diagnostic interpolation works
- Failure wrapping preserves cause chain

---

## Checklist

- [ ] Create `src/outcome/outcome.ts` with Outcome ADT
- [ ] Create `src/outcome/failure.ts` with Failure type
- [ ] Create `src/outcome/diagnostic.ts` with Diagnostic type
- [ ] Create `src/outcome/codes.ts` with all diagnostic codes
- [ ] Create `src/outcome/constructors.ts` with helpers
- [ ] Create `src/outcome/matchers.ts` with pattern matching
- [ ] Create `src/outcome/index.ts` with exports
- [ ] Create test suite
- [ ] All tests pass
- [ ] Refactor existing code to use Outcome

---

## Success Criteria

1. **All diagnostic codes defined** - E0001-E0502, W0001-W0005
2. **Outcome is universal** - All effects return Outcome
3. **Failures are structured** - Reason, context, diagnostics, cause chain
4. **Type guards work** - isDone, isFail, isPause
5. **Interop with existing code** - Can convert to/from current error types

---

## Test Plan

### Happy Path Tests

| ID | Test Case | Input | Expected Output |
|----|-----------|-------|-----------------|
| HP-1 | done() creates Done | done("value") | { tag: "Done", value: "value" } |
| HP-2 | fail() creates Fail | fail(failure("timeout", "msg")) | { tag: "Fail", failure: {...} } |
| HP-3 | pause() creates Pause | pause({ reason: {...}, resume: fn }) | { tag: "Pause", suspended: {...} } |
| HP-4 | isDone type guard | Done outcome | Returns true |
| HP-5 | isFail type guard | Fail outcome | Returns true |
| HP-6 | isPause type guard | Pause outcome | Returns true |
| HP-7 | makeDiagnostic interpolates | makeDiagnostic("E0301", { resource: "tokens" }) | message = "Budget exhausted: tokens" |
| HP-8 | match exhaustive | All 3 outcome types | Calls correct handler |
| HP-9 | mapOutcome transforms | Done with value, fn | New Done with transformed value |
| HP-10 | unwrap extracts value | Done outcome | Returns inner value |

### Edge Case Tests

| ID | Test Case | Input | Expected Output |
|----|-----------|-------|-----------------|
| EC-1 | Empty diagnostics | failure with no diagnostics | diagnostics: [] |
| EC-2 | Missing interpolation params | makeDiagnostic("E0301", {}) | Keeps {resource} placeholder |
| EC-3 | Deeply nested cause chain | 5 levels of wrapFailure | allDiagnostics collects all |
| EC-4 | Empty context | failure with context: undefined | No context field or empty |
| EC-5 | OutcomeMeta all optional | done("x", {}) | Valid outcome with empty meta |
| EC-6 | Custom failure reason | "custom:my-error" | Valid FailureReason |
| EC-7 | Null span in diagnostic | errorDiag with span: undefined | Works without span |
| EC-8 | Related diagnostics | Diagnostic with related: [...] | Nested diagnostics preserved |

### Error Cases

| ID | Test Case | Input | Expected Error |
|----|-----------|-------|----------------|
| ERR-1 | unwrap Fail | Fail outcome | Throws failure.message |
| ERR-2 | unwrap Pause | Pause outcome | Throws "Cannot unwrap paused" |
| ERR-3 | Unknown diagnostic code | makeDiagnostic("X9999") | Throws or returns undefined |
| ERR-4 | Invalid severity | Diagnostic with severity: "critical" | Type error at compile time |
| ERR-5 | Invalid reason | failure("unknown-reason") | Type error at compile time |

### Integration Tests

| ID | Test Case | Description |
|----|-----------|-------------|
| INT-1 | StepOutcome → Outcome | Convert existing StepOutcome to canonical Outcome |
| INT-2 | InferResult → Outcome | Convert OracleResp ok/error to Outcome |
| INT-3 | Error → Failure | Wrap caught Error into Failure |
| INT-4 | Failure → Error | Extract Error from Failure for rethrowing |
| INT-5 | Diagnostic serialization | JSON.stringify(diagnostic) round-trips |

---

## Notes

### Why Outcome ADT Over Exceptions

1. **Explicit**: Return types show that operation can fail
2. **Composable**: flatMapOutcome chains without try/catch
3. **Structured**: Failures carry diagnostics, context, cause chain
4. **Pausable**: Pause variant enables multi-turn, human-in-the-loop

### Diagnostic Code Scheme

```
E0xxx - Syntax errors
E01xx - Type errors
E02xx - Runtime errors
E03xx - Oracle/LLM errors
E04xx - Validation errors
E05xx - Capability errors
W00xx - Warnings (performance, style, oracle)
```

### Failure Cause Chain

```typescript
// Wrapping preserves cause
const inner = failure("tool-error", "HTTP 500");
const outer = wrapFailure(inner, "Tool call failed", { tool: "search" });
// outer.cause === inner
```

### Recoverable vs Non-Recoverable

- `recoverable: true` - Can retry (timeout, rate-limit, transient)
- `recoverable: false` - Cannot retry (budget, capability, validation)

---

## Proof of Completion

When this job is complete:

1. **File Structure Verified**
   ```bash
   ls src/outcome/*.ts  # 7 files
   ```

2. **All Diagnostic Codes Present**
   ```bash
   grep -c "E0" src/outcome/codes.ts  # Should be ≥ 20
   grep -c "W0" src/outcome/codes.ts  # Should be ≥ 5
   ```

3. **Type Guards Compile**
   ```bash
   npx tsc --noEmit src/outcome/outcome.ts
   ```

4. **Tests Pass**
   ```bash
   npx vitest run test/outcome/ --reporter=verbose
   ```

5. **Exhaustive Match Verified**
   ```typescript
   // This must type-check with no implicit any
   match(outcome, {
     done: (d) => d.value,
     fail: (f) => f.failure.message,
     pause: (p) => p.suspended.reason
   })
   ```

---

## Footer

| Field | Value |
|-------|-------|
| Created | 2025-01-20 |
| Last Updated | 2025-01-20 |
| Author | Claude |
| Related Docs | [ARCHITECTURE-LANGUAGES-2.md §20](../docs/ARCHITECTURE-LANGUAGES-2.md), [ARCHITECTURE-LANGUAGES-3.md §33](../docs/ARCHITECTURE-LANGUAGES-3.md) |
| Predecessor | [009-FRAMEIR-PACKAGE](./009-FRAMEIR-PACKAGE.md) |
| Successors | [011-PORT-ABSTRACTIONS](./011-PORT-ABSTRACTIONS.md), [013-LINT-PASSES](./013-LINT-PASSES.md) |
