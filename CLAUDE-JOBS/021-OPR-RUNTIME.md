# 021: ΩPR Runtime Integration

> **Output**: `src/path/to/file.ts`
> **Depends on**: None (Layer 0)

> **Scope**: Implement the ΩPR (Omega Prompt Runtime) as a first-class subsystem in OmegaLLM, providing contract enforcement, structured retry, and audit trail for LLM-as-interpreter calls.

## Reference Implementation

The reference implementation in `../opr-runtime/` provides the design basis:

```
src/
├── runtime.js      # OPRRuntime class - the main loop
├── validate.js     # validateKernelOutput() schema validation
├── receipts.js     # createReceipt(), verifyReceiptChain()
├── retry.js        # defaultRetryDecider(), defaultRepairPrompt()
├── effects.js      # commitEffects() with idempotency
├── hash.js         # sha256Of(), newId()
├── canonical.js    # Canonical JSON serialization
├── json_extract.js # extractJsonObject() from LLM response
├── errors.js       # Error types
└── index.js        # Public exports
```

This is **not a copy-paste port**. We implement the discipline in TypeScript, integrated with existing OmegaLLM infrastructure.

---

## Core Value Proposition

**Without ΩPR**: LLM calls are "hope for the best"
- No contract enforcement
- No structured retry
- No audit trail
- No termination guarantee

**With ΩPR**: LLM calls have runtime guarantees
- Typed JSON contract at boundary
- Counterexample-guided retry
- Hash-chain receipt audit
- Budget-enforced termination

---

## Type System

### Discriminated Union for Step Results

```typescript
// src/core/opr/types.ts

/** Base fields present in all OPR step results */
interface OprStepResultBase {
  attempts: number;
  receipts: OprReceipt[];
}

/** Successful step result */
interface OprStepResultOk extends OprStepResultBase {
  tag: 'ok';
  ok: true;
  output: KernelOutput;
}

/** Failed step result - budget exhausted */
interface OprStepResultBudgetExhausted extends OprStepResultBase {
  tag: 'budget-exhausted';
  ok: false;
  error: OprBudgetExhaustedError;
  lastOutput?: KernelOutput;  // Last attempt before exhaustion
}

/** Failed step result - validation failure after all retries */
interface OprStepResultValidationFailed extends OprStepResultBase {
  tag: 'validation-failed';
  ok: false;
  error: OprValidationError;
  violations: ValidationViolation[];
}

/** Failed step result - capability violation (no retry) */
interface OprStepResultCapabilityViolation extends OprStepResultBase {
  tag: 'capability-violation';
  ok: false;
  error: OprCapabilityError;
  requestedCapability: string;
}

/** Discriminated union */
type OprStepResult =
  | OprStepResultOk
  | OprStepResultBudgetExhausted
  | OprStepResultValidationFailed
  | OprStepResultCapabilityViolation;

/** Type guards */
function isOprStepResultOk(r: OprStepResult): r is OprStepResultOk {
  return r.tag === 'ok';
}
function isOprStepResultBudgetExhausted(r: OprStepResult): r is OprStepResultBudgetExhausted {
  return r.tag === 'budget-exhausted';
}
// ... etc
```

### Kernel Output Contract

```typescript
/** The contract every kernel response MUST satisfy */
interface KernelOutput {
  kernel: string;           // Which interpreter responded
  op: string;               // What operation performed
  ok: boolean;              // Success/failure
  result: any;              // Step result (type depends on kernel)
  next_state: object | null; // Memento for continuation (null if done)
  effects: Effect[];        // Commands to execute
  diagnostics: Diagnostics;
}

interface Diagnostics {
  invariants_checked?: string[];
  notes?: string[];
  errors?: string[];
}

interface Effect {
  type: EffectType;
  idempotency_key: string;  // For exactly-once execution
  correlation_id?: string;  // For callback matching
  payload: unknown;
  preconditions?: Precondition[];
}

type EffectType =
  | 'callback.eval_lisp'    // Evaluate Lisp in host
  | 'callback.artifact.get' // Fetch from CAS
  | 'callback.facts.query'  // Query fact store
  | 'callback.hash';        // Compute hash

interface Precondition {
  type: 'fact_exists' | 'artifact_exists' | 'capability_held';
  value: unknown;
}
```

### Hash and Reference Types

```typescript
/** Content-addressed hash (never null, use Hash | null for optional) */
type Hash = `sha256:${string}`;

/** Receipt ID */
type ReceiptId = `rct_${string}`;

/** Effect receipt ID */
type EffectReceiptId = `effr_${string}`;

/** Optional hash reference - explicit about nullability */
type HashRef = Hash | null;
```

---

## Validation Pipeline

### Single-Parse Architecture

```typescript
interface ValidationResult {
  ok: boolean;
  /** Parsed JSON object (if parsing succeeded) - reuse, don't re-parse */
  parsed?: unknown;
  /** Structured violations for repair prompt */
  violations: ValidationViolation[];
}

interface ValidationViolation {
  path: string;           // JSON path, e.g., "$.effects[0].idempotency_key"
  code: ViolationCode;    // Structured error code
  message: string;        // Human-readable message
  expected?: string;      // What was expected
  actual?: string;        // What was found
}

type ViolationCode =
  | 'NOT_JSON'            // Response wasn't valid JSON
  | 'NOT_OBJECT'          // JSON wasn't an object
  | 'MISSING_FIELD'       // Required field missing
  | 'WRONG_TYPE'          // Field has wrong type
  | 'INVALID_VALUE'       // Field value invalid
  | 'KERNEL_MISMATCH'     // kernel field doesn't match expected
  | 'OP_MISMATCH';        // op field doesn't match expected
```

### Validation Rules

```typescript
function validateKernelOutput(
  raw: string,
  expected: { kernelId: string; op: string }
): ValidationResult {
  const violations: ValidationViolation[] = [];

  // 1. Parse JSON (single parse, reuse result)
  let parsed: unknown;
  try {
    parsed = JSON.parse(raw);
  } catch (e) {
    return {
      ok: false,
      violations: [{
        path: '$',
        code: 'NOT_JSON',
        message: `Invalid JSON: ${e.message}`,
      }],
    };
  }

  // 2. Must be object
  if (typeof parsed !== 'object' || parsed === null || Array.isArray(parsed)) {
    return {
      ok: false,
      parsed,
      violations: [{
        path: '$',
        code: 'NOT_OBJECT',
        message: 'Response must be a JSON object',
        expected: 'object',
        actual: Array.isArray(parsed) ? 'array' : typeof parsed,
      }],
    };
  }

  const obj = parsed as Record<string, unknown>;

  // 3. Required fields
  const required = ['kernel', 'op', 'ok', 'result', 'next_state', 'effects', 'diagnostics'];
  for (const field of required) {
    if (!(field in obj)) {
      violations.push({
        path: `$.${field}`,
        code: 'MISSING_FIELD',
        message: `Missing required field: ${field}`,
      });
    }
  }

  // 4. Type checks (only if field exists)
  if ('kernel' in obj && typeof obj.kernel !== 'string') {
    violations.push({
      path: '$.kernel',
      code: 'WRONG_TYPE',
      message: 'kernel must be a string',
      expected: 'string',
      actual: typeof obj.kernel,
    });
  }

  // 5. Kernel/op match (critical for security)
  if (obj.kernel !== expected.kernelId) {
    violations.push({
      path: '$.kernel',
      code: 'KERNEL_MISMATCH',
      message: `Expected kernel "${expected.kernelId}", got "${obj.kernel}"`,
      expected: expected.kernelId,
      actual: String(obj.kernel),
    });
  }

  if (obj.op !== expected.op) {
    violations.push({
      path: '$.op',
      code: 'OP_MISMATCH',
      message: `Expected op "${expected.op}", got "${obj.op}"`,
      expected: expected.op,
      actual: String(obj.op),
    });
  }

  // 6. Effects array validation
  if ('effects' in obj) {
    if (!Array.isArray(obj.effects)) {
      violations.push({
        path: '$.effects',
        code: 'WRONG_TYPE',
        message: 'effects must be an array',
        expected: 'array',
        actual: typeof obj.effects,
      });
    } else {
      validateEffectsArray(obj.effects, violations);
    }
  }

  // 7. next_state must be object or null
  if ('next_state' in obj && obj.next_state !== null && typeof obj.next_state !== 'object') {
    violations.push({
      path: '$.next_state',
      code: 'WRONG_TYPE',
      message: 'next_state must be object or null',
      expected: 'object | null',
      actual: typeof obj.next_state,
    });
  }

  return {
    ok: violations.length === 0,
    parsed,
    violations,
  };
}
```

---

## Callback Protocol (Reentry)

### The Flow

```
┌──────────────────────────────────────────────────────────────────────────┐
│ 1. LISP EVALUATION                                                        │
│    (opr/step kernel program state)                                        │
│         │                                                                 │
│         ▼                                                                 │
│ 2. OPR RUNTIME                                                            │
│    Build prompt → Call LLM → Validate response                           │
│         │                                                                 │
│         ▼                                                                 │
│ 3. LLM RETURNS with callback request:                                     │
│    {                                                                      │
│      "effects": [{                                                        │
│        "type": "callback.eval_lisp",                                      │
│        "correlation_id": "cb-001",                                        │
│        "payload": { "expr": "(semantic-match? doc1 doc2)" }               │
│      }]                                                                   │
│    }                                                                      │
│         │                                                                 │
│         ▼                                                                 │
│ 4. OPR RUNTIME executes callback:                                         │
│    - Parse Lisp expression from payload                                   │
│    - REENTER CEKS evaluator with expression + current env                │
│    - Collect result                                                       │
│         │                                                                 │
│         ▼                                                                 │
│ 5. CONTINUE LLM with callback results:                                    │
│    CALLBACK_RESULTS:                                                      │
│    [{ "correlation_id": "cb-001", "ok": true, "value": true }]           │
│                                                                           │
│    Continue your computation with these results.                          │
│         │                                                                 │
│         ▼                                                                 │
│ 6. LLM RETURNS final result (no more callbacks):                          │
│    { "ok": true, "result": {...}, "effects": [] }                        │
│         │                                                                 │
│         ▼                                                                 │
│ 7. RETURN TO LISP with OprStepResult                                      │
└──────────────────────────────────────────────────────────────────────────┘
```

### Callback Types and Capabilities

```typescript
/** Callback capabilities - what the kernel is allowed to request */
interface OprCapabilities {
  allowedCallbacks: Set<EffectType>;
  maxCallbacksPerStep: number;
  callbackTimeout: number;  // ms
}

/** Default capabilities for different kernel types */
const LOGIC_KERNEL_CAPABILITIES: OprCapabilities = {
  allowedCallbacks: new Set([
    'callback.eval_lisp',
    'callback.facts.query',
  ]),
  maxCallbacksPerStep: 5,
  callbackTimeout: 30_000,
};

const ANALYZE_KERNEL_CAPABILITIES: OprCapabilities = {
  allowedCallbacks: new Set([
    'callback.artifact.get',
    'callback.hash',
  ]),
  maxCallbacksPerStep: 10,
  callbackTimeout: 60_000,
};
```

### Callback Result Type

```typescript
interface CallbackResult {
  correlation_id: string;
  ok: boolean;
  value?: unknown;
  error?: {
    code: string;
    message: string;
  };
}

/** Build continuation prompt with callback results */
function buildCallbackContinuationPrompt(
  results: CallbackResult[]
): string {
  return `CALLBACK_RESULTS:
${JSON.stringify(results, null, 2)}

Continue your computation using these callback results.
Return ONLY the next JSON response conforming to the OUTPUT CONTRACT.`;
}
```

---

## Receipt Chain (Audit Trail)

### Receipt Structure

```typescript
interface OprReceipt {
  receipt_version: 1;
  receipt_id: ReceiptId;
  created_at: string;  // ISO 8601

  // Chain link
  prev_receipt_hash: HashRef;

  // Request/response (hashes, not content)
  request_hash: Hash;
  response_hash: HashRef;  // null if no response (timeout)

  // Context
  kernel_id: string;
  op: string;
  attempt: number;
  status: ReceiptStatus;
  errors: string[];

  // Diagnostics from kernel (if available)
  diagnostics?: Diagnostics;

  // Self-hash (for chain integrity)
  receipt_hash: Hash;
}

type ReceiptStatus =
  | 'OK'              // Validation passed
  | 'ERROR'           // Validation failed
  | 'TIMEOUT'         // LLM call timed out
  | 'CALLBACK_ERROR'; // Callback execution failed
```

### Chain Verification

```typescript
function verifyReceiptChain(receipts: OprReceipt[]): {
  valid: boolean;
  brokenAt?: number;
  error?: string;
} {
  for (let i = 0; i < receipts.length; i++) {
    const receipt = receipts[i];

    // Verify self-hash
    const computedHash = computeReceiptHash(receipt);
    if (computedHash !== receipt.receipt_hash) {
      return {
        valid: false,
        brokenAt: i,
        error: `Receipt ${i} self-hash mismatch`,
      };
    }

    // Verify chain link (except first)
    if (i > 0) {
      const prevReceipt = receipts[i - 1];
      if (receipt.prev_receipt_hash !== prevReceipt.receipt_hash) {
        return {
          valid: false,
          brokenAt: i,
          error: `Receipt ${i} chain link broken`,
        };
      }
    }
  }

  return { valid: true };
}
```

---

## Budget Integration

### Two-Level Budget Checking

```typescript
interface OprBudgetConfig {
  /** Max attempts for this step (local budget) */
  maxAttempts: number;

  /** Reference to session budget (global budget) */
  sessionBudget: Budget;
}

class OprRuntime {
  constructor(private config: OprRuntimeConfig) {}

  async execute(params: OprExecuteParams): Promise<OprStepResult> {
    const { maxAttempts, sessionBudget } = this.config.budget;

    for (let attempt = 1; attempt <= maxAttempts; attempt++) {
      // Check SESSION budget (global)
      if (!sessionBudget.hasRemaining('tokens')) {
        return this.makeBudgetExhaustedResult('session-tokens', attempt);
      }
      if (!sessionBudget.hasRemaining('cost')) {
        return this.makeBudgetExhaustedResult('session-cost', attempt);
      }

      // Execute LLM call
      const response = await this.callLLM(params);

      // Consume from session budget
      sessionBudget.consumeTokens(response.usage.totalTokens);
      sessionBudget.consumeCost(response.usage.estimatedCost);

      // Validate and potentially retry (local budget)
      const validation = this.validate(response);
      if (validation.ok) {
        return this.makeOkResult(validation.parsed, attempt);
      }

      // Record failed attempt
      this.recordReceipt(attempt, 'ERROR', validation.violations);

      if (attempt < maxAttempts) {
        // Build repair prompt for next attempt
        params.prompt = this.buildRepairPrompt(validation);
      }
    }

    // Local budget exhausted
    return this.makeBudgetExhaustedResult('attempts', maxAttempts);
  }
}
```

---

## Progress Invariants (Multi-Step Fixed Point)

For multi-step kernel computations, enforce progress:

```typescript
interface ProgressInvariants {
  /** Iteration must increase monotonically */
  iterationMonotonic: boolean;

  /** Derived facts must grow monotonically (for Datalog) */
  derivedMonotonic: boolean;

  /** Delta must eventually become empty (for termination) */
  deltaTermination: boolean;
}

function checkProgressInvariants(
  prevState: KernelState | null,
  nextState: KernelState,
  invariants: ProgressInvariants
): ValidationViolation[] {
  const violations: ValidationViolation[] = [];

  if (prevState === null) return violations;  // First step, no invariants

  if (invariants.iterationMonotonic) {
    if (nextState.iteration <= prevState.iteration) {
      violations.push({
        path: '$.next_state.iteration',
        code: 'INVALID_VALUE',
        message: `Iteration must increase: ${prevState.iteration} → ${nextState.iteration}`,
      });
    }
  }

  if (invariants.derivedMonotonic) {
    const prevCount = prevState.derived?.length ?? 0;
    const nextCount = nextState.derived?.length ?? 0;
    if (nextCount < prevCount) {
      violations.push({
        path: '$.next_state.derived',
        code: 'INVALID_VALUE',
        message: `Derived facts must grow monotonically: ${prevCount} → ${nextCount}`,
      });
    }
  }

  return violations;
}
```

---

## CEKS Integration

### New Continuation Frame

```typescript
// In src/core/eval/machine.ts

/** Frame for OPR step in progress */
interface OprStepK {
  tag: 'OprStepK';
  kernel: OprKernel;
  program: unknown;
  oprState: unknown;
  attempt: number;
  receipts: OprReceipt[];
  pendingCallbacks: Effect[];
}

/** Handle OprStepK frame when value is ready */
function handleOprStepKFrame(
  state: MachineState,
  frame: OprStepK,
  value: Val
): MachineState {
  // Value is the callback result
  const callbackResult: CallbackResult = {
    correlation_id: frame.pendingCallbacks[0].correlation_id!,
    ok: true,
    value: valueToJson(value),
  };

  // Continue LLM with callback result
  // This creates a new OprStepK frame for the continuation
  return continueOprWithCallback(state, frame, callbackResult);
}
```

### Op Handler

```typescript
// In src/core/effects/runtimeImpl.ts

async function handleOprLogicStep(
  state: MachineState,
  opcall: OpCall,
  runtime: RuntimeImpl
): Promise<StepResult> {
  const { kernel, program, oprState } = extractOprArgs(opcall.args);

  // Create OPR runtime with session budget
  const oprRuntime = new OprRuntime({
    llm: createLLMAdapter(runtime.oracleAdapter),
    budget: {
      maxAttempts: 3,
      sessionBudget: state.budget,
    },
    capabilities: LOGIC_KERNEL_CAPABILITIES,
  });

  // Execute step
  const result = await oprRuntime.execute({ kernel, program, state: oprState });

  // Handle callbacks if present
  if (isOprStepResultOk(result) && result.output.effects.length > 0) {
    const callbackEffects = result.output.effects.filter(
      e => e.type.startsWith('callback.')
    );

    if (callbackEffects.length > 0) {
      // Push OprStepK frame and evaluate first callback
      return {
        tag: 'PushFrame',
        frame: {
          tag: 'OprStepK',
          kernel,
          program,
          oprState: result.output.next_state,
          attempt: result.attempts,
          receipts: result.receipts,
          pendingCallbacks: callbackEffects,
        },
        control: parseExpr(callbackEffects[0].payload.expr),
      };
    }
  }

  // No callbacks, return result
  return {
    tag: 'Value',
    value: oprStepResultToValue(result),
  };
}
```

---

## Lisp Forms

```lisp
;; Execute single kernel step
(opr/step kernel program state)
;; => #<OprStepResult tag=ok result={...} receipts=[...]>

;; Execute until done (next_state.done = true)
(opr/run kernel program initial-state)
;; => #<OprRunResult final-result={...} iterations=5 receipts=[...]>

;; Execute with custom budget
(opr/step kernel program state :max-attempts 5)

;; Check if result succeeded
(opr/ok? result)  ;; => #t or #f

;; Extract components
(opr/result result)      ;; => the kernel result
(opr/next-state result)  ;; => the next state (memento)
(opr/receipts result)    ;; => list of receipts
(opr/attempts result)    ;; => number of attempts

;; Pattern match on result
(match (opr/step kernel program state)
  [(opr-ok r) (process-result r)]
  [(opr-budget-exhausted e) (handle-budget e)]
  [(opr-validation-failed e) (handle-validation e)]
  [(opr-capability-violation e) (handle-capability e)])
```

---

---

## Kernel Prompt System

### Prompt File Structure

Prompts are stored as TypeScript modules using the PromptIR structure:

```typescript
// src/core/opr/prompts/index.ts

import type { PromptDoc, PSystem, PUser, PFewShot, PAttachSchema } from '../../frameir/prompt';

export interface KernelPromptConfig {
  id: string;
  version: string;
  description: string;
  prompt: PromptDoc;
  schema: KernelOutputSchema;
  capabilities: OprCapabilities;
}

export type KernelOutputSchema = typeof KERNEL_OUTPUT_SCHEMA;
```

### Core Kernel Prompts

#### 1. Logic Kernel (Datalog-style reasoning)

```typescript
// src/core/opr/prompts/logic-kernel.ts

export const LOGIC_KERNEL: KernelPromptConfig = {
  id: 'opr.logic.v1',
  version: '1.0.0',
  description: 'Datalog-style forward-chaining inference with callbacks',

  prompt: {
    tag: 'PromptDoc',
    parts: [
      {
        tag: 'PSystem',
        text: `You are a LOGIC KERNEL implementing forward-chaining inference.

═══════════════════════════════════════════════════════════════════════════════
IDENTITY
═══════════════════════════════════════════════════════════════════════════════
kernel: opr.logic.v1
op: infer

═══════════════════════════════════════════════════════════════════════════════
INPUT FORMAT
═══════════════════════════════════════════════════════════════════════════════
You receive:
- RULES: Datalog-style rules with head :- body syntax
- FACTS: Base facts (ground atoms)
- STATE: Current iteration, derived facts, delta

═══════════════════════════════════════════════════════════════════════════════
EXECUTION MODEL
═══════════════════════════════════════════════════════════════════════════════
Each step you MUST:
1. Apply rules to current facts to derive new facts (delta)
2. If a rule body contains a SEMANTIC PREDICATE (marked with @semantic),
   emit a callback.eval_lisp effect to evaluate it
3. Add delta to derived facts
4. Return next_state with updated iteration and facts

TERMINATION: When delta is empty, set next_state.done = true

═══════════════════════════════════════════════════════════════════════════════
SEMANTIC PREDICATES
═══════════════════════════════════════════════════════════════════════════════
When you encounter @semantic(...) in a rule body, you CANNOT evaluate it yourself.
You MUST emit a callback to have the host evaluate it.

Example rule:
  safe-to-refactor(X) :- code-block(X), @semantic(refactor-preserves-behavior? X)

When applying this rule:
1. Find all code-block(X) facts
2. For each X, emit callback: { type: "callback.eval_lisp", payload: { expr: "(refactor-preserves-behavior? X)" } }
3. WAIT for callback results before concluding

═══════════════════════════════════════════════════════════════════════════════
OUTPUT CONTRACT
═══════════════════════════════════════════════════════════════════════════════
Return ONLY valid JSON matching this EXACT structure:
{
  "kernel": "opr.logic.v1",
  "op": "infer",
  "ok": true,
  "result": {
    "delta": [<new facts derived this step>],
    "applied_rules": [<rule names that fired>]
  },
  "next_state": {
    "iteration": <previous + 1>,
    "facts": [<all facts including new>],
    "derived": [<all derived facts>],
    "done": <true if delta empty>
  },
  "effects": [<callback requests if any>],
  "diagnostics": {
    "invariants_checked": ["iteration_monotonic", "facts_monotonic"],
    "notes": [<reasoning notes>]
  }
}

CRITICAL: Return ONLY the JSON. No markdown, no explanation, no preamble.`,
      },
      {
        tag: 'PFewShot',
        examples: [
          {
            user: `RULES:
reachable(X, Y) :- edge(X, Y).
reachable(X, Z) :- edge(X, Y), reachable(Y, Z).

FACTS:
edge(a, b).
edge(b, c).
edge(c, d).

STATE:
{ "iteration": 0, "facts": ["edge(a,b)", "edge(b,c)", "edge(c,d)"], "derived": [], "done": false }`,
            assistant: `{
  "kernel": "opr.logic.v1",
  "op": "infer",
  "ok": true,
  "result": {
    "delta": ["reachable(a,b)", "reachable(b,c)", "reachable(c,d)"],
    "applied_rules": ["reachable-base"]
  },
  "next_state": {
    "iteration": 1,
    "facts": ["edge(a,b)", "edge(b,c)", "edge(c,d)", "reachable(a,b)", "reachable(b,c)", "reachable(c,d)"],
    "derived": ["reachable(a,b)", "reachable(b,c)", "reachable(c,d)"],
    "done": false
  },
  "effects": [],
  "diagnostics": {
    "invariants_checked": ["iteration_monotonic", "facts_monotonic"],
    "notes": ["Applied base case rule to all edges"]
  }
}`
          },
          {
            user: `RULES:
safe-change(X) :- code-block(X), @semantic(refactor-preserves-behavior? X).

FACTS:
code-block(fn-auth).
code-block(fn-parse).

STATE:
{ "iteration": 0, "facts": ["code-block(fn-auth)", "code-block(fn-parse)"], "derived": [], "done": false }`,
            assistant: `{
  "kernel": "opr.logic.v1",
  "op": "infer",
  "ok": true,
  "result": {
    "delta": [],
    "applied_rules": [],
    "pending_semantic_checks": 2
  },
  "next_state": {
    "iteration": 1,
    "facts": ["code-block(fn-auth)", "code-block(fn-parse)"],
    "derived": [],
    "awaiting_callbacks": true,
    "done": false
  },
  "effects": [
    {
      "type": "callback.eval_lisp",
      "idempotency_key": "sem-check-fn-auth-001",
      "correlation_id": "cb-001",
      "payload": { "expr": "(refactor-preserves-behavior? 'fn-auth)" }
    },
    {
      "type": "callback.eval_lisp",
      "idempotency_key": "sem-check-fn-parse-002",
      "correlation_id": "cb-002",
      "payload": { "expr": "(refactor-preserves-behavior? 'fn-parse)" }
    }
  ],
  "diagnostics": {
    "invariants_checked": ["iteration_monotonic"],
    "notes": ["Emitting callbacks for semantic predicates before deriving facts"]
  }
}`
          }
        ]
      }
    ]
  },

  schema: KERNEL_OUTPUT_SCHEMA,

  capabilities: {
    allowedCallbacks: new Set(['callback.eval_lisp', 'callback.facts.query']),
    maxCallbacksPerStep: 10,
    callbackTimeout: 30_000,
  }
};
```

#### 2. Analyze Kernel (Code Review)

```typescript
// src/core/opr/prompts/analyze-kernel.ts

export const ANALYZE_KERNEL: KernelPromptConfig = {
  id: 'opr.analyze.v1',
  version: '1.0.0',
  description: 'Code review and analysis with artifact access',

  prompt: {
    tag: 'PromptDoc',
    parts: [
      {
        tag: 'PSystem',
        text: `You are an ANALYZE KERNEL for code review and static analysis.

═══════════════════════════════════════════════════════════════════════════════
IDENTITY
═══════════════════════════════════════════════════════════════════════════════
kernel: opr.analyze.v1
op: review

═══════════════════════════════════════════════════════════════════════════════
CAPABILITIES
═══════════════════════════════════════════════════════════════════════════════
You can request:
- callback.artifact.get: Fetch file content by path or hash
- callback.hash: Compute content hash for deduplication

═══════════════════════════════════════════════════════════════════════════════
WORKFLOW
═══════════════════════════════════════════════════════════════════════════════
1. Receive list of files to review
2. Request artifact content for each file
3. Analyze code for issues
4. Return structured findings

═══════════════════════════════════════════════════════════════════════════════
OUTPUT CONTRACT
═══════════════════════════════════════════════════════════════════════════════
{
  "kernel": "opr.analyze.v1",
  "op": "review",
  "ok": true,
  "result": {
    "findings": [
      {
        "severity": "error" | "warning" | "info",
        "file": "<path>",
        "line": <number>,
        "code": "<issue-code>",
        "message": "<description>",
        "suggestion": "<fix suggestion>"
      }
    ],
    "summary": {
      "files_reviewed": <count>,
      "errors": <count>,
      "warnings": <count>
    }
  },
  "next_state": null,  // Single-step kernel
  "effects": [],
  "diagnostics": { ... }
}

CRITICAL: Return ONLY the JSON. No markdown.`,
      }
    ]
  },

  schema: ANALYZE_OUTPUT_SCHEMA,

  capabilities: {
    allowedCallbacks: new Set(['callback.artifact.get', 'callback.hash']),
    maxCallbacksPerStep: 50,
    callbackTimeout: 60_000,
  }
};
```

#### 3. Semantic Predicate Kernel

```typescript
// src/core/opr/prompts/semantic-kernel.ts

export const SEMANTIC_KERNEL: KernelPromptConfig = {
  id: 'opr.semantic.v1',
  version: '1.0.0',
  description: 'Single semantic judgment - yes/no with confidence',

  prompt: {
    tag: 'PromptDoc',
    parts: [
      {
        tag: 'PSystem',
        text: `You are a SEMANTIC JUDGMENT KERNEL.

═══════════════════════════════════════════════════════════════════════════════
IDENTITY
═══════════════════════════════════════════════════════════════════════════════
kernel: opr.semantic.v1
op: judge

═══════════════════════════════════════════════════════════════════════════════
TASK
═══════════════════════════════════════════════════════════════════════════════
Given a PREDICATE and SUBJECT, return a boolean judgment with confidence.

Examples of predicates:
- refactor-preserves-behavior?: Does the refactored code behave identically?
- is-professional?: Is this communication professional in tone?
- contains-pii?: Does this text contain personally identifiable information?
- type-safe?: Is this code type-safe according to its annotations?

═══════════════════════════════════════════════════════════════════════════════
OUTPUT CONTRACT
═══════════════════════════════════════════════════════════════════════════════
{
  "kernel": "opr.semantic.v1",
  "op": "judge",
  "ok": true,
  "result": {
    "judgment": true | false,
    "confidence": 0.0 - 1.0,
    "reasoning": "<brief explanation>"
  },
  "next_state": null,
  "effects": [],
  "diagnostics": {
    "notes": ["<how judgment was made>"]
  }
}`,
      },
      {
        tag: 'PFewShot',
        examples: [
          {
            user: `PREDICATE: refactor-preserves-behavior?
SUBJECT:
  BEFORE: function add(a, b) { return a + b; }
  AFTER:  const add = (a, b) => a + b;`,
            assistant: `{
  "kernel": "opr.semantic.v1",
  "op": "judge",
  "ok": true,
  "result": {
    "judgment": true,
    "confidence": 0.95,
    "reasoning": "Arrow function conversion preserves semantics for simple pure functions"
  },
  "next_state": null,
  "effects": [],
  "diagnostics": {
    "notes": ["No this binding issues", "No hoisting concerns for const"]
  }
}`
          }
        ]
      }
    ]
  },

  schema: SEMANTIC_OUTPUT_SCHEMA,

  capabilities: {
    allowedCallbacks: new Set([]),
    maxCallbacksPerStep: 0,
    callbackTimeout: 10_000,
  }
};
```

### Prompt Registry

```typescript
// src/core/opr/prompts/registry.ts

import { LOGIC_KERNEL } from './logic-kernel';
import { ANALYZE_KERNEL } from './analyze-kernel';
import { SEMANTIC_KERNEL } from './semantic-kernel';

class KernelPromptRegistry {
  private kernels = new Map<string, KernelPromptConfig>();

  constructor() {
    // Register built-in kernels
    this.register(LOGIC_KERNEL);
    this.register(ANALYZE_KERNEL);
    this.register(SEMANTIC_KERNEL);
  }

  register(config: KernelPromptConfig): void {
    this.kernels.set(config.id, config);
  }

  get(id: string): KernelPromptConfig | undefined {
    return this.kernels.get(id);
  }

  list(): string[] {
    return Array.from(this.kernels.keys());
  }

  /** Compile PromptDoc to provider-specific format */
  compile(
    id: string,
    provider: 'openai' | 'anthropic'
  ): ProviderMessages {
    const config = this.get(id);
    if (!config) throw new Error(`Unknown kernel: ${id}`);

    return compilePromptDoc(config.prompt, provider);
  }
}

export const kernelRegistry = new KernelPromptRegistry();
```

### Prompt Compilation

```typescript
// src/core/opr/prompts/compile.ts

interface OpenAIMessage {
  role: 'system' | 'user' | 'assistant';
  content: string;
}

interface AnthropicMessage {
  role: 'user' | 'assistant';
  content: string;
}

interface ProviderMessages {
  openai: { system: string; messages: OpenAIMessage[] };
  anthropic: { system: string; messages: AnthropicMessage[] };
}

function compilePromptDoc(doc: PromptDoc, provider: 'openai' | 'anthropic'): ProviderMessages {
  const systemParts: string[] = [];
  const examples: { user: string; assistant: string }[] = [];

  for (const part of doc.parts) {
    switch (part.tag) {
      case 'PSystem':
        systemParts.push(part.text);
        break;
      case 'PFewShot':
        examples.push(...part.examples);
        break;
      case 'PAttachSchema':
        systemParts.push(`\nOUTPUT SCHEMA:\n${JSON.stringify(part.schema, null, 2)}`);
        break;
    }
  }

  const system = systemParts.join('\n\n');

  if (provider === 'openai') {
    const messages: OpenAIMessage[] = [];
    for (const ex of examples) {
      messages.push({ role: 'user', content: ex.user });
      messages.push({ role: 'assistant', content: ex.assistant });
    }
    return { openai: { system, messages }, anthropic: null! };
  }

  // Anthropic format
  const messages: AnthropicMessage[] = [];
  for (const ex of examples) {
    messages.push({ role: 'user', content: ex.user });
    messages.push({ role: 'assistant', content: ex.assistant });
  }
  return { openai: null!, anthropic: { system, messages } };
}
```

---

## Test Suites

### Test File Structure

```
OmegaLLM/test/opr/
├── unit/
│   ├── validate.spec.ts        # Validation pipeline tests
│   ├── receipts.spec.ts        # Receipt chain tests
│   ├── retry.spec.ts           # Retry logic tests
│   └── hash.spec.ts            # Hash function tests
├── integration/
│   ├── logic-kernel.spec.ts    # Datalog kernel tests
│   ├── analyze-kernel.spec.ts  # Code review kernel tests
│   ├── semantic-kernel.spec.ts # Semantic predicate tests
│   └── callbacks.spec.ts       # Callback/reentry tests
├── e2e/
│   ├── datalog-e2e.spec.ts     # Full Datalog with callbacks
│   ├── review-e2e.spec.ts      # Code review end-to-end
│   └── refactor-e2e.spec.ts    # Refactor safety check e2e
└── live/
    ├── openai-opr.spec.ts      # Live OpenAI tests
    └── anthropic-opr.spec.ts   # Live Anthropic tests
```

### Unit Tests

```typescript
// test/opr/unit/validate.spec.ts

import { describe, it, expect } from 'vitest';
import { validateKernelOutput } from '../../../src/core/opr/validate';

describe('validateKernelOutput', () => {
  const expected = { kernelId: 'opr.logic.v1', op: 'infer' };

  it('V1: accepts valid kernel output', () => {
    const raw = JSON.stringify({
      kernel: 'opr.logic.v1',
      op: 'infer',
      ok: true,
      result: { delta: [] },
      next_state: { iteration: 1 },
      effects: [],
      diagnostics: {}
    });

    const result = validateKernelOutput(raw, expected);
    expect(result.ok).toBe(true);
    expect(result.violations).toHaveLength(0);
  });

  it('V2: rejects non-JSON', () => {
    const raw = 'This is not JSON at all';
    const result = validateKernelOutput(raw, expected);
    expect(result.ok).toBe(false);
    expect(result.violations[0].code).toBe('NOT_JSON');
  });

  it('V3: rejects missing required fields', () => {
    const raw = JSON.stringify({ kernel: 'opr.logic.v1' });
    const result = validateKernelOutput(raw, expected);
    expect(result.ok).toBe(false);
    const codes = result.violations.map(v => v.code);
    expect(codes).toContain('MISSING_FIELD');
  });

  it('V4: rejects kernel mismatch', () => {
    const raw = JSON.stringify({
      kernel: 'opr.WRONG.v1',
      op: 'infer',
      ok: true,
      result: {},
      next_state: null,
      effects: [],
      diagnostics: {}
    });

    const result = validateKernelOutput(raw, expected);
    expect(result.ok).toBe(false);
    expect(result.violations[0].code).toBe('KERNEL_MISMATCH');
  });

  it('V5: provides structured violations for repair', () => {
    const raw = JSON.stringify({
      kernel: 'opr.logic.v1',
      op: 'infer',
      ok: 'not-a-boolean',  // Wrong type
      result: null,
      next_state: 123,       // Wrong type
      effects: 'not-array',  // Wrong type
      diagnostics: {}
    });

    const result = validateKernelOutput(raw, expected);
    expect(result.ok).toBe(false);
    expect(result.violations.length).toBeGreaterThan(0);

    // Each violation has path, code, message
    for (const v of result.violations) {
      expect(v.path).toBeDefined();
      expect(v.code).toBeDefined();
      expect(v.message).toBeDefined();
    }
  });
});
```

### Integration Tests (with ScriptedOracleAdapter)

```typescript
// test/opr/integration/logic-kernel.spec.ts

import { describe, it, expect, beforeEach } from 'vitest';
import { OprRuntime } from '../../../src/core/opr/runtime';
import { ScriptedOprAdapter } from '../../../src/core/opr/adapters/scripted';
import { InMemoryReceiptStore } from '../../../src/core/opr/receipts';
import { LOGIC_KERNEL } from '../../../src/core/opr/prompts/logic-kernel';

describe('Logic Kernel Integration', () => {
  let runtime: OprRuntime;
  let receipts: InMemoryReceiptStore;

  beforeEach(() => {
    receipts = new InMemoryReceiptStore();
    runtime = new OprRuntime({
      kernel: LOGIC_KERNEL,
      adapter: new ScriptedOprAdapter(logicKernelResponses),
      receipts,
      budget: { maxAttempts: 3 },
    });
  });

  it('L1: derives facts from rules in single step', async () => {
    const result = await runtime.step({
      rules: `reachable(X, Y) :- edge(X, Y).`,
      facts: ['edge(a, b)', 'edge(b, c)'],
      state: { iteration: 0, facts: [], derived: [] }
    });

    expect(result.tag).toBe('ok');
    if (result.tag === 'ok') {
      expect(result.output.result.delta).toContain('reachable(a, b)');
      expect(result.output.result.delta).toContain('reachable(b, c)');
    }
  });

  it('L2: runs to fixpoint with transitive closure', async () => {
    const result = await runtime.runToFixpoint({
      rules: `
        reachable(X, Y) :- edge(X, Y).
        reachable(X, Z) :- edge(X, Y), reachable(Y, Z).
      `,
      facts: ['edge(a, b)', 'edge(b, c)', 'edge(c, d)'],
      state: { iteration: 0, facts: [], derived: [], done: false }
    });

    expect(result.tag).toBe('ok');
    if (result.tag === 'ok') {
      expect(result.finalState.derived).toContain('reachable(a, d)');
      expect(result.iterations).toBeGreaterThan(1);
    }
  });

  it('L3: emits callbacks for semantic predicates', async () => {
    const result = await runtime.step({
      rules: `safe(X) :- code(X), @semantic(type-safe? X).`,
      facts: ['code(fn1)', 'code(fn2)'],
      state: { iteration: 0, facts: [], derived: [] }
    });

    expect(result.tag).toBe('ok');
    if (result.tag === 'ok') {
      expect(result.output.effects.length).toBe(2);
      expect(result.output.effects[0].type).toBe('callback.eval_lisp');
    }
  });

  it('L4: enforces monotonic facts invariant', async () => {
    // ScriptedAdapter returns response that violates monotonicity
    runtime = new OprRuntime({
      kernel: LOGIC_KERNEL,
      adapter: new ScriptedOprAdapter([{
        ok: true,
        result: { delta: ['new-fact'] },
        next_state: {
          iteration: 2,
          facts: [],  // Empty! Violates monotonicity
          derived: []
        },
        effects: [],
        diagnostics: {}
      }]),
      receipts,
      budget: { maxAttempts: 1 },
      invariants: { derivedMonotonic: true }
    });

    const result = await runtime.step({
      rules: '',
      facts: ['existing-fact'],
      state: { iteration: 1, facts: ['existing-fact'], derived: ['existing-fact'] }
    });

    expect(result.tag).toBe('validation-failed');
  });

  it('L5: creates receipt chain for all attempts', async () => {
    // First two attempts fail, third succeeds
    runtime = new OprRuntime({
      kernel: LOGIC_KERNEL,
      adapter: new ScriptedOprAdapter([
        'not valid json',  // Attempt 1: parse fail
        '{"kernel": "wrong"}',  // Attempt 2: validation fail
        validLogicResponse,  // Attempt 3: success
      ]),
      receipts,
      budget: { maxAttempts: 3 },
    });

    const result = await runtime.step({ rules: '', facts: [], state: {} });

    expect(result.tag).toBe('ok');
    expect(result.receipts.length).toBe(3);
    expect(result.receipts[0].status).toBe('ERROR');
    expect(result.receipts[1].status).toBe('ERROR');
    expect(result.receipts[2].status).toBe('OK');

    // Verify chain integrity
    const chainValid = verifyReceiptChain(result.receipts);
    expect(chainValid.valid).toBe(true);
  });
});
```

### End-to-End Tests

```typescript
// test/opr/e2e/datalog-e2e.spec.ts

import { describe, it, expect } from 'vitest';
import { evalOmega } from '../../helpers/omegaHarness';

describe('Datalog with ΩPR End-to-End', () => {

  it('E1: full Datalog inference from Lisp', async () => {
    const result = await evalOmega(`
      (define rules "
        reachable(X, Y) :- edge(X, Y).
        reachable(X, Z) :- reachable(X, Y), edge(Y, Z).
      ")

      (define facts '(
        (edge a b)
        (edge b c)
        (edge c d)
      ))

      (define kernel (opr/load-kernel 'opr.logic.v1))

      (opr/run kernel
        (hash :rules rules :facts facts)
        (hash :iteration 0 :derived () :done #f))
    `);

    expect(result.tag).toBe('ok');
    expect(result.finalState.derived).toContain('reachable(a, d)');
  });

  it('E2: Datalog with semantic callback', async () => {
    // Define a semantic predicate in Lisp
    const result = await evalOmega(`
      ;; Define semantic predicate that kernel can call back to
      (define (type-safe? fn-name)
        ;; Simulate type checking
        (case fn-name
          ((fn-auth) #t)
          ((fn-parse) #f)
          (else #f)))

      (define rules "
        safe-to-deploy(X) :- function(X), @semantic(type-safe? X).
      ")

      (define facts '((function fn-auth) (function fn-parse)))

      (define kernel (opr/load-kernel 'opr.logic.v1))

      (opr/run kernel
        (hash :rules rules :facts facts)
        (hash :iteration 0 :derived () :done #f))
    `);

    expect(result.tag).toBe('ok');
    // Only fn-auth should be marked safe (type-safe? returns true)
    expect(result.finalState.derived).toContain('safe-to-deploy(fn-auth)');
    expect(result.finalState.derived).not.toContain('safe-to-deploy(fn-parse)');
  });

  it('E3: code review with artifact fetching', async () => {
    const result = await evalOmega(`
      ;; Set up artifact store with sample files
      (artifact/store! "src/auth.ts" "export function login(user) { eval(user.code) }")
      (artifact/store! "src/utils.ts" "export function add(a, b) { return a + b }")

      (define kernel (opr/load-kernel 'opr.analyze.v1))

      (opr/step kernel
        (hash :files '("src/auth.ts" "src/utils.ts"))
        #f)  ;; Single-step kernel, no state
    `);

    expect(result.tag).toBe('ok');
    expect(result.output.result.findings.length).toBeGreaterThan(0);
    // Should find the eval() security issue
    expect(result.output.result.findings.some(f =>
      f.file === 'src/auth.ts' && f.code.includes('eval')
    )).toBe(true);
  });

  it('E4: semantic predicate for refactor safety', async () => {
    const result = await evalOmega(`
      (define kernel (opr/load-kernel 'opr.semantic.v1))

      (opr/step kernel
        (hash
          :predicate 'refactor-preserves-behavior?
          :subject (hash
            :before "function add(a, b) { return a + b; }"
            :after "const add = (a, b) => a + b;"))
        #f)
    `);

    expect(result.tag).toBe('ok');
    expect(result.output.result.judgment).toBe(true);
    expect(result.output.result.confidence).toBeGreaterThan(0.8);
  });
});
```

### Live Tests (Real LLM)

```typescript
// test/opr/live/openai-opr.spec.ts

import { describe, it, expect } from 'vitest';
import { OprRuntime } from '../../../src/core/opr/runtime';
import { OpenAIOprAdapter } from '../../../src/core/opr/adapters/openai';
import { LOGIC_KERNEL } from '../../../src/core/opr/prompts/logic-kernel';
import { runLive, OPENAI_API_KEY } from '../../live/config';

describe.runIf(runLive)('Live ΩPR with OpenAI', () => {

  it('LIVE-1: real Datalog inference', async () => {
    const runtime = new OprRuntime({
      kernel: LOGIC_KERNEL,
      adapter: new OpenAIOprAdapter({
        apiKey: OPENAI_API_KEY,
        model: 'gpt-4o-mini',
        maxTokens: 1000,
      }),
      budget: { maxAttempts: 3 },
    });

    const result = await runtime.step({
      rules: `
        parent(tom, mary).
        parent(tom, ben).
        parent(mary, ann).
        sibling(X, Y) :- parent(Z, X), parent(Z, Y), X \\= Y.
        grandparent(X, Z) :- parent(X, Y), parent(Y, Z).
      `,
      facts: [],
      state: { iteration: 0, facts: [], derived: [], done: false }
    });

    expect(result.tag).toBe('ok');
    if (result.tag === 'ok') {
      // Should derive sibling(mary, ben) and grandparent(tom, ann)
      expect(result.output.result.delta.length).toBeGreaterThan(0);
    }
  }, 60000);

  it('LIVE-2: retry on malformed response', async () => {
    // Use a model that might make mistakes, verify retry works
    const runtime = new OprRuntime({
      kernel: LOGIC_KERNEL,
      adapter: new OpenAIOprAdapter({
        apiKey: OPENAI_API_KEY,
        model: 'gpt-3.5-turbo',  // More likely to make format errors
        maxTokens: 500,
      }),
      budget: { maxAttempts: 3 },
    });

    const result = await runtime.step({
      rules: `test(X) :- input(X).`,
      facts: ['input(a)', 'input(b)'],
      state: { iteration: 0, facts: [], derived: [], done: false }
    });

    // Even if first attempt fails, should eventually succeed
    expect(result.tag).toBe('ok');
    // Verify receipts show retry attempts if any
    console.log(`Completed in ${result.attempts} attempts`);
  }, 90000);
});
```

---

## REPL Integration

### New REPL Commands

```typescript
// src/repl/commands/opr.ts

export const oprCommands: ReplCommands = {
  ':opr-kernels': {
    description: 'List available ΩPR kernels',
    handler: async (repl) => {
      const kernels = kernelRegistry.list();
      console.log('\nAvailable ΩPR Kernels:');
      for (const id of kernels) {
        const config = kernelRegistry.get(id)!;
        console.log(`  ${id} - ${config.description}`);
      }
      console.log('');
    }
  },

  ':opr-step': {
    description: 'Execute single ΩPR kernel step',
    usage: ':opr-step <kernel-id> <program-expr>',
    handler: async (repl, args) => {
      const [kernelId, ...rest] = args.split(/\s+/);
      const programExpr = rest.join(' ');

      if (!kernelId) {
        console.log('Usage: :opr-step <kernel-id> <program-expr>');
        return;
      }

      const kernel = kernelRegistry.get(kernelId);
      if (!kernel) {
        console.log(`Unknown kernel: ${kernelId}. Use :opr-kernels to list.`);
        return;
      }

      const runtime = createOprRuntime(repl.session, kernel);
      const result = await runtime.step(repl.evalLispExpr(programExpr));

      // Pretty print result
      console.log(formatOprResult(result));
    }
  },

  ':opr-run': {
    description: 'Run ΩPR kernel to fixpoint',
    usage: ':opr-run <kernel-id> <program-expr>',
    handler: async (repl, args) => {
      // Similar to :opr-step but runs runToFixpoint
    }
  },

  ':opr-receipts': {
    description: 'Show ΩPR receipt chain for last operation',
    handler: async (repl) => {
      const receipts = repl.session.oprReceipts;
      if (!receipts || receipts.length === 0) {
        console.log('No ΩPR receipts in current session.');
        return;
      }

      console.log(`\n${receipts.length} receipts in chain:`);
      for (const r of receipts) {
        console.log(`  [${r.attempt}] ${r.status} - ${r.kernel_id}:${r.op}`);
        if (r.errors.length > 0) {
          console.log(`      Errors: ${r.errors.join(', ')}`);
        }
      }

      const validity = verifyReceiptChain(receipts);
      console.log(`\nChain integrity: ${validity.valid ? '✓ VALID' : '✗ BROKEN'}`);
    }
  },

  ':opr-verify': {
    description: 'Verify ΩPR receipt chain integrity',
    usage: ':opr-verify [receipt-file]',
    handler: async (repl, args) => {
      let receipts: OprReceipt[];

      if (args) {
        // Load from file
        const content = await readFile(args, 'utf-8');
        receipts = JSON.parse(content);
      } else {
        receipts = repl.session.oprReceipts;
      }

      const result = verifyReceiptChain(receipts);

      if (result.valid) {
        console.log('✓ Receipt chain is valid');
        console.log(`  ${receipts.length} receipts verified`);
      } else {
        console.log(`✗ Receipt chain broken at index ${result.brokenAt}`);
        console.log(`  Error: ${result.error}`);
      }
    }
  }
};
```

### Session Integration

```typescript
// src/core/session/session.ts - additions

interface Session {
  // ... existing fields ...

  /** ΩPR state for this session */
  oprState: {
    /** Receipts from all ΩPR operations */
    receipts: OprReceipt[];

    /** Active kernel states (for multi-step kernels) */
    activeKernels: Map<string, {
      kernelId: string;
      state: unknown;
      receipts: OprReceipt[];
    }>;

    /** Statistics */
    stats: {
      totalSteps: number;
      totalAttempts: number;
      successfulSteps: number;
      callbacksExecuted: number;
    };
  };
}

// Session methods for ΩPR
class SessionManager {
  // ... existing methods ...

  async oprStep(
    sessionId: string,
    kernelId: string,
    program: unknown,
    state?: unknown
  ): Promise<OprStepResult> {
    const session = this.get(sessionId);
    const kernel = kernelRegistry.get(kernelId);

    const runtime = new OprRuntime({
      kernel,
      adapter: this.createAdapter(session),
      receipts: session.oprState.receipts,
      budget: {
        maxAttempts: 3,
        sessionBudget: session.budget,
      },
      capabilities: kernel.capabilities,
    });

    const result = await runtime.step({ program, state });

    // Update session stats
    session.oprState.stats.totalSteps++;
    session.oprState.stats.totalAttempts += result.attempts;
    if (result.tag === 'ok') {
      session.oprState.stats.successfulSteps++;
    }

    // Store receipts
    session.oprState.receipts.push(...result.receipts);

    // Store active kernel state for multi-step
    if (result.tag === 'ok' && result.output.next_state !== null) {
      session.oprState.activeKernels.set(kernelId, {
        kernelId,
        state: result.output.next_state,
        receipts: result.receipts,
      });
    }

    return result;
  }
}
```

---

## End-to-End Demo Scripts

### Demo 1: Datalog Inference from REPL

```lisp
;; demo/opr/datalog-repl-demo.lisp
;; Run: npx tsx bin/omega-repl.ts --load demo/opr/datalog-repl-demo.lisp

;; ═══════════════════════════════════════════════════════════════════════════
;; Datalog inference with ΩPR - demonstrates contract enforcement and receipts
;; ═══════════════════════════════════════════════════════════════════════════

(display "Loading ΩPR Datalog Demo...")
(newline)

;; Load the logic kernel
(define kernel (opr/load-kernel 'opr.logic.v1))

;; Define our Datalog program
(define family-rules "
  ;; Base facts are provided in input
  ;; Rules derive new facts

  ;; Sibling rule: X and Y are siblings if same parent
  sibling(X, Y) :- parent(P, X), parent(P, Y), X \\= Y.

  ;; Grandparent rule
  grandparent(G, C) :- parent(G, P), parent(P, C).

  ;; Ancestor rule (transitive)
  ancestor(X, Y) :- parent(X, Y).
  ancestor(X, Z) :- parent(X, Y), ancestor(Y, Z).
")

(define family-facts '(
  (parent tom mary)
  (parent tom ben)
  (parent mary ann)
  (parent mary joe)
  (parent ben sam)
))

;; Initial state
(define initial-state
  (hash :iteration 0
        :facts (map (lambda (f) (format #f "~a(~a, ~a)" (car f) (cadr f) (caddr f)))
                    family-facts)
        :derived ()
        :done #f))

;; Run to fixpoint
(display "Running Datalog inference to fixpoint...")
(newline)

(define result
  (opr/run kernel
    (hash :rules family-rules :facts family-facts)
    initial-state))

;; Display results
(newline)
(display "═══════════════════════════════════════════════════════════════════")
(newline)
(display "RESULTS")
(newline)
(display "═══════════════════════════════════════════════════════════════════")
(newline)

(if (opr/ok? result)
    (begin
      (display (format #f "Converged in ~a iterations" (opr/iterations result)))
      (newline)
      (display (format #f "Derived ~a new facts" (length (opr/derived result))))
      (newline)
      (newline)
      (display "Derived facts:")
      (newline)
      (for-each (lambda (f) (display (format #f "  ~a" f)) (newline))
                (opr/derived result))

      (newline)
      (display "Receipt chain:")
      (newline)
      (display (format #f "  ~a receipts, chain valid: ~a"
                       (length (opr/receipts result))
                       (opr/verify-receipts result))))
    (begin
      (display "Error: ")
      (display (opr/error result))))

(newline)
```

### Demo 2: Code Review with Artifact Callbacks

```lisp
;; demo/opr/code-review-demo.lisp
;; Run: npx tsx bin/omega-repl.ts --load demo/opr/code-review-demo.lisp

(display "Loading ΩPR Code Review Demo...")
(newline)

;; Set up sample codebase in artifact store
(artifact/store! "src/auth.ts" "
export async function login(username: string, password: string) {
  // ISSUE: SQL injection vulnerability
  const query = `SELECT * FROM users WHERE name = '${username}'`;
  const user = await db.query(query);

  // ISSUE: Timing attack vulnerability
  if (user.password === password) {
    return createSession(user);
  }
  return null;
}
")

(artifact/store! "src/utils.ts" "
export function sanitize(input: string): string {
  return input.replace(/[<>\"']/g, '');
}

export function hash(data: string): string {
  return crypto.createHash('sha256').update(data).digest('hex');
}
")

(artifact/store! "src/api.ts" "
import { login } from './auth';

export async function handleLogin(req: Request) {
  const { username, password } = await req.json();
  const session = await login(username, password);
  if (!session) {
    return new Response('Unauthorized', { status: 401 });
  }
  return new Response(JSON.stringify({ session }));
}
")

;; Load the analyze kernel
(define kernel (opr/load-kernel 'opr.analyze.v1))

;; Run code review
(display "Running code review...")
(newline)

(define result
  (opr/step kernel
    (hash :files '("src/auth.ts" "src/utils.ts" "src/api.ts")
          :rules '("security" "best-practices"))
    #f))  ;; Single-step kernel

;; Display results
(newline)
(display "═══════════════════════════════════════════════════════════════════")
(newline)
(display "CODE REVIEW FINDINGS")
(newline)
(display "═══════════════════════════════════════════════════════════════════")
(newline)

(if (opr/ok? result)
    (let ((findings (opr/result-get result 'findings))
          (summary (opr/result-get result 'summary)))
      (display (format #f "Files reviewed: ~a" (hash-ref summary 'files_reviewed)))
      (newline)
      (display (format #f "Errors: ~a, Warnings: ~a"
                       (hash-ref summary 'errors)
                       (hash-ref summary 'warnings)))
      (newline)
      (newline)

      (for-each
        (lambda (finding)
          (display (format #f "[~a] ~a:~a"
                           (hash-ref finding 'severity)
                           (hash-ref finding 'file)
                           (hash-ref finding 'line)))
          (newline)
          (display (format #f "  ~a" (hash-ref finding 'message)))
          (newline)
          (when (hash-ref finding 'suggestion)
            (display (format #f "  Suggestion: ~a" (hash-ref finding 'suggestion)))
            (newline))
          (newline))
        findings))
    (begin
      (display "Error: ")
      (display (opr/error result))))

(newline)
```

### Demo 3: Semantic Refactor Safety Check

```lisp
;; demo/opr/refactor-safety-demo.lisp
;; Run: npx tsx bin/omega-repl.ts --load demo/opr/refactor-safety-demo.lisp

(display "Loading ΩPR Refactor Safety Demo...")
(newline)

;; Load semantic kernel
(define kernel (opr/load-kernel 'opr.semantic.v1))

;; Define our refactoring examples
(define refactors
  '(
    ;; Safe: function to arrow function
    (:before "function add(a, b) { return a + b; }"
     :after  "const add = (a, b) => a + b;"
     :name   "function-to-arrow")

    ;; Safe: var to const
    (:before "var x = 5; console.log(x);"
     :after  "const x = 5; console.log(x);"
     :name   "var-to-const")

    ;; UNSAFE: changes behavior (removed null check)
    (:before "function process(x) { if (x != null) return x.toString(); return ''; }"
     :after  "function process(x) { return x.toString(); }"
     :name   "removed-null-check")

    ;; Safe: extract variable
    (:before "console.log(a + b + c + d);"
     :after  "const sum = a + b + c + d; console.log(sum);"
     :name   "extract-variable")
  ))

;; Check each refactor
(display "Checking refactor safety...")
(newline)
(newline)

(for-each
  (lambda (refactor)
    (let* ((name (hash-ref refactor ':name))
           (before (hash-ref refactor ':before))
           (after (hash-ref refactor ':after))
           (result (opr/step kernel
                     (hash :predicate 'refactor-preserves-behavior?
                           :subject (hash :before before :after after))
                     #f)))

      (display (format #f "~a:" name))
      (newline)
      (display (format #f "  Before: ~a" (string-take before 50)))
      (newline)
      (display (format #f "  After:  ~a" (string-take after 50)))
      (newline)

      (if (opr/ok? result)
          (let ((judgment (opr/result-get result 'judgment))
                (confidence (opr/result-get result 'confidence))
                (reasoning (opr/result-get result 'reasoning)))
            (display (format #f "  Safe: ~a (confidence: ~a)"
                             (if judgment "YES ✓" "NO ✗")
                             confidence))
            (newline)
            (display (format #f "  Reasoning: ~a" reasoning))
            (newline))
          (begin
            (display "  Error checking safety")
            (newline)))

      (newline)))
  refactors)
```

---

## Imports Contract

What this task needs from its dependencies:

### From 021-prompt (../../frameir/prompt):
- PromptDoc
- PSystem
- PUser
- PFewShot
- PAttachSchema

### From 021-logic-kernel (./logic-kernel):
- LOGIC_KERNEL

### From 021-analyze-kernel (./analyze-kernel):
- ANALYZE_KERNEL

### From 021-semantic-kernel (./semantic-kernel):
- SEMANTIC_KERNEL

### From 021-vitest (vitest):
- describe
- it
- expect

### From 021-validate (../../../src/core/opr/validate):
- validateKernelOutput

### From 021-runtime (../../../src/core/opr/runtime):
- OprRuntime

### From 021-scripted (../../../src/core/opr/adapters/scripted):
- ScriptedOprAdapter

### From 021-receipts (../../../src/core/opr/receipts):
- InMemoryReceiptStore

### From 021-logic-kernel (../../../src/core/opr/prompts/logic-kernel):
- LOGIC_KERNEL

### From 021-omegaHarness (../../helpers/omegaHarness):
- evalOmega

### From 021-openai (../../../src/core/opr/adapters/openai):
- OpenAIOprAdapter

### From 021-config (../../live/config):
- runLive
- OPENAI_API_KEY

## Implementation Tasks

### Phase 1: Core TypeScript Port

1. Create `src/core/opr/` directory structure
2. Port `runtime.js` → `runtime.ts` with types
3. Port `validate.js` → `validate.ts` with ValidationResult
4. Port `receipts.js` → `receipts.ts` with ReceiptStore
5. Port `retry.js` → `retry.ts`
6. Port `effects.js` → `effects.ts`
7. Port utility modules (hash, canonical, etc.)
8. Write unit tests for all modules

### Phase 2: Kernel Prompt System

1. Create `src/core/opr/prompts/` structure
2. Implement `PromptDoc` → provider message compilation
3. Create LOGIC_KERNEL prompt
4. Create ANALYZE_KERNEL prompt
5. Create SEMANTIC_KERNEL prompt
6. Implement KernelPromptRegistry
7. Write integration tests with ScriptedOprAdapter

### Phase 3: Runtime Integration

1. Add `opr.logic.step` effect handler in `runtimeImpl.ts`
2. Add `opr.analyze.step` effect handler
3. Add `opr.semantic.step` effect handler
4. Implement callback/reentry flow via OprStepK frame
5. Wire Budget integration
6. Write callback integration tests

### Phase 4: REPL Commands

1. Add `:opr-kernels` command
2. Add `:opr-step` command
3. Add `:opr-run` command
4. Add `:opr-receipts` command
5. Add `:opr-verify` command
6. Update session to track OPR state

### Phase 5: Session & Persistence

1. Add `oprState` to Session interface
2. Implement receipt persistence
3. Implement kernel state persistence (for multi-step)
4. Add session-level OPR statistics

### Phase 6: End-to-End Testing & Demos

1. Create `test/opr/e2e/` test suite
2. Create `demo/opr/` demo scripts
3. Create `test/opr/live/` live LLM tests
4. Verify all demos work from REPL

---

## Files to Create

```
OmegaLLM/
├── src/core/opr/
│   ├── runtime.ts           # OprRuntime class - main execution loop
│   ├── validate.ts          # validateKernelOutput() with structured violations
│   ├── receipts.ts          # Receipt creation, chain verification
│   ├── retry.ts             # Retry decision logic, repair prompts
│   ├── effects.ts           # Callback execution, effect commit
│   ├── hash.ts              # sha256Of(), content hashing
│   ├── canonical.ts         # Deterministic JSON serialization
│   ├── jsonExtract.ts       # Extract JSON from LLM response
│   ├── errors.ts            # Error types (OprValidationError, etc.)
│   ├── types.ts             # TypeScript interfaces
│   ├── index.ts             # Public exports
│   │
│   ├── prompts/
│   │   ├── index.ts         # KernelPromptConfig, exports
│   │   ├── registry.ts      # KernelPromptRegistry
│   │   ├── compile.ts       # PromptDoc → provider messages
│   │   ├── logic-kernel.ts  # Datalog inference kernel prompt
│   │   ├── analyze-kernel.ts # Code review kernel prompt
│   │   └── semantic-kernel.ts # Semantic predicate kernel prompt
│   │
│   └── adapters/
│       ├── index.ts         # Adapter exports
│       ├── types.ts         # OprLLMAdapter interface
│       ├── scripted.ts      # ScriptedOprAdapter for tests
│       ├── openai.ts        # OpenAIOprAdapter
│       └── anthropic.ts     # AnthropicOprAdapter
│
├── src/repl/commands/
│   └── opr.ts               # REPL commands (:opr-step, :opr-run, etc.)
│
├── test/opr/
│   ├── unit/
│   │   ├── validate.spec.ts
│   │   ├── receipts.spec.ts
│   │   ├── retry.spec.ts
│   │   ├── hash.spec.ts
│   │   └── prompts.spec.ts
│   ├── integration/
│   │   ├── logic-kernel.spec.ts
│   │   ├── analyze-kernel.spec.ts
│   │   ├── semantic-kernel.spec.ts
│   │   └── callbacks.spec.ts
│   ├── e2e/
│   │   ├── datalog-e2e.spec.ts
│   │   ├── review-e2e.spec.ts
│   │   └── refactor-e2e.spec.ts
│   └── live/
│       ├── openai-opr.spec.ts
│       └── anthropic-opr.spec.ts
│
└── demo/opr/
    ├── datalog-repl-demo.lisp
    ├── code-review-demo.lisp
    ├── refactor-safety-demo.lisp
    └── multi-step-demo.lisp
```

---

## Relationship to Existing ARCHITECTURE

| Existing Spec | Relationship |
|---------------|--------------|
| 08-PROTOCOL | ΩPR exposes via nREPL `opr/step` and `opr/run` ops |
| 21-SECURITY | Capability attenuation for callback effects |
| 22-PROVENANCE | Receipt chain complements evidence chain |
| 23-FACTS | Datalog kernel can query/assert facts via callbacks |
| 24-FIXPOINT | ΩPR `next_state.done` is kernel-level fixpoint |
| 25-BUDGET | ΩPR uses existing Budget for session limits |
| 26-ARTIFACTS | Analyze kernel fetches via callback.artifact.get |
| 28-SESSION | ΩPR receipts integrate with session audit |

The existing specs are for **host Lisp runtime**. ΩPR is specifically for **LLM-as-interpreter boundary** - a different layer that wires INTO the existing infrastructure.

---

## Success Criteria

### Core Functionality
1. ✅ `npm test src/core/opr/` passes all unit tests
2. ✅ `npm test test/opr/integration/` passes integration tests
3. ✅ Can execute `(opr/step kernel program state)` from Lisp
4. ✅ Retry works when LLM returns malformed JSON
5. ✅ Receipts are stored and chain is verifiable

### Kernel Prompts
6. ✅ Logic kernel (opr.logic.v1) derives Datalog facts correctly
7. ✅ Analyze kernel (opr.analyze.v1) produces code review findings
8. ✅ Semantic kernel (opr.semantic.v1) returns judgments with confidence

### Callbacks
9. ✅ `callback.eval_lisp` reenters CEKS evaluator
10. ✅ `callback.artifact.get` fetches from artifact store
11. ✅ `callback.facts.query` queries fact store

### REPL Integration
12. ✅ `:opr-kernels` lists available kernels
13. ✅ `:opr-step kernel expr` executes single step
14. ✅ `:opr-run kernel expr` runs to fixpoint
15. ✅ `:opr-receipts` displays receipt chain
16. ✅ Session tracks OPR statistics

### End-to-End
17. ✅ `demo/opr/datalog-repl-demo.lisp` runs soup-to-nuts
18. ✅ `demo/opr/code-review-demo.lisp` runs soup-to-nuts
19. ✅ `demo/opr/refactor-safety-demo.lisp` runs soup-to-nuts
20. ✅ Live tests pass with real OpenAI API

---

## Non-Goals (Out of Scope)

- **Complex governance profiles** - Use existing ARCHITECTURE specs
- **Elaborate retry strategies** - Simple maxAttempts sufficient for v1
- **Custom effect executors** - Standard callback pattern only
- **Distributed receipts** - Single-process for now
- **Multi-LLM orchestration** - Single kernel per step
- **Prompt versioning/migration** - Manual updates for v1
- **Receipt persistence to disk** - In-memory per session

---

## Reference Documents

- [opr-runtime/README.md](../../opr-runtime/README.md) - Usage and value proposition
- [opr-runtime/ARCHITECTURE.md](../../opr-runtime/ARCHITECTURE.md) - Deep dive on contracts
- [ARCHITECTURE/08-PROTOCOL.md](../ARCHITECTURE/08-PROTOCOL.md) - nREPL protocol
- [ARCHITECTURE/25-BUDGET.md](../ARCHITECTURE/25-BUDGET.md) - Budget infrastructure
- [ARCHITECTURE/28-SESSION.md](../ARCHITECTURE/28-SESSION.md) - Session/audit infrastructure
- [frameir/prompt.ts](../src/frameir/prompt.ts) - PromptIR types

## Verification

```bash
npx tsc --noEmit src/path/to/file.ts
```
