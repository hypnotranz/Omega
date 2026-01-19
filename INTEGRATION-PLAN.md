# OmegaLLM Integration Plan

## Executive Summary

This document details the integration plan for building packages on top of the OmegaLLM kernel. The architecture follows a layered approach:

```
┌──────────────────────────────────────────────────────────────────────────┐
│                          APPLICATION LAYER                               │
│  Solvers, Agents, Workflows, REPLs, IDE Integrations                     │
├──────────────────────────────────────────────────────────────────────────┤
│                          PACKAGE LAYER                                   │
│  ┌─────────────────┐  ┌─────────────────┐  ┌─────────────────┐          │
│  │ Problem-Solving │  │   Enterprise    │  │     World       │          │
│  │     Package     │  │    Package      │  │    Package      │          │
│  │                 │  │                 │  │                 │          │
│  │ • fixpoint      │  │ • policy/pdp    │  │ • world.read    │          │
│  │ • facts         │  │ • audit         │  │ • world.write   │          │
│  │ • memo          │  │ • approval      │  │ • world.run     │          │
│  │ • subeval       │  │ • session       │  │ • world.list    │          │
│  │ • budget        │  │ • replay        │  │ • world.search  │          │
│  │ • intent        │  │ • assurance     │  │ • world.exec    │          │
│  └─────────────────┘  └─────────────────┘  └─────────────────┘          │
├──────────────────────────────────────────────────────────────────────────┤
│                        OMEGA KERNEL LAYER                                │
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐     │
│  │   Effects   │  │   Oracle    │  │ Governance  │  │  Snapshots  │     │
│  │  Handlers   │  │  Protocol   │  │   Caps/     │  │  Receipts   │     │
│  │   (op/k)    │  │  (int.op)   │  │   Budgets   │  │   Stores    │     │
│  └─────────────┘  └─────────────┘  └─────────────┘  └─────────────┘     │
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐     │
│  │  Meaning    │  │    Dist     │  │   Profile   │  │   Machine   │     │
│  │  (MeaningV) │  │  (DistVal)  │  │  (TruthReg) │  │   (CEK)     │     │
│  └─────────────┘  └─────────────┘  └─────────────┘  └─────────────┘     │
└──────────────────────────────────────────────────────────────────────────┘
```

---

## Part 1: Omega Kernel Assessment

### 1.1 Current Kernel Capabilities (READY)

The Omega kernel provides these **unique primitives** that enable the package layer:

| Primitive | File | Status | Description |
|-----------|------|--------|-------------|
| Effect Handlers | `effects/runtimeImpl.ts` | ✅ Ready | Deep handlers with delimited continuations |
| Oracle Protocol | `oracle/protocol.ts` | ✅ Ready | `int.op`, `infer.op`, `rewrite.op`, `search.op` |
| Meaning Values | `oracle/meaning.ts` | ✅ Ready | First-class `MeaningVal` with denotation + obligation |
| Dist Values | `eval/dist.ts` | ✅ Ready | `Dist<Val>` for multi-shot sampling |
| SnapshotRepo | `oracle/snapshots.ts` | ✅ Ready | Env/State snapshotting for REPL-at-any-level |
| ReceiptStore | `oracle/receipts.ts` | ✅ Ready | Oracle request/response recording for replay |
| Capability System | `governance/caps.ts` | ✅ Ready | `capRequire`, `capHas`, wildcard matching |
| Budget Limits | `governance/budgets.ts` | ✅ Ready | `BudgetLimits` type with enforcement |
| Profiles | `governance/profile.ts` | ✅ Ready | `TruthRegime` (speculative/test-certified/proof-certified) |
| Portal (REPL) | `oracle/portalImpl.ts` | ✅ Ready | Oracle can eval/apply/observe back into runtime |
| CEK Machine | `eval/machine.ts` | ✅ Ready | State-based stepper with handler frames |

### 1.2 Kernel Gaps (NEED COMPLETION FIRST)

Before building packages, these kernel primitives should be verified/completed:

| Gap | Priority | Required For | Resolution |
|-----|----------|--------------|------------|
| commit.op enforcement | HIGH | Enterprise Package | Already in `runtimeImpl.ts`, verify truth regime check |
| amb.op handler | MEDIUM | Problem-Solving | Currently returns "Uncaught", need nondet runner |
| Tool registry | MEDIUM | World Package | Need `tool.call` op that dispatches to MCP/native |
| Receipt persistence | LOW | Enterprise replay | InMemory is fine for now |

### 1.3 Kernel Extension Points

Packages build on these **extension mechanisms**:

1. **Effect Handlers**: Install handlers via `(handle expr clauses...)` that intercept any `op`
2. **RuntimeImpl.dispatch**: Fallback handlers for built-in ops (`int.op`, `commit.op`, etc.)
3. **Profiles**: Customize `caps`, `budgets`, `truth` per execution context
4. **SnapshotRepo**: Packages can snapshot/restore state for isolation
5. **ReceiptStore**: Packages can record/replay nondeterministic effects

---

## Part 2: Problem-Solving Package

### 2.1 Overview

The Problem-Solving Package brings LambdaRLM patterns to OmegaLLM with superior semantics.

**Source Documents:**
- [ARCHITECTURE/23-FACTS.md](../LambdaLLM/ARCHITECTURE/23-FACTS.md) - Monotone epistemic state
- [ARCHITECTURE/24-FIXPOINT.md](../LambdaLLM/ARCHITECTURE/24-FIXPOINT.md) - Convergence detection
- [ARCHITECTURE/25-BUDGET.md](../LambdaLLM/ARCHITECTURE/25-BUDGET.md) - Resource tracking
- [ARCHITECTURE/28-SESSION.md](../LambdaLLM/ARCHITECTURE/28-SESSION.md) - Host-controlled execution

### 2.2 Components

#### 2.2.1 Facts System (Monotone Epistemic State)

**What it affords:**
- Propositions that accumulate (never retract)
- Phase gating ("must complete analysis before planning")
- Knowledge sharing across iterations
- Fixpoint convergence detection via fact signatures

**New powers from Omega:**
- Facts become first-class values, can be passed to oracle
- Oracle can observe fact state via portal
- Facts can carry `MeaningVal` evidence (not just IDs)
- Counterfactual fact reasoning via `amb` (what if this fact were true?)

**Implementation:**

```typescript
// src/packages/problem-solving/facts.ts

import type { Val } from "../../core/eval/values";
import { sha256JSON } from "../../core/artifacts/hash";

interface Fact {
  proposition: string;  // Canonical key
  expr: Val;            // Original expression
  evidence?: Val;       // Can be MeaningVal!
  assertedAt: number;
}

export class FactStore {
  private keys = new Set<string>();
  private facts = new Map<string, Fact>();

  assert(expr: Val, evidence?: Val): boolean {
    const key = canonicalKey(expr);
    if (this.keys.has(key)) return false;

    this.keys.add(key);
    this.facts.set(key, {
      proposition: key,
      expr,
      evidence,
      assertedAt: Date.now(),
    });
    return true;
  }

  has(expr: Val): boolean {
    return this.keys.has(canonicalKey(expr));
  }

  all(): Val[] {
    return Array.from(this.facts.values()).map(f => f.expr);
  }

  signature(): string {
    const sorted = Array.from(this.keys).sort();
    return sha256JSON(sorted);
  }
}

// Exposed as effect ops:
// (fact.assert expr) | (fact.assert expr evidence)
// (fact? expr) => bool
// (facts) => list
// (facts.signature) => hash
```

**Lisp API:**
```lisp
;; Assert fact (returns #t if newly added, #f if already known)
(fact.assert '(file-exists "auth.ts"))

;; Assert with evidence (evidence can be a Meaning!)
(let ((m (int.op '(analyze "auth.ts"))))
  (fact.assert '(has-bug "auth.ts" 42) m))

;; Check if fact is known
(fact? '(file-exists "auth.ts"))  ;; => #t

;; Get all facts
(facts)  ;; => ((file-exists "auth.ts") (has-bug "auth.ts" 42) ...)

;; Compute signature for fixpoint
(facts.signature)  ;; => "abc123..."
```

**Testing:**
```typescript
// test/packages/problem-solving/facts.test.ts

import { test, expect } from "bun:test";
import { FactStore } from "../../../src/packages/problem-solving/facts";

test("facts are monotone", () => {
  const fs = new FactStore();
  expect(fs.assert({ tag: "Str", s: "a" })).toBe(true);
  expect(fs.assert({ tag: "Str", s: "a" })).toBe(false); // Already known
  expect(fs.all().length).toBe(1);
});

test("signature changes on new facts", () => {
  const fs = new FactStore();
  const sig1 = fs.signature();
  fs.assert({ tag: "Str", s: "a" });
  const sig2 = fs.signature();
  expect(sig1).not.toBe(sig2);
});

test("same facts produce same signature", () => {
  const fs1 = new FactStore();
  const fs2 = new FactStore();
  fs1.assert({ tag: "Str", s: "a" });
  fs2.assert({ tag: "Str", s: "a" });
  expect(fs1.signature()).toBe(fs2.signature());
});
```

---

#### 2.2.2 Fixpoint System (Convergence Detection)

**What it affords:**
- Iterative refinement with automatic termination
- Multiple signature modes (facts-only, +bindings, +world)
- Cycle detection (oscillation handling)
- Structured outcomes (converged | cycle | nonconverged)

**New powers from Omega:**
- Fixpoint can observe Meaning evolution (not just values)
- Oracle can participate in convergence (emit facts during session)
- Truth regime gates fixpoint commits (speculative can explore, certified commits)
- Dist sampling inside fixpoint for multi-hypothesis convergence

**Implementation:**

```typescript
// src/packages/problem-solving/fixpoint.ts

import type { Val } from "../../core/eval/values";
import type { FactStore } from "./facts";

export type SignatureMode = "facts" | "facts+bindings" | "facts+world" | "facts+bindings+world";

export type FixpointOutcome =
  | { status: "converged"; value: Val; iterations: number; signature: string }
  | { status: "cycle"; value: Val; iterations: number; cycleLength: number }
  | { status: "nonconverged"; value: Val; iterations: number; reason: "max-iters" | "budget" };

export interface FixpointContext {
  facts: FactStore;
  computeBindingsSignature: () => string;
  computeWorldSignature: () => string;
  maxIterations: number;
  mode: SignatureMode;
  detectCycles: boolean;
}

export function computeStateSignature(ctx: FixpointContext): string {
  const parts: string[] = [`facts:${ctx.facts.signature()}`];

  if (ctx.mode.includes("bindings")) {
    parts.push(`bindings:${ctx.computeBindingsSignature()}`);
  }
  if (ctx.mode.includes("world")) {
    parts.push(`world:${ctx.computeWorldSignature()}`);
  }

  return sha256JSON(parts.join("|"));
}

// Effect handler for (fixpoint body :max-iters N :mode M)
// This is implemented as a handler that tracks signatures across iterations
```

**Lisp API:**
```lisp
;; Basic fixpoint (throws on non-convergence)
(fixpoint
  (begin
    (let ((issues (analyze-code)))
      (for-each (lambda (i) (fact.assert `(issue ,i))) issues))
    (facts.count))
  :max-iters 10)

;; Fixpoint with structured outcome
(fixpoint.outcome
  body
  :max-iters 10
  :mode 'facts+world)
;; => (converged value meta) | (cycle value meta) | (nonconverged value meta)

;; Handle all outcomes
(match (fixpoint.outcome body :max-iters 10)
  [(converged value _) value]
  [(cycle value meta)
   (log "Oscillation detected!")
   value]
  [(nonconverged _ _)
   (error "Failed to converge")])
```

**Testing:**
```typescript
test("fixpoint converges when facts stabilize", async () => {
  // Setup: body that asserts facts until condition met
  // Verify: converged status, correct iteration count
});

test("fixpoint detects cycles", async () => {
  // Setup: body that oscillates between two states
  // Verify: cycle status, cycleLength
});

test("fixpoint respects budget", async () => {
  // Setup: body with expensive operations, tight budget
  // Verify: nonconverged with reason: "budget"
});
```

---

#### 2.2.3 Memo System (Memoization)

**What it affords:**
- Cache expensive LLM calls by canonical input
- Invalidation based on world fingerprint changes
- Per-session or persistent memo stores
- Memo eviction policies (LRU, time-based)

**New powers from Omega:**
- Memo can store full `MeaningVal` (not just denotation)
- Memo key includes capability context (different caps = different cache)
- Receipt-backed memo for deterministic replay
- Dist-aware memo (cache entire distributions)

**Implementation:**

```typescript
// src/packages/problem-solving/memo.ts

import type { Val } from "../../core/eval/values";
import type { Hash } from "../../core/artifacts/hash";

export interface MemoStore {
  get(key: Hash): Val | undefined;
  put(key: Hash, value: Val, metadata?: MemoMetadata): void;
  invalidate(predicate: (meta: MemoMetadata) => boolean): number;
  signature(): Hash;
}

export interface MemoMetadata {
  createdAt: number;
  worldFingerprint?: Hash;
  capsDigest?: Hash;
  ttlMs?: number;
}

// Effect: (memo key body) - evaluate body only if not cached
// Effect: (memo.invalidate predicate) - clear matching entries
```

**Lisp API:**
```lisp
;; Memoize expensive LLM call
(memo '(analyze "auth.ts")
  (int.op '(analyze (world.read "auth.ts"))))

;; Memoize with world-aware invalidation
(memo.world '(lint "src/")
  (int.op '(lint (world.list "src/**/*.ts"))))

;; Manual invalidation
(memo.invalidate (lambda (meta) (< (meta :created-at) cutoff)))
```

---

#### 2.2.4 SubEval System (Isolated Evaluation)

**What it affords:**
- Isolated fact stores (facts don't leak out)
- Isolated bindings (can shadow parent)
- Timeout/budget scoping
- Speculative evaluation without commitment

**New powers from Omega:**
- SubEval inherits oracle protocol (can ask LLM in isolation)
- SubEval can return `MeaningVal` (including obligations)
- SubEval can run at different truth regimes
- SubEval snapshots are first-class (can be stored, replayed)

**Implementation:**

```typescript
// src/packages/problem-solving/subeval.ts

import type { Profile } from "../../core/governance/profile";
import type { SnapshotRepo } from "../../core/oracle/snapshots";

export interface SubEvalOptions {
  inheritFacts?: boolean;       // Copy parent facts? (default: false)
  inheritBindings?: boolean;    // Copy parent bindings? (default: true)
  profile?: Profile;            // Override profile for this subeval
  maxSteps?: number;            // Step limit
}

// Effect: (subeval expr options?) => result
// The subeval creates an isolated context, evaluates expr, returns result
// Facts/bindings changes do NOT propagate to parent
```

**Lisp API:**
```lisp
;; Basic subeval (isolated)
(subeval
  (begin
    (fact.assert '(in-subeval))
    (process-data)))
;; Parent does NOT see '(in-subeval) fact

;; Subeval with speculative profile
(subeval
  (begin
    (commit 'file (edit-file "auth.ts")))
  :profile 'speculative)
;; Commit will fail in speculative mode

;; Subeval with inherited facts
(subeval
  body
  :inherit-facts #t)
```

---

#### 2.2.5 Intent System (LLM Goal Decomposition)

**What it affords:**
- Natural language goal → structured plan
- Intent compilation to executable Lisp
- Intent refinement through dialogue
- Intent tracking (what was the original goal?)

**New powers from Omega:**
- Intent is a `MeaningVal` with denotation (plan) + obligation (must execute)
- Intent can be rewritten via `rewrite.op`
- Intent chains form provenance (intent → sub-intent → action)
- Search over intents via `search.op` and `Dist<Meaning>`

**Implementation:**

```typescript
// src/packages/problem-solving/intent.ts

import type { MeaningVal } from "../../core/oracle/meaning";
import type { Val } from "../../core/eval/values";

export interface Intent {
  goal: string;              // Natural language goal
  plan?: Val;                // Compiled plan (Lisp expression)
  meaning?: MeaningVal;      // Full meaning from oracle
  parent?: Intent;           // Parent intent (if decomposed)
  status: "pending" | "executing" | "completed" | "failed";
}

// Effect: (intent goal) => compile goal to executable plan
// Effect: (intent.refine intent feedback) => refine based on feedback
// Effect: (intent.execute intent) => run the compiled plan
```

**Lisp API:**
```lisp
;; Compile intent
(let ((i (intent "fix the authentication bug in auth.ts")))
  (intent.execute i))

;; Intent with refinement
(let* ((i (intent "optimize the database queries"))
       (refined (intent.refine i "focus on N+1 queries")))
  (intent.execute refined))

;; Search for multiple intent interpretations
(let ((interpretations (search.op "implement user logout")))
  (dist.best interpretations))
```

---

### 2.3 Problem-Solving Package Testing Strategy

```
test/packages/problem-solving/
├── facts.test.ts           # Monotone semantics, signatures, evidence
├── fixpoint.test.ts        # Convergence, cycles, budget, modes
├── memo.test.ts            # Caching, invalidation, world-aware
├── subeval.test.ts         # Isolation, inheritance, profiles
├── intent.test.ts          # Compilation, refinement, execution
└── integration.test.ts     # Facts + fixpoint + memo together
```

**Integration Test Example:**
```typescript
test("facts + fixpoint + memo work together", async () => {
  // Setup: solver that uses all three
  const result = await run(`
    (fixpoint
      (begin
        (let ((issues (memo '(lint)
                        (int.op '(lint (world.read "src/**/*.ts"))))))
          (for-each (lambda (i) (fact.assert \`(issue ,i))) issues))
        (facts.count))
      :max-iters 5)
  `);

  expect(result.status).toBe("converged");
  expect(result.iterations).toBeLessThan(5);
});
```

---

## Part 3: Enterprise Package

### 3.1 Overview

The Enterprise Package brings LambdaLLM's enterprise security infrastructure to OmegaLLM.

**Source Documents:**
- [CLAUDE-JOBS-OLD-SPEC/23-POLICY-BUNDLE.md](../LambdaLLM/CLAUDE-JOBS-OLD-SPEC/23-POLICY-BUNDLE.md) - Policy bundles
- [CLAUDE-JOBS-OLD-SPEC/29-PDP.md](../LambdaLLM/CLAUDE-JOBS-OLD-SPEC/29-PDP.md) - Policy Decision Point
- [CLAUDE-JOBS-OLD-SPEC/24-AUDIT-SCHEMA.md](../LambdaLLM/CLAUDE-JOBS-OLD-SPEC/24-AUDIT-SCHEMA.md) - Audit trail
- [CLAUDE-JOBS-OLD-SPEC/25-APPROVAL-SCHEMA.md](../LambdaLLM/CLAUDE-JOBS-OLD-SPEC/25-APPROVAL-SCHEMA.md) - Approvals
- [CLAUDE-JOBS-OLD-SPEC/43-ASSURANCE-CASE.md](../LambdaLLM/CLAUDE-JOBS-OLD-SPEC/43-ASSURANCE-CASE.md) - GSN cases
- [CLAUDE-JOBS-OLD-SPEC/52-EFFECT-REPLAY.md](../LambdaLLM/CLAUDE-JOBS-OLD-SPEC/52-EFFECT-REPLAY.md) - Replay

### 3.2 Components

#### 3.2.1 Policy System (PDP + PolicyBundle)

**What it affords:**
- Declarative policy rules (allow/deny/require-approval)
- XACML-style combining algorithms (deny-overrides, permit-overrides)
- Versioned, hash-addressable policy bundles
- Obligations that must be satisfied (redaction, audit, approval)

**New powers from Omega:**
- Policy decisions are `MeaningVal` (can reason about why decision was made)
- Policies can reference oracle judgments (LLM-in-the-policy-loop)
- Policy obligations can include Dist requirements (sampling constraints)
- Truth regime gates policy enforcement (speculative = advisory only)

**Implementation:**

```typescript
// src/packages/enterprise/policy/policy-bundle.ts

import type { Val } from "../../../core/eval/values";
import type { Hash } from "../../../core/artifacts/hash";

export interface PolicyRule {
  id: string;
  target: PolicyTarget;          // What this rule applies to
  effect: "allow" | "deny";
  condition?: Val;               // Lisp expression that evaluates to bool
  obligations?: PolicyObligation[];
}

export interface PolicyBundle {
  schemaVersion: 1;
  bundleId: string;
  tenantId: string;
  version: number;
  rules: PolicyRule[];
  combiningAlgorithm: "deny-overrides" | "permit-overrides" | "first-applicable";
  bundleHash: Hash;
}

// src/packages/enterprise/policy/pdp.ts

export interface PolicyInput {
  op: string;              // Operation being requested
  resource?: string;       // Resource path
  principal?: string;      // Who is requesting
  labels?: string[];       // Data classification labels
  purpose?: string;        // Purpose of use
}

export interface PolicyDecision {
  allow: boolean;
  reason: string;
  obligations: PolicyObligation[];
  advice: PolicyAdvice[];
  decisionHash: Hash;
  policyHash: Hash;
  ruleIds: string[];
}

export interface PDP {
  readonly bundle: PolicyBundle;
  decide(input: PolicyInput): PolicyDecision;
}
```

**Lisp API:**
```lisp
;; Check policy before operation
(let ((decision (policy.check 'world.write "src/auth.ts")))
  (if (decision :allow)
      (world.write "src/auth.ts" content)
      (error (str "Policy denied: " (decision :reason)))))

;; Policy with obligations
(let ((decision (policy.check 'llm.complete prompt)))
  (when (decision :allow)
    (for-each policy.satisfy (decision :obligations))
    (llm.complete prompt)))
```

---

#### 3.2.2 Audit System (Tamper-Evident Ledger)

**What it affords:**
- Hash-chained audit trail (append-only, tamper-evident)
- Deterministic envelope hashing for verification
- Operation + principal + timestamp + result
- Export to external SIEM systems

**New powers from Omega:**
- Audit entries can include `MeaningVal` (audit the reasoning, not just action)
- Audit chain includes oracle receipts (deterministic replay of LLM calls)
- Truth regime included in audit (was this speculative or certified?)
- Dist samples audited (which hypothesis was chosen?)

**Implementation:**

```typescript
// src/packages/enterprise/audit/audit-ledger.ts

import type { Hash } from "../../../core/artifacts/hash";
import type { Val } from "../../../core/eval/values";

export interface AuditEntry {
  seq: number;               // Sequential number
  timestamp: string;         // ISO 8601
  principalId: string;
  sessionId: string;
  operation: string;
  args: Val;                 // Redacted/summarized
  result?: Val;              // Redacted/summarized
  policyDecision?: Hash;     // Reference to policy decision
  truthRegime: string;       // speculative/test-certified/proof-certified
  prevHash: Hash;            // Previous entry hash
  entryHash: Hash;           // This entry hash
}

export interface AuditLedger {
  append(entry: Omit<AuditEntry, "seq" | "prevHash" | "entryHash">): AuditEntry;
  verify(): boolean;         // Verify hash chain integrity
  export(from: number, to: number): AuditEntry[];
}
```

**Lisp API:**
```lisp
;; Audit is automatic for all operations, but can be queried
(audit.entries :from 0 :to 100)

;; Verify audit chain integrity
(audit.verify)  ;; => #t or (error ...)

;; Export for external analysis
(audit.export "audit-2024-01.jsonl")
```

---

#### 3.2.3 Approval System

**What it affords:**
- Human-in-the-loop approvals for sensitive operations
- Cryptographically signed approval receipts
- Time-bounded, scoped approvals (not blanket permissions)
- Approval workflow (request → review → approve/deny)

**New powers from Omega:**
- Approval requests include `MeaningVal` (explain why needed)
- Approval can be conditional on obligation satisfaction
- Oracle can participate in approval (AI-assisted review)
- Approval flows through truth regime (speculative = no approval needed)

**Implementation:**

```typescript
// src/packages/enterprise/approval/approval-store.ts

export interface ApprovalRequest {
  id: string;
  sessionId: string;
  principalId: string;
  operation: string;
  resource?: string;
  justification: string;     // Why is this needed?
  meaning?: Val;             // MeaningVal explaining the request
  createdAt: string;
  expiresAt: string;
  status: "pending" | "approved" | "denied" | "expired";
}

export interface ApprovalReceipt {
  requestId: string;
  approverId: string;
  decision: "approved" | "denied";
  reason?: string;
  approvedAt: string;
  signature: string;         // Cryptographic signature
}
```

**Lisp API:**
```lisp
;; Request approval (blocks until approved or denied)
(let ((receipt (approval.request
                 :op 'world.write
                 :resource "src/auth.ts"
                 :justification "Fix security vulnerability")))
  (if receipt
      (world.write "src/auth.ts" content)
      (error "Approval denied")))

;; Check if operation requires approval
(if (approval.required? 'world.run "rm -rf /")
    (approval.request ...)
    (world.run ...))
```

---

#### 3.2.4 Session System (Host-Controlled Execution)

**What it affords:**
- Host controls the loop (not LLM)
- Per-session budget, policy, audit context
- Proposal mode (writes are staged, not committed)
- Pause/resume/abort lifecycle
- Turn-based execution with metrics

**New powers from Omega:**
- Session profile includes truth regime (speculative exploration vs certified commits)
- Session can snapshot/restore via SnapshotRepo
- Session receipts enable exact replay
- Multiple sessions can share oracle (consistent reasoning)

**Implementation:**

```typescript
// src/packages/enterprise/session/session.ts

import type { Profile } from "../../../core/governance/profile";
import type { SnapshotRepo } from "../../../core/oracle/snapshots";
import type { ReceiptStore } from "../../../core/oracle/receipts";

export interface Session {
  id: string;
  profile: Profile;
  snapshots: SnapshotRepo;
  receipts: ReceiptStore;

  // State
  turnCount: number;
  status: SessionStatus;
  proposals: Proposal[];     // Staged writes

  // Methods
  executeTurn(code: string): Promise<TurnResult>;
  commit(): Promise<void>;   // Apply proposals
  abort(): void;             // Discard proposals
  pause(): void;
  resume(): void;
  snapshot(): SessionSnapshot;
}

export type SessionStatus =
  | "active"
  | "paused"
  | "completed"
  | "aborted"
  | "budget-exceeded";
```

---

#### 3.2.5 Replay System (Deterministic Reproduction)

**What it affords:**
- Record all nondeterministic effects (LLM, time, random, network)
- Replay with exact same outputs
- Incident response (reproduce the bug)
- Regression testing (same inputs = same outputs)

**New powers from Omega:**
- Replay via ReceiptStore already in kernel
- Oracle sessions are receipt-backed by default
- Replay includes Meaning evolution (how did reasoning develop?)
- Dist samples are reproducible (same seed = same choices)

**Implementation:**

```typescript
// src/packages/enterprise/replay/effect-tape.ts

import type { Val } from "../../../core/eval/values";
import type { Hash } from "../../../core/artifacts/hash";
import type { OracleReceipt, ReceiptStore } from "../../../core/oracle/receipts";

export type EffectKind = "llm.complete" | "time.now" | "random.u64" | "network.http";

export interface EffectRecord {
  seq: number;
  kind: EffectKind;
  request: Val;
  response: Val;
  requestHash: Hash;
  responseHash: Hash;
}

export interface EffectTape {
  records: EffectRecord[];
  mode: "record" | "replay";

  record(kind: EffectKind, request: Val, response: Val): void;
  replay(kind: EffectKind, request: Val): Val | undefined;
}

// Integration with ReceiptStore
export function receiptStoreToEffectTape(store: ReceiptStore): EffectTape;
export function effectTapeToReceiptStore(tape: EffectTape): ReceiptStore;
```

---

#### 3.2.6 Assurance Case System (GSN Argumentation)

**What it affords:**
- GSN-style argumentation graphs (Goal → Strategy → Solution)
- Machine-checkable claims with evidence
- Regulatory compliance documentation
- Trust certificate generation

**New powers from Omega:**
- Goals/strategies/solutions are `MeaningVal` with obligations
- Assurance case can reference oracle reasoning chains
- Evidence can include Dist (confidence distributions)
- Auto-generate cases from execution traces

**Implementation:**

```typescript
// src/packages/enterprise/assurance/assurance-case.ts

import type { Val } from "../../../core/eval/values";
import type { Hash } from "../../../core/artifacts/hash";

export type CaseNode =
  | { kind: "goal"; id: string; statement: string; supportedBy: string[] }
  | { kind: "strategy"; id: string; statement: string; supportedBy: string[] }
  | { kind: "solution"; id: string; statement: string; evidence: string[] }
  | { kind: "context"; id: string; statement: string }
  | { kind: "assumption"; id: string; statement: string };

export interface AssuranceCase {
  schemaVersion: 1;
  tenantId: string;
  sessionId: string;
  policyHash: Hash;
  title: string;
  topGoalId: string;
  nodes: CaseNode[];
  createdAt: string;
  caseHash: Hash;
}
```

---

### 3.3 Enterprise Package Testing Strategy

```
test/packages/enterprise/
├── policy/
│   ├── policy-bundle.test.ts    # Bundle creation, hashing
│   ├── pdp.test.ts              # Decision making, combining algorithms
│   └── enforcement.test.ts      # Integration with runtime
├── audit/
│   ├── audit-ledger.test.ts     # Hash chain, append, verify
│   └── export.test.ts           # SIEM export formats
├── approval/
│   ├── approval-store.test.ts   # Request/receipt lifecycle
│   └── workflow.test.ts         # End-to-end approval flow
├── session/
│   ├── session.test.ts          # Lifecycle, turns, proposals
│   └── proposal-mode.test.ts    # Staged writes, commit/abort
├── replay/
│   ├── effect-tape.test.ts      # Record/replay effects
│   └── determinism.test.ts      # Same inputs = same outputs
└── assurance/
    ├── assurance-case.test.ts   # GSN graph construction
    └── evidence.test.ts         # Evidence collection
```

---

## Part 4: World Package

### 4.1 Overview

The World Package provides the interface to external reality (filesystem, network, tools).

**Source Documents:**
- [ARCHITECTURE/07-FFI.md](../LambdaLLM/ARCHITECTURE/07-FFI.md) - Foreign function interface
- [CLAUDE-JOBS-OLD-SPEC/06-FFI.md](../LambdaLLM/CLAUDE-JOBS-OLD-SPEC/06-FFI.md) - FFI implementation

### 4.2 Components

#### 4.2.1 World Interface (Core Operations)

**What it affords:**
- Read files (`world.read`)
- Write files (`world.write`)
- List/search files (`world.list`, `world.search`)
- Execute commands (`world.run`, `world.exec`)
- Compute fingerprints (`world.fingerprint`)

**New powers from Omega:**
- World operations go through effect handlers (can be intercepted)
- Policy enforcement automatic via Enterprise package
- Audit trail automatic for all world mutations
- Proposal mode via session (writes are staged)

**Implementation:**

```typescript
// src/packages/world/world.ts

import type { Val } from "../../core/eval/values";
import type { Hash } from "../../core/artifacts/hash";

export interface World {
  // Read
  read(ref: string): Promise<Val>;
  list(pattern: string): Promise<Val>;  // Glob pattern
  search(pattern: string, options?: SearchOptions): Promise<Val>;

  // Write
  write(ref: string, content: Val): Promise<void>;
  delete(ref: string): Promise<void>;

  // Execute
  run(command: string, options?: RunOptions): Promise<Val>;
  exec(script: string, options?: ExecOptions): Promise<Val>;

  // Fingerprint
  fingerprint(pattern?: string): Promise<Hash>;

  // Snapshotting
  snapshot(): Promise<WorldSnapshot>;
  restore(snapshot: WorldSnapshot): Promise<void>;
}

// Implementations
export class FileSystemWorld implements World { ... }
export class InMemoryWorld implements World { ... }
export class ProposalWorld implements World { ... }  // Wraps another world, stages writes
```

**Lisp API:**
```lisp
;; Read
(world.read "src/auth.ts")
(world.list "src/**/*.ts")
(world.search "function authenticate")

;; Write
(world.write "src/auth.ts" new-content)

;; Execute (DANGEROUS - usually policy-denied)
(world.run "npm test")

;; Fingerprint for fixpoint
(world.fingerprint "src/")
```

---

#### 4.2.2 Tool Registry (MCP + Native Tools)

**What it affords:**
- Registry of available tools (MCP servers, native functions)
- Tool discovery and introspection
- Unified call interface
- Tool capability requirements

**New powers from Omega:**
- Tools are effect ops (can be handled/intercepted)
- Tools require capabilities (`tool.read`, `tool.write`, `tool.exec`)
- Tool calls are audited
- Tool results can be `MeaningVal`

**Implementation:**

```typescript
// src/packages/world/tools/registry.ts

export interface Tool {
  name: string;
  description: string;
  inputSchema: JSONSchema;
  outputSchema?: JSONSchema;
  requiredCaps: string[];

  call(args: Val): Promise<Val>;
}

export interface ToolRegistry {
  register(tool: Tool): void;
  get(name: string): Tool | undefined;
  list(): Tool[];
  call(name: string, args: Val): Promise<Val>;
}

// MCP adapter
export class MCPToolRegistry implements ToolRegistry { ... }
```

**Lisp API:**
```lisp
;; List available tools
(tools.list)  ;; => (("bash" ...) ("read_file" ...) ...)

;; Call tool
(tool.call "bash" '(("command" . "ls -la")))

;; Check tool availability
(tool.available? "bash")
```

---

### 4.3 World Package Testing Strategy

```
test/packages/world/
├── filesystem-world.test.ts    # Real filesystem operations
├── inmemory-world.test.ts      # In-memory for fast tests
├── proposal-world.test.ts      # Staged writes
├── fingerprint.test.ts         # Deterministic fingerprinting
├── tools/
│   ├── registry.test.ts        # Tool registration/lookup
│   └── mcp-adapter.test.ts     # MCP server integration
└── integration.test.ts         # World + policy + audit
```

---

## Part 5: Package Dependencies

### 5.1 Build Order

```
Phase 1: Kernel Verification
├── Verify effect handlers work correctly
├── Verify oracle protocol (int.op, search.op, etc.)
├── Verify governance (caps, budgets, profiles)
└── Verify snapshots/receipts

Phase 2: World Package (Foundation for others)
├── World interface
├── FileSystemWorld
├── InMemoryWorld
├── ProposalWorld
└── Tool registry

Phase 3: Problem-Solving Package
├── Facts (depends on World for signatures)
├── Memo (depends on World for invalidation)
├── Fixpoint (depends on Facts)
├── SubEval (depends on Snapshots)
└── Intent (depends on Oracle)

Phase 4: Enterprise Package
├── Policy Bundle (no deps)
├── PDP (depends on Policy Bundle)
├── Audit Ledger (no deps)
├── Approval Store (no deps)
├── Session (depends on World, Policy, Audit)
├── Replay (depends on Receipts)
└── Assurance (depends on everything)
```

### 5.2 Dependency Graph

```
                                  ┌─────────────────┐
                                  │   Assurance     │
                                  │     Case        │
                                  └────────┬────────┘
                                           │
            ┌──────────────────────────────┼──────────────────────────────┐
            │                              │                              │
            ▼                              ▼                              ▼
     ┌─────────────┐               ┌─────────────┐               ┌─────────────┐
     │   Session   │◀──────────────│   Replay    │               │   Intent    │
     └──────┬──────┘               └──────┬──────┘               └──────┬──────┘
            │                              │                              │
            ├──────────────┬───────────────┤                              │
            ▼              ▼               ▼                              │
     ┌─────────────┐ ┌─────────────┐ ┌─────────────┐                     │
     │   Policy    │ │   Audit     │ │  Approval   │                     │
     │    (PDP)    │ │   Ledger    │ │   Store     │                     │
     └──────┬──────┘ └─────────────┘ └─────────────┘                     │
            │                                                             │
            ▼                                                             │
     ┌─────────────┐               ┌─────────────┐               ┌───────┴───────┐
     │   Policy    │               │   Fixpoint  │◀──────────────│    SubEval    │
     │   Bundle    │               └──────┬──────┘               └───────────────┘
     └─────────────┘                      │
                                          │
            ┌──────────────────────────────┼──────────────────────────────┐
            ▼                              ▼                              ▼
     ┌─────────────┐               ┌─────────────┐               ┌─────────────┐
     │   Facts     │               │    Memo     │               │   World     │
     └──────┬──────┘               └──────┬──────┘               └──────┬──────┘
            │                              │                              │
            └──────────────────────────────┴──────────────────────────────┘
                                           │
                                           ▼
                              ┌────────────────────────┐
                              │    OMEGA KERNEL        │
                              │ (Effects, Oracle, Caps,│
                              │  Snapshots, Receipts)  │
                              └────────────────────────┘
```

---

## Part 6: What's Gained by This Architecture

### 6.1 From Omega Kernel

| Kernel Primitive | Traditional Approach | Omega Approach | Gain |
|------------------|---------------------|----------------|------|
| Effect Handlers | Try/catch, callbacks | Delimited continuations | **Composable interception** - any op can be handled at any level |
| `MeaningVal` | Return strings/JSON | First-class Meaning | **Reasoning is data** - can inspect, store, compare reasoning |
| `Dist<Val>` | Single answer | Distribution of answers | **Multi-hypothesis** - explore alternatives, pick best |
| Truth Regimes | All-or-nothing | Speculative/Certified | **Graduated commitment** - explore freely, commit carefully |
| Snapshots | Manual state saving | First-class snapshots | **Time-travel** - replay from any point |
| Receipts | Logging | Deterministic replay | **Perfect reproduction** - debug any incident |

### 6.2 New Capabilities Enabled

#### 6.2.1 Counterfactual Reasoning
```lisp
;; What would happen if this file didn't exist?
(let ((result (subeval
                (begin
                  (world.delete "auth.ts")
                  (int.op '(analyze-security)))
                :profile 'speculative)))
  ;; File is NOT deleted in reality, just in the counterfactual
  (format "If auth.ts were missing: ~a" result))
```

#### 6.2.2 Multi-Hypothesis Search
```lisp
;; Get distribution of possible fixes
(let ((fixes (search.op '(fix-bug (world.read "auth.ts")))))
  ;; fixes is Dist<Meaning>
  (let ((best (dist.argmax fixes :by obligation-score)))
    (if (obligation-satisfied? best)
        (commit 'file (meaning-denotation best))
        (error "No fix satisfies all obligations"))))
```

#### 6.2.3 Reasoning Provenance
```lisp
;; Track why each decision was made
(let* ((analysis (int.op '(analyze "auth.ts")))
       (plan (int.op '(plan-fix analysis)))
       (fix (int.op '(execute-plan plan))))
  ;; Each step is a MeaningVal with full provenance
  ;; analysis → plan → fix forms a reasoning chain
  (audit.record fix :ancestors (list analysis plan)))
```

#### 6.2.4 Truth-Gated Commits
```lisp
;; Speculative exploration (no commits)
(subeval
  (begin
    (world.write "auth.ts" bad-code)
    (commit 'file))
  :profile 'speculative)
;; Error: commit rejected in speculative truth regime

;; Certified commit (requires obligations)
(let ((fix (int.op '(fix-and-test "auth.ts"))))
  (if (obligation-satisfied? fix)
      (commit 'file (meaning-denotation fix))
      (error "Tests must pass before commit")))
```

---

## Part 7: Full Reference List

### ARCHITECTURE Documents (LambdaLLM)

| Doc | Description | Relevant Package |
|-----|-------------|-----------------|
| [23-FACTS.md](../LambdaLLM/ARCHITECTURE/23-FACTS.md) | Monotone epistemic state | Problem-Solving |
| [24-FIXPOINT.md](../LambdaLLM/ARCHITECTURE/24-FIXPOINT.md) | Convergence detection | Problem-Solving |
| [25-BUDGET.md](../LambdaLLM/ARCHITECTURE/25-BUDGET.md) | Resource tracking | Problem-Solving + Enterprise |
| [26-ARTIFACTS.md](../LambdaLLM/ARCHITECTURE/26-ARTIFACTS.md) | Content-addressed storage | All |
| [27-OUTCOMES.md](../LambdaLLM/ARCHITECTURE/27-OUTCOMES.md) | Structured returns | Problem-Solving |
| [28-SESSION.md](../LambdaLLM/ARCHITECTURE/28-SESSION.md) | Host-controlled execution | Enterprise |
| [29-EXPERTS.md](../LambdaLLM/ARCHITECTURE/29-EXPERTS.md) | Expert routing | Problem-Solving |

### CLAUDE-JOBS-OLD-SPEC Documents

| Doc | Description | Relevant Package |
|-----|-------------|-----------------|
| [20-CRYPTO.md](../LambdaLLM/CLAUDE-JOBS-OLD-SPEC/20-CRYPTO.md) | SHA-256, HMAC, canonical JSON | Enterprise |
| [21-CONTEXT.md](../LambdaLLM/CLAUDE-JOBS-OLD-SPEC/21-CONTEXT.md) | ExecutionContext, Principal | Enterprise |
| [22-CAPABILITY.md](../LambdaLLM/CLAUDE-JOBS-OLD-SPEC/22-CAPABILITY.md) | Capability refs | Enterprise |
| [23-POLICY-BUNDLE.md](../LambdaLLM/CLAUDE-JOBS-OLD-SPEC/23-POLICY-BUNDLE.md) | Policy bundles | Enterprise |
| [24-AUDIT-SCHEMA.md](../LambdaLLM/CLAUDE-JOBS-OLD-SPEC/24-AUDIT-SCHEMA.md) | Audit trail | Enterprise |
| [25-APPROVAL-SCHEMA.md](../LambdaLLM/CLAUDE-JOBS-OLD-SPEC/25-APPROVAL-SCHEMA.md) | Approval flow | Enterprise |
| [26-CLASSIFIER-REDACTOR.md](../LambdaLLM/CLAUDE-JOBS-OLD-SPEC/26-CLASSIFIER-REDACTOR.md) | Data classification | Enterprise |
| [27-JOURNAL-EGRESS.md](../LambdaLLM/CLAUDE-JOBS-OLD-SPEC/27-JOURNAL-EGRESS.md) | Effect recording | Enterprise |
| [29-PDP.md](../LambdaLLM/CLAUDE-JOBS-OLD-SPEC/29-PDP.md) | Policy Decision Point | Enterprise |
| [30-AUDIT-LEDGER.md](../LambdaLLM/CLAUDE-JOBS-OLD-SPEC/30-AUDIT-LEDGER.md) | Hash-chained audit | Enterprise |
| [31-APPROVAL-STORE.md](../LambdaLLM/CLAUDE-JOBS-OLD-SPEC/31-APPROVAL-STORE.md) | Approval storage | Enterprise |
| [32-ENFORCER.md](../LambdaLLM/CLAUDE-JOBS-OLD-SPEC/32-ENFORCER.md) | Policy enforcement | Enterprise |
| [43-ASSURANCE-CASE.md](../LambdaLLM/CLAUDE-JOBS-OLD-SPEC/43-ASSURANCE-CASE.md) | GSN argumentation | Enterprise |
| [52-EFFECT-REPLAY.md](../LambdaLLM/CLAUDE-JOBS-OLD-SPEC/52-EFFECT-REPLAY.md) | Deterministic replay | Enterprise |

---

## Part 8: Testing Verification Checklist

### 8.1 Kernel Verification (Before Packages)

- [ ] Effect handlers correctly intercept ops
- [ ] `int.op` returns `MeaningVal`
- [ ] `search.op` returns `Dist<MeaningVal>`
- [ ] `commit.op` respects truth regime
- [ ] SnapshotRepo stores/retrieves correctly
- [ ] ReceiptStore records/replays correctly
- [ ] `capRequire` throws on missing caps
- [ ] `capHas` handles wildcards

### 8.2 World Package Verification

- [ ] `world.read` returns file content
- [ ] `world.write` persists to disk (or staged)
- [ ] `world.list` matches glob patterns
- [ ] `world.fingerprint` is deterministic
- [ ] `world.run` executes commands
- [ ] ProposalWorld stages writes
- [ ] Tool registry loads/calls tools

### 8.3 Problem-Solving Package Verification

- [ ] Facts are monotone (never retract)
- [ ] Fact signatures are deterministic
- [ ] Fixpoint converges when facts stabilize
- [ ] Fixpoint detects cycles
- [ ] Memo caches by canonical key
- [ ] Memo invalidates on world change
- [ ] SubEval isolates facts/bindings
- [ ] Intent compiles to executable plan

### 8.4 Enterprise Package Verification

- [ ] Policy bundles hash correctly
- [ ] PDP evaluates rules with combining
- [ ] Audit ledger is hash-chained
- [ ] Audit verify catches tampering
- [ ] Approval request/receipt lifecycle
- [ ] Session respects budget
- [ ] Session proposal mode works
- [ ] Replay produces identical output
- [ ] Assurance case validates structure

---

## Part 9: Implementation Timeline

### Phase 1: Kernel Verification (1 day)
- Run existing tests
- Add missing kernel tests
- Document any gaps

### Phase 2: World Package (2-3 days)
- FileSystemWorld implementation
- InMemoryWorld implementation
- ProposalWorld wrapper
- Tool registry

### Phase 3: Problem-Solving Package (3-4 days)
- Facts system
- Memo system
- Fixpoint system
- SubEval system
- Intent system

### Phase 4: Enterprise Package (4-5 days)
- Policy bundle + PDP
- Audit ledger
- Approval store
- Session management
- Replay system
- Assurance case

### Phase 5: Integration Testing (2-3 days)
- Cross-package integration
- End-to-end scenarios
- Performance profiling

**Total Estimated Time: 2-3 weeks**

---

## Appendix A: Effect Op Summary

| Op | Package | Description | Returns |
|----|---------|-------------|---------|
| `int.op` | Kernel | Interpret payload | `MeaningVal` |
| `infer.op` | Kernel | Interpret, return denotation | `Val` |
| `rewrite.op` | Kernel | Rewrite interpretation | `MeaningVal` |
| `search.op` | Kernel | Multi-sample search | `Dist<MeaningVal>` |
| `commit.op` | Kernel | Commit with truth check | `Val` |
| `fact.assert` | Problem-Solving | Assert monotone fact | `Bool` |
| `fact?` | Problem-Solving | Check fact exists | `Bool` |
| `fixpoint` | Problem-Solving | Iterate to convergence | `Outcome` |
| `memo` | Problem-Solving | Memoize computation | `Val` |
| `subeval` | Problem-Solving | Isolated evaluation | `Val` |
| `world.read` | World | Read file | `Val` |
| `world.write` | World | Write file | `Unit` |
| `world.run` | World | Execute command | `Val` |
| `policy.check` | Enterprise | Check policy | `Decision` |
| `audit.append` | Enterprise | Log audit entry | `Entry` |
| `approval.request` | Enterprise | Request approval | `Receipt?` |

---

## Appendix B: File Structure

```
OmegaLLM/
├── src/
│   ├── core/                      # Kernel (existing)
│   │   ├── eval/
│   │   ├── effects/
│   │   ├── oracle/
│   │   ├── governance/
│   │   └── ...
│   │
│   └── packages/                  # New package layer
│       ├── problem-solving/
│       │   ├── facts.ts
│       │   ├── fixpoint.ts
│       │   ├── memo.ts
│       │   ├── subeval.ts
│       │   ├── intent.ts
│       │   └── index.ts
│       │
│       ├── enterprise/
│       │   ├── policy/
│       │   │   ├── policy-bundle.ts
│       │   │   └── pdp.ts
│       │   ├── audit/
│       │   │   └── audit-ledger.ts
│       │   ├── approval/
│       │   │   └── approval-store.ts
│       │   ├── session/
│       │   │   └── session.ts
│       │   ├── replay/
│       │   │   └── effect-tape.ts
│       │   ├── assurance/
│       │   │   └── assurance-case.ts
│       │   └── index.ts
│       │
│       └── world/
│           ├── world.ts
│           ├── filesystem-world.ts
│           ├── inmemory-world.ts
│           ├── proposal-world.ts
│           ├── tools/
│           │   ├── registry.ts
│           │   └── mcp-adapter.ts
│           └── index.ts
│
├── test/
│   └── packages/                  # Package tests
│       ├── problem-solving/
│       ├── enterprise/
│       └── world/
│
└── INTEGRATION-PLAN.md            # This document
```

---

*Document Version: 1.0*
*Last Updated: 2025-01-17*
*Author: Integration planning session*
