# JOB-011: Unified Layer Documentation - RSR + FrameLisp + LambdaRLM/LLM

**Status**: OPEN
**Priority**: P0 (Architectural foundation)
**Supersedes**: JOB-010 (FrameLisp-only documentation)

---

## The Problem: Primitive Proliferation

We have primitives from multiple sources that SEEM parallel but are actually LAYERED:

1. **ARCHITECTURE-EXPLANATION.md** - RSR protocol (EffectReq, RuntimeDriver, Receipts)
2. **REFERENCE-ALGEBRA.md** - FrameLisp (infer, prompt+, chat-turn)
3. **LambdaRLM lib/*.lisp** - Domain patterns (composable-solvers, meta-search)
4. **LambdaLLM ARCHITECTURE/*.md** - Specs (facts, fixpoint, experts, continuations)

The smell: treating these as peer primitives when they're actually LAYERS.

---

## The Insight: RSR is the Kernel, FrameLisp is the Standard Library

From ARCHITECTURE-EXPLANATION.md:

> "The evaluator interprets your object language (Ω/Lisp). The RuntimeDriver interprets effects."

FrameLisp's `infer` is NOT a primitive - it's implemented via:
```typescript
EffectReq({ tag: "ReqTool", call: { tool: "infer.op", prompt: "..." } })
```

The RSR effect protocol is the ACTUAL mechanism. FrameLisp is a convenient API on top.

---

## Correct Layering (7 Layers)

```
┌─────────────────────────────────────────────────────────────┐
│ Layer 6: LambdaRLM/LambdaLLM Domain Patterns                 │
│   composable-solvers, meta-search, facts, fixpoint, experts │
├─────────────────────────────────────────────────────────────┤
│ Layer 5: FrameLisp Protocols                                 │
│   chat-turn, tool-loop, rag, complete                       │
├─────────────────────────────────────────────────────────────┤
│ Layer 4: FrameLisp Execution Algebra                         │
│   bind, branch, retry-until, all/race, streaming            │
├─────────────────────────────────────────────────────────────┤
│ Layer 3: FrameLisp Prompt Algebra + Kernel API               │
│   infer, call-tool, validate + prompt+, as-data, defprompt  │
├═════════════════════════════════════════════════════════════┤
│ Layer 2: RSR Effect Protocol                                 │
│   EffectReq/Resp, RuntimeDriver, EffectLedger, Portal       │
│   Receipts, Snapshots, Adapters, Canonicalization           │
├─────────────────────────────────────────────────────────────┤
│ Layer 1: OmegaLLM Effects & Standard Library                 │
│   conditions, handlers, restarts, streams, amb, concurrency │
├─────────────────────────────────────────────────────────────┤
│ Layer 0: OmegaLLM Core                                       │
│   CEKS machine, eval/apply, primitives (cons, car, etc.)    │
└─────────────────────────────────────────────────────────────┘
```

---

## Layer 2: RSR Effect Protocol (from ARCHITECTURE-EXPLANATION.md)

### RSR-01: Two Loops Architecture

| # | Symbol | Type | Purpose | OmegaLLM |
|---|--------|------|---------|----------|
| 1 | `EffectSession` | Type | `AsyncGenerator<EffectReq, Val, EffectResp>` - the suspension protocol | ✓ OracleSession |
| 2 | `EffectReq` | Union | Request from evaluator (ReqEval/ReqApply/ReqTool/ReqReturn/ReqFail) | ✓ OracleReq |
| 3 | `EffectResp` | Union | Response to evaluator (RespVal/RespFail) | ✓ OracleResp |
| 4 | `RuntimeDriver` | Class | The "second eval/apply loop" - dispatches EffectReq to handlers | Partial |
| 5 | `Portal` | Interface | Reentrancy bridge - `eval(expr, envRef)`, `apply(fn, args, envRef)` | ✓ PortalImpl |
| 6 | `LLMAdapter` | Interface | Strategy for LLM calls - can request reentrant eval/apply | ✓ ScriptedOracleAdapter |

### RSR Request Types

| # | Symbol | Source | Signature | Purpose | OmegaLLM |
|---|--------|--------|-----------|---------|----------|
| 7 | `ReqEval` | RSR-01 | `{ tag: "ReqEval", qexpr: Expr, envRef: Hash }` | Evaluate expression | ✓ |
| 8 | `ReqApply` | RSR-01 | `{ tag: "ReqApply", fn: Val, args: Val[], envRef: Hash }` | Apply function | ✓ |
| 9 | `ReqTool` | RSR-01 | `{ tag: "ReqTool", call: ToolCall, envRef: Hash }` | Invoke tool (infer, observe, etc.) | ✓ |
| 10 | `ReqReturn` | RSR-01 | `{ tag: "ReqReturn", val: Val }` | Return value | ✓ |
| 11 | `ReqFail` | RSR-01 | `{ tag: "ReqFail", reason: string }` | Signal failure | ✓ |
| 12 | `ReqObserve` | RSR-01 | `{ tag: "ReqObserve", ctxRef: Hash, schema: unknown }` | Observe external | ✓ |

### RSR-02: Reentrancy Protocol

| # | Symbol | Source | Signature | Purpose | OmegaLLM |
|---|--------|--------|-----------|---------|----------|
| 13 | `LLMContinuation` | RSR-02 | `Done \| CallEval \| CallApply` | LLM mid-inference callback | Partial |
| 14 | `CallEval` | RSR-02 | `{ tag: "CallEval", qexpr: Expr }` | LLM requests Lisp eval | Partial |
| 15 | `CallApply` | RSR-02 | `{ tag: "CallApply", fn: Val, args: Val[] }` | LLM requests Lisp apply | Partial |
| 16 | `scratch` | RSR-02 | `Record<string, unknown>` | State passed between LLM continuations | MISSING |

### RSR-03: Receipt Ledger & Replay

| # | Symbol | Source | Signature | Purpose | OmegaLLM |
|---|--------|--------|-----------|---------|----------|
| 17 | `Receipt` | RSR-03 | `{ reqKey, receiptKey, req, resp, meta }` | Content-addressed effect record | ✓ ReceiptStore |
| 18 | `ReceiptMeta` | RSR-03 | `{ sessionId, stepId, mode, t0, dtMs, envRef?, stateRef?, parents? }` | Receipt metadata | Partial |
| 19 | `EffectLedger` | RSR-03 | `{ getByReqKey, getByReq, put, bySession, byTool }` | Receipt store interface | Partial |
| 20 | `StableCodec` | RSR-03 | `{ canon(x): string, hashCanon(x): string }` | Canonicalization for deterministic replay | MISSING |
| 21 | `ReplayPolicy` | RSR-03 | `replayOnly \| replayThenLive \| validateAgainstLive \| recordOnly` | Replay strategy | MISSING |
| 22 | `SnapshotRepo` | RSR-03 | Checkpoint storage for EnvRef/StateRef | ✓ |
| 23 | `ArtifactRef` | RSR-03 | `{ kind: model\|decoder\|policy\|promptView\|toolSpec, id }` | Versioned artifact references | MISSING |

### RSR Invariants (Laws)

| # | Law | Description |
|---|-----|-------------|
| 24 | Idempotent Receiver | `put(receipt)` is idempotent by `receiptKey` |
| 25 | Deterministic Lookup | `getByReq(req)` returns receipt where `reqKey == H(Canon(req))` |
| 26 | Replay Purity | In replay mode, no external execution for requests with receipts |
| 27 | Causal Ordering | `stepId` is total order within `sessionId`; `parents` forms consistent DAG |

---

## How FrameLisp Maps to RSR

FrameLisp primitives are IMPLEMENTED via RSR:

| FrameLisp | RSR Implementation |
|-----------|-------------------|
| `(infer prompt ...)` | `ReqTool({ tool: "infer.op", prompt, ... })` |
| `(call-tool name args)` | `ReqTool({ tool: name, args })` |
| `(validate spec val)` | Could be `ReqTool({ tool: "validate", ... })` or pure |
| `(commit store k v)` | `ReqTool({ tool: "commit", ... })` or direct store |
| `(emit sink item)` | `ReqTool({ tool: "emit", ... })` |
| `(observe source)` | `ReqObserve({ ... })` |

**Key insight**: FrameLisp's 84 items are NOT new primitives - they're a VOCABULARY built on ~27 RSR primitives.

---

## How LambdaRLM/LambdaLLM Map Through FrameLisp

| LambdaRLM Pattern | Uses FrameLisp |
|-------------------|----------------|
| `(oracle-infer ...)` | → `(infer ...)` |
| `(repair-loop ...)` | → `(retry-until ...)` |
| `(composable-solver ...)` | → `(bind ...)` + `(branch ...)` |
| `(beam-select ...)` | → `(race ...)` + custom frontier |

| LambdaLLM Pattern | Uses FrameLisp |
|-------------------|----------------|
| `(fact-check ...)` | → `(validate ...)` + `(infer ...)` |
| `(fixpoint f x)` | → `(retry-until ...)` |
| `(expert-session ...)` | → `(chat-turn ...)` |

---

## Total Inventory by Layer

| Layer | Source | Items | Status |
|-------|--------|-------|--------|
| 0 | OmegaLLM Core | ~50 | ✓ Complete |
| 1 | OmegaLLM Effects | ~30 | ✓ Mostly complete |
| 2 | RSR Protocol | ~27 | Partial |
| 3 | FrameLisp Kernel+Prompt | ~26 | MISSING |
| 4 | FrameLisp Execution | ~21 | Partial |
| 5 | FrameLisp Protocols | ~8 | MISSING |
| 6 | LambdaRLM/LLM Patterns | ~50+ | Need audit |

**Total: ~210+ items across all layers**

---

## Acceptance Criteria

1. [ ] Layer 2 (RSR) fully documented with all items from ARCHITECTURE-EXPLANATION.md
2. [ ] Layer 3-5 (FrameLisp) documented showing RSR implementation
3. [ ] Layer 6 (LambdaRLM/LLM) documented showing FrameLisp usage
4. [ ] Each item shows: source, signature, purpose, implementation status, dependencies
5. [ ] Clear visualization of how everything flows THROUGH RSR to the LLM

---

## The Cohesive Picture

Everything that touches the LLM goes through this path:

```
User Code
    ↓
LambdaRLM/LLM patterns (composable-solvers, facts, etc.)
    ↓
FrameLisp protocols (chat-turn, tool-loop, rag)
    ↓
FrameLisp execution algebra (bind, retry-until, all/race)
    ↓
FrameLisp kernel (infer, call-tool, validate)
    ↓
RSR Effect Protocol (EffectReq → RuntimeDriver → EffectResp)
    ↓
Receipts (provenance, replay)
    ↓
LLM/Tool/Observe adapters
```

This is ONE path, not multiple competing paths. That's the cohesion.
