# JOB-018: Session Serialization, Traces, and Resume

**Priority**: P1 - Core Infrastructure
**Estimated Effort**: 8-12 hours
**Skills Required**: TypeScript, Serialization, File I/O
**Status**: DONE
**Depends On**: None
**Blocks**: Future replay/debugging features

> **MUST READ**: [LOGISTICS.md](LOGISTICS.md) before starting.

---

## Executive Summary

The REPL currently keeps full CEKS machine states in memory but cannot persist them. The trace output is console.log spam - not structured, not loadable, not resumable.

**Goal**: Implement proper session serialization so users can:
1. See readable, structured traces showing REPL vs EVAL vs LLM with proper nesting/depth
2. Save sessions to disk
3. Load sessions and jump to ANY point instantly (or near-instantly)
4. Resume execution from any checkpoint

**Key Insight**: Homoiconicity means AST is data. The entire CEKS state (control, continuations, closures) is fundamentally serializable because code IS data.

---

## Architecture Overview

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                           SESSION ARCHITECTURE                               │
├─────────────────────────────────────────────────────────────────────────────┤
│                                                                              │
│  ┌──────────────┐     ┌──────────────┐     ┌──────────────┐                │
│  │    REPL      │────▶│ SessionWriter│────▶│  .jsonl file │                │
│  │  (events)    │     │  (serialize) │     │  (on disk)   │                │
│  └──────────────┘     └──────────────┘     └──────────────┘                │
│         │                                          │                        │
│         │                                          ▼                        │
│         │                                  ┌──────────────┐                 │
│         │                                  │ SessionReader│                 │
│         │                                  │ (deserialize)│                 │
│         │                                  └──────────────┘                 │
│         │                                          │                        │
│         ▼                                          ▼                        │
│  ┌──────────────┐                          ┌──────────────┐                 │
│  │ PrettyRender │                          │ JumpController│                │
│  │  (display)   │                          │   (resume)    │                │
│  └──────────────┘                          └──────────────┘                 │
│                                                                              │
├─────────────────────────────────────────────────────────────────────────────┤
│                           FILE STRUCTURE                                     │
├─────────────────────────────────────────────────────────────────────────────┤
│                                                                              │
│  .omega-session/                                                             │
│  ├── current.jsonl          # Active session (append-only)                  │
│  ├── current.index.json     # Checkpoint index for fast lookup              │
│  ├── sessions/                                                              │
│  │   ├── 2024-01-21-debug.jsonl                                            │
│  │   ├── 2024-01-21-debug.index.json                                       │
│  │   └── my-work.jsonl                                                     │
│  └── receipts/                                                              │
│      ├── sha256-abc123.json  # LLM request/response pairs                  │
│      └── sha256-def456.json                                                │
│                                                                              │
└─────────────────────────────────────────────────────────────────────────────┘
```

---

## Part 1: State Serializer

### 1.1 The Problem

The CEKS `State` contains types that don't serialize with `JSON.stringify`:

```typescript
// PROBLEM 1: Maps don't serialize
const m = new Map([["a", 1]]);
JSON.stringify(m);  // "{}" - EMPTY!

// PROBLEM 2: Circular references throw
const ctx = { parent: null };
ctx.parent = ctx;
JSON.stringify(ctx);  // TypeError: Converting circular structure to JSON

// PROBLEM 3: Functions can't serialize
const native = { tag: "Native", fn: (args) => args[0] };
JSON.stringify(native);  // {"tag":"Native"} - fn is GONE!
```

### 1.2 Complete Type Inventory

Before coding, here's EVERY type that needs serialization handling:

**All Frame Types (from machine.ts:19-37)**:
```typescript
Frame =
  | { tag: "KIf"; conseq: Expr; alt: Expr; env: Env }
  | { tag: "KBegin"; rest: Expr[]; env: Env }
  | { tag: "KDefine"; name: string; env: Env }
  | { tag: "KSet"; name: string; env: Env }
  | { tag: "KAppFun"; args: Expr[]; env: Env }
  | { tag: "KAppArg"; fnVal: Val; pending: Expr[]; acc: Val[]; env: Env }
  | { tag: "KAppArgLazy"; fnVal: Val; pending: Array<{expr: Expr; idx: number}>; acc: Array<{idx: number; val: Val}>; env: Env; totalArgs: number; currentIdx: number }
  | { tag: "KCall"; savedEnv: Env }
  | { tag: "KEffect"; op: string; pending: Expr[]; acc: Val[]; env: Env }
  | { tag: "KHandleBoundary"; hid: string; savedHandlersDepth: number; resumeTo?: {kont: Frame[]; handlersDepth: number} }
  | { tag: "KHandleReturn"; mode: "exit"|"resume"; hid: string; targetKont: Frame[]; targetHandlersDepth: number; savedHandlersDepth: number }
  | { tag: "KPrompt"; promptTag: Val; handler: Val; env: Env; savedKont: Frame[]; savedHandlersDepth: number }
  | { tag: "KMatch"; clauses: Array<{pat: Pattern; body: Expr}>; env: Env }
  | { tag: "KOracleLambda"; params: string[]; env: Env }
  | { tag: "KBind"; fn: Val; env: Env }
  | { tag: "KHandlerBind"; handlers: ConditionHandler[] }
  | { tag: "KRestartBind"; restarts: RestartBinding[]; savedKont: Frame[]; env: Env; store: Store; handlers: HandlerFrame[] }
  | { tag: "KSignaling"; condition: ConditionVal; required: boolean }
```

**All Val Types (from values.ts:620-666)**:
```
ATOMIC (JSON-safe):           Unit, Uninit, Bool, Num, Str, Sym
BIGINT (needs toString):      Int
COMPOUND (recursive Val):     Pair, Vector, Map, List, Tagged, Dist, Explanation, Contradiction
HAS ENV (needs ctxTable):     Closure, OracleProc, ContinuationVal
HAS STATE (recursive):        MachineVal
HAS JS FUNCTION (registry):   Native, SolverVal
HAS RESUMPTION:               Cont
REFERENCE IDs (already safe): Fiber, Mutex, IVar, Channel, Actor, ConnRef, NetRef, Promise, Profile, Ctx, Module, ReceiptRef
SPECIAL:                      Syntax (stx is JSON-safe), Err, GenericMiss, GenericRegistry, Stream, IR, Budget, Result, CostEstimate, FactStore
```

### 1.3 Resumption Serialization (CRITICAL)

From `capture.ts:20-27`, the invoke function is trivially reconstructable:
```typescript
return {
  rid,
  base,  // Full State snapshot
  invoke: (v: Val) => ({
    ...base,
    control: { tag: "Val", v },  // Just sets control to Val
  }),
  digest: () => JSON.stringify({ rid, store: base.store.digest(), ... })
};
```

**Key Insight**: `invoke` just creates `{ ...base, control: { tag: "Val", v } }`. We can serialize the base State and reconstruct invoke on deserialize.

```typescript
// Serialized Resumption
type SerializedResumption = {
  rid: string;
  baseState: SerializedState;  // The full serialized base state
};

// Deserialize by reconstructing invoke
function deserializeResumption(sr: SerializedResumption, nativeRegistry: Map<string, Val>): Resumption {
  const base = deserializeState(sr.baseState, nativeRegistry);
  return {
    rid: sr.rid,
    base,
    invoke: (v: Val) => ({ ...base, control: { tag: "Val", v } }),
    digest: () => JSON.stringify({
      rid: sr.rid,
      store: base.store.digest(),
      kontDepth: base.kont.length,
      handlersDepth: base.handlers.length
    }),
  };
}
```

### 1.4 Native Registry Construction (CRITICAL)

The Native functions are installed in `installPrims()`. We need a registry:

```typescript
// src/core/session/nativeRegistry.ts

import type { Val } from "../eval/values";
import type { State } from "../eval/machine";

/**
 * Global registry of Native functions by name.
 * Built during installPrims() and used during state deserialization.
 */
export const nativeRegistry = new Map<string, Val>();

/**
 * Call this during installPrims to register each Native.
 * (Or call buildNativeRegistry after prims are installed)
 */
export function registerNative(name: string, native: Val): void {
  if (native.tag !== "Native") throw new Error(`Not a Native: ${name}`);
  nativeRegistry.set(name, native);
}

/**
 * Build the registry by scanning a primed store.
 */
export function buildNativeRegistry(store: Store): Map<string, Val> {
  const registry = new Map<string, Val>();
  // COWStore has private cells field - access via casting or expose method
  for (let addr = 0; addr < store.next; addr++) {
    try {
      const val = store.read(addr);
      if (val && val.tag === "Native") {
        registry.set(val.name, val);
      }
    } catch { /* addr doesn't exist */ }
  }
  return registry;
}
```

**Integration in REPL**:
```typescript
// In omega-repl.ts startup
const baseStore = new COWStore();
const { env: baseEnv, store: primedStore } = installPrims(baseStore);
const nativeRegistry = buildNativeRegistry(primedStore);  // <-- Build registry
```

### 1.5 SolverVal Registry (for Job 008 solvers)

SolverVal contains `solve` and `estimate` functions - same pattern as Native:

```typescript
export const solverRegistry = new Map<string, Val>();

export function registerSolver(name: string, solver: SolverVal): void {
  solverRegistry.set(name, solver);
}

// Serialize SolverVal as just { tag: "Solver", name: "..." }
// Deserialize by lookup in solverRegistry
```

### 1.6 The Solution

Create `src/core/session/serializer.ts`:

```typescript
// src/core/session/serializer.ts
// State serialization with Map, circular ref, and Native function handling

import type { State, Frame, Control } from "../eval/machine";
import type { Val } from "../eval/values";
import type { Ctx } from "../ctx/ctx";
import type { Store, StoreAddr } from "../eval/store";
import type { Expr } from "../ast";

// ═══════════════════════════════════════════════════════════════════════════
// SERIALIZED TYPES (JSON-safe versions of runtime types)
// ═══════════════════════════════════════════════════════════════════════════

/**
 * Serialized State - all Maps converted to arrays, circulars flattened
 */
export type SerializedState = {
  control: SerializedControl;
  envId: string;              // Reference to flattened Ctx
  storeEntries: [number, SerializedVal][];  // Map as array
  kont: SerializedFrame[];
  handlers: SerializedHandlerFrame[];
  // Ctx table - flattened, no circular refs
  ctxTable: Record<string, SerializedCtx>;
};

export type SerializedControl =
  | { tag: "Expr"; e: Expr }  // Expr is already JSON-safe (homoiconicity!)
  | { tag: "Val"; v: SerializedVal };

export type SerializedCtx = {
  id: string;
  parentId: string | null;    // Reference, not embedded
  frameEntries: [string, number][];  // Map<string, Addr> as array
  profile: string;
  caps: string[];             // CapSet as array
  budgets: Record<string, number>;
  constraints: any[];
  sealed: boolean;
  evidence: any[];
};

export type SerializedVal =
  // ═══════════════════════════════════════════════════════════════════
  // ATOMIC VALUES - serialize directly (JSON-safe)
  // ═══════════════════════════════════════════════════════════════════
  | { tag: "Unit" }
  | { tag: "Uninit" }
  | { tag: "Num"; n: number }
  | { tag: "Int"; value: string }           // BigInt as string
  | { tag: "Bool"; b: boolean }
  | { tag: "Str"; s: string }
  | { tag: "Sym"; name: string }
  | { tag: "Err"; message?: string }

  // ═══════════════════════════════════════════════════════════════════
  // COMPOUND VALUES - recursive serialization
  // ═══════════════════════════════════════════════════════════════════
  | { tag: "Pair"; car: SerializedVal; cdr: SerializedVal }
  | { tag: "Vector"; items: SerializedVal[] }
  | { tag: "List"; elements: SerializedVal[] }
  | { tag: "Map"; entries: [SerializedVal, SerializedVal][] }
  | { tag: "Tagged"; typeTag: string; payload: SerializedVal }
  | { tag: "Syntax"; stx: any }             // Syntax is JSON-safe

  // ═══════════════════════════════════════════════════════════════════
  // VALUES WITH ENVIRONMENT - replace Env with envId
  // ═══════════════════════════════════════════════════════════════════
  | { tag: "Closure"; params: string[]; body: Expr; envId: string }
  | { tag: "OracleProc"; params: string[]; spec: SerializedVal; envId: string; policyDigest?: string }
  | { tag: "Continuation"; kont: SerializedFrame[]; envId: string; storeEntries: [number, SerializedVal][]; handlers: SerializedHandlerFrame[] }

  // ═══════════════════════════════════════════════════════════════════
  // VALUES WITH STATE - recursive state serialization
  // ═══════════════════════════════════════════════════════════════════
  | { tag: "Machine"; stateRef: string; label?: string; stepCount: number; breakOnOps?: string[]; isDone: boolean; machineId: string; parentId?: string }

  // ═══════════════════════════════════════════════════════════════════
  // VALUES WITH JS FUNCTIONS - store name only, lookup in registry
  // ═══════════════════════════════════════════════════════════════════
  | { tag: "Native"; name: string; arity: number | "variadic" }
  | { tag: "Solver"; name: string }

  // ═══════════════════════════════════════════════════════════════════
  // VALUES WITH RESUMPTION - serialize rid + base state
  // ═══════════════════════════════════════════════════════════════════
  | { tag: "Cont"; hid: string; boundaryIndex: number; resumption: { rid: string; baseState: SerializedState } }

  // ═══════════════════════════════════════════════════════════════════
  // REFERENCE VALUES - IDs are already JSON-safe
  // ═══════════════════════════════════════════════════════════════════
  | { tag: "Fiber"; id: number; name?: string }
  | { tag: "Mutex"; id: string; name?: string }
  | { tag: "IVar"; id: string; name?: string }
  | { tag: "Channel"; id: string; bufferSize: number; name?: string }
  | { tag: "Actor"; id: string; fiberId: number; name?: string }
  | { tag: "ConnRef"; id: string; netId: string; name?: string }
  | { tag: "NetRef"; id: string; name?: string }
  | { tag: "Promise"; id: string; label?: string }
  | { tag: "Profile"; profileId: string; profile: any }
  | { tag: "Ctx"; ctx: SerializedCtx }
  | { tag: "Module"; moduleId: string; sealedCtxId: string; exports: string[]; meta?: any }
  | { tag: "ReceiptRef"; rid: string; kind: string }
  | { tag: "GenericRegistry"; id: string; name?: string }
  | { tag: "GenericMiss"; op: string; signature: string[]; argsPreview: SerializedVal[]; registryId: string }
  | { tag: "Stream"; isEmpty: boolean; head?: SerializedVal; tail?: SerializedVal }
  | { tag: "IR"; form: string; digest: string; irRef: string; label?: string }
  | { tag: "Budget"; tokens: number; calls: number; time: number }
  | { tag: "Result"; kind: string; solution?: SerializedVal; remaining?: SerializedVal; reason?: string; cost: number }
  | { tag: "CostEstimate"; minCost: number; maxCost: number; expectedCost: number; confidence: number }
  | { tag: "FactStore"; factsEntries: [string, SerializedVal][] }

  // ═══════════════════════════════════════════════════════════════════
  // SPECIAL COMPOUND VALUES
  // ═══════════════════════════════════════════════════════════════════
  | { tag: "Dist"; support: Array<{ v: SerializedVal; w: number }>; normalized?: boolean; meta?: any }
  | { tag: "Meaning"; denotation?: SerializedVal; residual?: SerializedVal; rewrite?: SerializedVal; obligations?: any[]; evidence?: any[]; confidence?: number; trace?: SerializedVal }
  | { tag: "Condition"; kind: string; message?: string; payload?: SerializedVal }
  | { tag: "Explanation"; kind: string; conn?: SerializedVal; valueHash?: string; because?: SerializedVal; rule?: string; deps?: SerializedVal[]; left?: SerializedVal; right?: SerializedVal; message?: string; op?: string; reason?: string; profile?: string }
  | { tag: "Contradiction"; explanation: SerializedVal; constraintId?: string; netId?: string };

export type SerializedFrame =
  // ALL 18 Frame types from machine.ts - COMPLETE LIST
  | { tag: "KIf"; conseq: Expr; alt: Expr; envId: string }
  | { tag: "KBegin"; rest: Expr[]; envId: string }
  | { tag: "KDefine"; name: string; envId: string }
  | { tag: "KSet"; name: string; envId: string }
  | { tag: "KAppFun"; args: Expr[]; envId: string }
  | { tag: "KAppArg"; fnVal: SerializedVal; pending: Expr[]; acc: SerializedVal[]; envId: string }
  | { tag: "KAppArgLazy"; fnVal: SerializedVal; pending: Array<{expr: Expr; idx: number}>; acc: Array<{idx: number; val: SerializedVal}>; envId: string; totalArgs: number; currentIdx: number }
  | { tag: "KCall"; savedEnvId: string }
  | { tag: "KEffect"; op: string; pending: Expr[]; acc: SerializedVal[]; envId: string }
  | { tag: "KHandleBoundary"; hid: string; savedHandlersDepth: number; resumeTo?: { kont: SerializedFrame[]; handlersDepth: number } }
  | { tag: "KHandleReturn"; mode: "exit" | "resume"; hid: string; targetKont: SerializedFrame[]; targetHandlersDepth: number; savedHandlersDepth: number }
  | { tag: "KPrompt"; promptTag: SerializedVal; handler: SerializedVal; envId: string; savedKont: SerializedFrame[]; savedHandlersDepth: number }
  | { tag: "KMatch"; clauses: Array<{ pat: any; body: Expr }>; envId: string }
  | { tag: "KOracleLambda"; params: string[]; envId: string }
  | { tag: "KBind"; fn: SerializedVal; envId: string }
  | { tag: "KHandlerBind"; handlers: any[] }
  | { tag: "KRestartBind"; restarts: any[]; savedKont: SerializedFrame[]; envId: string; storeEntries: [number, SerializedVal][]; handlers: SerializedHandlerFrame[] }
  | { tag: "KSignaling"; condition: SerializedVal; required: boolean };

export type SerializedHandlerFrame = {
  hid: string;
  envId: string;
  on: [string, { op: string; params: string[]; k: string; body: Expr }][];  // Map as array
  ret?: { v: string; body: Expr };
  fin?: { body: Expr };
};

// ═══════════════════════════════════════════════════════════════════════════
// SERIALIZATION
// ═══════════════════════════════════════════════════════════════════════════

/**
 * Serialize a State to JSON-safe format.
 *
 * Strategy:
 * 1. Collect all Ctx nodes into a flat table (break circular parent refs)
 * 2. Convert all Maps to arrays
 * 3. Replace Ctx references with IDs
 * 4. Replace Native fn with just name (will lookup on restore)
 */
export function serializeState(state: State): SerializedState {
  const ctxTable: Record<string, SerializedCtx> = {};

  // Helper: collect and serialize a Ctx, return its ID
  function collectCtx(ctx: Ctx): string {
    const id = ctx.cid || `ctx-${Object.keys(ctxTable).length}`;

    if (ctxTable[id]) return id;  // Already collected

    // Serialize this ctx (without parent embedded)
    ctxTable[id] = {
      id,
      parentId: ctx.parent ? collectCtx(ctx.parent) : null,
      frameEntries: Array.from(ctx.frame?.entries?.() ?? []),
      profile: ctx.profile,
      caps: Array.from(ctx.caps ?? []),
      budgets: ctx.budgets ?? {},
      constraints: ctx.constraints ?? [],
      sealed: ctx.sealed ?? false,
      evidence: ctx.evidence ?? [],
    };

    return id;
  }

  // Helper: serialize a Val (COMPLETE - handles all 50+ Val types)
  function serializeVal(v: Val): SerializedVal {
    if (!v || typeof v !== 'object') {
      return { tag: "Unit" };
    }

    switch (v.tag) {
      // ═════════════════════════════════════════════════════════════
      // ATOMIC - pass through
      // ═════════════════════════════════════════════════════════════
      case "Unit":
      case "Uninit":
      case "Bool":
      case "Num":
      case "Str":
      case "Sym":
      case "Err":
        return v as SerializedVal;

      case "Int":
        // BigInt needs toString
        return { tag: "Int", value: v.value.toString() };

      // ═════════════════════════════════════════════════════════════
      // COMPOUND - recursive
      // ═════════════════════════════════════════════════════════════
      case "Pair":
        return { tag: "Pair", car: serializeVal(v.car), cdr: serializeVal(v.cdr) };

      case "Vector":
        return { tag: "Vector", items: v.items.map(serializeVal) };

      case "List":
        return { tag: "List", elements: v.elements.map(serializeVal) };

      case "Map":
        return { tag: "Map", entries: v.entries.map(([k, val]) => [serializeVal(k), serializeVal(val)]) };

      case "Tagged":
        return { tag: "Tagged", typeTag: v.typeTag, payload: serializeVal(v.payload) };

      case "Syntax":
        return { tag: "Syntax", stx: v.stx };  // Syntax is JSON-safe

      case "Dist":
        return { tag: "Dist", support: v.support.map(it => ({ v: serializeVal(it.v), w: it.w })), normalized: v.normalized, meta: v.meta };

      // ═════════════════════════════════════════════════════════════
      // WITH ENVIRONMENT - replace env with envId
      // ═════════════════════════════════════════════════════════════
      case "Closure":
        return { tag: "Closure", params: v.params, body: v.body, envId: collectCtx(v.env) };

      case "OracleProc":
        return { tag: "OracleProc", params: v.params, spec: serializeVal(v.spec), envId: collectCtx(v.env), policyDigest: v.policyDigest };

      case "Continuation":
        return {
          tag: "Continuation",
          kont: v.kont.map(serializeFrame),
          envId: collectCtx(v.env),
          storeEntries: serializeStore(v.store),
          handlers: v.handlers.map(serializeHandler),
        };

      // ═════════════════════════════════════════════════════════════
      // WITH STATE - reference to nested state
      // ═════════════════════════════════════════════════════════════
      case "Machine":
        const machineStateId = `machine-state-${v.machineId}`;
        // Store the machine's state in a separate entry
        ctxTable[machineStateId] = serializeState(v.state) as any;
        return {
          tag: "Machine",
          stateRef: machineStateId,
          label: v.label,
          stepCount: v.stepCount,
          breakOnOps: v.breakOnOps ? Array.from(v.breakOnOps) : undefined,
          isDone: v.isDone,
          machineId: v.machineId,
          parentId: v.parentId,
        };

      // ═════════════════════════════════════════════════════════════
      // WITH JS FUNCTIONS - store name only, lookup in registry
      // ═════════════════════════════════════════════════════════════
      case "Native":
        return { tag: "Native", name: v.name, arity: v.arity };

      case "Solver":
        return { tag: "Solver", name: v.name };

      // ═════════════════════════════════════════════════════════════
      // WITH RESUMPTION - serialize base state (invoke is reconstructable)
      // ═════════════════════════════════════════════════════════════
      case "Cont":
        return {
          tag: "Cont",
          hid: v.hid,
          boundaryIndex: v.boundaryIndex,
          resumption: {
            rid: v.resumption.rid,
            baseState: serializeState(v.resumption.base),  // FULL state serialization!
          },
        };

      // ═════════════════════════════════════════════════════════════
      // REFERENCE VALUES - IDs are already JSON-safe
      // ═════════════════════════════════════════════════════════════
      case "Fiber":
      case "Mutex":
      case "IVar":
      case "Channel":
      case "Actor":
      case "ConnRef":
      case "NetRef":
      case "Promise":
      case "GenericRegistry":
      case "IR":
      case "Budget":
      case "CostEstimate":
        return v as SerializedVal;

      case "Profile":
        return { tag: "Profile", profileId: v.profileId, profile: v.profile };

      case "Ctx":
        return { tag: "Ctx", ctx: serializeCtxNode(v.ctx) };

      case "Module":
        return { tag: "Module", moduleId: v.moduleId, sealedCtxId: collectCtx(v.sealedCtx), exports: Array.from(v.exports), meta: v.meta };

      case "ReceiptRef":
        return { tag: "ReceiptRef", rid: v.rid, kind: v.kind };

      case "GenericMiss":
        return { tag: "GenericMiss", op: v.op, signature: v.signature, argsPreview: v.argsPreview.map(serializeVal), registryId: v.registryId };

      case "Stream":
        return { tag: "Stream", isEmpty: v.isEmpty, head: v.head ? serializeVal(v.head) : undefined, tail: v.tail ? serializeVal(v.tail) : undefined };

      case "Result":
        return { tag: "Result", kind: v.kind, solution: v.solution ? serializeVal(v.solution) : undefined, remaining: v.remaining ? serializeVal(v.remaining) : undefined, reason: v.reason, cost: v.cost };

      case "FactStore":
        return { tag: "FactStore", factsEntries: Array.from(v.facts.entries()).map(([k, val]) => [k, serializeVal(val)]) };

      // ═════════════════════════════════════════════════════════════
      // SPECIAL COMPOUND
      // ═════════════════════════════════════════════════════════════
      case "Meaning":
        return {
          tag: "Meaning",
          denotation: v.denotation ? serializeVal(v.denotation as Val) : undefined,
          residual: v.residual ? serializeVal(v.residual) : undefined,
          rewrite: v.rewrite ? serializeVal(v.rewrite) : undefined,
          obligations: v.obligations,
          evidence: v.evidence,
          confidence: v.confidence,
          trace: v.trace ? serializeVal(v.trace as Val) : undefined,
        };

      case "Explanation":
        return v as any;  // Explanation is self-referential but JSON-safe structure

      case "Contradiction":
        return { tag: "Contradiction", explanation: serializeVal(v.explanation as Val), constraintId: v.constraintId, netId: v.netId };

      case "Condition":
        return v as SerializedVal;

      default:
        // Fallback - warn and pass through
        console.warn(`serializeVal: unhandled tag ${(v as any).tag}`);
        return v as any;
    }
  }

  // Helper: serialize store entries
  function serializeStore(store: Store): [number, SerializedVal][] {
    const entries: [number, SerializedVal][] = [];
    for (let addr = 0; addr < store.next; addr++) {
      try {
        entries.push([addr, serializeVal(store.read(addr))]);
      } catch { /* skip invalid */ }
    }
    return entries;
  }

  // Helper: serialize Ctx node (for embedded Ctx values)
  function serializeCtxNode(ctx: Ctx): SerializedCtx {
    return {
      id: ctx.cid,
      parentId: ctx.parent ? collectCtx(ctx.parent) : null,
      frameEntries: Array.from(ctx.frame?.entries?.() ?? []),
      profile: ctx.profile,
      caps: Array.from(ctx.caps ?? []),
      budgets: ctx.budgets ?? {},
      constraints: ctx.constraints ?? [],
      sealed: ctx.sealed ?? false,
      evidence: ctx.evidence ?? [],
    };
  }

  // Helper: serialize a Frame (COMPLETE - handles all 18 Frame types)
  function serializeFrame(f: Frame): SerializedFrame {
    switch (f.tag) {
      case "KIf":
        return { tag: "KIf", conseq: f.conseq, alt: f.alt, envId: collectCtx(f.env) };

      case "KBegin":
        return { tag: "KBegin", rest: f.rest, envId: collectCtx(f.env) };

      case "KDefine":
        return { tag: "KDefine", name: f.name, envId: collectCtx(f.env) };

      case "KSet":
        return { tag: "KSet", name: f.name, envId: collectCtx(f.env) };

      case "KAppFun":
        return { tag: "KAppFun", args: f.args, envId: collectCtx(f.env) };

      case "KAppArg":
        return { tag: "KAppArg", fnVal: serializeVal(f.fnVal), pending: f.pending, acc: f.acc.map(serializeVal), envId: collectCtx(f.env) };

      case "KAppArgLazy":
        return {
          tag: "KAppArgLazy",
          fnVal: serializeVal(f.fnVal),
          pending: f.pending,  // Array<{expr: Expr; idx: number}> - Expr is JSON-safe
          acc: f.acc.map(a => ({ idx: a.idx, val: serializeVal(a.val) })),
          envId: collectCtx(f.env),
          totalArgs: f.totalArgs,
          currentIdx: f.currentIdx,
        };

      case "KCall":
        return { tag: "KCall", savedEnvId: collectCtx(f.savedEnv) };

      case "KEffect":
        return { tag: "KEffect", op: f.op, pending: f.pending, acc: f.acc.map(serializeVal), envId: collectCtx(f.env) };

      case "KHandleBoundary":
        return {
          tag: "KHandleBoundary",
          hid: f.hid,
          savedHandlersDepth: f.savedHandlersDepth,
          resumeTo: f.resumeTo ? {
            kont: f.resumeTo.kont.map(serializeFrame),
            handlersDepth: f.resumeTo.handlersDepth,
          } : undefined,
        };

      case "KHandleReturn":
        return {
          tag: "KHandleReturn",
          mode: f.mode,
          hid: f.hid,
          targetKont: f.targetKont.map(serializeFrame),
          targetHandlersDepth: f.targetHandlersDepth,
          savedHandlersDepth: f.savedHandlersDepth,
        };

      case "KPrompt":
        return {
          tag: "KPrompt",
          promptTag: serializeVal(f.promptTag),
          handler: serializeVal(f.handler),
          envId: collectCtx(f.env),
          savedKont: f.savedKont.map(serializeFrame),
          savedHandlersDepth: f.savedHandlersDepth,
        };

      case "KMatch":
        return { tag: "KMatch", clauses: f.clauses, envId: collectCtx(f.env) };  // clauses have Pattern (JSON-safe) and Expr

      case "KOracleLambda":
        return { tag: "KOracleLambda", params: f.params, envId: collectCtx(f.env) };

      case "KBind":
        return { tag: "KBind", fn: serializeVal(f.fn), envId: collectCtx(f.env) };

      case "KHandlerBind":
        return { tag: "KHandlerBind", handlers: f.handlers };  // ConditionHandler[] - should be JSON-safe

      case "KRestartBind":
        return {
          tag: "KRestartBind",
          restarts: f.restarts,  // RestartBinding[] - should be JSON-safe
          savedKont: f.savedKont.map(serializeFrame),
          envId: collectCtx(f.env),
          storeEntries: serializeStore(f.store),
          handlers: f.handlers.map(serializeHandler),
        };

      case "KSignaling":
        return { tag: "KSignaling", condition: serializeVal(f.condition as Val), required: f.required };

      default:
        console.warn(`serializeFrame: unhandled tag ${(f as any).tag}`);
        return f as any;
    }
  }

  // Helper: serialize a HandlerFrame
  function serializeHandler(h: any): SerializedHandlerFrame {
    return {
      hid: h.hid,
      envId: collectCtx(h.env),
      on: Array.from(h.on?.entries?.() ?? []),
      ret: h.ret,
      fin: h.fin,
    };
  }

  // Serialize control
  const control: SerializedControl = state.control.tag === "Expr"
    ? { tag: "Expr", e: state.control.e }
    : { tag: "Val", v: serializeVal(state.control.v) };

  // Serialize store (Map to array)
  const storeEntries: [number, SerializedVal][] = [];
  if (state.store && typeof state.store.entries === 'function') {
    for (const [addr, val] of state.store.entries()) {
      storeEntries.push([addr, serializeVal(val)]);
    }
  } else if (state.store && (state.store as any).m) {
    // COWStore has private m field
    for (const [addr, val] of (state.store as any).m.entries()) {
      storeEntries.push([addr, serializeVal(val)]);
    }
  }

  // Serialize env (collect into table)
  const envId = state.env ? collectCtx(state.env) : "empty";

  return {
    control,
    envId,
    storeEntries,
    kont: state.kont.map(serializeFrame),
    handlers: state.handlers.map(serializeHandler),
    ctxTable,
  };
}

// ═══════════════════════════════════════════════════════════════════════════
// DESERIALIZATION
// ═══════════════════════════════════════════════════════════════════════════

/**
 * Restore a State from serialized format.
 *
 * Requires:
 * - nativeRegistry: Map<string, NativeVal> to restore Native functions
 */
export function deserializeState(
  s: SerializedState,
  nativeRegistry: Map<string, Val>
): State {
  // Rebuild Ctx table with parent pointers
  const ctxInstances: Record<string, Ctx> = {};

  function rebuildCtx(id: string): Ctx {
    if (ctxInstances[id]) return ctxInstances[id];

    const ser = s.ctxTable[id];
    if (!ser) throw new Error(`Missing ctx in table: ${id}`);

    const ctx: Ctx = {
      tag: "Ctx",
      cid: ser.id,
      parent: ser.parentId ? rebuildCtx(ser.parentId) : undefined,
      frame: new Map(ser.frameEntries),
      profile: ser.profile,
      caps: new Set(ser.caps) as any,
      budgets: ser.budgets as any,
      constraints: ser.constraints as any,
      sealed: ser.sealed,
      evidence: ser.evidence as any,
    };

    ctxInstances[id] = ctx;
    return ctx;
  }

  function deserializeVal(v: SerializedVal): Val {
    switch (v.tag) {
      case "Num":
      case "Bool":
      case "Str":
      case "Sym":
      case "Unit":
        return v as Val;

      case "Pair":
        return { tag: "Pair", car: deserializeVal(v.car), cdr: deserializeVal(v.cdr) };

      case "Vector":
        return { tag: "Vector", items: v.items.map(deserializeVal) };

      case "Closure":
        return {
          tag: "Closure",
          params: v.params,
          body: v.body,
          env: rebuildCtx(v.envId),
        };

      case "Native":
        // Lookup in registry
        const native = nativeRegistry.get(v.name);
        if (!native) throw new Error(`Native function not found: ${v.name}`);
        return native;

      case "OracleProc":
        return {
          tag: "OracleProc",
          params: v.params,
          spec: deserializeVal(v.spec),
          env: rebuildCtx(v.envId),
        };

      case "Meaning":
        return {
          tag: "Meaning",
          denotation: deserializeVal(v.denotation),
          confidence: v.confidence,
          trace: deserializeVal(v.trace),
        };

      case "Map":
        return {
          tag: "Map",
          entries: v.entries.map(([k, val]) => [deserializeVal(k), deserializeVal(val)]),
        };

      default:
        return v as Val;
    }
  }

  function deserializeFrame(f: SerializedFrame): Frame {
    switch (f.tag) {
      case "KIf":
        return { tag: "KIf", conseq: f.conseq, alt: f.alt, env: rebuildCtx(f.envId) };
      case "KBegin":
        return { tag: "KBegin", rest: f.rest, env: rebuildCtx(f.envId) };
      case "KDefine":
        return { tag: "KDefine", name: f.name, env: rebuildCtx(f.envId) };
      case "KAppFun":
        return { tag: "KAppFun", args: f.args, env: rebuildCtx(f.envId) };
      case "KAppArg":
        return {
          tag: "KAppArg",
          fnVal: deserializeVal(f.fnVal),
          pending: f.pending,
          acc: f.acc.map(deserializeVal),
          env: rebuildCtx(f.envId),
        };
      case "KCall":
        return { tag: "KCall", savedEnv: rebuildCtx(f.savedEnvId) };
      case "KEffect":
        return {
          tag: "KEffect",
          op: f.op,
          pending: f.pending,
          acc: f.acc.map(deserializeVal),
          env: rebuildCtx(f.envId),
        };
      default:
        return f as any;
    }
  }

  function deserializeHandler(h: SerializedHandlerFrame): any {
    return {
      hid: h.hid,
      env: rebuildCtx(h.envId),
      on: new Map(h.on),
      ret: h.ret,
      fin: h.fin,
    };
  }

  // Rebuild store
  const { COWStore } = require("../eval/store");
  const store = new COWStore();
  for (const [addr, val] of s.storeEntries) {
    store.set(addr, deserializeVal(val));
  }

  // Rebuild control
  const control: Control = s.control.tag === "Expr"
    ? { tag: "Expr", e: s.control.e }
    : { tag: "Val", v: deserializeVal(s.control.v) };

  return {
    control,
    env: s.envId !== "empty" ? rebuildCtx(s.envId) : undefined as any,
    store,
    kont: s.kont.map(deserializeFrame),
    handlers: s.handlers.map(deserializeHandler),
  };
}
```

### 1.3 Tests for Serializer

Create `test/session/serializer.spec.ts`:

```typescript
import { describe, it, expect } from "vitest";
import { serializeState, deserializeState } from "../../src/core/session/serializer";
import { COWStore } from "../../src/core/eval/store";
import { installPrims } from "../../src/core/prims";

describe("State Serializer", () => {
  it("round-trips a simple state", () => {
    const store = new COWStore();
    const { env, store: primedStore } = installPrims(store);

    const state = {
      control: { tag: "Val" as const, v: { tag: "Num" as const, n: 42 } },
      env,
      store: primedStore,
      kont: [],
      handlers: [],
    };

    const serialized = serializeState(state);
    const json = JSON.stringify(serialized);  // Must not throw!

    // Rebuild native registry
    const nativeRegistry = new Map();
    for (const [addr, val] of (primedStore as any).m.entries()) {
      if (val.tag === "Native") {
        nativeRegistry.set(val.name, val);
      }
    }

    const restored = deserializeState(JSON.parse(json), nativeRegistry);

    expect(restored.control).toEqual(state.control);
    expect(restored.kont.length).toBe(0);
  });

  it("handles closures with captured environments", () => {
    // ... test closure serialization
  });

  it("handles continuations with pending expressions", () => {
    // ... test continuation serialization
  });

  it("handles circular Ctx.parent references", () => {
    // ... test that parent chain is preserved
  });
});
```

---

## Part 2: Session File Format

### 2.1 Event Types

```typescript
// src/core/session/events.ts

/**
 * Session event types - one event per line in JSONL file
 */

export type SessionEvent =
  | SessionHeaderEvent
  | InputEvent
  | StepEvent
  | CheckpointEvent
  | LLMRequestEvent
  | LLMResponseEvent
  | EffectEvent
  | ResumeEvent
  | ResultEvent
  | ErrorEvent;

/** First line of session file - metadata */
export type SessionHeaderEvent = {
  type: "session";
  version: 1;
  id: string;
  created: string;  // ISO timestamp
  profile?: string;
};

/** User entered code in REPL */
export type InputEvent = {
  seq: number;
  ts: number;
  type: "input";
  code: string;
};

/** CEKS evaluation step (summary only, no full state) */
export type StepEvent = {
  seq: number;
  ts: number;
  type: "step";
  d: number;        // depth
  ctrl: string;     // control summary (e.g., "call:factorial", "if:test")
};

/** Full state checkpoint - can jump here instantly */
export type CheckpointEvent = {
  seq: number;
  ts: number;
  type: "checkpoint";
  d: number;
  reason: "llm_boundary" | "periodic" | "manual" | "effect";
  stateId: string;  // Reference to state in index file
};

/** LLM request sent */
export type LLMRequestEvent = {
  seq: number;
  ts: number;
  type: "llm_req";
  d: number;
  model: string;
  promptPreview: string;  // First 200 chars
  receiptKey: string;     // Hash for receipt lookup
};

/** LLM response received */
export type LLMResponseEvent = {
  seq: number;
  ts: number;
  type: "llm_resp";
  d: number;
  valuePreview: string;
  tokens?: number;
  durationMs: number;
  receiptKey: string;
};

/** Effect triggered (before LLM) */
export type EffectEvent = {
  seq: number;
  ts: number;
  type: "effect";
  d: number;
  op: string;       // e.g., "infer.op"
  argsPreview: string;
};

/** Evaluation resumed after effect */
export type ResumeEvent = {
  seq: number;
  ts: number;
  type: "resume";
  d: number;
  valuePreview: string;
};

/** Final result of expression */
export type ResultEvent = {
  seq: number;
  ts: number;
  type: "result";
  value: string;    // S-expression representation
};

/** Error occurred */
export type ErrorEvent = {
  seq: number;
  ts: number;
  type: "error";
  d: number;
  message: string;
  stack?: string;
};
```

### 2.2 Index File Format

The index file provides fast lookup without parsing entire JSONL:

```typescript
// src/core/session/index.ts

export type SessionIndex = {
  sessionId: string;
  eventCount: number;

  /** Checkpoint locations for fast seeking */
  checkpoints: CheckpointIndex[];

  /** Serialized states (inline or referenced) */
  states: Record<string, SerializedState>;

  /** LLM receipts for deterministic replay */
  receipts: Record<string, LLMReceipt>;
};

export type CheckpointIndex = {
  seq: number;
  byteOffset: number;   // Position in JSONL for seeking
  stateId: string;
  reason: string;
};

export type LLMReceipt = {
  key: string;          // sha256 of request
  request: {
    model: string;
    prompt: string;
    tools?: any[];
  };
  response: {
    content: string;
    toolCalls?: any[];
  };
  timestamp: number;
  durationMs: number;
  tokens?: number;
};
```

---

## Part 3: Session Writer

### 3.1 Implementation

```typescript
// src/core/session/writer.ts

import * as fs from "fs";
import * as path from "path";
import { serializeState } from "./serializer";
import type { SessionEvent, SessionIndex, CheckpointIndex } from "./types";
import type { State } from "../eval/machine";
import { sha256JSON } from "../artifacts/hash";

export class SessionWriter {
  private sessionId: string;
  private sessionDir: string;
  private eventFile: string;
  private indexFile: string;

  private seq = 0;
  private depth = 0;
  private checkpoints: CheckpointIndex[] = [];
  private states: Record<string, any> = {};
  private receipts: Record<string, any> = {};
  private byteOffset = 0;

  constructor(sessionDir: string, sessionId?: string) {
    this.sessionDir = sessionDir;
    this.sessionId = sessionId || `session-${Date.now().toString(36)}`;

    // Ensure directories exist
    fs.mkdirSync(path.join(sessionDir, "sessions"), { recursive: true });
    fs.mkdirSync(path.join(sessionDir, "receipts"), { recursive: true });

    this.eventFile = path.join(sessionDir, "sessions", `${this.sessionId}.jsonl`);
    this.indexFile = path.join(sessionDir, "sessions", `${this.sessionId}.index.json`);

    // Write header
    this.writeEvent({
      type: "session",
      version: 1,
      id: this.sessionId,
      created: new Date().toISOString(),
    } as any);
  }

  private writeEvent(event: SessionEvent): void {
    const line = JSON.stringify(event) + "\n";
    fs.appendFileSync(this.eventFile, line);
    this.byteOffset += Buffer.byteLength(line);
    if ('seq' in event) {
      this.seq = event.seq + 1;
    }
  }

  /** Save index to disk */
  private saveIndex(): void {
    const index: SessionIndex = {
      sessionId: this.sessionId,
      eventCount: this.seq,
      checkpoints: this.checkpoints,
      states: this.states,
      receipts: this.receipts,
    };
    fs.writeFileSync(this.indexFile, JSON.stringify(index, null, 2));
  }

  // ─────────────────────────────────────────────────────────────────
  // Public API
  // ─────────────────────────────────────────────────────────────────

  getSessionId(): string {
    return this.sessionId;
  }

  getSeq(): number {
    return this.seq;
  }

  pushDepth(): void {
    this.depth++;
  }

  popDepth(): void {
    this.depth = Math.max(0, this.depth - 1);
  }

  input(code: string): void {
    this.writeEvent({
      seq: this.seq,
      ts: Date.now(),
      type: "input",
      code,
    });
  }

  step(controlSummary: string): void {
    this.writeEvent({
      seq: this.seq,
      ts: Date.now(),
      type: "step",
      d: this.depth,
      ctrl: controlSummary,
    });
  }

  checkpoint(state: State, reason: "llm_boundary" | "periodic" | "manual" | "effect"): void {
    const stateId = `state-${this.seq}`;
    const serialized = serializeState(state);

    this.states[stateId] = serialized;

    this.checkpoints.push({
      seq: this.seq,
      byteOffset: this.byteOffset,
      stateId,
      reason,
    });

    this.writeEvent({
      seq: this.seq,
      ts: Date.now(),
      type: "checkpoint",
      d: this.depth,
      reason,
      stateId,
    });

    // Save index after each checkpoint for crash recovery
    this.saveIndex();
  }

  effect(op: string, args: any[]): void {
    this.writeEvent({
      seq: this.seq,
      ts: Date.now(),
      type: "effect",
      d: this.depth,
      op,
      argsPreview: JSON.stringify(args).slice(0, 200),
    });
  }

  llmRequest(model: string, prompt: string, fullRequest: any): string {
    const receiptKey = sha256JSON(fullRequest);

    this.writeEvent({
      seq: this.seq,
      ts: Date.now(),
      type: "llm_req",
      d: this.depth,
      model,
      promptPreview: prompt.slice(0, 200),
      receiptKey,
    });

    return receiptKey;
  }

  llmResponse(receiptKey: string, value: string, fullResponse: any, durationMs: number, tokens?: number): void {
    // Store receipt for replay
    this.receipts[receiptKey] = {
      key: receiptKey,
      request: fullResponse.request,
      response: fullResponse.response,
      timestamp: Date.now(),
      durationMs,
      tokens,
    };

    this.writeEvent({
      seq: this.seq,
      ts: Date.now(),
      type: "llm_resp",
      d: this.depth,
      valuePreview: value.slice(0, 200),
      tokens,
      durationMs,
      receiptKey,
    });

    // Save index to persist receipt
    this.saveIndex();
  }

  resume(value: string): void {
    this.writeEvent({
      seq: this.seq,
      ts: Date.now(),
      type: "resume",
      d: this.depth,
      valuePreview: value.slice(0, 200),
    });
  }

  result(value: string): void {
    this.writeEvent({
      seq: this.seq,
      ts: Date.now(),
      type: "result",
      value,
    });
    this.saveIndex();
  }

  error(message: string, stack?: string): void {
    this.writeEvent({
      seq: this.seq,
      ts: Date.now(),
      type: "error",
      d: this.depth,
      message,
      stack,
    });
    this.saveIndex();
  }

  /** Finalize and close session */
  close(): void {
    this.saveIndex();
  }
}
```

---

## Part 4: Session Reader and Jump Controller

### 4.1 Session Reader

```typescript
// src/core/session/reader.ts

import * as fs from "fs";
import * as readline from "readline";
import type { SessionEvent, SessionIndex } from "./types";
import { deserializeState } from "./serializer";
import type { State } from "../eval/machine";
import type { Val } from "../eval/values";

export class SessionReader {
  private events: SessionEvent[] = [];
  private index: SessionIndex;
  private nativeRegistry: Map<string, Val>;

  constructor(
    private eventFile: string,
    private indexFile: string,
    nativeRegistry: Map<string, Val>
  ) {
    this.nativeRegistry = nativeRegistry;
    this.index = JSON.parse(fs.readFileSync(indexFile, "utf8"));
  }

  /** Load all events into memory (for small sessions) */
  async loadAll(): Promise<void> {
    const fileStream = fs.createReadStream(this.eventFile);
    const rl = readline.createInterface({ input: fileStream });

    for await (const line of rl) {
      if (line.trim()) {
        this.events.push(JSON.parse(line));
      }
    }
  }

  /** Get event by sequence number */
  getEvent(seq: number): SessionEvent | undefined {
    return this.events.find(e => 'seq' in e && e.seq === seq);
  }

  /** Find nearest checkpoint at or before target seq */
  findCheckpointBefore(targetSeq: number): CheckpointIndex | undefined {
    let best: CheckpointIndex | undefined;
    for (const cp of this.index.checkpoints) {
      if (cp.seq <= targetSeq) {
        if (!best || cp.seq > best.seq) {
          best = cp;
        }
      }
    }
    return best;
  }

  /** Get state from checkpoint */
  getCheckpointState(stateId: string): State {
    const serialized = this.index.states[stateId];
    if (!serialized) {
      throw new Error(`State not found: ${stateId}`);
    }
    return deserializeState(serialized, this.nativeRegistry);
  }

  /** Get LLM receipt for replay */
  getReceipt(key: string): any {
    return this.index.receipts[key];
  }

  /** Get all events in range */
  getEventsInRange(startSeq: number, endSeq: number): SessionEvent[] {
    return this.events.filter(e => {
      if (!('seq' in e)) return false;
      return e.seq >= startSeq && e.seq <= endSeq;
    });
  }

  /** Get total event count */
  getEventCount(): number {
    return this.index.eventCount;
  }

  /** Get all checkpoints */
  getCheckpoints(): CheckpointIndex[] {
    return this.index.checkpoints;
  }
}
```

### 4.2 Jump Controller

```typescript
// src/core/session/jump.ts

import { SessionReader } from "./reader";
import { stepOnce, type StepOutcome } from "../eval/machineStep";
import { captureValueResumption } from "../effects/capture";
import type { State } from "../eval/machine";
import type { Val } from "../eval/values";

export type JumpResult = {
  state: State;
  seq: number;
  replayedSteps: number;
  usedReceipts: string[];
};

/**
 * Convert LLM response content to Val.
 * The receipt stores raw response; we need to convert to runtime Val.
 */
function responseToVal(responseContent: string): Val {
  // LLM responses are typically strings
  return { tag: "Str", s: responseContent };
}

export class JumpController {
  constructor(private reader: SessionReader) {}

  /**
   * Jump to a specific sequence number.
   *
   * Strategy:
   * 1. Find nearest checkpoint at or before target
   * 2. Load state from checkpoint (INSTANT)
   * 3. Replay events from checkpoint to target (FAST - no LLM, uses receipts)
   *
   * Effect Resumption:
   * When we hit an llm_resp event, the state should be waiting at an effect boundary.
   * We use captureValueResumption to create a resumption, then invoke it with the
   * cached response value to continue execution.
   */
  async jumpTo(targetSeq: number): Promise<JumpResult> {
    const checkpoint = this.reader.findCheckpointBefore(targetSeq);

    if (!checkpoint) {
      throw new Error(`No checkpoint found before seq ${targetSeq}`);
    }

    // Load state directly - INSTANT!
    let state = this.reader.getCheckpointState(checkpoint.stateId);
    let currentSeq = checkpoint.seq;
    const usedReceipts: string[] = [];

    // If we're exactly at checkpoint, done
    if (currentSeq === targetSeq) {
      return {
        state,
        seq: currentSeq,
        replayedSteps: 0,
        usedReceipts,
      };
    }

    // Replay from checkpoint to target
    const events = this.reader.getEventsInRange(checkpoint.seq + 1, targetSeq);
    let pendingResumption: { resumption: any; receiptKey: string } | null = null;

    for (const event of events) {
      if (!('seq' in event)) continue;

      switch (event.type) {
        case "step":
          // Replay CEKS step
          const result = stepOnce(state);
          if (result.tag === "State") {
            state = result.state;
          } else if (result.tag === "Op") {
            // Hit an effect - the next event should be llm_req or llm_resp
            // Capture the resumption for later use
            pendingResumption = {
              resumption: captureValueResumption(result.state),
              receiptKey: "", // Will be filled by llm_req event
            };
            state = result.state;
          }
          break;

        case "llm_req":
          // Record the receipt key for the pending effect
          if (pendingResumption) {
            pendingResumption.receiptKey = event.receiptKey;
          }
          break;

        case "llm_resp":
          // ═══════════════════════════════════════════════════════════
          // EFFECT RESUMPTION - the key operation!
          // ═══════════════════════════════════════════════════════════
          const receipt = this.reader.getReceipt(event.receiptKey);
          if (!receipt) {
            throw new Error(`Receipt not found: ${event.receiptKey}`);
          }
          usedReceipts.push(event.receiptKey);

          // Convert cached response to Val
          const cachedVal = responseToVal(receipt.response.content);

          // If we have a pending resumption, use it to inject the cached value
          if (pendingResumption) {
            state = pendingResumption.resumption.invoke(cachedVal);
            pendingResumption = null;
          } else {
            // The checkpoint was at an effect boundary - state is already
            // waiting for a value. Create resumption and invoke immediately.
            const resumption = captureValueResumption(state);
            state = resumption.invoke(cachedVal);
          }
          break;

        case "resume":
          // Resume event is logged after llm_resp; state already updated above
          break;

        case "checkpoint":
          // Checkpoints don't require action during replay
          break;

        case "effect":
          // Effect event is logged before llm_req; state will pause at Op
          break;
      }

      currentSeq = event.seq;
    }

    return {
      state,
      seq: currentSeq,
      replayedSteps: events.length,
      usedReceipts,
    };
  }

  /**
   * Resume execution from current state with live LLM calls.
   * Uses cached receipts when available, calls LLM when not.
   */
  async resumeLive(state: State, runtime: any): Promise<State> {
    // Step until completion or effect
    let current = state;
    while (true) {
      const result = stepOnce(current);

      if (result.tag === "Done") {
        return result.state;
      }

      if (result.tag === "Op") {
        // Hit an effect - check for cached receipt
        const receiptKey = result.opcall.digest();
        const cachedReceipt = this.reader.getReceipt(receiptKey);

        if (cachedReceipt) {
          // Use cached response
          const cachedVal = responseToVal(cachedReceipt.response.content);
          current = result.opcall.resumption.invoke(cachedVal);
        } else {
          // No cache - need live LLM call
          // Return state and let caller handle the LLM call
          return result.state;
        }
      } else {
        current = result.state;
      }
    }
  }
}
```

---

## Part 5: Pretty Renderer

### 5.1 Implementation

```typescript
// src/core/session/render.ts

import type { SessionEvent } from "./types";

export type RenderOptions = {
  /** Include timestamps */
  showTime?: boolean;
  /** Include sequence numbers */
  showSeq?: boolean;
  /** Max width for values */
  maxValueWidth?: number;
  /** Use colors (ANSI) */
  colors?: boolean;
};

const DEFAULT_OPTIONS: RenderOptions = {
  showTime: false,
  showSeq: true,
  maxValueWidth: 60,
  colors: false,
};

/**
 * Render a session event as a human-readable line.
 *
 * Output format:
 *   [seq] <indent> SOURCE > content
 *
 * Where indent is based on depth (d).
 */
export function renderEvent(event: SessionEvent, opts: RenderOptions = {}): string {
  const o = { ...DEFAULT_OPTIONS, ...opts };

  if (event.type === "session") {
    return `=== Session ${event.id} (${event.created}) ===`;
  }

  if (!('seq' in event)) return "";

  const seq = o.showSeq ? `[${String(event.seq).padStart(3, "0")}]` : "";
  const depth = 'd' in event ? (event as any).d : 0;
  const indent = "  ".repeat(depth);

  let source: string;
  let content: string;
  let symbol: string;

  switch (event.type) {
    case "input":
      source = "REPL";
      symbol = ">";
      content = truncate(event.code, o.maxValueWidth!);
      break;

    case "step":
      source = "EVAL";
      symbol = "~";
      content = event.ctrl;
      break;

    case "checkpoint":
      source = "SAVE";
      symbol = "*";
      content = `checkpoint (${event.reason})`;
      break;

    case "effect":
      source = "EFCT";
      symbol = "!";
      content = `${event.op}`;
      break;

    case "llm_req":
      source = "LLM";
      symbol = "->";
      content = `${event.model}: ${truncate(event.promptPreview, o.maxValueWidth! - 20)}`;
      break;

    case "llm_resp":
      source = "LLM";
      symbol = "<-";
      content = `${truncate(event.valuePreview, o.maxValueWidth! - 20)} (${event.durationMs}ms)`;
      break;

    case "resume":
      source = "RSME";
      symbol = "<~";
      content = truncate(event.valuePreview, o.maxValueWidth!);
      break;

    case "result":
      source = "OUT";
      symbol = "=>";
      content = truncate(event.value, o.maxValueWidth!);
      break;

    case "error":
      source = "ERR";
      symbol = "!!";
      content = event.message;
      break;

    default:
      return "";
  }

  return `${seq} ${indent}${source} ${symbol} ${content}`;
}

function truncate(s: string, max: number): string {
  if (s.length <= max) return s;
  return s.slice(0, max - 3) + "...";
}

/**
 * Render multiple events as a formatted trace.
 */
export function renderTrace(events: SessionEvent[], opts: RenderOptions = {}): string {
  return events.map(e => renderEvent(e, opts)).filter(Boolean).join("\n");
}
```

### 5.2 Example Output

```
=== Session session-lx7k2m9 (2024-01-21T12:00:00Z) ===
[000] REPL > (define (classify x) (effect infer.op (list "Sentiment: " x)))
[001] EVAL ~ define:classify
[002] OUT  => classify
[003] REPL > (classify "I am frustrated")
[004] EVAL ~ call:classify
[005]   EVAL ~ effect:infer.op
[006]   SAVE * checkpoint (llm_boundary)
[007]   EFCT ! infer.op
[008]   LLM  -> gpt-4o-mini: "Sentiment: I am frustrated"
[009]   LLM  <- "negative" (1234ms)
[010]   SAVE * checkpoint (llm_boundary)
[011]   RSME <~ "negative"
[012] EVAL ~ return
[013] OUT  => "negative"
```

---

## Part 6: REPL Integration

### 6.1 New Commands

Add to `bin/omega-repl.ts`:

```typescript
// Session management commands

// :session save <name> - Save current session
if (trimmed.startsWith(":session save ")) {
  const name = trimmed.slice(14).trim();
  if (!name) {
    log("Usage: :session save <name>");
  } else {
    sessionWriter.close();
    const src = path.join(SESSION_DIR, "current.jsonl");
    const dst = path.join(SESSION_DIR, "sessions", `${name}.jsonl`);
    fs.copyFileSync(src, dst);
    // Copy index too
    fs.copyFileSync(
      path.join(SESSION_DIR, "current.index.json"),
      path.join(SESSION_DIR, "sessions", `${name}.index.json")
    );
    log(`Session saved as '${name}'`);
  }
  return { replState, output: output.join("\n"), shouldExit };
}

// :session load <name> - Load a session (read-only view)
if (trimmed.startsWith(":session load ")) {
  const name = trimmed.slice(14).trim();
  const eventFile = path.join(SESSION_DIR, "sessions", `${name}.jsonl`);
  const indexFile = path.join(SESSION_DIR, "sessions", `${name}.index.json`);

  if (!fs.existsSync(eventFile)) {
    log(`Session '${name}' not found`);
  } else {
    const reader = new SessionReader(eventFile, indexFile, nativeRegistry);
    await reader.loadAll();
    replState.loadedSession = reader;
    log(`Loaded session '${name}' (${reader.getEventCount()} events)`);
    log("Use :session goto <seq> to jump, :session trace to view");
  }
  return { replState, output: output.join("\n"), shouldExit };
}

// :session goto <seq> - Jump to specific point
if (trimmed.startsWith(":session goto ")) {
  const seq = parseInt(trimmed.slice(14).trim());
  if (!replState.loadedSession) {
    log("No session loaded. Use :session load <name> first.");
  } else if (isNaN(seq)) {
    log("Usage: :session goto <seq>");
  } else {
    const controller = new JumpController(replState.loadedSession);
    const result = await controller.jumpTo(seq);
    replState.debugState = result.state;
    replState.stepCount = result.seq;
    replState.debugMode = true;
    log(`Jumped to seq ${result.seq}`);
    log(`  Replayed ${result.replayedSteps} steps`);
    log(`  Used ${result.usedReceipts.length} cached LLM receipts`);
    log(`  Control: ${controlToString(result.state.control)}`);
  }
  return { replState, output: output.join("\n"), shouldExit };
}

// :session list - List saved sessions
if (trimmed === ":session list") {
  const sessionsDir = path.join(SESSION_DIR, "sessions");
  const files = fs.readdirSync(sessionsDir).filter(f => f.endsWith(".jsonl"));
  if (files.length === 0) {
    log("No saved sessions.");
  } else {
    log("Saved sessions:");
    for (const f of files) {
      const name = f.replace(".jsonl", "");
      const indexPath = path.join(sessionsDir, `${name}.index.json`);
      if (fs.existsSync(indexPath)) {
        const index = JSON.parse(fs.readFileSync(indexPath, "utf8"));
        log(`  ${name} (${index.eventCount} events, ${index.checkpoints.length} checkpoints)`);
      } else {
        log(`  ${name}`);
      }
    }
  }
  return { replState, output: output.join("\n"), shouldExit };
}

// :session trace [--verbose] - Show trace of loaded session
if (trimmed === ":session trace" || trimmed.startsWith(":session trace ")) {
  if (!replState.loadedSession) {
    log("No session loaded. Use :session load <name> first.");
  } else {
    const verbose = trimmed.includes("--verbose");
    // ... render and display trace
  }
  return { replState, output: output.join("\n"), shouldExit };
}

// :session checkpoints - Show checkpoint summary
if (trimmed === ":session checkpoints") {
  if (!replState.loadedSession) {
    log("No session loaded.");
  } else {
    const cps = replState.loadedSession.getCheckpoints();
    log(`Checkpoints (${cps.length}):`);
    for (const cp of cps) {
      log(`  [${String(cp.seq).padStart(3, "0")}] ${cp.reason} (state: ${cp.stateId})`);
    }
  }
  return { replState, output: output.join("\n"), shouldExit };
}

// :session resume - Continue from current position
if (trimmed === ":session resume") {
  if (!replState.debugState) {
    log("No state to resume from. Use :session goto <seq> first.");
  } else {
    log("Resuming execution...");
    // Switch to live mode with current state
    // Continue with stepOnce / runToCompletion
  }
  return { replState, output: output.join("\n"), shouldExit };
}
```

### 6.2 Wire Session Writer into REPL

In `evalInRepl`:

```typescript
async function evalInRepl(src: string, replState: ReplState): Promise<...> {
  // Emit input event
  replState.sessionWriter?.input(src);

  // ... existing eval logic ...

  // On effect:
  replState.sessionWriter?.pushDepth();
  replState.sessionWriter?.effect(op, args);
  replState.sessionWriter?.checkpoint(state, "llm_boundary");

  // On LLM call:
  const receiptKey = replState.sessionWriter?.llmRequest(model, prompt, fullReq);
  // ... call LLM ...
  replState.sessionWriter?.llmResponse(receiptKey, value, fullResp, durationMs);
  replState.sessionWriter?.checkpoint(state, "llm_boundary");

  // On resume:
  replState.sessionWriter?.resume(valueStr);
  replState.sessionWriter?.popDepth();

  // On result:
  replState.sessionWriter?.result(valueStr);
}
```

---

## Part 7: File Structure

### 7.1 New Files to Create

```
src/core/session/
├── index.ts              # Public exports
├── types.ts              # Event and index types
├── serializer.ts         # State serialization
├── writer.ts             # Session writing
├── reader.ts             # Session reading
├── jump.ts               # Jump controller
└── render.ts             # Pretty rendering

test/session/
├── serializer.spec.ts    # Serialization tests
├── writer.spec.ts        # Writer tests
├── reader.spec.ts        # Reader tests
├── jump.spec.ts          # Jump tests
└── integration.spec.ts   # Full integration tests
```

### 7.2 Export from src/index.ts

```typescript
// Session management
export { SessionWriter } from "./core/session/writer";
export { SessionReader } from "./core/session/reader";
export { JumpController } from "./core/session/jump";
export { serializeState, deserializeState } from "./core/session/serializer";
export { renderEvent, renderTrace } from "./core/session/render";
export type { SessionEvent, SessionIndex, LLMReceipt } from "./core/session/types";
```

---

## Verification

### Test 1: Serialization Round-Trip

```bash
cd OmegaLLM
npm test -- --run test/session/serializer.spec.ts
# All tests should pass
```

### Test 2: Session Save/Load

```bash
npx tsx bin/omega-repl.ts
> (define (classify x) (effect infer.op (list "Sentiment: " x)))
> (classify "happy")
> :session save test1
> :quit

npx tsx bin/omega-repl.ts
> :session load test1
> :session trace
# Should show the full trace
> :session checkpoints
# Should show checkpoint at LLM boundaries
> :session goto 6
# Should jump to that point instantly
```

### Test 3: Jump Performance

```bash
npx tsx bin/omega-repl.ts
> :session load large-session
> :time :session goto 500
# Should complete in < 100ms for checkpoint jumps
```

### Test 4: Resume from Checkpoint

```bash
npx tsx bin/omega-repl.ts
> :session load test1
> :session goto 6  # Just before LLM response
> :session resume
# Should continue execution (may call LLM live or use receipt)
```

---

## Checklist

### Core Implementation
- [ ] Create `src/core/session/` directory
- [ ] Implement `types.ts` with all event types
- [ ] Implement `nativeRegistry.ts` with `buildNativeRegistry()`
- [ ] Implement `serializer.ts` with serializeState/deserializeState
- [ ] Implement `writer.ts` with SessionWriter class
- [ ] Implement `reader.ts` with SessionReader class
- [ ] Implement `jump.ts` with JumpController class
- [ ] Implement `render.ts` with pretty renderer

### REPL Integration
- [ ] Wire SessionWriter into omega-repl.ts (auto-save to current.jsonl)
- [ ] Add `:session save <name>` command
- [ ] Add `:session load <name>` command
- [ ] Add `:session list` command
- [ ] Add `:session goto <seq>` command
- [ ] Add `:session trace` command
- [ ] Add `:session checkpoints` command
- [ ] Add `:session resume` command
- [ ] Add `:session fork <name>` command (for what-if exploration)
- [ ] Update `:help` with new commands
- [ ] Export from src/index.ts

### Required Tests (ALL MUST PASS)
- [ ] `test/session/serializer.spec.ts`:
  - [ ] Atomic values round-trip
  - [ ] BigInt round-trips via string
  - [ ] Closures preserve environment
  - [ ] Native functions restore via registry lookup
  - [ ] Circular Ctx.parent is flattened and restored
  - [ ] Continuation stack round-trips (all 18 Frame types)
  - [ ] Cont with Resumption round-trips
- [ ] `test/session/integration.spec.ts`:
  - [ ] Auto-saves session to current.jsonl
  - [ ] Explicit save creates named session
  - [ ] Load restores environment bindings
  - [ ] Checkpoints created at LLM boundaries
  - [ ] Jump restores exact state at checkpoint
  - [ ] Resume uses cached receipts (no live LLM call)
  - [ ] **What-if: jump then run different code with same env**
  - [ ] **What-if: modify env at checkpoint then continue**
  - [ ] Trace output shows depth and source correctly
  - [ ] Full state round-trips through serialization

### Manual Verification
- [ ] REPL: Define functions, save, quit, reload, verify functions work
- [ ] REPL: Jump to checkpoint, run different input, get different result
- [ ] REPL: Jump to checkpoint, modify env, continue with modified env
- [ ] REPL: Trace shows readable output with [seq] SOURCE > content format

---

---

## What You Get When This Works

### Capability 1: Save and Resume Sessions Across REPL Restarts

```bash
# Day 1: Do some work
npx tsx bin/omega-repl.ts
> (define (classify x) (effect infer.op (list "Sentiment: " x)))
> (classify "I love this!")
=> "positive"
> :session save my-classifier
Session saved as 'my-classifier'
> :quit

# Day 2: Pick up where you left off
npx tsx bin/omega-repl.ts
> :session load my-classifier
Loaded session 'my-classifier' (47 events, 4 checkpoints)
> :session trace
=== Session my-classifier ===
[000] REPL > (define (classify x) ...)
[001] EVAL ~ define:classify
...
```

### Capability 2: See Readable, Structured Traces

```
[000] REPL > (classify "I am frustrated")
[001] EVAL ~ call:classify
[002]   EVAL ~ effect:infer.op
[003]   SAVE * checkpoint (llm_boundary)
[004]   EFCT ! infer.op
[005]   LLM  -> gpt-4o-mini: "Sentiment: I am frustrated"
[006]   LLM  <- "negative" (1234ms)
[007]   SAVE * checkpoint (llm_boundary)
[008]   RSME <~ "negative"
[009] EVAL ~ return
[010] OUT  => "negative"
```

**Depth is visible. Source is clear. Machine-readable AND human-readable.**

### Capability 3: Jump to Any Point Instantly

```bash
> :session goto 5   # Jump to just before LLM response
Jumped to seq 5
  Replayed 0 steps       # Direct checkpoint load!
  Used 0 cached receipts
  Control: (effect infer.op ...)
```

### Capability 4: Resume Execution from Any Checkpoint

```bash
> :session goto 5
> :session resume
# Continues from there - uses cached receipt for LLM
=> "negative"
```

### Capability 5: Time-Travel Debugging

```bash
> :session checkpoints
Checkpoints (4):
  [003] llm_boundary (state: state-3)
  [007] llm_boundary (state: state-7)
  [015] periodic (state: state-15)
  [023] llm_boundary (state: state-23)

> :session goto 3   # Go back before first LLM call
> :step             # Step forward manually
> :step
> :state            # Inspect state at any point
```

### Capability 6: Deterministic Replay

LLM calls replay with cached receipts = same responses without API calls:

```
# First run:
LLM  -> gpt-4o-mini: "Sentiment: happy"
LLM  <- "positive" (892ms)     # Actual API call, saved receipt

# Replay:
Used 1 cached LLM receipts     # No API call, instant
```

### Capability 7: Share Sessions

```bash
# Export
cp .omega-session/sessions/my-debug.jsonl ./share-with-team.jsonl
cp .omega-session/sessions/my-debug.index.json ./share-with-team.index.json

# Import on another machine
:session load share-with-team
```

### Capability 8: Crash Recovery

- Index saved after every checkpoint
- JSONL is append-only
- If REPL crashes, reload and resume from last checkpoint

### Capability 9: "What-If" Exploration - Rerun with Different Prompts (CRITICAL)

**This is a key use case**: Jump to an LLM call checkpoint, then continue with different inputs while keeping ALL your environment setup.

```bash
# Original session built up state:
> (define system-prompt "You are a sentiment classifier")
> (define (classify text)
    (effect infer.op (list system-prompt ": " text)))
> (define results '())
> (classify "I love this")
=> "positive"
> (set! results (cons "positive" results))

# Now I want to try different inputs WITHOUT re-running everything:
> :session goto 4                    # Jump to just before LLM call
Jumped to seq 4
  Control: (effect infer.op ...)
  Environment has: system-prompt, classify, results

# Option A: Call with different input (same function, new args)
> (classify "I hate this")           # Uses live LLM - different input!
=> "negative"

# Option B: Modify the system prompt and retry
> (set! system-prompt "You classify emotions, not sentiment")
> (classify "I love this")           # Same input, different prompt!
=> "joy"

# Option C: Fork the session to explore alternatives
> :session fork what-if-emotions     # Creates branch from current point
> (classify "I am confused")
=> "confusion"
> :session switch main               # Go back to main timeline
```

**Key insight**: After `:session goto`, you're in a **live REPL** with:
- All your `define`s intact
- All your `set!` mutations preserved
- The exact execution point restored
- Full ability to run NEW code

---

## Part 8: Required Tests (MUST IMPLEMENT)

These tests MUST pass before the job is complete. They exercise the actual REPL workflows.

### Test File: `test/session/integration.spec.ts`

```typescript
import { describe, it, expect, beforeEach, afterEach } from "vitest";
import { spawn } from "child_process";
import * as fs from "fs";
import * as path from "path";

const SESSION_DIR = ".omega-session-test";

// Helper: Run REPL commands and capture output
async function runRepl(commands: string[]): Promise<string[]> {
  return new Promise((resolve, reject) => {
    const repl = spawn("npx", ["tsx", "bin/omega-repl.ts"], {
      env: { ...process.env, OMEGA_SESSION_DIR: SESSION_DIR },
    });

    const outputs: string[] = [];
    let buffer = "";

    repl.stdout.on("data", (data) => {
      buffer += data.toString();
      // Collect outputs after prompts
      const lines = buffer.split("\n");
      for (const line of lines) {
        if (line.startsWith("=>") || line.startsWith("Jumped") || line.includes("checkpoint")) {
          outputs.push(line.trim());
        }
      }
    });

    // Send commands one by one
    let i = 0;
    const sendNext = () => {
      if (i < commands.length) {
        repl.stdin.write(commands[i] + "\n");
        i++;
        setTimeout(sendNext, 100);  // Wait for processing
      } else {
        repl.stdin.write(":quit\n");
      }
    };

    repl.on("close", () => resolve(outputs));
    repl.on("error", reject);

    setTimeout(sendNext, 500);  // Wait for REPL startup
  });
}

describe("Session Integration Tests", () => {
  beforeEach(() => {
    // Clean test session directory
    if (fs.existsSync(SESSION_DIR)) {
      fs.rmSync(SESSION_DIR, { recursive: true });
    }
  });

  afterEach(() => {
    if (fs.existsSync(SESSION_DIR)) {
      fs.rmSync(SESSION_DIR, { recursive: true });
    }
  });

  // ═══════════════════════════════════════════════════════════════════════════
  // TEST 1: Auto-save happens (current.jsonl created)
  // ═══════════════════════════════════════════════════════════════════════════
  it("auto-saves session to current.jsonl", async () => {
    await runRepl([
      "(define x 42)",
      "(+ x 1)",
    ]);

    const currentFile = path.join(SESSION_DIR, "current.jsonl");
    expect(fs.existsSync(currentFile)).toBe(true);

    const content = fs.readFileSync(currentFile, "utf8");
    expect(content).toContain('"type":"input"');
    expect(content).toContain("define x 42");
  });

  // ═══════════════════════════════════════════════════════════════════════════
  // TEST 2: Explicit save creates named session
  // ═══════════════════════════════════════════════════════════════════════════
  it("explicit save creates named session", async () => {
    await runRepl([
      "(define x 42)",
      ":session save my-test",
    ]);

    const savedFile = path.join(SESSION_DIR, "sessions", "my-test.jsonl");
    const indexFile = path.join(SESSION_DIR, "sessions", "my-test.index.json");

    expect(fs.existsSync(savedFile)).toBe(true);
    expect(fs.existsSync(indexFile)).toBe(true);
  });

  // ═══════════════════════════════════════════════════════════════════════════
  // TEST 3: Load session and verify environment is restored
  // ═══════════════════════════════════════════════════════════════════════════
  it("load restores environment bindings", async () => {
    // Session 1: Define something
    await runRepl([
      "(define my-fn (lambda (x) (* x 2)))",
      ":session save env-test",
    ]);

    // Session 2: Load and use it
    const outputs = await runRepl([
      ":session load env-test",
      ":session goto 999",  // Go to end
      "(my-fn 21)",         // Should work!
    ]);

    expect(outputs).toContain("=> 42");
  });

  // ═══════════════════════════════════════════════════════════════════════════
  // TEST 4: Checkpoints created at LLM boundaries
  // ═══════════════════════════════════════════════════════════════════════════
  it("creates checkpoints at LLM boundaries", async () => {
    // This test requires scripted oracle to avoid real LLM calls
    await runRepl([
      '(effect infer.op "test prompt")',  // Will create checkpoint
      ":session save llm-test",
    ]);

    const indexFile = path.join(SESSION_DIR, "sessions", "llm-test.index.json");
    const index = JSON.parse(fs.readFileSync(indexFile, "utf8"));

    // Should have at least one llm_boundary checkpoint
    const llmCheckpoints = index.checkpoints.filter(
      (cp: any) => cp.reason === "llm_boundary"
    );
    expect(llmCheckpoints.length).toBeGreaterThan(0);
  });

  // ═══════════════════════════════════════════════════════════════════════════
  // TEST 5: Jump to checkpoint and verify state
  // ═══════════════════════════════════════════════════════════════════════════
  it("jump restores exact state at checkpoint", async () => {
    // Session 1: Build up state
    await runRepl([
      "(define counter 0)",
      "(set! counter 1)",
      "(set! counter 2)",
      "(set! counter 3)",
      ":session save counter-test",
    ]);

    // Session 2: Jump to middle and check value
    const outputs = await runRepl([
      ":session load counter-test",
      ":session goto 3",  // After second set!
      "counter",          // Should be 2, not 3
    ]);

    expect(outputs).toContain("=> 2");
  });

  // ═══════════════════════════════════════════════════════════════════════════
  // TEST 6: Resume uses cached LLM receipts
  // ═══════════════════════════════════════════════════════════════════════════
  it("resume uses cached receipts (no live LLM call)", async () => {
    // This needs scripted oracle that records receipts
    // First run: LLM call happens, receipt cached
    await runRepl([
      '(effect infer.op "What is 2+2?")',
      ":session save receipt-test",
    ]);

    // Second run: Jump back and resume - should use cached receipt
    const outputs = await runRepl([
      ":session load receipt-test",
      ":session goto 1",   // Before LLM response
      ":session resume",   // Should use cached receipt
    ]);

    // Check that it says "Used X cached receipts"
    const usedCached = outputs.some(o => o.includes("cached"));
    expect(usedCached).toBe(true);
  });

  // ═══════════════════════════════════════════════════════════════════════════
  // TEST 7: "What-If" - Jump and run different code
  // ═══════════════════════════════════════════════════════════════════════════
  it("what-if: jump then run different code with same env", async () => {
    // Session 1: Define classifier
    await runRepl([
      "(define (double x) (* x 2))",
      "(double 5)",  // => 10
      ":session save what-if-test",
    ]);

    // Session 2: Jump back and try different input
    const outputs = await runRepl([
      ":session load what-if-test",
      ":session goto 1",   // After define, before (double 5)
      "(double 100)",      // Different input!
    ]);

    expect(outputs).toContain("=> 200");
  });

  // ═══════════════════════════════════════════════════════════════════════════
  // TEST 8: "What-If" - Modify environment then continue
  // ═══════════════════════════════════════════════════════════════════════════
  it("what-if: modify env at checkpoint then continue", async () => {
    await runRepl([
      "(define multiplier 2)",
      "(define (scale x) (* x multiplier))",
      "(scale 5)",  // => 10
      ":session save modify-env-test",
    ]);

    // Jump back, change multiplier, re-run
    const outputs = await runRepl([
      ":session load modify-env-test",
      ":session goto 2",         // After defines, before (scale 5)
      "(set! multiplier 10)",    // Change the multiplier!
      "(scale 5)",               // Should now be 50
    ]);

    expect(outputs).toContain("=> 50");
  });

  // ═══════════════════════════════════════════════════════════════════════════
  // TEST 9: Trace output is readable and structured
  // ═══════════════════════════════════════════════════════════════════════════
  it("trace output shows depth and source correctly", async () => {
    await runRepl([
      "(define (nested x) (+ x 1))",
      "(nested (nested 1))",
      ":session save trace-test",
    ]);

    const outputs = await runRepl([
      ":session load trace-test",
      ":session trace",
    ]);

    // Should have REPL, EVAL lines with proper formatting
    const traceOutput = outputs.join("\n");
    expect(traceOutput).toMatch(/\[\d+\]\s+REPL/);  // [000] REPL >
    expect(traceOutput).toMatch(/\[\d+\]\s+EVAL/);  // [001] EVAL ~
    expect(traceOutput).toMatch(/\[\d+\]\s+OUT/);   // [00X] OUT =>
  });

  // ═══════════════════════════════════════════════════════════════════════════
  // TEST 10: Full round-trip serialization
  // ═══════════════════════════════════════════════════════════════════════════
  it("full state round-trips through serialization", async () => {
    // Complex state with closures, mutations, nested calls
    await runRepl([
      "(define acc '())",
      "(define (push x) (set! acc (cons x acc)))",
      "(push 1)",
      "(push 2)",
      "(push 3)",
      ":session save roundtrip-test",
    ]);

    // Load and verify acc has all values
    const outputs = await runRepl([
      ":session load roundtrip-test",
      ":session goto 999",
      "acc",
    ]);

    // acc should be (3 2 1)
    expect(outputs.some(o => o.includes("3") && o.includes("2") && o.includes("1"))).toBe(true);
  });
});
```

### Test File: `test/session/serializer.spec.ts`

```typescript
import { describe, it, expect } from "vitest";
import { serializeState, deserializeState } from "../../src/core/session/serializer";
import { buildNativeRegistry } from "../../src/core/session/nativeRegistry";
import { COWStore } from "../../src/core/eval/store";
import { installPrims } from "../../src/core/prims";

describe("State Serializer", () => {

  // Setup: Get a primed store with natives
  function setup() {
    const store = new COWStore();
    const { env, store: primedStore } = installPrims(store);
    const nativeRegistry = buildNativeRegistry(primedStore);
    return { env, store: primedStore, nativeRegistry };
  }

  // ═══════════════════════════════════════════════════════════════════════════
  // TEST: Atomic values round-trip
  // ═══════════════════════════════════════════════════════════════════════════
  it("round-trips atomic values", () => {
    const { env, store, nativeRegistry } = setup();

    const state = {
      control: { tag: "Val" as const, v: { tag: "Num" as const, n: 42 } },
      env, store, kont: [], handlers: [],
    };

    const serialized = serializeState(state);
    const json = JSON.stringify(serialized);
    const restored = deserializeState(JSON.parse(json), nativeRegistry);

    expect(restored.control).toEqual({ tag: "Val", v: { tag: "Num", n: 42 } });
  });

  // ═══════════════════════════════════════════════════════════════════════════
  // TEST: BigInt round-trips via string
  // ═══════════════════════════════════════════════════════════════════════════
  it("round-trips BigInt values", () => {
    const { env, store, nativeRegistry } = setup();

    const state = {
      control: { tag: "Val" as const, v: { tag: "Int" as const, value: BigInt("12345678901234567890") } },
      env, store, kont: [], handlers: [],
    };

    const serialized = serializeState(state);
    const json = JSON.stringify(serialized);
    const restored = deserializeState(JSON.parse(json), nativeRegistry);

    expect(restored.control.tag).toBe("Val");
    expect((restored.control as any).v.tag).toBe("Int");
    expect((restored.control as any).v.value.toString()).toBe("12345678901234567890");
  });

  // ═══════════════════════════════════════════════════════════════════════════
  // TEST: Closures preserve environment
  // ═══════════════════════════════════════════════════════════════════════════
  it("round-trips closures with captured environment", () => {
    const { env, store, nativeRegistry } = setup();

    const closure = {
      tag: "Closure" as const,
      params: ["x"],
      body: { tag: "Var" as const, name: "x" },
      env: env,  // Captures the environment
    };

    const state = {
      control: { tag: "Val" as const, v: closure },
      env, store, kont: [], handlers: [],
    };

    const serialized = serializeState(state);
    const json = JSON.stringify(serialized);
    const restored = deserializeState(JSON.parse(json), nativeRegistry);

    expect((restored.control as any).v.tag).toBe("Closure");
    expect((restored.control as any).v.params).toEqual(["x"]);
    expect((restored.control as any).v.env).toBeDefined();
  });

  // ═══════════════════════════════════════════════════════════════════════════
  // TEST: Native functions restore via registry lookup
  // ═══════════════════════════════════════════════════════════════════════════
  it("round-trips Native functions via registry lookup", () => {
    const { env, store, nativeRegistry } = setup();

    // Get a native from the registry
    const plusNative = nativeRegistry.get("+");
    expect(plusNative).toBeDefined();

    const state = {
      control: { tag: "Val" as const, v: plusNative! },
      env, store, kont: [], handlers: [],
    };

    const serialized = serializeState(state);
    const json = JSON.stringify(serialized);

    // Verify fn is NOT in JSON (can't serialize functions)
    expect(json).not.toContain('"fn"');
    expect(json).toContain('"name":"+"');

    const restored = deserializeState(JSON.parse(json), nativeRegistry);

    // Verify fn is restored via registry
    expect((restored.control as any).v.tag).toBe("Native");
    expect((restored.control as any).v.name).toBe("+");
    expect(typeof (restored.control as any).v.fn).toBe("function");
  });

  // ═══════════════════════════════════════════════════════════════════════════
  // TEST: Circular Ctx.parent is flattened and restored
  // ═══════════════════════════════════════════════════════════════════════════
  it("handles circular Ctx.parent references", () => {
    const { env, store, nativeRegistry } = setup();

    // env has parent chain - verify it serializes without throwing
    const state = { control: { tag: "Val" as const, v: { tag: "Unit" as const } }, env, store, kont: [], handlers: [] };

    const serialized = serializeState(state);
    const json = JSON.stringify(serialized);  // Should NOT throw circular ref error

    const restored = deserializeState(JSON.parse(json), nativeRegistry);

    // Verify parent chain is restored
    expect(restored.env).toBeDefined();
    // Parent should be restorable if it existed
  });

  // ═══════════════════════════════════════════════════════════════════════════
  // TEST: Continuation stack round-trips
  // ═══════════════════════════════════════════════════════════════════════════
  it("round-trips continuation stack (all Frame types)", () => {
    const { env, store, nativeRegistry } = setup();

    // Create a state with various frame types
    const state = {
      control: { tag: "Expr" as const, e: { tag: "Num" as const, n: 1 } },
      env, store,
      kont: [
        { tag: "KIf" as const, conseq: { tag: "Num" as const, n: 2 }, alt: { tag: "Num" as const, n: 3 }, env },
        { tag: "KBegin" as const, rest: [], env },
        { tag: "KCall" as const, savedEnv: env },
      ],
      handlers: [],
    };

    const serialized = serializeState(state);
    const json = JSON.stringify(serialized);
    const restored = deserializeState(JSON.parse(json), nativeRegistry);

    expect(restored.kont.length).toBe(3);
    expect(restored.kont[0].tag).toBe("KIf");
    expect(restored.kont[1].tag).toBe("KBegin");
    expect(restored.kont[2].tag).toBe("KCall");
  });

  // ═══════════════════════════════════════════════════════════════════════════
  // TEST: Resumption serializes base state
  // ═══════════════════════════════════════════════════════════════════════════
  it("round-trips Cont with Resumption", () => {
    const { env, store, nativeRegistry } = setup();

    // Create a mock resumption
    const baseState = { control: { tag: "Val" as const, v: { tag: "Unit" as const } }, env, store, kont: [], handlers: [] };
    const resumption = {
      rid: "test-rid-123",
      base: baseState,
      invoke: (v: any) => ({ ...baseState, control: { tag: "Val", v } }),
      digest: () => "test-digest",
    };

    const cont = {
      tag: "Cont" as const,
      hid: "handler-1",
      boundaryIndex: 0,
      resumption,
    };

    const state = { control: { tag: "Val" as const, v: cont }, env, store, kont: [], handlers: [] };

    const serialized = serializeState(state);
    const json = JSON.stringify(serialized);
    const restored = deserializeState(JSON.parse(json), nativeRegistry);

    const restoredCont = (restored.control as any).v;
    expect(restoredCont.tag).toBe("Cont");
    expect(restoredCont.hid).toBe("handler-1");
    expect(restoredCont.resumption.rid).toBe("test-rid-123");
    expect(typeof restoredCont.resumption.invoke).toBe("function");
  });
});
```

---

## How to Verify Implementation (Step by Step)

### Step 1: Run Unit Tests
```bash
cd OmegaLLM
npm test -- --run test/session/serializer.spec.ts
npm test -- --run test/session/integration.spec.ts
# ALL tests must pass
```

### Step 2: Manual REPL Verification

```bash
# Terminal 1: Start REPL, build up state
npx tsx bin/omega-repl.ts

> (define pi 3.14159)
> (define (area r) (* pi (* r r)))
> (area 5)
=> 78.53975
> :session save area-test
Session saved as 'area-test'
> :quit
```

```bash
# Terminal 2: Load and verify
npx tsx bin/omega-repl.ts

> :session load area-test
Loaded session 'area-test' (X events, Y checkpoints)

> :session trace
[000] REPL > (define pi 3.14159)
[001] EVAL ~ define:pi
...

> :session goto 2
Jumped to seq 2
  Control: ...

> pi                    # Should be defined!
=> 3.14159

> (area 10)             # Should work!
=> 314.159
```

### Step 3: "What-If" Verification

```bash
npx tsx bin/omega-repl.ts

> (define prompt "Classify sentiment:")
> (define (classify x) (effect infer.op (list prompt " " x)))
> (classify "happy")
=> "positive"
> :session save sentiment-test

# Now do what-if exploration:
> :session goto 3          # Before (classify "happy")
> (classify "sad")         # Different input, same setup
=> "negative"

> :session goto 3
> (set! prompt "Classify emotion:")  # Different prompt!
> (classify "happy")
=> "joy"                   # Different result!
```

---

## Design Decisions Resolved

| Gap | Resolution |
|-----|------------|
| Only some Frame types shown | Complete list of all 18 Frame types from machine.ts included |
| Resumption serialization | `invoke` just does `{...base, control: {tag:"Val", v}}` - serialize base State, reconstruct invoke |
| Effect resumption | JumpController uses `captureValueResumption` then `invoke(cachedVal)` |
| Native registry construction | `buildNativeRegistry(store)` scans primed store for Native values |
| SolverVal functions | Same pattern - `solverRegistry` with name lookup |
| BigInt serialization | Serialize as `{tag:"Int", value: bigint.toString()}` |
| Circular Ctx.parent | Flatten to `ctxTable` with ID references |
| Map serialization | Convert to `[key, value][]` arrays |

---

## Notes

### Why JSONL?

- **Append-only**: Can write events as they happen, crash-safe
- **Streamable**: Can process large files without loading all into memory
- **Human-readable**: Can inspect with `cat`, `head`, `grep`
- **Standard**: Widely supported, easy to parse

### Why Separate Index File?

- **Fast lookup**: Find checkpoints without parsing entire JSONL
- **State storage**: Keep serialized states separate from event log
- **Receipts**: LLM receipts need to persist for replay

### Checkpoint Strategy

- **Always checkpoint at LLM boundaries**: Before and after every LLM call
- **Periodic checkpoints**: Every 100 steps for long-running evals
- **Manual checkpoints**: User can trigger with `:session checkpoint`

This ensures:
- Jump to any LLM call = instant (direct state load)
- Jump to intermediate step = fast (< 100 steps replay)
- Replay is deterministic (receipts for LLM)

---

## Footer

| Field | Value |
|-------|-------|
| Created | 2024-01-21 |
| Author | Claude |
| Depends On | Core CEKS machine, REPL |
| Affects | bin/omega-repl.ts, src/core/session/* |
