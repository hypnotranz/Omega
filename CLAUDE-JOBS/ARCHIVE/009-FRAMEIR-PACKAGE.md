# JOB-009: Implement FrameIR Package

**Priority**: P0 - Foundation (all other architecture work depends on this)
**Estimated Effort**: 3-5 days
**Skills Required**: TypeScript, compiler design, JSON serialization
**Status**: DONE

> **MUST READ**: [LOGISTICS.md](LOGISTICS.md) before starting - covers testing, file locations, and proof of completion requirements.

### Quick Start

```bash
cd c:\Users\Richa\parmenides-dev\agent-harness\OmegaLLM

# Run existing tests (verify baseline)
npm run test

# After implementation, run new tests
npx vitest run test/frameir/

# Watch mode during development
npm run test:watch
```

### Dependencies

- **Depends On**: None (this is foundational)
- **Blocks**: Job 010 (Primitive Registry), Job 011 (Port Abstractions), Job 012 (Outcome/Failure/Diagnostic), Job 013 (Lint Passes), Job 014 (Compilation Pipeline), Job 015 (Replay System)

---

## Reference Documents

This job implements specifications from:

| Document | Sections | Topics |
|----------|----------|--------|
| [ARCHITECTURE-LANGUAGES-4.md](../docs/ARCHITECTURE-LANGUAGES-4.md) | §42-43 | FrameIR spec, canonical encoding, merkleization |
| [ARCHITECTURE-LANGUAGES-5.md](../docs/ARCHITECTURE-LANGUAGES-5.md) | §56.1-56.6 | **Complete TypeScript type definitions** |

**Key Quote from §56**:
> "What you want next is *not* 'more functions'; it's a **precise, mechanically-checkable IR contract**. Everything else (runtime, language, solver library, docgen, lints, replay, cache, provenance) becomes a projection of this contract."

---

## Executive Summary

Create a new `@frameir` package containing the canonical intermediate representation for all OmegaLLM programs. This IR is:

1. **Tagged** - Every node has a discriminator tag for pattern matching
2. **Versioned** - `IRVersion = "frameir@1"`
3. **Canonical** - Deterministic serialization for hashing/caching
4. **Merkleizable** - Content-addressed nodes for incremental computation

This is the "truth layer" that enables caching, replay, linting, and doc generation.

---

## Output Files

| File | Action | Description |
|------|--------|-------------|
| `src/frameir/index.ts` | **CREATE** | Package exports |
| `src/frameir/version.ts` | **CREATE** | IRVersion constant and migration helpers |
| `src/frameir/meta.ts` | **CREATE** | Span, Meta, NodeBase types |
| `src/frameir/value.ts` | **CREATE** | ValueIR types (VNil, VBool, VInt, etc.) |
| `src/frameir/expr.ts` | **CREATE** | VExpr types (EIf, EEq, EAdd, etc.) |
| `src/frameir/prompt.ts` | **CREATE** | PromptIR types (PromptDoc, PSystem, etc.) |
| `src/frameir/flow.ts` | **CREATE** | FlowIR types (FPure, FBind, FInfer, etc.) |
| `src/frameir/bundle.ts` | **CREATE** | IRBundle, FnDefIR types |
| `src/frameir/schema.ts` | **CREATE** | SchemaIR, FrameSchemaNode types |
| `src/frameir/contract.ts` | **CREATE** | ToolContractIR type |
| `src/frameir/codec.ts` | **CREATE** | encodeCanonical(), decode() |
| `src/frameir/hash.ts` | **CREATE** | hashNode(), merkleize() |
| `src/frameir/normalize.ts` | **CREATE** | normalizePrompt(), normalizeFlow() |
| `src/frameir/visitor.ts` | **CREATE** | visitFlow(), rewriteFlow() |
| `test/frameir/codec.spec.ts` | **CREATE** | Canonical encoding tests |
| `test/frameir/hash.spec.ts` | **CREATE** | Hashing determinism tests |
| `test/frameir/normalize.spec.ts` | **CREATE** | Normalization tests |

---

## Task 1: Core Types and NodeBase

**Reference**: [ARCHITECTURE-LANGUAGES-5.md §56.2](../docs/ARCHITECTURE-LANGUAGES-5.md)

### 1.1 Create `src/frameir/version.ts`

```typescript
export type IRVersion = "frameir@1";
export const CURRENT_IR_VERSION: IRVersion = "frameir@1";
```

### 1.2 Create `src/frameir/meta.ts`

```typescript
import { IRVersion } from "./version";

export interface Span {
  file?: string;        // logical source file id
  startLine?: number;
  startCol?: number;
  endLine?: number;
  endCol?: number;
}

export interface Meta {
  span?: Span;                  // non-semantic (excluded from hash)
  doc?: string;                 // non-semantic
  attrs?: Record<string, unknown>; // non-semantic
}

export interface NodeBase {
  v: IRVersion;         // required
  tag: string;          // required discriminator
  meta?: Meta;          // optional
}
```

**Key invariant**: `meta` is NEVER included in semantic hashing unless explicitly requested.

---

## Task 2: ValueIR Types

**Reference**: [ARCHITECTURE-LANGUAGES-5.md §56.3](../docs/ARCHITECTURE-LANGUAGES-5.md)

### 2.1 Create `src/frameir/value.ts`

```typescript
import { NodeBase } from "./meta";
import { VExpr } from "./expr";

// Primitive values
export interface VNil extends NodeBase { tag: "VNil" }
export interface VBool extends NodeBase { tag: "VBool"; value: boolean }
export interface VInt extends NodeBase { tag: "VInt"; value: string } // canonical decimal
export interface VFloat extends NodeBase { tag: "VFloat"; value: string } // canonical decimal
export interface VStr extends NodeBase { tag: "VStr"; value: string }
export interface VSymbol extends NodeBase { tag: "VSymbol"; name: string }   // 'foo
export interface VKeyword extends NodeBase { tag: "VKeyword"; name: string } // :foo

// Compound values
export interface VList extends NodeBase { tag: "VList"; items: ValueIR[] }
export interface VRecord extends NodeBase {
  tag: "VRecord";
  entries: Array<{ k: ValueIR; v: ValueIR }>
}

// References (into bundle or external resources)
export type RefKind = "Global" | "Fn" | "ToolContract" | "Schema" | "Store" | "Sink" | "Source";

export interface VRef extends NodeBase {
  tag: "VRef";
  ref: {
    kind: RefKind;
    id: string;         // stable id (sha256)
    name?: string;      // human label (non-semantic)
  };
}

// Union of all value types
export type ValueIR =
  | VNil | VBool | VInt | VFloat | VStr | VSymbol | VKeyword
  | VList | VRecord
  | VRef
  | VExpr;
```

### 2.2 Create `src/frameir/expr.ts`

Tiny expression language for predicates, options, budget calculations:

```typescript
import { NodeBase } from "./meta";
import type { ValueIR } from "./value";

// Boolean ops
export interface EIf extends NodeBase { tag: "EIf"; cond: ValueIR; then: ValueIR; else: ValueIR }
export interface EEq extends NodeBase { tag: "EEq"; a: ValueIR; b: ValueIR }
export interface ENot extends NodeBase { tag: "ENot"; x: ValueIR }
export interface EAnd extends NodeBase { tag: "EAnd"; xs: ValueIR[] } // short-circuit
export interface EOr extends NodeBase { tag: "EOr"; xs: ValueIR[] }  // short-circuit

// Arithmetic ops
export interface EAdd extends NodeBase { tag: "EAdd"; xs: ValueIR[] }
export interface ESub extends NodeBase { tag: "ESub"; xs: ValueIR[] }
export interface EMul extends NodeBase { tag: "EMul"; xs: ValueIR[] }
export interface EDiv extends NodeBase { tag: "EDiv"; a: ValueIR; b: ValueIR }
export interface EMod extends NodeBase { tag: "EMod"; a: ValueIR; b: ValueIR }

// Map ops
export interface EGet extends NodeBase { tag: "EGet"; map: ValueIR; key: ValueIR; default?: ValueIR }
export interface EAssoc extends NodeBase { tag: "EAssoc"; map: ValueIR; key: ValueIR; val: ValueIR }

// Primitive call
export interface ECallPrim extends NodeBase {
  tag: "ECallPrim";
  prim: string;     // e.g. "lambdallm.core/hash"
  args: ValueIR[];
}

export type VExpr =
  | EIf | EEq | ENot | EAnd | EOr
  | EAdd | ESub | EMul | EDiv | EMod
  | EGet | EAssoc
  | ECallPrim;
```

---

## Task 3: PromptIR Types

**Reference**: [ARCHITECTURE-LANGUAGES-5.md §56.4](../docs/ARCHITECTURE-LANGUAGES-5.md)

### 3.1 Create `src/frameir/prompt.ts`

```typescript
import { NodeBase } from "./meta";
import { ValueIR, VRef } from "./value";

// Role-based segments
export interface PSystem extends NodeBase { tag: "PSystem"; text: string }
export interface PUser extends NodeBase { tag: "PUser"; text: string }
export interface PAssistant extends NodeBase { tag: "PAssistant"; text: string }

// Few-shot examples
export interface PFewShot extends NodeBase {
  tag: "PFewShot";
  examples: Array<{ user: string; assistant: string }>;
}

// Data embedding
export interface PData extends NodeBase { tag: "PData"; value: ValueIR }

// Structure wrappers
export interface PXml extends NodeBase { tag: "PXml"; tagName: string; inner: PromptIR }
export interface PCodeBlock extends NodeBase { tag: "PCodeBlock"; lang: string; code: string }
export interface PNumbered extends NodeBase { tag: "PNumbered"; items: PromptIR[] }

// Attachments (for tools/schemas)
export interface PAttachTools extends NodeBase {
  tag: "PAttachTools";
  tools: VRef[];      // each VRef kind = ToolContract
  inner: PromptIR;
}

export interface PAttachSchema extends NodeBase {
  tag: "PAttachSchema";
  schema: VRef;       // kind = Schema
  inner: PromptIR;
}

export interface PAttachFormat extends NodeBase {
  tag: "PAttachFormat";
  format: { kind: "json" | "xml" | "text"; details?: ValueIR };
  inner: PromptIR;
}

// Transforms
export interface PTransform extends NodeBase {
  tag: "PTransform";
  transform: string;  // registry id for transformer
  inner: PromptIR;
}

// Part union
export type PromptPart =
  | PSystem | PUser | PAssistant
  | PFewShot
  | PData
  | PXml | PCodeBlock | PNumbered
  | PAttachTools | PAttachSchema | PAttachFormat
  | PTransform;

// Top-level prompt document
export interface PromptDoc extends NodeBase {
  tag: "PromptDoc";
  parts: PromptPart[];
}

export type PromptIR = PromptDoc;
```

---

## Task 4: FlowIR Types

**Reference**: [ARCHITECTURE-LANGUAGES-5.md §56.5](../docs/ARCHITECTURE-LANGUAGES-5.md)

### 4.1 Create `src/frameir/flow.ts`

```typescript
import { NodeBase } from "./meta";
import { ValueIR, VRef } from "./value";
import { PromptIR } from "./prompt";

// Pure value
export interface FPure extends NodeBase { tag: "FPure"; value: ValueIR }

// Monadic bind (binder is Fn reference, closure-converted)
export interface FBind extends NodeBase {
  tag: "FBind";
  flow: FlowIR;
  k: VRef;            // kind = Fn
}

// Error handling
export interface FCatch extends NodeBase {
  tag: "FCatch";
  flow: FlowIR;
  handler: VRef;      // kind = Fn (receives Failure)
}

export interface FFail extends NodeBase {
  tag: "FFail";
  reason: ValueIR;    // keyword or symbol
  ctx?: ValueIR;      // structured context record
}

// Resource control
export interface FWithBudget extends NodeBase {
  tag: "FWithBudget";
  budget: ValueIR;    // record: {llmCalls?, tokens?, timeMs?}
  flow: FlowIR;
}

export interface FWithTimeout extends NodeBase {
  tag: "FWithTimeout";
  ms: ValueIR;
  flow: FlowIR;
}

// Concurrency
export interface FAll extends NodeBase { tag: "FAll"; flows: FlowIR[] }
export interface FRace extends NodeBase { tag: "FRace"; flows: FlowIR[] }
export interface FAny extends NodeBase { tag: "FAny"; flows: FlowIR[] }
export interface FSequence extends NodeBase { tag: "FSequence"; flows: FlowIR[] }

// Control flow
export interface FBranch extends NodeBase {
  tag: "FBranch";
  pred: ValueIR;
  then: FlowIR;
  else: FlowIR;
}

export interface FLoop extends NodeBase {
  tag: "FLoop";
  init: ValueIR;
  step: VRef;         // Fn: state -> Flow[state]
  until: VRef;        // Fn: state -> Bool
}

// Effects (PortEffects - must emit spans, be replay-loggable)
export interface FInfer extends NodeBase {
  tag: "FInfer";
  prompt: PromptIR;
  options?: ValueIR;  // record: model, temperature, maxTokens, etc.
}

export interface FToolCall extends NodeBase {
  tag: "FToolCall";
  tool: ValueIR;      // tool name
  args: ValueIR;      // arguments record
  contract?: VRef;    // kind = ToolContract (strongly recommended)
}

export interface FValidate extends NodeBase {
  tag: "FValidate";
  schema: VRef;       // kind = Schema
  value: ValueIR;
}

export interface FCommit extends NodeBase {
  tag: "FCommit";
  store: VRef;        // kind = Store
  key: ValueIR;
  value: ValueIR;
}

export interface FEmit extends NodeBase {
  tag: "FEmit";
  sink: VRef;         // kind = Sink
  item: ValueIR;
}

export interface FObserve extends NodeBase {
  tag: "FObserve";
  source: VRef;       // kind = Source
  query?: ValueIR;
}

export interface FSuspend extends NodeBase {
  tag: "FSuspend";
  reason: ValueIR;
}

// Union of all flow types
export type FlowIR =
  | FPure
  | FBind
  | FCatch
  | FFail
  | FWithBudget
  | FWithTimeout
  | FAll | FRace | FAny | FSequence
  | FBranch
  | FLoop
  | FInfer
  | FToolCall
  | FValidate
  | FCommit
  | FEmit
  | FObserve
  | FSuspend;
```

---

## Task 5: Bundle and Function Definitions

**Reference**: [ARCHITECTURE-LANGUAGES-5.md §56.5.1](../docs/ARCHITECTURE-LANGUAGES-5.md)

### 5.1 Create `src/frameir/bundle.ts`

```typescript
import { NodeBase, IRVersion } from "./meta";
import { ValueIR } from "./value";
import { FlowIR } from "./flow";
import { SchemaIR } from "./schema";
import { ToolContractIR } from "./contract";

// Closure-converted function definition
export interface FnDefIR extends NodeBase {
  tag: "FnDef";
  fnId: string;           // stable id (sha256)
  params: string[];       // for debug only; semantics uses positional
  body: FlowIR | ValueIR; // Flow or pure Value
  captures?: ValueIR;     // record of captured values (closure env)
}

// Complete IR bundle (the unit of compilation/caching)
export interface IRBundle {
  v: IRVersion;
  entry: FlowIR;                              // top-level plan
  fns: Record<string, FnDefIR>;               // keyed by fnId
  schemas: Record<string, SchemaIR>;          // keyed by schema id
  toolContracts: Record<string, ToolContractIR>; // keyed by contract id
  // Optional extensions
  modules?: Record<string, string[]>;         // module -> exported fnIds
  docs?: Record<string, string>;              // id -> documentation
}
```

---

## Task 6: Schema and Contract Types

**Reference**: [ARCHITECTURE-LANGUAGES-5.md §58](../docs/ARCHITECTURE-LANGUAGES-5.md)

### 6.1 Create `src/frameir/schema.ts`

```typescript
import { NodeBase } from "./meta";

// Atomic schema nodes
export interface SAny extends NodeBase { tag: "SAny" }
export interface SString extends NodeBase { tag: "SString" }
export interface SNumber extends NodeBase { tag: "SNumber" }
export interface SInt extends NodeBase { tag: "SInt" }
export interface SBool extends NodeBase { tag: "SBool" }
export interface SNil extends NodeBase { tag: "SNil" }

// Compound schema nodes
export interface SList extends NodeBase { tag: "SList"; item: FrameSchemaNode }
export interface SRecord extends NodeBase {
  tag: "SRecord";
  fields: Array<{ key: string; schema: FrameSchemaNode; optional?: boolean }>;
  closed?: boolean;   // if true, no extra fields allowed
}
export interface SUnion extends NodeBase { tag: "SUnion"; options: FrameSchemaNode[] }
export interface SRef extends NodeBase { tag: "SRef"; schemaId: string }

export type FrameSchemaNode =
  | SAny | SString | SNumber | SInt | SBool | SNil
  | SList | SRecord
  | SUnion
  | SRef;

// Top-level schema (supports both FrameSchema and JSON Schema)
export interface SchemaIR extends NodeBase {
  tag: "Schema";
  id: string;
  kind: "JsonSchema" | "FrameSchema";
  jsonSchema?: unknown;       // canonical JSON for interop
  frameSchema?: FrameSchemaNode;
}
```

### 6.2 Create `src/frameir/contract.ts`

```typescript
import { NodeBase } from "./meta";
import { VRef, ValueIR } from "./value";

export interface ToolContractIR extends NodeBase {
  tag: "ToolContract";
  id: string;                 // stable hash id
  name: string;               // tool name at runtime
  version: string;            // semver

  inputSchema: VRef;          // Schema ref
  outputSchema: VRef;
  errorSchema?: VRef;

  idempotency: "idempotent" | "non-idempotent" | "unknown";
  capabilityTag: string;      // for object-capability policy
  quotaGroup?: string;

  resourceModel?: {
    typicalTimeMs?: number;
    worstTimeMs?: number;
    typicalTokens?: number;
  };

  provenancePolicy?: {
    mustAttachEvidence?: boolean;
    evidenceMode?: Array<"observed" | "measured" | "derived">;
    stalenessInputs?: Array<"toolContract" | "schema" | "sourceFingerprint" | "oracleConfig">;
  };
}
```

---

## Task 7: Canonical Codec

**Reference**: [ARCHITECTURE-LANGUAGES-5.md §56.6](../docs/ARCHITECTURE-LANGUAGES-5.md)

### 7.1 Create `src/frameir/codec.ts`

Canonical JSON encoding rules:
- Object keys sorted lexicographically (UTF-8 byte order)
- No undefined; use omission for optional fields
- Integers as strings in base-10 without leading zeros
- Floats as strings with canonical decimal format
- Arrays preserve order
- `meta` excluded from semantic encoding

```typescript
import { NodeBase } from "./meta";

/**
 * Encode IR node to canonical JSON bytes.
 * Used for hashing and caching.
 *
 * @param node - Any IR node
 * @param options.includeMeta - If true, include meta in output (for debugging)
 * @returns Canonical JSON string
 */
export function encodeCanonical(node: NodeBase, options?: { includeMeta?: boolean }): string {
  return JSON.stringify(node, (key, value) => {
    // Exclude meta unless explicitly requested
    if (key === "meta" && !options?.includeMeta) {
      return undefined;
    }
    // Sort object keys
    if (value && typeof value === "object" && !Array.isArray(value)) {
      return Object.keys(value)
        .sort()
        .reduce((sorted, k) => {
          sorted[k] = value[k];
          return sorted;
        }, {} as Record<string, unknown>);
    }
    return value;
  });
}

/**
 * Decode canonical JSON to IR node.
 * Validates tag and version.
 */
export function decode<T extends NodeBase>(json: string): T {
  const node = JSON.parse(json);
  if (!node.v || !node.tag) {
    throw new Error("Invalid IR node: missing v or tag");
  }
  return node as T;
}
```

---

## Task 8: Hashing

**Reference**: [ARCHITECTURE-LANGUAGES-4.md §43](../docs/ARCHITECTURE-LANGUAGES-4.md)

### 8.1 Create `src/frameir/hash.ts`

```typescript
import { createHash } from "crypto";
import { NodeBase } from "./meta";
import { encodeCanonical } from "./codec";
import { FlowIR } from "./flow";

export type NodeHash = `ir:sha256:${string}`;

/**
 * Compute semantic hash for an IR node.
 * Excludes meta by default (pure semantic hash).
 */
export function hashNode(node: NodeBase, semanticSalt?: string): NodeHash {
  const canonical = encodeCanonical(node, { includeMeta: false });
  const input = semanticSalt ? `${canonical}:${semanticSalt}` : canonical;
  const hash = createHash("sha256").update(input).digest("hex").slice(0, 32);
  return `ir:sha256:${hash}`;
}

/**
 * Compute merkle hash for a flow graph.
 * Each node's hash includes its children's hashes.
 */
export function merkleize(flow: FlowIR): Map<FlowIR, NodeHash> {
  const hashes = new Map<FlowIR, NodeHash>();

  function visit(node: FlowIR): NodeHash {
    if (hashes.has(node)) {
      return hashes.get(node)!;
    }

    // Hash based on tag, replacing child flows with their hashes
    const nodeWithChildHashes = replaceChildFlowsWithHashes(node, visit);
    const hash = hashNode(nodeWithChildHashes);
    hashes.set(node, hash);
    return hash;
  }

  visit(flow);
  return hashes;
}

// Helper: replace child FlowIR nodes with their hash references
function replaceChildFlowsWithHashes(
  node: FlowIR,
  getHash: (f: FlowIR) => NodeHash
): NodeBase {
  // Implementation depends on node type
  // Each FlowIR type has specific child flow fields
  // ...
}
```

---

## Task 9: Normalization

**Reference**: [ARCHITECTURE-LANGUAGES-5.md §56.6.2](../docs/ARCHITECTURE-LANGUAGES-5.md)

### 9.1 Create `src/frameir/normalize.ts`

```typescript
import { PromptIR, PromptDoc, PromptPart } from "./prompt";
import { FlowIR, FSequence, FAll, FPure } from "./flow";
import { VList } from "./value";
import { CURRENT_IR_VERSION } from "./version";

/**
 * Normalize prompt document.
 * - Flatten nested PromptDoc
 * - Merge adjacent same-role segments (optional)
 * - Delete empty segments
 */
export function normalizePrompt(prompt: PromptIR): PromptIR {
  const flatParts: PromptPart[] = [];

  for (const part of prompt.parts) {
    // Flatten nested PromptDoc
    if (part.tag === "PromptDoc") {
      flatParts.push(...(part as PromptDoc).parts);
    } else {
      flatParts.push(part);
    }
  }

  // Filter empty segments
  const nonEmpty = flatParts.filter(p => {
    if (p.tag === "PSystem" || p.tag === "PUser" || p.tag === "PAssistant") {
      return (p as { text: string }).text.length > 0;
    }
    return true;
  });

  return {
    v: CURRENT_IR_VERSION,
    tag: "PromptDoc",
    parts: nonEmpty,
  };
}

/**
 * Normalize flow graph.
 * - Flatten nested FSequence
 * - Collapse FAll([]) -> FPure([])
 * - (Optional) Beta-reduce FBind(FPure(x), k) if safe
 */
export function normalizeFlow(flow: FlowIR): FlowIR {
  switch (flow.tag) {
    case "FSequence": {
      const flat: FlowIR[] = [];
      for (const child of (flow as FSequence).flows) {
        const normalized = normalizeFlow(child);
        if (normalized.tag === "FSequence") {
          flat.push(...(normalized as FSequence).flows);
        } else {
          flat.push(normalized);
        }
      }
      return { ...flow, flows: flat } as FSequence;
    }

    case "FAll": {
      const f = flow as FAll;
      if (f.flows.length === 0) {
        return {
          v: CURRENT_IR_VERSION,
          tag: "FPure",
          value: { v: CURRENT_IR_VERSION, tag: "VList", items: [] }
        } as FPure;
      }
      return { ...f, flows: f.flows.map(normalizeFlow) };
    }

    // Add other cases...

    default:
      return flow;
  }
}
```

---

## Task 10: Visitor Pattern

### 10.1 Create `src/frameir/visitor.ts`

```typescript
import { FlowIR } from "./flow";
import { ValueIR } from "./value";
import { PromptIR } from "./prompt";

export interface FlowVisitor<R> {
  visitFPure?(node: FlowIR): R;
  visitFBind?(node: FlowIR): R;
  visitFCatch?(node: FlowIR): R;
  visitFFail?(node: FlowIR): R;
  // ... all other flow types
  default(node: FlowIR): R;
}

export function visitFlow<R>(flow: FlowIR, visitor: FlowVisitor<R>): R {
  const method = visitor[`visit${flow.tag}` as keyof FlowVisitor<R>];
  if (typeof method === "function") {
    return (method as (n: FlowIR) => R)(flow);
  }
  return visitor.default(flow);
}

/**
 * Rewrite flow by applying transformation.
 * Recursively visits and rebuilds the tree.
 */
export function rewriteFlow(
  flow: FlowIR,
  transform: (node: FlowIR) => FlowIR | null
): FlowIR {
  // Apply transform to current node
  const transformed = transform(flow);
  if (transformed !== null) {
    return transformed;
  }

  // Recursively rewrite children based on flow type
  // ...

  return flow;
}
```

---

## Verification Steps

### 1. Type Checking
```bash
npx tsc --noEmit src/frameir/**/*.ts
```

### 2. Canonical Encoding Tests
```bash
npx vitest run test/frameir/codec.spec.ts
```

Tests must verify:
- Same node always produces identical JSON
- Key ordering is deterministic
- Meta exclusion works correctly

### 3. Hashing Determinism Tests
```bash
npx vitest run test/frameir/hash.spec.ts
```

Tests must verify:
- Same node always produces same hash
- Different nodes produce different hashes
- Semantic salt changes hash
- Alpha-equivalent flows hash identically after normalization

### 4. Round-Trip Tests
```bash
npx vitest run test/frameir/roundtrip.spec.ts
```

Tests must verify:
- `decode(encodeCanonical(node))` equals original node
- All node types round-trip correctly

---

## Checklist

- [x] Create `src/frameir/` directory structure
- [x] Implement `version.ts` with IRVersion
- [x] Implement `meta.ts` with Span, Meta, NodeBase
- [x] Implement `value.ts` with all ValueIR types
- [x] Implement `expr.ts` with all VExpr types
- [x] Implement `prompt.ts` with all PromptIR types
- [x] Implement `flow.ts` with all FlowIR types
- [x] Implement `bundle.ts` with IRBundle, FnDefIR
- [x] Implement `schema.ts` with SchemaIR types
- [x] Implement `contract.ts` with ToolContractIR
- [x] Implement `codec.ts` with canonical encoding
- [x] Implement `hash.ts` with hashNode, merkleize
- [x] Implement `normalize.ts` with normalizers
- [x] Implement `visitor.ts` with visitor pattern
- [x] Implement `index.ts` with package exports
- [x] Create test suite in `test/frameir/`
- [ ] All tests pass
- [x] Update `tsconfig.json` to include frameir

---

## Success Criteria

1. **All types compile** - No TypeScript errors
2. **Canonical encoding is deterministic** - Same input always produces same output
3. **Hashing is stable** - Same node hash across runs, machines
4. **Tests pass** - Full test coverage for codec, hash, normalize
5. **Documentation** - JSDoc on all exported functions

This package becomes the foundation for all subsequent jobs (registry, compilation, linting, replay).

---

## What Already Exists

### AST Types (Partial Foundation)

**File**: [src/core/ast.ts](../src/core/ast.ts)

```typescript
// Existing expression types (not canonical IR but shows patterns)
export type Expr =
  | { tag: "Num"; n: number }
  | { tag: "Str"; s: string }
  | { tag: "Bool"; b: boolean }
  | { tag: "Sym"; name: string }
  | { tag: "Kw"; name: string }
  | { tag: "List"; elems: Expr[] }
  // ...
```

**Gap**: These are runtime AST values, not canonical serializable IR with versioning and hashing.

### Value Types

**File**: [src/core/eval/values.ts](../src/core/eval/values.ts)

```typescript
// Runtime values - not the same as IR values
export type Val =
  | { tag: "Num"; n: number }
  | { tag: "Str"; s: string }
  | { tag: "Closure"; ... }
  // ...
```

**Gap**: Runtime values are execution artifacts, not canonical compile-time IR.

---

## Test Plan

### Codec Tests (`test/frameir/codec.spec.ts`)

#### Happy Path Tests

```typescript
// HP-1: Encode simple value produces canonical JSON
const node: VInt = { v: "frameir@1", tag: "VInt", value: "42" };
const encoded = encodeCanonical(node);
expect(encoded).toBe('{"tag":"VInt","v":"frameir@1","value":"42"}');

// HP-2: Keys are sorted lexicographically
const record: VRecord = {
  v: "frameir@1",
  tag: "VRecord",
  entries: [
    { k: { v: "frameir@1", tag: "VKeyword", name: "zebra" }, v: { v: "frameir@1", tag: "VInt", value: "1" } },
    { k: { v: "frameir@1", tag: "VKeyword", name: "apple" }, v: { v: "frameir@1", tag: "VInt", value: "2" } },
  ]
};
// Keys within each object are sorted

// HP-3: Meta excluded by default
const withMeta: VInt = {
  v: "frameir@1",
  tag: "VInt",
  value: "42",
  meta: { span: { file: "test.lisp", startLine: 1 } }
};
const encoded = encodeCanonical(withMeta);
expect(encoded).not.toContain("meta");
expect(encoded).not.toContain("span");

// HP-4: Meta included when requested
const encoded = encodeCanonical(withMeta, { includeMeta: true });
expect(encoded).toContain("meta");

// HP-5: Decode recovers original node
const node: VBool = { v: "frameir@1", tag: "VBool", value: true };
const decoded = decode<VBool>(encodeCanonical(node));
expect(decoded.value).toBe(true);
```

#### Edge Case Tests

```typescript
// EC-1: Empty string value
const node: VStr = { v: "frameir@1", tag: "VStr", value: "" };
const encoded = encodeCanonical(node);
expect(encoded).toContain('"value":""');

// EC-2: Empty list
const node: VList = { v: "frameir@1", tag: "VList", items: [] };
const encoded = encodeCanonical(node);
const decoded = decode<VList>(encoded);
expect(decoded.items).toHaveLength(0);

// EC-3: Deeply nested structure
const deep: VList = {
  v: "frameir@1",
  tag: "VList",
  items: [{
    v: "frameir@1",
    tag: "VList",
    items: [{ v: "frameir@1", tag: "VInt", value: "1" }]
  }]
};
const encoded = encodeCanonical(deep);
const decoded = decode<VList>(encoded);
expect((decoded.items[0] as VList).items).toHaveLength(1);

// EC-4: Unicode strings
const node: VStr = { v: "frameir@1", tag: "VStr", value: "héllo 世界 🚀" };
const decoded = decode<VStr>(encodeCanonical(node));
expect(decoded.value).toBe("héllo 世界 🚀");

// EC-5: Float precision
const node: VFloat = { v: "frameir@1", tag: "VFloat", value: "3.141592653589793" };
const decoded = decode<VFloat>(encodeCanonical(node));
expect(decoded.value).toBe("3.141592653589793");
```

#### Error Cases

```typescript
// ERR-1: Decode invalid JSON
expect(() => decode("not json")).toThrow();

// ERR-2: Decode missing version
expect(() => decode('{"tag":"VInt","value":"42"}')).toThrow(/missing v/);

// ERR-3: Decode missing tag
expect(() => decode('{"v":"frameir@1","value":"42"}')).toThrow(/missing.*tag/);
```

### Hash Tests (`test/frameir/hash.spec.ts`)

#### Happy Path Tests

```typescript
// HP-1: Same node produces same hash
const node: VInt = { v: "frameir@1", tag: "VInt", value: "42" };
const hash1 = hashNode(node);
const hash2 = hashNode(node);
expect(hash1).toBe(hash2);

// HP-2: Hash has correct format
const hash = hashNode(node);
expect(hash).toMatch(/^ir:sha256:[a-f0-9]{32}$/);

// HP-3: Different nodes produce different hashes
const node1: VInt = { v: "frameir@1", tag: "VInt", value: "42" };
const node2: VInt = { v: "frameir@1", tag: "VInt", value: "43" };
expect(hashNode(node1)).not.toBe(hashNode(node2));

// HP-4: Semantic salt changes hash
const node: VInt = { v: "frameir@1", tag: "VInt", value: "42" };
const hash1 = hashNode(node, "salt1");
const hash2 = hashNode(node, "salt2");
expect(hash1).not.toBe(hash2);

// HP-5: Meta does not affect hash
const node1: VInt = { v: "frameir@1", tag: "VInt", value: "42" };
const node2: VInt = { v: "frameir@1", tag: "VInt", value: "42", meta: { doc: "test" } };
expect(hashNode(node1)).toBe(hashNode(node2));
```

#### Edge Case Tests

```typescript
// EC-1: Hash stability across runs
const node: VInt = { v: "frameir@1", tag: "VInt", value: "42" };
// Hash should be deterministic - same value every time
expect(hashNode(node)).toBe("ir:sha256:...(known value)...");

// EC-2: Merkleize simple flow
const flow: FPure = { v: "frameir@1", tag: "FPure", value: { v: "frameir@1", tag: "VInt", value: "1" } };
const hashes = merkleize(flow);
expect(hashes.size).toBe(1);

// EC-3: Merkleize flow with children
const inner: FPure = { v: "frameir@1", tag: "FPure", value: { v: "frameir@1", tag: "VInt", value: "1" } };
const flow: FSequence = { v: "frameir@1", tag: "FSequence", flows: [inner, inner] };
const hashes = merkleize(flow);
expect(hashes.size).toBe(2); // FSequence + FPure (shared)
```

### Normalize Tests (`test/frameir/normalize.spec.ts`)

#### Happy Path Tests

```typescript
// HP-1: Flatten nested FSequence
const inner: FSequence = { v: "frameir@1", tag: "FSequence", flows: [pure1] };
const outer: FSequence = { v: "frameir@1", tag: "FSequence", flows: [inner, pure2] };
const normalized = normalizeFlow(outer);
expect((normalized as FSequence).flows).toHaveLength(2); // flattened

// HP-2: FAll([]) becomes FPure([])
const empty: FAll = { v: "frameir@1", tag: "FAll", flows: [] };
const normalized = normalizeFlow(empty);
expect(normalized.tag).toBe("FPure");

// HP-3: Normalize prompt flattens nested PromptDoc
const inner: PromptDoc = { v: "frameir@1", tag: "PromptDoc", parts: [system1] };
const outer: PromptDoc = { v: "frameir@1", tag: "PromptDoc", parts: [inner as any, user1] };
const normalized = normalizePrompt(outer);
expect(normalized.parts).toHaveLength(2);

// HP-4: Empty text segments removed
const prompt: PromptDoc = {
  v: "frameir@1",
  tag: "PromptDoc",
  parts: [
    { v: "frameir@1", tag: "PSystem", text: "" },
    { v: "frameir@1", tag: "PUser", text: "Hello" }
  ]
};
const normalized = normalizePrompt(prompt);
expect(normalized.parts).toHaveLength(1);
```

### Integration Tests

1. **Round-trip all node types** - Every IR type survives encode/decode
2. **Hash determinism across processes** - Child process computes same hash
3. **Large bundle handling** - 1000+ function definitions
4. **Incremental hashing** - Change one node, only affected hashes change

---

## Notes

### Why String Numbers?

From ARCHITECTURE-LANGUAGES-4.md §43.1:

> "Numbers: either forbid NaN/Infinity, encode floats as strings with canonical decimal"

Using strings avoids:
- Floating point representation differences across machines
- JSON's number precision limitations
- Non-deterministic serialization of special values

### Why Closure Conversion at IR Level?

From ARCHITECTURE-LANGUAGES-4.md §42.3:

> "Option C: Closure conversion at lower time - turn lambdas into named functions with explicit environments"

Benefits:
- Hashing is stable (function identity is stable)
- Environment is structural data, hashable
- Simpler runtime (no implicit closure capture)

### Merkleization vs Flat Hashing

Merkleization enables:
- Incremental cache invalidation
- Subflow result caching
- Localized repair (only re-run failed subtrees)

---

## Proof of Completion

When marking this job DONE:

1. **Build passes**: `npm run build` - no TypeScript errors
2. **Baseline tests pass**: `npm run test` - existing tests still green
3. **New tests pass**:
   - `npx vitest run test/frameir/codec.spec.ts`
   - `npx vitest run test/frameir/hash.spec.ts`
   - `npx vitest run test/frameir/normalize.spec.ts`
4. **Round-trip verified**: All IR node types survive encode/decode
5. **Hash determinism verified**: Same node produces same hash across runs
6. **Update status**: Change `NOT STARTED` → `DONE` in this file
7. **Update README**: Mark Job 009 as DONE in [README.md](README.md)
8. **Unblock dependents**: Jobs 010-015 can now proceed

---

*Created: 2026-01-20*
*Related: [ARCHITECTURE-LANGUAGES-4.md](../docs/ARCHITECTURE-LANGUAGES-4.md), [ARCHITECTURE-LANGUAGES-5.md](../docs/ARCHITECTURE-LANGUAGES-5.md)*
*Blocks: Jobs 010, 011, 012, 013, 014, 015*
