# JOB-010: Implement PrimitiveDescriptor Registry

**Priority**: P0 - Foundation (enables docgen, linting, budgeting, capability calculus)
**Estimated Effort**: 2-3 days
**Skills Required**: TypeScript, compiler design, registry patterns
**Status**: NOT STARTED

> **MUST READ**: [LOGISTICS.md](LOGISTICS.md) before starting - covers testing, file locations, and proof of completion requirements.

### Quick Start

```bash
cd c:\Users\Richa\parmenides-dev\agent-harness\OmegaLLM

# Verify Job 009 is complete first (dependency)
npx vitest run test/frameir/

# Run existing tests (verify baseline)
npm run test

# After implementation, run new tests
npx vitest run test/registry/

# Watch mode during development
npm run test:watch
```

### Dependencies

- **Depends On**: [Job 009](./009-FRAMEIR-PACKAGE.md) (FrameIR Package)
- **Blocks**: [Job 013](./013-LINT-PASSES.md) (Lint Passes - needs descriptor constraints)

---

## Reference Documents

This job implements specifications from:

| Document | Sections | Topics |
|----------|----------|--------|
| [ARCHITECTURE-LANGUAGES-5.md](../docs/ARCHITECTURE-LANGUAGES-5.md) | §57 | PrimitiveDescriptor shape, registry structure |
| [ARCHITECTURE-LANGUAGES-6.md](../docs/ARCHITECTURE-LANGUAGES-6.md) | §70 | Registry seed examples, golden fields |

**Key Quote from §57**:
> "You're sitting on a combinatorial explosion of 'function lists.' The way out is: **a single, queryable registry**. Think of this as the platform's **Reflection API** plus **Compiler Table**."

---

## Executive Summary

Create a **PrimitiveDescriptor Registry** that serves as the single source of truth for:

1. **Documentation** - Auto-generate REFERENCE-LIBRARIES.md
2. **Lowering rules** - How surface forms compile to FlowIR
3. **Effect typing** - What capabilities each primitive requires
4. **Budget estimation** - Cost models for resource planning
5. **Lint rules** - Constraints like "must be dominated by budget"
6. **Capability gating** - Which tools/oracles are accessible

This eliminates documentation drift and makes the architecture mechanically enforceable.

---

## Output Files

| File | Action | Description |
|------|--------|-------------|
| `src/registry/index.ts` | **CREATE** | Package exports |
| `src/registry/types.ts` | **CREATE** | PrimitiveDescriptor, Effect, TypeSig types |
| `src/registry/registry.ts` | **CREATE** | Registry class with registration and query |
| `src/registry/descriptors/framelisp.ts` | **CREATE** | FrameLisp primitives (infer, bind, etc.) |
| `src/registry/descriptors/lambdallm.ts` | **CREATE** | LambdaLLM stdlib primitives |
| `src/registry/descriptors/lambdarlm.ts` | **CREATE** | LambdaRLM solver primitives |
| `src/registry/descriptors/omegallm.ts` | **CREATE** | OmegaLLM runtime primitives |
| `src/registry/query.ts` | **CREATE** | apropos, search, filter utilities |
| `src/registry/docgen.ts` | **CREATE** | Generate markdown from registry |
| `src/registry/validate.ts` | **CREATE** | Validate descriptor completeness |
| `test/registry/registry.spec.ts` | **CREATE** | Registry tests |
| `test/registry/docgen.spec.ts` | **CREATE** | Docgen tests |

---

## Task 1: Define Core Types

**Reference**: [ARCHITECTURE-LANGUAGES-5.md §57.1](../docs/ARCHITECTURE-LANGUAGES-5.md)

### 1.1 Create `src/registry/types.ts`

```typescript
/**
 * Effect kinds for capability calculus.
 * Closed set - all effects must be one of these.
 */
export type Effect =
  | "Pure"           // No effects, referentially transparent
  | "Oracle"         // LLM inference (requires OracleCap)
  | "Tool"           // External tool call (requires ToolCap + contract)
  | "Store"          // Persistent storage (requires StoreCap)
  | "Sink"           // Emit to stream/output (requires SinkCap)
  | "Source"         // Observe from input (requires SourceCap)
  | "Clock"          // Time access (requires ClockPort for determinism)
  | "Concurrency"    // Fiber spawning/scheduling
  | "Constraint"     // Constraint propagation
  | "Nondet"         // Nondeterministic choice
  | "Control";       // Control flow effects (bind, catch, loop)

/**
 * Type signature for primitives.
 * Initially string-based; can upgrade to full type system later.
 */
export interface TypeSig {
  params: Array<{
    name: string;
    type: string;
    optional?: boolean;
  }>;
  returns: string;
}

/**
 * Cost model for budget estimation.
 */
export interface CostModel {
  /**
   * Static cost estimate (or formula string).
   */
  estimate?: {
    llmCalls?: number | string;   // e.g., 1 or "promptTokens / 1000"
    tokens?: number | string;
    timeMs?: number | string;
    toolCalls?: number | string;
  };
  /**
   * Dynamic estimator function (for runtime estimation).
   */
  estimator?: (args: unknown[], ctx: unknown) => {
    llmCalls?: number;
    tokens?: number;
    timeMs?: number;
    toolCalls?: number;
  };
}

/**
 * Lowering rule: how surface form compiles to IR.
 */
export interface LoweringRule {
  kind: "Intrinsic" | "MacroExpand" | "LowerHook";
  irTag?: string;              // For Intrinsic: which FlowIR tag
  hook?: string;               // For LowerHook: hook function id
}

/**
 * Lint constraints for static analysis.
 */
export interface LintConstraints {
  mustBeDominatedByBudget?: boolean;
  mustBeDominatedByTimeout?: boolean;
  requiresToolContract?: boolean;
  requiresSchema?: boolean;
}

/**
 * Documentation for the primitive.
 */
export interface PrimitiveDoc {
  summary: string;
  detail?: string;
  laws?: string[];             // Equational laws (crucial for refactoring)
  examples?: Array<{
    input: string;
    output: string;
    description?: string;
  }>;
}

/**
 * Deprecation info.
 */
export interface DeprecationInfo {
  since: string;
  replacedBy?: string;
  note?: string;
}

/**
 * Complete primitive descriptor.
 */
export interface PrimitiveDescriptor {
  /** Canonical namespaced ID, e.g., "framelisp/infer" */
  id: string;

  /** Which layer this primitive belongs to */
  layer: "FrameLisp" | "LambdaLLM" | "OmegaLLM" | "LambdaRLM";

  /** Kind of primitive */
  kind: "SpecialForm" | "Function" | "Macro" | "ProtocolMethod";

  /** Type signature */
  signature: TypeSig;

  /** Effect requirements (closed set) */
  effects: Effect[];

  /** Cost model for budget planning */
  resources?: CostModel;

  /** Documentation */
  doc: PrimitiveDoc;

  /** How to compile to IR */
  lowering?: LoweringRule;

  /** Runtime implementation reference */
  runtime?: {
    implementer?: string;      // e.g., "omega-kernel/prim/add"
  };

  /** Lint constraints */
  constraints?: LintConstraints;

  /** Semantic version */
  version: string;

  /** Deprecation info if deprecated */
  deprecated?: DeprecationInfo;
}
```

---

## Task 2: Implement Registry Class

### 2.1 Create `src/registry/registry.ts`

```typescript
import { PrimitiveDescriptor, Effect } from "./types";

export class PrimitiveRegistry {
  private descriptors: Map<string, PrimitiveDescriptor> = new Map();
  private byLayer: Map<string, Set<string>> = new Map();
  private byEffect: Map<Effect, Set<string>> = new Map();

  /**
   * Register a primitive descriptor.
   * @throws if id already registered
   */
  register(descriptor: PrimitiveDescriptor): void {
    if (this.descriptors.has(descriptor.id)) {
      throw new Error(`Primitive already registered: ${descriptor.id}`);
    }

    this.descriptors.set(descriptor.id, descriptor);

    // Index by layer
    if (!this.byLayer.has(descriptor.layer)) {
      this.byLayer.set(descriptor.layer, new Set());
    }
    this.byLayer.get(descriptor.layer)!.add(descriptor.id);

    // Index by effects
    for (const effect of descriptor.effects) {
      if (!this.byEffect.has(effect)) {
        this.byEffect.set(effect, new Set());
      }
      this.byEffect.get(effect)!.add(descriptor.id);
    }
  }

  /**
   * Get descriptor by ID.
   */
  get(id: string): PrimitiveDescriptor | undefined {
    return this.descriptors.get(id);
  }

  /**
   * Get all descriptors.
   */
  getAll(): PrimitiveDescriptor[] {
    return Array.from(this.descriptors.values());
  }

  /**
   * Get all descriptors for a layer.
   */
  getByLayer(layer: string): PrimitiveDescriptor[] {
    const ids = this.byLayer.get(layer);
    if (!ids) return [];
    return Array.from(ids).map(id => this.descriptors.get(id)!);
  }

  /**
   * Get all descriptors with a specific effect.
   */
  getByEffect(effect: Effect): PrimitiveDescriptor[] {
    const ids = this.byEffect.get(effect);
    if (!ids) return [];
    return Array.from(ids).map(id => this.descriptors.get(id)!);
  }

  /**
   * Search descriptors by text (apropos).
   */
  search(query: string): PrimitiveDescriptor[] {
    const q = query.toLowerCase();
    return this.getAll().filter(d =>
      d.id.toLowerCase().includes(q) ||
      d.doc.summary.toLowerCase().includes(q) ||
      (d.doc.detail?.toLowerCase().includes(q) ?? false)
    );
  }

  /**
   * Get lowering rule for an IR tag.
   */
  getByIrTag(irTag: string): PrimitiveDescriptor | undefined {
    return this.getAll().find(d => d.lowering?.irTag === irTag);
  }

  /**
   * Validate registry completeness.
   */
  validate(): { valid: boolean; errors: string[] } {
    const errors: string[] = [];

    for (const [id, desc] of this.descriptors) {
      // Check required fields
      if (!desc.signature) {
        errors.push(`${id}: missing signature`);
      }
      if (!desc.doc?.summary) {
        errors.push(`${id}: missing doc.summary`);
      }
      if (!desc.version) {
        errors.push(`${id}: missing version`);
      }

      // Check effect consistency
      if (desc.effects.includes("Oracle") && !desc.constraints?.mustBeDominatedByBudget) {
        errors.push(`${id}: Oracle effect should require budget dominance`);
      }
      if (desc.effects.includes("Tool") && !desc.constraints?.requiresToolContract) {
        errors.push(`${id}: Tool effect should require tool contract`);
      }
    }

    return { valid: errors.length === 0, errors };
  }

  /**
   * Export registry as JSON for tooling.
   */
  toJSON(): Record<string, PrimitiveDescriptor> {
    return Object.fromEntries(this.descriptors);
  }

  /**
   * Load registry from JSON.
   */
  static fromJSON(data: Record<string, PrimitiveDescriptor>): PrimitiveRegistry {
    const registry = new PrimitiveRegistry();
    for (const desc of Object.values(data)) {
      registry.register(desc);
    }
    return registry;
  }
}

/**
 * Global default registry instance.
 */
export const defaultRegistry = new PrimitiveRegistry();
```

---

## Task 3: Populate FrameLisp Descriptors

**Reference**: [ARCHITECTURE-LANGUAGES-6.md §70.2-70.4](../docs/ARCHITECTURE-LANGUAGES-6.md)

### 3.1 Create `src/registry/descriptors/framelisp.ts`

```typescript
import { PrimitiveDescriptor } from "../types";

export const frameLispDescriptors: PrimitiveDescriptor[] = [
  // === Core Effects ===
  {
    id: "framelisp/infer",
    layer: "FrameLisp",
    kind: "SpecialForm",
    signature: {
      params: [
        { name: "prompt", type: "Prompt" },
        { name: "options", type: "Record", optional: true }
      ],
      returns: "Str"
    },
    effects: ["Oracle"],
    resources: {
      estimate: { llmCalls: 1, tokens: "promptTokens + maxTokens" }
    },
    doc: {
      summary: "Core LLM inference primitive.",
      detail: "Sends prompt to oracle and returns response text. Subject to budget constraints.",
      laws: [
        "infer(prompt) is referentially opaque (Oracle effect).",
        "infer must be dominated by with-budget in linted bundles."
      ],
      examples: [
        { input: '(infer "What is 2+2?")', output: '"4"', description: "Simple inference" }
      ]
    },
    lowering: { kind: "Intrinsic", irTag: "FInfer" },
    constraints: { mustBeDominatedByBudget: true },
    version: "1.0.0"
  },

  {
    id: "framelisp/call-tool",
    layer: "FrameLisp",
    kind: "SpecialForm",
    signature: {
      params: [
        { name: "name", type: "Str" },
        { name: "args", type: "Record" },
        { name: "contract", type: "ToolContract", optional: true }
      ],
      returns: "Any"
    },
    effects: ["Tool"],
    resources: {
      estimate: { toolCalls: 1 }
    },
    doc: {
      summary: "Call an external tool with validated arguments.",
      laws: [
        "call-tool must have associated ToolContract for audited execution.",
        "Arguments validated against contract.inputSchema before call."
      ]
    },
    lowering: { kind: "Intrinsic", irTag: "FToolCall" },
    constraints: { requiresToolContract: true },
    version: "1.0.0"
  },

  // === Monad Operations ===
  {
    id: "framelisp/bind",
    layer: "FrameLisp",
    kind: "Function",
    signature: {
      params: [
        { name: "m", type: "Flow[A]" },
        { name: "k", type: "Fn[A -> Flow[B]]" }
      ],
      returns: "Flow[B]"
    },
    effects: ["Control"],
    doc: {
      summary: "Monadic bind for Flow.",
      laws: [
        "Left identity: bind(pure(x), k) ≡ k(x).",
        "Right identity: bind(m, pure) ≡ m.",
        "Associativity: bind(bind(m, f), g) ≡ bind(m, (λx. bind(f(x), g)))."
      ]
    },
    lowering: { kind: "Intrinsic", irTag: "FBind" },
    version: "1.0.0"
  },

  {
    id: "framelisp/pure",
    layer: "FrameLisp",
    kind: "Function",
    signature: {
      params: [{ name: "x", type: "A" }],
      returns: "Flow[A]"
    },
    effects: ["Pure"],
    doc: {
      summary: "Lift a value into Flow.",
      laws: ["pure(x) has no effects."]
    },
    lowering: { kind: "Intrinsic", irTag: "FPure" },
    version: "1.0.0"
  },

  {
    id: "framelisp/fail",
    layer: "FrameLisp",
    kind: "Function",
    signature: {
      params: [
        { name: "reason", type: "Keyword" },
        { name: "ctx", type: "Record", optional: true }
      ],
      returns: "Flow[Never]"
    },
    effects: ["Control"],
    doc: {
      summary: "Signal a failure.",
      detail: "Unwinds until a catch handler is found."
    },
    lowering: { kind: "Intrinsic", irTag: "FFail" },
    version: "1.0.0"
  },

  {
    id: "framelisp/catch",
    layer: "FrameLisp",
    kind: "Function",
    signature: {
      params: [
        { name: "flow", type: "Flow[A]" },
        { name: "handler", type: "Fn[Failure -> Flow[A]]" }
      ],
      returns: "Flow[A]"
    },
    effects: ["Control"],
    doc: {
      summary: "Handle failures from a flow.",
      laws: ["catch(pure(x), h) ≡ pure(x).", "catch(fail(r), h) ≡ h(Failure(r))."]
    },
    lowering: { kind: "Intrinsic", irTag: "FCatch" },
    version: "1.0.0"
  },

  // === Resource Control ===
  {
    id: "framelisp/with-budget",
    layer: "FrameLisp",
    kind: "SpecialForm",
    signature: {
      params: [
        { name: "budget", type: "Budget" },
        { name: "flow", type: "Flow[A]" }
      ],
      returns: "Flow[A]"
    },
    effects: ["Control"],
    doc: {
      summary: "Run flow with budget constraint.",
      detail: "Fails with :budget-exceeded if resources exhausted."
    },
    lowering: { kind: "Intrinsic", irTag: "FWithBudget" },
    version: "1.0.0"
  },

  {
    id: "framelisp/with-timeout",
    layer: "FrameLisp",
    kind: "SpecialForm",
    signature: {
      params: [
        { name: "ms", type: "Int" },
        { name: "flow", type: "Flow[A]" }
      ],
      returns: "Flow[A]"
    },
    effects: ["Control", "Clock"],
    doc: {
      summary: "Run flow with timeout.",
      detail: "Fails with :timeout if time exceeded."
    },
    lowering: { kind: "Intrinsic", irTag: "FWithTimeout" },
    version: "1.0.0"
  },

  // === Concurrency ===
  {
    id: "framelisp/all",
    layer: "FrameLisp",
    kind: "Function",
    signature: {
      params: [{ name: "flows", type: "List[Flow[A]]" }],
      returns: "Flow[List[A]]"
    },
    effects: ["Concurrency"],
    doc: {
      summary: "Run all flows, collect all results.",
      detail: "Fails if any flow fails (fail-fast by default)."
    },
    lowering: { kind: "Intrinsic", irTag: "FAll" },
    version: "1.0.0"
  },

  {
    id: "framelisp/race",
    layer: "FrameLisp",
    kind: "Function",
    signature: {
      params: [{ name: "flows", type: "List[Flow[A]]" }],
      returns: "Flow[A]"
    },
    effects: ["Concurrency"],
    doc: {
      summary: "Run flows, return first success.",
      detail: "Cancels remaining flows after first completion."
    },
    lowering: { kind: "Intrinsic", irTag: "FRace" },
    version: "1.0.0"
  },

  {
    id: "framelisp/any",
    layer: "FrameLisp",
    kind: "Function",
    signature: {
      params: [{ name: "flows", type: "List[Flow[A]]" }],
      returns: "Flow[A]"
    },
    effects: ["Concurrency"],
    doc: {
      summary: "Run flows, return first non-failure.",
      detail: "Tries flows in order, returns first success."
    },
    lowering: { kind: "Intrinsic", irTag: "FAny" },
    version: "1.0.0"
  },

  // === Storage & IO ===
  {
    id: "framelisp/commit",
    layer: "FrameLisp",
    kind: "Function",
    signature: {
      params: [
        { name: "store", type: "StoreRef" },
        { name: "key", type: "Str" },
        { name: "value", type: "Any" }
      ],
      returns: "Flow[Unit]"
    },
    effects: ["Store"],
    doc: {
      summary: "Write value to persistent store."
    },
    lowering: { kind: "Intrinsic", irTag: "FCommit" },
    version: "1.0.0"
  },

  {
    id: "framelisp/emit",
    layer: "FrameLisp",
    kind: "Function",
    signature: {
      params: [
        { name: "sink", type: "SinkRef" },
        { name: "item", type: "Any" }
      ],
      returns: "Flow[Unit]"
    },
    effects: ["Sink"],
    doc: {
      summary: "Emit item to output sink."
    },
    lowering: { kind: "Intrinsic", irTag: "FEmit" },
    version: "1.0.0"
  },

  {
    id: "framelisp/observe",
    layer: "FrameLisp",
    kind: "Function",
    signature: {
      params: [
        { name: "source", type: "SourceRef" },
        { name: "query", type: "Any", optional: true }
      ],
      returns: "Flow[Any]"
    },
    effects: ["Source"],
    doc: {
      summary: "Observe from input source."
    },
    lowering: { kind: "Intrinsic", irTag: "FObserve" },
    version: "1.0.0"
  },

  // === Validation ===
  {
    id: "framelisp/validate",
    layer: "FrameLisp",
    kind: "Function",
    signature: {
      params: [
        { name: "schema", type: "SchemaRef" },
        { name: "value", type: "Any" }
      ],
      returns: "Flow[Any]"
    },
    effects: ["Pure"],
    doc: {
      summary: "Validate value against schema.",
      detail: "Fails with :validation-failed if invalid."
    },
    lowering: { kind: "Intrinsic", irTag: "FValidate" },
    version: "1.0.0"
  }
];
```

---

## Task 4: Implement Docgen

### 4.1 Create `src/registry/docgen.ts`

```typescript
import { PrimitiveRegistry } from "./registry";
import { PrimitiveDescriptor } from "./types";

/**
 * Generate markdown documentation from registry.
 */
export function generateMarkdown(registry: PrimitiveRegistry): string {
  const lines: string[] = [];

  lines.push("# Generated Primitive Reference\n");
  lines.push(`Generated: ${new Date().toISOString()}\n`);
  lines.push(`Total primitives: ${registry.getAll().length}\n`);
  lines.push("---\n");

  const layers = ["FrameLisp", "LambdaLLM", "OmegaLLM", "LambdaRLM"];

  for (const layer of layers) {
    const descriptors = registry.getByLayer(layer);
    if (descriptors.length === 0) continue;

    lines.push(`## ${layer}\n`);

    // Group by kind
    const byKind = new Map<string, PrimitiveDescriptor[]>();
    for (const d of descriptors) {
      if (!byKind.has(d.kind)) byKind.set(d.kind, []);
      byKind.get(d.kind)!.push(d);
    }

    for (const [kind, descs] of byKind) {
      lines.push(`### ${kind}s\n`);
      lines.push("| Function | Signature | Effects | Description |");
      lines.push("|----------|-----------|---------|-------------|");

      for (const d of descs.sort((a, b) => a.id.localeCompare(b.id))) {
        const name = d.id.split("/")[1];
        const sig = formatSignature(d.signature);
        const effects = d.effects.join(", ") || "Pure";
        const desc = d.doc.summary;
        lines.push(`| \`${name}\` | \`${sig}\` | ${effects} | ${desc} |`);
      }

      lines.push("");
    }
  }

  return lines.join("\n");
}

function formatSignature(sig: { params: Array<{ name: string; type: string; optional?: boolean }>; returns: string }): string {
  const params = sig.params
    .map(p => p.optional ? `${p.name}?` : p.name)
    .join(", ");
  return `(${params}) -> ${sig.returns}`;
}

/**
 * Generate JSON API reference.
 */
export function generateJSON(registry: PrimitiveRegistry): string {
  return JSON.stringify(registry.toJSON(), null, 2);
}
```

---

## Task 5: Seed the Default Registry

### 5.1 Create `src/registry/index.ts`

```typescript
export * from "./types";
export * from "./registry";
export * from "./query";
export * from "./docgen";
export * from "./validate";

import { defaultRegistry } from "./registry";
import { frameLispDescriptors } from "./descriptors/framelisp";
// import { lambdaLLMDescriptors } from "./descriptors/lambdallm";
// import { lambdaRLMDescriptors } from "./descriptors/lambdarlm";

// Seed default registry
for (const desc of frameLispDescriptors) {
  defaultRegistry.register(desc);
}
// Add other layers as implemented

export { defaultRegistry };
```

---

## Verification Steps

### 1. Type Checking
```bash
npx tsc --noEmit src/registry/**/*.ts
```

### 2. Registry Tests
```bash
npx vitest run test/registry/
```

Tests must verify:
- Registration works
- Duplicate detection works
- Query by layer works
- Query by effect works
- Search (apropos) works
- Validation catches missing fields

### 3. Docgen Output
```bash
npx ts-node -e "
import { defaultRegistry } from './src/registry';
import { generateMarkdown } from './src/registry/docgen';
console.log(generateMarkdown(defaultRegistry));
"
```

---

## Checklist

- [ ] Create `src/registry/` directory structure
- [ ] Implement `types.ts` with all descriptor types
- [ ] Implement `registry.ts` with Registry class
- [ ] Implement `docgen.ts` for markdown generation
- [ ] Implement `validate.ts` for descriptor validation
- [ ] Create `descriptors/framelisp.ts` with core primitives
- [ ] Create `descriptors/lambdallm.ts` with stdlib primitives
- [ ] Create `descriptors/lambdarlm.ts` with solver primitives
- [ ] Seed default registry in `index.ts`
- [ ] Create test suite
- [ ] All tests pass
- [ ] Generated docs match REFERENCE-LIBRARIES.md structure

---

## Success Criteria

1. **All FrameLisp kernel ops registered** - infer, bind, pure, fail, catch, etc.
2. **Validation catches missing fields** - No incomplete descriptors
3. **Docgen produces valid markdown** - Can replace hand-written docs
4. **Effect indexing works** - Can query "all Oracle primitives"
5. **Lowering rules complete** - Every IR tag has a descriptor

---

## What Already Exists

### REFERENCE-LIBRARIES.md (Manual Documentation)

**File**: [docs/REFERENCE-LIBRARIES.md](../docs/REFERENCE-LIBRARIES.md)

```markdown
# Hand-written docs that will be replaced by generated output
## FrameLisp Kernel
- infer, bind, pure, fail, catch, ...
## LambdaLLM Stdlib
- complete, tool-call, prompt+, ...
```

**Gap**: Documentation is manual, can drift from implementation.

### prims.ts (Primitive Implementations)

**File**: [src/core/prims.ts](../src/core/prims.ts)

```typescript
// Primitives exist but without formal descriptors
def("infer", { arity: 1, fn: ... });
def("bind", { arity: 2, fn: ... });
```

**Gap**: No effect declarations, no cost models, no machine-readable docs.

---

## Test Plan

### Registry Tests (`test/registry/registry.spec.ts`)

#### Happy Path Tests

```typescript
// HP-1: Register and retrieve descriptor
const registry = new PrimitiveRegistry();
registry.register(inferDescriptor);
const retrieved = registry.get("framelisp/infer");
expect(retrieved?.id).toBe("framelisp/infer");

// HP-2: Query by layer
registry.register(inferDescriptor);
registry.register(bindDescriptor);
const frameLisp = registry.getByLayer("FrameLisp");
expect(frameLisp.length).toBe(2);

// HP-3: Query by effect
const oracleOps = registry.getByEffect("Oracle");
expect(oracleOps).toContainEqual(expect.objectContaining({ id: "framelisp/infer" }));

// HP-4: Search (apropos)
const results = registry.search("monad");
expect(results).toContainEqual(expect.objectContaining({ id: "framelisp/bind" }));

// HP-5: Get by IR tag
const desc = registry.getByIrTag("FInfer");
expect(desc?.id).toBe("framelisp/infer");
```

#### Edge Case Tests

```typescript
// EC-1: Duplicate registration throws
registry.register(inferDescriptor);
expect(() => registry.register(inferDescriptor)).toThrow(/already registered/);

// EC-2: Get non-existent returns undefined
expect(registry.get("nonexistent/foo")).toBeUndefined();

// EC-3: Query empty layer returns empty array
expect(registry.getByLayer("NonExistentLayer")).toEqual([]);

// EC-4: Search with no matches returns empty array
expect(registry.search("xyznonexistent123")).toEqual([]);

// EC-5: Case-insensitive search
registry.register({ ...inferDescriptor, doc: { summary: "LLM INFERENCE" } });
expect(registry.search("llm")).toHaveLength(1);
```

#### Error Cases

```typescript
// ERR-1: Validation catches missing signature
const invalid = { ...inferDescriptor, signature: undefined };
const { valid, errors } = registry.validate();
expect(valid).toBe(false);
expect(errors).toContain(expect.stringMatching(/missing signature/));

// ERR-2: Validation catches missing doc summary
const invalid = { ...inferDescriptor, doc: {} };
// ...

// ERR-3: Validation catches Oracle without budget constraint
const invalid = { ...inferDescriptor, constraints: {} };
// Should warn about Oracle needing budget dominance
```

### Docgen Tests (`test/registry/docgen.spec.ts`)

#### Happy Path Tests

```typescript
// HP-1: Generate markdown has correct structure
const md = generateMarkdown(registry);
expect(md).toContain("# Generated Primitive Reference");
expect(md).toContain("## FrameLisp");
expect(md).toContain("| `infer` |");

// HP-2: Signature formatted correctly
expect(md).toContain("(prompt, options?) -> Str");

// HP-3: Effects column populated
expect(md).toContain("| Oracle |");

// HP-4: Generated JSON is valid
const json = generateJSON(registry);
expect(() => JSON.parse(json)).not.toThrow();
```

### Integration Tests

1. **Full registry seeding** - All primitives from all layers registered
2. **Generated docs match structure** - Output similar to REFERENCE-LIBRARIES.md
3. **Lowering lookup chain** - IR tag -> descriptor -> lowering rule
4. **Effect aggregation** - Compute total effects for a flow graph

---

## Notes

### Why a Registry Instead of Inline Metadata?

From ARCHITECTURE-LANGUAGES-5.md §57:

> "You're sitting on a combinatorial explosion of 'function lists.' The way out is: **a single, queryable registry**."

Benefits:
- **Single source of truth** - No documentation drift
- **Machine-readable** - Tools can query and validate
- **Extensible** - Plugins can register their own primitives
- **Cross-cutting concerns** - Effects, costs, constraints in one place

### Effect Composition Rules

When composing flows, effects combine via union:
- `bind(m:E1, f:E2)` yields `E1 ∪ E2`
- `all([f1:E1, f2:E2])` yields `E1 ∪ E2 ∪ {Concurrency}`

The registry enables static effect inference.

### Future: Automated Conformance Tests

Once registry is complete, we can generate:
- Tests that verify each primitive is implemented
- Tests that verify effect declarations are accurate
- Tests that verify cost models are reasonable

---

## Proof of Completion

When marking this job DONE:

1. **Prerequisite check**: `npx vitest run test/frameir/` - Job 009 tests pass
2. **Build passes**: `npm run build` - no TypeScript errors
3. **Baseline tests pass**: `npm run test` - existing tests still green
4. **New tests pass**:
   - `npx vitest run test/registry/registry.spec.ts`
   - `npx vitest run test/registry/docgen.spec.ts`
5. **Docgen works**: Can generate markdown from registry
6. **Validation catches errors**: Missing fields detected
7. **Update status**: Change `NOT STARTED` → `DONE` in this file
8. **Update README**: Mark Job 010 as DONE in [README.md](README.md)
9. **Unblock Job 013**: Lint passes can now use descriptor constraints

---

*Created: 2026-01-20*
*Related: [ARCHITECTURE-LANGUAGES-5.md](../docs/ARCHITECTURE-LANGUAGES-5.md), [ARCHITECTURE-LANGUAGES-6.md](../docs/ARCHITECTURE-LANGUAGES-6.md)*
*Depends on: Job 009 (FrameIR Package)*
*Blocks: Job 013 (Lint Passes)*
