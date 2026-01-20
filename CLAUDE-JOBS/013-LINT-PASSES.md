# JOB-013: Implement IR Lint Passes

**Priority**: P1 - Quality Infrastructure
**Estimated Effort**: 2-3 days
**Skills Required**: TypeScript, static analysis, compiler design
**Status**: NOT STARTED
**Depends On**: [009-FRAMEIR-PACKAGE](./009-FRAMEIR-PACKAGE.md), [010-PRIMITIVE-REGISTRY](./010-PRIMITIVE-REGISTRY.md)

> **MUST READ**: [LOGISTICS.md](LOGISTICS.md) before starting - covers testing, file locations, and proof of completion requirements.

---

## Quick Start

```bash
# 1. Read the architecture spec
cat docs/ARCHITECTURE-LANGUAGES-4.md | grep -A 100 "§49"
cat docs/ARCHITECTURE-LANGUAGES-6.md | grep -A 50 "§64"

# 2. Create lint files
mkdir -p src/lint/passes src/lint/analysis
touch src/lint/{types,runner,index}.ts
touch src/lint/analysis/{dominators,effects}.ts
touch src/lint/passes/{budgetDominator,timeoutGuard,toolContract,schemaAttachment,boundedForcing}.ts

# 3. Run tests as you implement
npx vitest run test/lint/

# 4. Verify type safety
npx tsc --noEmit
```

---

## Dependencies

| Type | Job | Reason |
|------|-----|--------|
| **REQUIRES** | [009-FRAMEIR-PACKAGE](./009-FRAMEIR-PACKAGE.md) | Needs FlowIR types for analysis |
| **REQUIRES** | [010-PRIMITIVE-REGISTRY](./010-PRIMITIVE-REGISTRY.md) | Needs effect descriptors |
| **REQUIRES** | [012-OUTCOME-FAILURE-DIAGNOSTIC](./012-OUTCOME-FAILURE-DIAGNOSTIC.md) | Emits Diagnostic types |
| **BLOCKS** | [014-COMPILATION-PIPELINE](./014-COMPILATION-PIPELINE.md) | Pipeline runs lint passes |

---

## What Already Exists

### No Existing Lint Infrastructure
Currently there is no lint pass system in OmegaLLM. All validation happens at runtime.

### Runtime Checks That Should Be Lint
```typescript
// src/core/governance/enforcement.ts - runtime checks
function checkBudget(ctx: ExecContext): void {
  if (ctx.budget.used >= ctx.budget.limit) {
    throw new Error("Budget exceeded");  // Should be lint error instead
  }
}
```

### Gap Analysis
| Should Exist | Currently |
|--------------|-----------|
| Budget dominator lint | Runtime budget check |
| Timeout guard lint | No timeout enforcement |
| Tool contract lint | Tool calls unchecked |
| Schema attachment lint | No schema validation |
| Effect analysis | No static effect tracking |
| Dominator tree | Not computed |

---

## Reference Documents

This job implements specifications from:

| Document | Sections | Topics |
|----------|----------|--------|
| [ARCHITECTURE-LANGUAGES-4.md](../docs/ARCHITECTURE-LANGUAGES-4.md) | §49 | Lint rule-set (architecture + semantic lints) |
| [ARCHITECTURE-LANGUAGES-6.md](../docs/ARCHITECTURE-LANGUAGES-6.md) | §64, §73 | Lints as IR passes, capability calculus |

**Key Quote from §49**:
> "You don't just want code style linting; you want **semantic linting** of flows."

---

## Executive Summary

Implement **IR lint passes** that enforce architectural invariants on FlowIR:

1. **Budget Dominator Lint** - Every Oracle/Tool effect must be dominated by `FWithBudget`
2. **Timeout Guard Lint** - Every Oracle call must have a timeout
3. **Tool Contract Lint** - Every `FToolCall` must have a `contract` reference
4. **Schema Attachment Lint** - Structured output must have attached schema
5. **Bounded Forcing Lint** - Stream materialization must have explicit bounds
6. **Architecture Lint** - Package boundary violations

These are compiler-grade correctness checks that prevent runtime failures.

---

## Output Files

| File | Action | Description |
|------|--------|-------------|
| `src/lint/index.ts` | **CREATE** | Package exports |
| `src/lint/types.ts` | **CREATE** | Pass interface, PassResult |
| `src/lint/runner.ts` | **CREATE** | PassRunner pipeline |
| `src/lint/passes/budgetDominator.ts` | **CREATE** | Budget dominance check |
| `src/lint/passes/timeoutGuard.ts` | **CREATE** | Timeout requirement check |
| `src/lint/passes/toolContract.ts` | **CREATE** | Tool contract check |
| `src/lint/passes/schemaAttachment.ts` | **CREATE** | Schema attachment check |
| `src/lint/passes/boundedForcing.ts` | **CREATE** | Bounded materialization check |
| `src/lint/analysis/dominators.ts` | **CREATE** | Dominator tree computation |
| `src/lint/analysis/effects.ts` | **CREATE** | Effect analysis |
| `test/lint/passes.spec.ts` | **CREATE** | Lint pass tests |

---

## Task 1: Define Pass Interface

**Reference**: [ARCHITECTURE-LANGUAGES-6.md §64](../docs/ARCHITECTURE-LANGUAGES-6.md)

### 1.1 Create `src/lint/types.ts`

```typescript
import { IRBundle, FlowIR } from "../frameir";
import { Diagnostic } from "../outcome/diagnostic";
import { PrimitiveRegistry } from "../registry";

/**
 * Result of running a pass.
 */
export interface PassResult {
  /** Diagnostics produced by this pass */
  diagnostics: Diagnostic[];

  /** Optionally transformed bundle (for rewriting passes) */
  transformed?: IRBundle;

  /** Pass-specific metadata */
  metadata?: Record<string, unknown>;
}

/**
 * Pass phase determines ordering.
 */
export type PassPhase =
  | "parsing"      // After parsing, before lowering
  | "lowering"     // During lowering to IR
  | "normalize"    // Normalization rewrites
  | "lint"         // Lint checks
  | "optimize";    // Optimizations

/**
 * A compiler/lint pass over IR.
 */
export interface Pass {
  /** Unique pass ID */
  id: string;

  /** Human-readable name */
  name: string;

  /** Phase determines ordering */
  phase: PassPhase;

  /** Pass dependencies (other pass IDs) */
  dependencies?: string[];

  /**
   * Run the pass on an IR bundle.
   * @param bundle - The IR bundle to analyze/transform
   * @param registry - Primitive registry for descriptor lookups
   * @returns Pass result with diagnostics and optional transformation
   */
  run(bundle: IRBundle, registry: PrimitiveRegistry): PassResult;
}

/**
 * Pass configuration.
 */
export interface PassConfig {
  /** Whether pass is enabled */
  enabled: boolean;

  /** Pass-specific options */
  options?: Record<string, unknown>;

  /** Severity override for this pass's diagnostics */
  severityOverride?: "error" | "warning" | "info" | "off";
}

/**
 * Lint configuration for all passes.
 */
export interface LintConfig {
  passes: Record<string, PassConfig>;
}
```

---

## Task 2: Implement Pass Runner

### 2.1 Create `src/lint/runner.ts`

```typescript
import { Pass, PassResult, PassPhase, LintConfig, PassConfig } from "./types";
import { IRBundle } from "../frameir";
import { Diagnostic } from "../outcome/diagnostic";
import { PrimitiveRegistry, defaultRegistry } from "../registry";

/**
 * Run passes in order.
 */
export class PassRunner {
  private passes: Map<string, Pass> = new Map();
  private config: LintConfig;
  private registry: PrimitiveRegistry;

  constructor(
    config: LintConfig = { passes: {} },
    registry: PrimitiveRegistry = defaultRegistry
  ) {
    this.config = config;
    this.registry = registry;
  }

  /**
   * Register a pass.
   */
  register(pass: Pass): void {
    this.passes.set(pass.id, pass);
  }

  /**
   * Run all enabled passes on a bundle.
   */
  run(bundle: IRBundle): {
    bundle: IRBundle;
    diagnostics: Diagnostic[];
    passResults: Map<string, PassResult>;
  } {
    let currentBundle = bundle;
    const allDiagnostics: Diagnostic[] = [];
    const passResults = new Map<string, PassResult>();

    // Sort passes by phase
    const sortedPasses = this.sortPasses();

    for (const pass of sortedPasses) {
      const config = this.config.passes[pass.id] ?? { enabled: true };

      if (!config.enabled || config.severityOverride === "off") {
        continue;
      }

      const result = pass.run(currentBundle, this.registry);
      passResults.set(pass.id, result);

      // Apply severity override
      const diagnostics = config.severityOverride
        ? result.diagnostics.map(d => ({ ...d, severity: config.severityOverride! }))
        : result.diagnostics;

      allDiagnostics.push(...diagnostics);

      // Apply transformation if present
      if (result.transformed) {
        currentBundle = result.transformed;
      }
    }

    return {
      bundle: currentBundle,
      diagnostics: allDiagnostics,
      passResults
    };
  }

  /**
   * Check if any error diagnostics were produced.
   */
  hasErrors(diagnostics: Diagnostic[]): boolean {
    return diagnostics.some(d => d.severity === "error");
  }

  /**
   * Sort passes by phase and dependencies.
   */
  private sortPasses(): Pass[] {
    const phaseOrder: PassPhase[] = ["parsing", "lowering", "normalize", "lint", "optimize"];

    return Array.from(this.passes.values()).sort((a, b) => {
      const phaseA = phaseOrder.indexOf(a.phase);
      const phaseB = phaseOrder.indexOf(b.phase);
      if (phaseA !== phaseB) return phaseA - phaseB;
      return a.id.localeCompare(b.id);
    });
  }
}

/**
 * Default pass runner with standard passes.
 */
export function createDefaultRunner(config?: LintConfig): PassRunner {
  const runner = new PassRunner(config);

  // Register standard passes
  // runner.register(budgetDominatorPass);
  // runner.register(timeoutGuardPass);
  // runner.register(toolContractPass);
  // etc.

  return runner;
}
```

---

## Task 3: Implement Dominator Analysis

**Reference**: Compiler textbooks (Dragon Book, etc.)

### 3.1 Create `src/lint/analysis/dominators.ts`

```typescript
import { FlowIR, FBind, FSequence, FBranch, FLoop, FAll, FRace, FAny, FCatch, FWithBudget, FWithTimeout } from "../../frameir/flow";

/**
 * Node in the flow graph.
 */
export interface FlowNode {
  id: string;
  flow: FlowIR;
  children: string[];
  parents: string[];
}

/**
 * Build a flow graph from FlowIR.
 */
export function buildFlowGraph(flow: FlowIR): Map<string, FlowNode> {
  const nodes = new Map<string, FlowNode>();
  let nodeId = 0;

  function addNode(f: FlowIR, parentId?: string): string {
    const id = `n${nodeId++}`;
    const node: FlowNode = {
      id,
      flow: f,
      children: [],
      parents: parentId ? [parentId] : []
    };
    nodes.set(id, node);

    if (parentId) {
      nodes.get(parentId)!.children.push(id);
    }

    // Recursively add children based on flow type
    switch (f.tag) {
      case "FBind": {
        const bind = f as FBind;
        addNode(bind.flow, id);
        break;
      }
      case "FSequence": {
        const seq = f as FSequence;
        let prevId = id;
        for (const child of seq.flows) {
          prevId = addNode(child, prevId);
        }
        break;
      }
      case "FBranch": {
        const branch = f as FBranch;
        addNode(branch.then, id);
        addNode(branch.else, id);
        break;
      }
      case "FAll":
      case "FRace":
      case "FAny": {
        const parallel = f as FAll | FRace | FAny;
        for (const child of parallel.flows) {
          addNode(child, id);
        }
        break;
      }
      case "FCatch": {
        const cat = f as FCatch;
        addNode(cat.flow, id);
        break;
      }
      case "FWithBudget": {
        const wb = f as FWithBudget;
        addNode(wb.flow, id);
        break;
      }
      case "FWithTimeout": {
        const wt = f as FWithTimeout;
        addNode(wt.flow, id);
        break;
      }
      case "FLoop": {
        // Loop body is reachable from loop node
        // but we don't recurse into Fn refs here
        break;
      }
    }

    return id;
  }

  addNode(flow);
  return nodes;
}

/**
 * Compute dominators for a flow graph.
 * A node D dominates node N if every path from entry to N goes through D.
 */
export function computeDominators(nodes: Map<string, FlowNode>, entryId: string): Map<string, Set<string>> {
  const dominators = new Map<string, Set<string>>();

  // Initialize: entry dominates itself, all others dominated by all
  const allIds = new Set(nodes.keys());
  for (const id of nodes.keys()) {
    if (id === entryId) {
      dominators.set(id, new Set([id]));
    } else {
      dominators.set(id, new Set(allIds));
    }
  }

  // Iterate until fixpoint
  let changed = true;
  while (changed) {
    changed = false;

    for (const [id, node] of nodes) {
      if (id === entryId) continue;

      // Dom(n) = {n} ∪ ∩{Dom(p) | p ∈ predecessors(n)}
      const predDoms = node.parents.map(p => dominators.get(p)!);
      const intersection = predDoms.length > 0
        ? predDoms.reduce((acc, s) => new Set([...acc].filter(x => s.has(x))))
        : new Set<string>();

      intersection.add(id);

      const oldSize = dominators.get(id)!.size;
      dominators.set(id, intersection);

      if (intersection.size !== oldSize) {
        changed = true;
      }
    }
  }

  return dominators;
}

/**
 * Check if a node is dominated by a node with a specific tag.
 */
export function isDominatedBy(
  nodeId: string,
  tag: string,
  nodes: Map<string, FlowNode>,
  dominators: Map<string, Set<string>>
): boolean {
  const doms = dominators.get(nodeId);
  if (!doms) return false;

  for (const domId of doms) {
    const domNode = nodes.get(domId);
    if (domNode && domNode.flow.tag === tag) {
      return true;
    }
  }
  return false;
}
```

---

## Task 4: Implement Effect Analysis

### 4.1 Create `src/lint/analysis/effects.ts`

```typescript
import { FlowIR } from "../../frameir/flow";
import { PrimitiveRegistry } from "../../registry";
import { Effect } from "../../registry/types";

/**
 * Compute effects required by a flow node.
 */
export function nodeEffects(flow: FlowIR, registry: PrimitiveRegistry): Set<Effect> {
  const effects = new Set<Effect>();

  // Get effects from registry based on IR tag
  const descriptor = registry.getByIrTag(flow.tag);
  if (descriptor) {
    for (const effect of descriptor.effects) {
      effects.add(effect);
    }
  }

  return effects;
}

/**
 * Check if a flow node requires a specific effect.
 */
export function requiresEffect(flow: FlowIR, effect: Effect, registry: PrimitiveRegistry): boolean {
  return nodeEffects(flow, registry).has(effect);
}

/**
 * Collect all nodes in a flow that require a specific effect.
 */
export function collectByEffect(
  flow: FlowIR,
  effect: Effect,
  registry: PrimitiveRegistry
): FlowIR[] {
  const nodes: FlowIR[] = [];

  function visit(f: FlowIR): void {
    if (requiresEffect(f, effect, registry)) {
      nodes.push(f);
    }

    // Visit children based on flow type
    switch (f.tag) {
      case "FBind":
        visit((f as any).flow);
        break;
      case "FCatch":
        visit((f as any).flow);
        break;
      case "FWithBudget":
      case "FWithTimeout":
        visit((f as any).flow);
        break;
      case "FSequence":
      case "FAll":
      case "FRace":
      case "FAny":
        for (const child of (f as any).flows) {
          visit(child);
        }
        break;
      case "FBranch":
        visit((f as any).then);
        visit((f as any).else);
        break;
    }
  }

  visit(flow);
  return nodes;
}
```

---

## Task 5: Implement Budget Dominator Pass

**This is the most important lint.**

### 5.1 Create `src/lint/passes/budgetDominator.ts`

```typescript
import { Pass, PassResult } from "../types";
import { IRBundle } from "../../frameir";
import { PrimitiveRegistry } from "../../registry";
import { errorDiag } from "../../outcome/diagnostic";
import { buildFlowGraph, computeDominators, isDominatedBy } from "../analysis/dominators";
import { collectByEffect } from "../analysis/effects";

/**
 * Budget Dominator Lint Pass
 *
 * Ensures every node with Oracle or Tool effect is dominated by FWithBudget.
 *
 * Reference: ARCHITECTURE-LANGUAGES-4.md §49.2
 */
export const budgetDominatorPass: Pass = {
  id: "lint/budget-dominator",
  name: "Budget Dominator Check",
  phase: "lint",

  run(bundle: IRBundle, registry: PrimitiveRegistry): PassResult {
    const diagnostics: import("../../outcome/diagnostic").Diagnostic[] = [];
    const flow = bundle.entry;

    // Build flow graph
    const nodes = buildFlowGraph(flow);
    const entryId = nodes.keys().next().value;
    const dominators = computeDominators(nodes, entryId);

    // Find all Oracle and Tool effect nodes
    const oracleNodes = collectByEffect(flow, "Oracle", registry);
    const toolNodes = collectByEffect(flow, "Tool", registry);

    // Check each Oracle node is dominated by FWithBudget
    for (const oracleNode of oracleNodes) {
      const nodeId = findNodeId(oracleNode, nodes);
      if (nodeId && !isDominatedBy(nodeId, "FWithBudget", nodes, dominators)) {
        diagnostics.push(errorDiag(
          "E0600",
          `Oracle call (${oracleNode.tag}) is not dominated by with-budget`,
          {
            span: (oracleNode as any).meta?.span,
            data: { tag: oracleNode.tag }
          }
        ));
      }
    }

    // Check each Tool node is dominated by FWithBudget
    for (const toolNode of toolNodes) {
      const nodeId = findNodeId(toolNode, nodes);
      if (nodeId && !isDominatedBy(nodeId, "FWithBudget", nodes, dominators)) {
        diagnostics.push(errorDiag(
          "E0601",
          `Tool call (${toolNode.tag}) is not dominated by with-budget`,
          {
            span: (toolNode as any).meta?.span,
            data: { tag: toolNode.tag }
          }
        ));
      }
    }

    return { diagnostics };
  }
};

function findNodeId(flow: FlowIR, nodes: Map<string, { flow: FlowIR }>): string | undefined {
  for (const [id, node] of nodes) {
    if (node.flow === flow) return id;
  }
  return undefined;
}
```

---

## Task 6: Implement Tool Contract Pass

### 6.1 Create `src/lint/passes/toolContract.ts`

```typescript
import { Pass, PassResult } from "../types";
import { IRBundle, FToolCall } from "../../frameir";
import { PrimitiveRegistry } from "../../registry";
import { errorDiag } from "../../outcome/diagnostic";

/**
 * Tool Contract Lint Pass
 *
 * Ensures every FToolCall has a contract reference.
 *
 * Reference: ARCHITECTURE-LANGUAGES-4.md §49.2
 */
export const toolContractPass: Pass = {
  id: "lint/tool-contract",
  name: "Tool Contract Check",
  phase: "lint",

  run(bundle: IRBundle, registry: PrimitiveRegistry): PassResult {
    const diagnostics: import("../../outcome/diagnostic").Diagnostic[] = [];

    function visit(flow: import("../../frameir/flow").FlowIR): void {
      if (flow.tag === "FToolCall") {
        const toolCall = flow as FToolCall;
        if (!toolCall.contract) {
          diagnostics.push(errorDiag(
            "E0610",
            `Tool call missing contract reference`,
            {
              span: (flow as any).meta?.span,
              data: { tool: (toolCall.tool as any)?.value }
            }
          ));
        } else {
          // Verify contract exists in bundle
          const contractId = (toolCall.contract as any).ref?.id;
          if (contractId && !bundle.toolContracts[contractId]) {
            diagnostics.push(errorDiag(
              "E0611",
              `Tool contract not found in bundle: ${contractId}`,
              {
                span: (flow as any).meta?.span,
                data: { contractId }
              }
            ));
          }
        }
      }

      // Recursively visit children
      visitChildren(flow, visit);
    }

    visit(bundle.entry);
    return { diagnostics };
  }
};

function visitChildren(flow: import("../../frameir/flow").FlowIR, visitor: (f: import("../../frameir/flow").FlowIR) => void): void {
  switch (flow.tag) {
    case "FBind":
    case "FCatch":
    case "FWithBudget":
    case "FWithTimeout":
      visitor((flow as any).flow);
      break;
    case "FSequence":
    case "FAll":
    case "FRace":
    case "FAny":
      for (const child of (flow as any).flows) {
        visitor(child);
      }
      break;
    case "FBranch":
      visitor((flow as any).then);
      visitor((flow as any).else);
      break;
  }
}
```

---

## Task 7: Implement Timeout Guard Pass

### 7.1 Create `src/lint/passes/timeoutGuard.ts`

```typescript
import { Pass, PassResult } from "../types";
import { IRBundle } from "../../frameir";
import { PrimitiveRegistry } from "../../registry";
import { warnDiag } from "../../outcome/diagnostic";
import { buildFlowGraph, computeDominators, isDominatedBy } from "../analysis/dominators";
import { collectByEffect } from "../analysis/effects";

/**
 * Timeout Guard Lint Pass
 *
 * Warns if Oracle calls are not dominated by FWithTimeout.
 * This is a warning (not error) by default since some use cases don't need timeouts.
 */
export const timeoutGuardPass: Pass = {
  id: "lint/timeout-guard",
  name: "Timeout Guard Check",
  phase: "lint",

  run(bundle: IRBundle, registry: PrimitiveRegistry): PassResult {
    const diagnostics: import("../../outcome/diagnostic").Diagnostic[] = [];
    const flow = bundle.entry;

    const nodes = buildFlowGraph(flow);
    const entryId = nodes.keys().next().value;
    const dominators = computeDominators(nodes, entryId);

    const oracleNodes = collectByEffect(flow, "Oracle", registry);

    for (const oracleNode of oracleNodes) {
      const nodeId = findNodeId(oracleNode, nodes);
      if (nodeId && !isDominatedBy(nodeId, "FWithTimeout", nodes, dominators)) {
        diagnostics.push(warnDiag(
          "W0010",
          `Oracle call (${oracleNode.tag}) has no timeout guard`,
          {
            span: (oracleNode as any).meta?.span,
            data: { tag: oracleNode.tag }
          }
        ));
      }
    }

    return { diagnostics };
  }
};

function findNodeId(flow: import("../../frameir/flow").FlowIR, nodes: Map<string, { flow: import("../../frameir/flow").FlowIR }>): string | undefined {
  for (const [id, node] of nodes) {
    if (node.flow === flow) return id;
  }
  return undefined;
}
```

---

## Verification Steps

### 1. Type Checking
```bash
npx tsc --noEmit src/lint/**/*.ts
```

### 2. Unit Tests
```bash
npx vitest run test/lint/
```

Tests must verify:
- Budget dominator lint catches unguarded Oracle calls
- Budget dominator lint catches unguarded Tool calls
- Tool contract lint catches missing contracts
- Timeout guard lint warns on unguarded Oracle calls
- Pass runner orders passes correctly
- Configuration enables/disables passes

### 3. Integration Test
Create a test bundle that violates all rules, verify all diagnostics produced.

---

## Checklist

- [ ] Create `src/lint/types.ts` with Pass interface
- [ ] Create `src/lint/runner.ts` with PassRunner
- [ ] Create `src/lint/analysis/dominators.ts` for dominator computation
- [ ] Create `src/lint/analysis/effects.ts` for effect analysis
- [ ] Create `src/lint/passes/budgetDominator.ts`
- [ ] Create `src/lint/passes/toolContract.ts`
- [ ] Create `src/lint/passes/timeoutGuard.ts`
- [ ] Create `src/lint/passes/schemaAttachment.ts`
- [ ] Create `src/lint/index.ts` with exports
- [ ] Create test suite with positive and negative cases
- [ ] All tests pass

---

## Success Criteria

1. **Budget dominance enforced** - No unguarded Oracle/Tool calls pass lint
2. **Tool contracts required** - Every FToolCall has a contract
3. **Passes are configurable** - Can enable/disable/override severity
4. **Good diagnostics** - Clear error messages with spans
5. **Fast execution** - Lint a typical bundle in <100ms

---

## Test Plan

### Happy Path Tests

| ID | Test Case | Input | Expected Output |
|----|-----------|-------|-----------------|
| HP-1 | Budget-guarded Oracle passes | FWithBudget(FInfer(...)) | No diagnostics |
| HP-2 | Budget-guarded Tool passes | FWithBudget(FToolCall(...)) | No diagnostics |
| HP-3 | Tool with contract passes | FToolCall with contract ref | No diagnostics |
| HP-4 | Timeout-guarded Oracle passes | FWithTimeout(FInfer(...)) | No diagnostics |
| HP-5 | PassRunner orders by phase | Multiple phases | Correct order |
| HP-6 | Disabled pass skipped | config.enabled = false | Pass not run |
| HP-7 | Severity override applied | severityOverride = "warning" | Diagnostic is warning |
| HP-8 | Dominator tree computed | Simple flow graph | Correct dominators |
| HP-9 | Effect analysis works | FInfer node | Has Oracle effect |
| HP-10 | Multiple passes compose | All passes registered | All run in order |

### Edge Case Tests

| ID | Test Case | Input | Expected Output |
|----|-----------|-------|-----------------|
| EC-1 | Nested budget guards | FWithBudget(FWithBudget(FInfer)) | No diagnostics |
| EC-2 | Budget in one branch | FBranch(FWithBudget(FInfer), FInfer) | Error on else branch |
| EC-3 | Budget in parallel | FAll([FWithBudget(FInfer), FInfer]) | Error on second parallel |
| EC-4 | Empty bundle | No flows | No diagnostics |
| EC-5 | Deeply nested flow | 20 levels deep | Handles without stack overflow |
| EC-6 | Loop body analysis | FLoop with Oracle in body | Detects unguarded in body |
| EC-7 | Multiple Oracle nodes | FSequence([FInfer, FInfer]) | Reports both if unguarded |
| EC-8 | Contract in different bundle section | toolContracts["x"] exists | Resolves correctly |

### Error Cases

| ID | Test Case | Input | Expected Error |
|----|-----------|-------|----------------|
| ERR-1 | Unguarded Oracle | FInfer without FWithBudget | E0600: Oracle call not dominated |
| ERR-2 | Unguarded Tool | FToolCall without FWithBudget | E0601: Tool call not dominated |
| ERR-3 | Missing tool contract | FToolCall with contract: undefined | E0610: missing contract reference |
| ERR-4 | Contract not in bundle | FToolCall with unknown contractId | E0611: contract not found |
| ERR-5 | Unguarded Oracle (timeout) | FInfer without FWithTimeout | W0010: no timeout guard |
| ERR-6 | Unknown pass dependency | dependencies: ["nonexistent"] | Error or warning |
| ERR-7 | Cyclic pass dependencies | A depends on B, B depends on A | Detected and reported |

### Integration Tests

| ID | Test Case | Description |
|----|-----------|-------------|
| INT-1 | Full violation bundle | Bundle violating all rules, verify all diagnostics |
| INT-2 | Clean bundle | Valid bundle, zero diagnostics |
| INT-3 | Lint then compile | Lint passes, then compile, verify consistent |
| INT-4 | Custom pass registration | User-defined pass added to runner |
| INT-5 | Performance test | Lint 1000-node bundle in < 100ms |

---

## Notes

### Why Dominator Analysis

Dominator analysis is a classic compiler technique:
- Node D **dominates** node N if every path from entry to N goes through D
- If `FWithBudget` dominates `FInfer`, then budget is guaranteed to be checked before Oracle call
- This is a **static guarantee** - no runtime overhead

### Lint vs Runtime Check Trade-off

| Check Type | When | Pros | Cons |
|------------|------|------|------|
| Lint (static) | Compile time | Zero runtime cost, early feedback | Can't check dynamic values |
| Runtime | Execution time | Can check dynamic values | Runtime overhead, late feedback |

Budget dominator is a lint because budget **existence** is static (the `with-budget` form is present or not).

### Pass Phase Ordering

```
parsing → lowering → normalize → lint → optimize
```

Lint passes run after IR is in normal form, before optimizations might transform it.

### Configurable Severity

Some teams want timeout warnings, others want them as errors:
```typescript
const config: LintConfig = {
  passes: {
    "lint/timeout-guard": { enabled: true, severityOverride: "error" }
  }
};
```

---

## Proof of Completion

When this job is complete:

1. **File Structure Verified**
   ```bash
   ls src/lint/*.ts           # 3 files
   ls src/lint/analysis/*.ts  # 2 files
   ls src/lint/passes/*.ts    # 4+ files
   ```

2. **All Passes Registered**
   ```bash
   grep "export const.*Pass:" src/lint/passes/*.ts | wc -l  # Should be ≥ 4
   ```

3. **Dominator Analysis Works**
   ```bash
   npx vitest run test/lint/ --grep "dominator"
   ```

4. **All Tests Pass**
   ```bash
   npx vitest run test/lint/ --reporter=verbose
   ```

5. **Lint Catches Budget Violation**
   ```typescript
   // This must produce E0600 diagnostic
   const bundle = { entry: { tag: "FInfer", ... } };
   const result = runner.run(bundle);
   assert(result.diagnostics.some(d => d.code === "E0600"));
   ```

---

## Footer

| Field | Value |
|-------|-------|
| Created | 2025-01-20 |
| Last Updated | 2025-01-20 |
| Author | Claude |
| Related Docs | [ARCHITECTURE-LANGUAGES-4.md §49](../docs/ARCHITECTURE-LANGUAGES-4.md), [ARCHITECTURE-LANGUAGES-6.md §64](../docs/ARCHITECTURE-LANGUAGES-6.md) |
| Predecessors | [009-FRAMEIR-PACKAGE](./009-FRAMEIR-PACKAGE.md), [010-PRIMITIVE-REGISTRY](./010-PRIMITIVE-REGISTRY.md), [012-OUTCOME-FAILURE-DIAGNOSTIC](./012-OUTCOME-FAILURE-DIAGNOSTIC.md) |
| Successor | [014-COMPILATION-PIPELINE](./014-COMPILATION-PIPELINE.md) |
