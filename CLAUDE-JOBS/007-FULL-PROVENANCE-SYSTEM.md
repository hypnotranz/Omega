# JOB-007: Full Provenance System (ProvenanceGraph + Persistence)

**Priority**: P1 - Important (Phase B)
**Estimated Effort**: 2-3 days
**Skills Required**: TypeScript, Graph data structures, Persistence patterns
**Status**: DONE

> **MUST READ**: [LOGISTICS.md](LOGISTICS.md) before starting - covers testing, file locations, and proof of completion requirements.

### Quick Start

```bash
cd c:\Users\Richa\parmenides-dev\agent-harness\OmegaLLM

# Verify Job 004 Task 2 is complete first (evidence primitives)
npx vitest run test/evidence/

# Run existing tests (verify baseline)
npm run test

# After implementation, run new tests
npx vitest run test/provenance/

# Watch mode during development
npm run test:watch
```

### Output Files

| File | Action | Package |
|------|--------|---------|
| `src/core/provenance/evidence.ts` | **CREATE** - Evidence types, evidenceId(), computeSourceHash() | `@omega/provenance` |
| `src/core/provenance/graph.ts` | **CREATE** - ProvenanceGraph DAG | `@omega/provenance` |
| `src/core/provenance/store/interface.ts` | **CREATE** - ProvenanceStore interface | `@omega/provenance` |
| `src/core/provenance/store/file.ts` | **CREATE** - FileProvenanceStore | `@omega/provenance` |
| `src/core/provenance/prims.ts` | **CREATE** - provenance-trace, etc. | `@omega/provenance` |
| `src/core/oracle/meaning.ts` | Extend Evidence type | `@omega/core` |
| `src/core/prims.ts` | Add provenance primitives | `@omega/core` |
| `test/provenance/graph.spec.ts` | **CREATE** - graph tests | test |
| `test/provenance/store.spec.ts` | **CREATE** - persistence tests | test |
| `test/provenance/staleness.spec.ts` | **CREATE** - staleness detection tests | test |

### LambdaRLM Reference

| LambdaRLM Job | Relevance |
|---------------|-----------|
| [14-PROVENANCE.md](../../LambdaRLM/CLAUDE-JOBS/14-PROVENANCE.md) | **DIRECT EQUIVALENT** - same concept, Lisp implementation |
| [EXPLORATION-12](../../LambdaRLM/EXPLORATION/EXPLORATOIN-12.md) | Full template and design rationale |

---

## Executive Summary

Complete the provenance system by adding:

1. **ProvenanceGraph** - DAG tracking how values were derived from oracle calls
2. **Persistent Receipt Ledger** - Durable storage for oracle call records
3. **Evidence Queries** - APIs to trace claims back to sources

Job 004 adds the basic evidence primitives (`evidence-id`, `verify-evidence`, `evidence-stale?`). This job builds the **full infrastructure** for LLM audit trails.

**Why This Matters**: Without provenance, LLM outputs are black boxes. With it, you can:
- Trace any claim to its oracle source
- Detect when evidence is stale (source changed)
- Build audit trails for compliance
- Prevent hallucination propagation

---

## Package

**Package**: `@omega/provenance` (optional)

This is an **optional package** for LLM audit trails and evidence tracking.

```
@omega/provenance/
├── evidence.ts         # Evidence types, evidenceId(), computeSourceHash()
├── graph.ts            # ProvenanceGraph DAG
├── store/
│   ├── interface.ts    # ProvenanceStore interface
│   ├── file.ts         # FileProvenanceStore implementation
│   └── sqlite.ts       # SQLiteProvenanceStore (optional, for production)
└── prims.ts            # provenance-trace, provenance-record, provenance-check-staleness
```

**Use Cases**:
- Tracing LLM claims back to oracle sources
- Detecting stale evidence
- Compliance and audit trails
- Preventing hallucination propagation

**Dependency**: Requires `@omega/core` (absorbs Job 004 Task 2)

---

## Dependencies

- **REQUIRES**: [Job 004](004-IMPLEMENT-CORE-MAGIC.md) Task 2 (Evidence primitives) **MUST BE COMPLETE FIRST**
  - `evidence-id`, `verify-evidence`, `evidence-stale?` from Task 2
  - Verify: `npx vitest run test/evidence/` must pass before starting this job
- **Builds on**: Existing `Evidence` types in `src/core/oracle/meaning.ts`, `InMemoryReceiptStore` in `src/core/oracle/receipts.ts`
- **Blocks**: Nothing directly (but enables advanced audit workflows)

---

## What Already Exists

### Evidence Types

**File**: [src/core/oracle/meaning.ts](../src/core/oracle/meaning.ts)

```typescript
export type Evidence =
  | { tag: "TestEvidence"; passed: number; total: number; receipt?: string }
  | { tag: "NoMatchEvidence"; pattern: Expr; searched: number; found: number }
  | { tag: "EqExtEvidence"; tests: number; allPassed: boolean; ... };

export type MeaningVal = {
  evidence?: Evidence[];  // Values can carry evidence
};
```

### Receipt Store

**File**: [src/core/oracle/receipts.ts](../src/core/oracle/receipts.ts)

```typescript
// In-memory receipt storage exists
export class InMemoryReceiptStore {
  store(receipt: Receipt): void;
  get(id: string): Receipt | undefined;
}

export type Receipt = {
  id: string;
  timestamp: number;
  request: OracleRequest;
  response: OracleResponse;
  // ...
};
```

### Gap Analysis

| Exists | Missing |
|--------|---------|
| Evidence types | ProvenanceGraph (DAG) |
| InMemoryReceiptStore | Persistent storage |
| evidence-id primitive (Job 004) | evidence-query API |
| Basic receipts | Source hash tracking |

---

## Design: Provenance Graph

### Data Model

```
┌─────────────────────────────────────────────────────────────┐
│                    ProvenanceGraph (DAG)                     │
│                                                              │
│  ┌──────────┐     ┌──────────┐     ┌──────────┐            │
│  │ Oracle   │────▶│ Transform│────▶│ Derived  │            │
│  │ Call A   │     │ T1       │     │ Value V  │            │
│  └──────────┘     └──────────┘     └──────────┘            │
│       │                                  ▲                  │
│       │           ┌──────────┐           │                  │
│       └──────────▶│ Transform│───────────┘                  │
│                   │ T2       │                              │
│                   └──────────┘                              │
│                                                              │
│  Nodes: OracleCalls, Transforms, DerivedValues              │
│  Edges: "derived-from" relationships                        │
└─────────────────────────────────────────────────────────────┘
```

### Node Types

```typescript
// A node in the provenance graph
export type ProvenanceNode =
  | { tag: "OracleCall"; receiptId: string; timestamp: number; sourceHash?: string }
  | { tag: "Transform"; operation: string; inputs: string[]; output: string }
  | { tag: "Assertion"; claim: string; evidenceIds: string[] }
  | { tag: "Derivation"; expression: string; dependencies: string[] };

// Edge: "B was derived from A"
export type ProvenanceEdge = {
  from: string;  // Node ID
  to: string;    // Node ID
  relation: "derived-from" | "validated-by" | "depends-on";
};
```

---

## Implementation Plan

### Task 1: Extend Evidence Types (2 hours)

**File to Modify**: `src/core/oracle/meaning.ts`

```typescript
// Add new evidence types
export type Evidence =
  | { tag: "TestEvidence"; ... }
  | { tag: "NoMatchEvidence"; ... }
  | { tag: "EqExtEvidence"; ... }
  // NEW:
  | { tag: "OracleEvidence"; receiptId: string; sourceHash: string; timestamp: number }
  | { tag: "TransformEvidence"; operation: string; inputEvidenceIds: string[] }
  | { tag: "DerivedEvidence"; dependencies: string[]; derivationExpr: string };

// Evidence ID generation
export function evidenceId(evidence: Evidence): string {
  // Hash the evidence content for stable IDs
  return `ev-${hash(JSON.stringify(evidence)).slice(0, 12)}`;
}

// Source hash for staleness detection
export function computeSourceHash(content: string): string {
  return `sh-${hash(content).slice(0, 16)}`;
}
```

### Task 2: Create ProvenanceGraph (4 hours)

**File to Create**: `src/core/provenance/graph.ts`

```typescript
import { Evidence, evidenceId } from "../oracle/meaning";

export type ProvenanceNode = {
  id: string;
  evidence: Evidence;
  timestamp: number;
};

export type ProvenanceEdge = {
  from: string;
  to: string;
  relation: "derived-from" | "validated-by" | "depends-on";
};

export class ProvenanceGraph {
  private nodes: Map<string, ProvenanceNode> = new Map();
  private edges: ProvenanceEdge[] = [];
  private reverseIndex: Map<string, string[]> = new Map();  // to -> froms

  // Add a node
  addNode(evidence: Evidence): string {
    const id = evidenceId(evidence);
    if (!this.nodes.has(id)) {
      this.nodes.set(id, {
        id,
        evidence,
        timestamp: Date.now(),
      });
    }
    return id;
  }

  // Add an edge
  addEdge(from: string, to: string, relation: ProvenanceEdge["relation"]): void {
    this.edges.push({ from, to, relation });

    // Update reverse index
    const froms = this.reverseIndex.get(to) || [];
    froms.push(from);
    this.reverseIndex.set(to, froms);
  }

  // Record that B was derived from A
  derivedFrom(derived: Evidence, sources: Evidence[]): string {
    const derivedId = this.addNode(derived);
    for (const source of sources) {
      const sourceId = this.addNode(source);
      this.addEdge(sourceId, derivedId, "derived-from");
    }
    return derivedId;
  }

  // Get all sources for an evidence ID (transitive)
  getSources(evidenceId: string): ProvenanceNode[] {
    const visited = new Set<string>();
    const result: ProvenanceNode[] = [];

    const visit = (id: string) => {
      if (visited.has(id)) return;
      visited.add(id);

      const froms = this.reverseIndex.get(id) || [];
      for (const from of froms) {
        const node = this.nodes.get(from);
        if (node) {
          result.push(node);
          visit(from);
        }
      }
    };

    visit(evidenceId);
    return result;
  }

  // Check if any source is stale
  async checkStaleness(evidenceId: string, sourceChecker: SourceChecker): Promise<StalenessReport> {
    const sources = this.getSources(evidenceId);
    const oracleSources = sources.filter(n =>
      n.evidence.tag === "OracleEvidence"
    ) as (ProvenanceNode & { evidence: OracleEvidence })[];

    const staleItems: StaleItem[] = [];

    for (const source of oracleSources) {
      const currentHash = await sourceChecker.getSourceHash(source.evidence.receiptId);
      if (currentHash !== source.evidence.sourceHash) {
        staleItems.push({
          evidenceId: source.id,
          receiptId: source.evidence.receiptId,
          originalHash: source.evidence.sourceHash,
          currentHash,
        });
      }
    }

    return {
      isStale: staleItems.length > 0,
      staleItems,
      totalSources: oracleSources.length,
    };
  }

  // Export for persistence
  toJSON(): ProvenanceGraphData {
    return {
      nodes: Array.from(this.nodes.values()),
      edges: this.edges,
    };
  }

  // Import from persistence
  static fromJSON(data: ProvenanceGraphData): ProvenanceGraph {
    const graph = new ProvenanceGraph();
    for (const node of data.nodes) {
      graph.nodes.set(node.id, node);
    }
    graph.edges = data.edges;

    // Rebuild reverse index
    for (const edge of data.edges) {
      const froms = graph.reverseIndex.get(edge.to) || [];
      froms.push(edge.from);
      graph.reverseIndex.set(edge.to, froms);
    }

    return graph;
  }
}
```

### Task 3: Create Persistent Receipt Store (4 hours)

**File to Create**: `src/core/provenance/persistentStore.ts`

```typescript
import { Receipt } from "../oracle/receipts";
import { ProvenanceGraphData } from "./graph";

// Abstract interface for persistence
export interface ProvenanceStore {
  // Receipts
  storeReceipt(receipt: Receipt): Promise<void>;
  getReceipt(id: string): Promise<Receipt | undefined>;
  queryReceipts(filter: ReceiptFilter): Promise<Receipt[]>;

  // Graph
  storeGraph(graph: ProvenanceGraphData): Promise<void>;
  loadGraph(): Promise<ProvenanceGraphData | undefined>;

  // Cleanup
  pruneOlderThan(timestamp: number): Promise<number>;
}

// File-based implementation
export class FileProvenanceStore implements ProvenanceStore {
  constructor(private basePath: string) {}

  async storeReceipt(receipt: Receipt): Promise<void> {
    const path = `${this.basePath}/receipts/${receipt.id}.json`;
    await fs.writeFile(path, JSON.stringify(receipt, null, 2));
  }

  async getReceipt(id: string): Promise<Receipt | undefined> {
    const path = `${this.basePath}/receipts/${id}.json`;
    try {
      const data = await fs.readFile(path, "utf-8");
      return JSON.parse(data);
    } catch {
      return undefined;
    }
  }

  async queryReceipts(filter: ReceiptFilter): Promise<Receipt[]> {
    const receiptDir = `${this.basePath}/receipts`;
    const files = await fs.readdir(receiptDir);
    const receipts: Receipt[] = [];

    for (const file of files) {
      if (file.endsWith(".json")) {
        const receipt = await this.getReceipt(file.replace(".json", ""));
        if (receipt && matchesFilter(receipt, filter)) {
          receipts.push(receipt);
        }
      }
    }

    return receipts;
  }

  async storeGraph(graph: ProvenanceGraphData): Promise<void> {
    const path = `${this.basePath}/graph.json`;
    await fs.writeFile(path, JSON.stringify(graph, null, 2));
  }

  async loadGraph(): Promise<ProvenanceGraphData | undefined> {
    const path = `${this.basePath}/graph.json`;
    try {
      const data = await fs.readFile(path, "utf-8");
      return JSON.parse(data);
    } catch {
      return undefined;
    }
  }

  async pruneOlderThan(timestamp: number): Promise<number> {
    // Delete receipts older than timestamp
    const receipts = await this.queryReceipts({ before: timestamp });
    let count = 0;
    for (const receipt of receipts) {
      await fs.unlink(`${this.basePath}/receipts/${receipt.id}.json`);
      count++;
    }
    return count;
  }
}

// SQLite implementation (optional, for production)
export class SQLiteProvenanceStore implements ProvenanceStore {
  // More efficient for large-scale usage
  // Implementation omitted - use better-sqlite3 or similar
}
```

### Task 4: Add Provenance Primitives (4 hours)

**File to Modify**: `src/core/prims.ts`

```typescript
// provenance-graph: Get/create the provenance graph
def("provenance-graph", {
  arity: 0,
  fn: (args, state) => {
    // Return the graph as a value
    // The graph lives in state.provenanceGraph or similar
    return getProvenanceGraph(state);
  }
});

// provenance-trace: Trace a value back to its sources
// (provenance-trace value) => list of source evidence
def("provenance-trace", {
  arity: 1,
  fn: (args, state) => {
    const val = args[0];
    if (!isMeaning(val) || !val.evidence || val.evidence.length === 0) {
      return { tag: "List", elems: [] };
    }

    const graph = getProvenanceGraph(state);
    const sources = graph.getSources(evidenceId(val.evidence[0]));

    return {
      tag: "List",
      elems: sources.map(s => evidenceToValue(s.evidence)),
    };
  }
});

// provenance-check-staleness: Check if sources have changed
// (provenance-check-staleness value) => staleness report
def("provenance-check-staleness", {
  arity: 1,
  async: true,  // This needs to check external sources
  fn: async (args, state) => {
    const val = args[0];
    if (!isMeaning(val) || !val.evidence) {
      return { tag: "Bool", b: true };  // No evidence = stale
    }

    const graph = getProvenanceGraph(state);
    const report = await graph.checkStaleness(
      evidenceId(val.evidence[0]),
      getSourceChecker(state)
    );

    return {
      tag: "Record",
      fields: {
        "stale?": { tag: "Bool", b: report.isStale },
        "stale-count": { tag: "Num", n: report.staleItems.length },
        "total-sources": { tag: "Num", n: report.totalSources },
      },
    };
  }
});

// provenance-record: Explicitly record a derivation
// (provenance-record derived-value source-values operation-name)
def("provenance-record", {
  arity: 3,
  fn: (args, state) => {
    const [derived, sources, operation] = args;

    const graph = getProvenanceGraph(state);

    // Extract evidence from sources
    const sourceEvidences: Evidence[] = [];
    if (sources.tag === "List") {
      for (const s of sources.elems) {
        if (isMeaning(s) && s.evidence) {
          sourceEvidences.push(...s.evidence);
        }
      }
    }

    // Create derivation evidence
    const derivedEvidence: Evidence = {
      tag: "DerivedEvidence",
      dependencies: sourceEvidences.map(evidenceId),
      derivationExpr: asString(operation),
    };

    // Record in graph
    graph.derivedFrom(derivedEvidence, sourceEvidences);

    // Attach evidence to derived value
    return attachEvidence(derived, [derivedEvidence]);
  }
});
```

### Task 5: Wire Oracle Calls to Provenance (2 hours)

**File to Modify**: `src/core/oracle/portal.ts` (or similar)

When an oracle call returns, automatically:

1. Create a `Receipt`
2. Store it in the receipt store
3. Create `OracleEvidence`
4. Record in provenance graph
5. Attach evidence to the result value

```typescript
async function executeOracleCall(request: OracleRequest, state: State): Promise<OracleResponse> {
  // Execute the call
  const response = await oracle.call(request);

  // Create receipt
  const receipt: Receipt = {
    id: generateReceiptId(),
    timestamp: Date.now(),
    request,
    response,
    sourceHash: computeSourceHash(JSON.stringify(request)),
  };

  // Store receipt
  const store = getProvenanceStore(state);
  await store.storeReceipt(receipt);

  // Create evidence
  const evidence: OracleEvidence = {
    tag: "OracleEvidence",
    receiptId: receipt.id,
    sourceHash: receipt.sourceHash,
    timestamp: receipt.timestamp,
  };

  // Record in graph
  const graph = getProvenanceGraph(state);
  graph.addNode(evidence);

  // Return response with evidence attached
  return {
    ...response,
    evidence: [evidence],
  };
}
```

---

## Verification

### Test 1: Basic Tracing

```lisp
;; Make an oracle call
(define answer (oracle-infer "What is 2+2?"))

;; Trace it
(provenance-trace answer)
;; => (OracleEvidence receipt-id="rx-123" ...)
```

### Test 2: Derived Values

```lisp
;; Get two answers
(define a (oracle-infer "What is 2+2?"))
(define b (oracle-infer "What is 3+3?"))

;; Derive a new value
(define combined
  (provenance-record
    (+ a b)
    (list a b)
    "sum"))

;; Trace shows both sources
(provenance-trace combined)
;; => ((OracleEvidence rx-1 ...) (OracleEvidence rx-2 ...))
```

### Test 3: Staleness Detection

```lisp
;; Get an answer
(define answer (oracle-infer "Current temperature in SF?"))

;; Check staleness immediately
(provenance-check-staleness answer)
;; => (record stale? #f stale-count 0 total-sources 1)

;; ... time passes, source changes ...

;; Check again
(provenance-check-staleness answer)
;; => (record stale? #t stale-count 1 total-sources 1)
```

---

## Test Plan

### Evidence Tests (`test/provenance/evidence.spec.ts`)

#### Happy Path Tests

```lisp
;; HP-1: Create OracleEvidence from oracle call
(define answer (oracle-infer "What is 2+2?"))
(evidence-list answer)
;; => ((OracleEvidence receipt-id="rx-..." source-hash="sh-..." timestamp=...))

;; HP-2: evidence-id returns unique identifier
(define ev (car (evidence-list answer)))
(evidence-id ev)
;; => "ev-abc123..." (12 char hash)

;; HP-3: Same evidence produces same ID (deterministic)
(define ev1 (make-oracle-evidence "rx-1" "sh-1" 1000))
(define ev2 (make-oracle-evidence "rx-1" "sh-1" 1000))
(equal? (evidence-id ev1) (evidence-id ev2))
;; => #t

;; HP-4: Different evidence produces different ID
(define ev1 (make-oracle-evidence "rx-1" "sh-1" 1000))
(define ev2 (make-oracle-evidence "rx-2" "sh-2" 2000))
(equal? (evidence-id ev1) (evidence-id ev2))
;; => #f
```

#### Edge Case Tests

```lisp
;; EC-1: Evidence with empty data fields
(make-oracle-evidence "" "" 0)
;; => valid evidence (empty strings OK)

;; EC-2: Value without evidence
(evidence-list 42)
;; => () (plain values have no evidence)

;; EC-3: Computed value preserves no evidence by default
(define a (oracle-infer "What is 2+2?"))
(define b (+ a 1))  ; arithmetic on result
(evidence-list b)
;; => () (computation strips evidence without explicit recording)
```

### ProvenanceGraph Tests (`test/provenance/graph.spec.ts`)

#### Happy Path Tests

```lisp
;; HP-1: Add node to graph
(define g (make-provenance-graph))
(define ev (make-oracle-evidence "rx-1" "sh-1" 1000))
(define node-id (provenance-add-node g ev))
node-id
;; => "ev-..." (returns node ID)

;; HP-2: Record derivation relationship
(define g (make-provenance-graph))
(define src1 (make-oracle-evidence "rx-1" "sh-1" 1000))
(define src2 (make-oracle-evidence "rx-2" "sh-2" 2000))
(define derived (make-derived-evidence (list (evidence-id src1) (evidence-id src2)) "combine"))
(provenance-derived-from g derived (list src1 src2))
;; => derived node ID

;; HP-3: Get sources for derived value
(define sources (provenance-get-sources g (evidence-id derived)))
(length sources)
;; => 2

;; HP-4: Transitive source tracing
(define g (make-provenance-graph))
(define a (make-oracle-evidence "rx-a" "sh-a" 1000))
(define b (make-derived-evidence (list (evidence-id a)) "step1"))
(define c (make-derived-evidence (list (evidence-id b)) "step2"))
(provenance-derived-from g b (list a))
(provenance-derived-from g c (list b))
(define all-sources (provenance-get-sources g (evidence-id c)))
(length all-sources)
;; => 2 (includes both a and b)
```

#### Edge Case Tests

```lisp
;; EC-1: Add same node twice (idempotent)
(define g (make-provenance-graph))
(define ev (make-oracle-evidence "rx-1" "sh-1" 1000))
(define id1 (provenance-add-node g ev))
(define id2 (provenance-add-node g ev))
(equal? id1 id2)
;; => #t

;; EC-2: Node with no sources (root)
(define g (make-provenance-graph))
(define ev (make-oracle-evidence "rx-1" "sh-1" 1000))
(provenance-add-node g ev)
(provenance-get-sources g (evidence-id ev))
;; => () (oracle evidence has no sources)

;; EC-3: Diamond dependency graph
;;       A
;;      / \
;;     B   C
;;      \ /
;;       D
(define g (make-provenance-graph))
(define a (make-oracle-evidence "rx-a" "sh-a" 1000))
(define b (make-derived-evidence (list (evidence-id a)) "B"))
(define c (make-derived-evidence (list (evidence-id a)) "C"))
(define d (make-derived-evidence (list (evidence-id b) (evidence-id c)) "D"))
(provenance-derived-from g b (list a))
(provenance-derived-from g c (list a))
(provenance-derived-from g d (list b c))
(define sources-of-d (provenance-get-sources g (evidence-id d)))
(length sources-of-d)
;; => 3 (b, c, and a - a is not duplicated)

;; EC-4: Self-referential edge (error)
(define g (make-provenance-graph))
(define ev (make-oracle-evidence "rx-1" "sh-1" 1000))
(provenance-add-edge g (evidence-id ev) (evidence-id ev) 'derived-from)
;; => Error: Cannot create self-referential edge

;; EC-5: Query non-existent node
(define g (make-provenance-graph))
(provenance-get-sources g "nonexistent-id")
;; => () (empty, not error)
```

#### Error Cases

```lisp
;; ERR-1: Add edge with non-existent source
(define g (make-provenance-graph))
(provenance-add-edge g "nonexistent" "also-nonexistent" 'derived-from)
;; => Error: Source node does not exist

;; ERR-2: Graph serialization with circular reference (should not happen but protect)
;; (internal error, not user-facing)
```

### Staleness Tests (`test/provenance/staleness.spec.ts`)

#### Happy Path Tests

```lisp
;; HP-1: Fresh evidence is not stale
(define answer (oracle-infer "What is 2+2?"))
(define report (provenance-check-staleness answer))
(record-ref report 'stale?)
;; => #f

;; HP-2: Evidence after source change is stale
(define answer (oracle-infer "Current temperature?"))
;; ... source changes ...
(mock-source-hash-change! (car (evidence-list answer)))
(define report (provenance-check-staleness answer))
(record-ref report 'stale?)
;; => #t

;; HP-3: Staleness report includes count
(define a (oracle-infer "Q1"))
(define b (oracle-infer "Q2"))
(define combined (provenance-record (+ a b) (list a b) "sum"))
(mock-source-hash-change! (car (evidence-list a)))
(define report (provenance-check-staleness combined))
(record-ref report 'stale-count)
;; => 1 (only one source changed)
(record-ref report 'total-sources)
;; => 2
```

#### Edge Case Tests

```lisp
;; EC-1: Value with no evidence
(define report (provenance-check-staleness 42))
(record-ref report 'stale?)
;; => #t (no evidence = considered stale)

;; EC-2: Value with non-oracle evidence (e.g., TestEvidence)
(define ev (make-test-evidence 10 10))  ; 10/10 tests passed
(define val (attach-evidence 'result (list ev)))
(define report (provenance-check-staleness val))
(record-ref report 'stale?)
;; => #f (TestEvidence doesn't have external source, never stale)
(record-ref report 'total-sources)
;; => 0 (no oracle sources)

;; EC-3: Derived value where all sources are fresh
(define a (oracle-infer "Q1"))
(define b (oracle-infer "Q2"))
(define combined (provenance-record (+ a b) (list a b) "sum"))
(define report (provenance-check-staleness combined))
(record-ref report 'stale?)
;; => #f

;; EC-4: Check staleness on value from previous session (persistence)
;; (Requires persistence to be implemented)
```

### Persistent Store Tests (`test/provenance/store.spec.ts`)

#### Happy Path Tests

```lisp
;; HP-1: Store and retrieve receipt
(define store (make-file-provenance-store "/tmp/prov-test"))
(define receipt (make-receipt "rx-1" (make-request "Q") (make-response "A")))
(provenance-store-receipt store receipt)
(define retrieved (provenance-get-receipt store "rx-1"))
(equal? (receipt-id retrieved) "rx-1")
;; => #t

;; HP-2: Store and load graph
(define store (make-file-provenance-store "/tmp/prov-test"))
(define g (make-provenance-graph))
(provenance-add-node g (make-oracle-evidence "rx-1" "sh-1" 1000))
(provenance-store-graph store g)
(define loaded (provenance-load-graph store))
(provenance-graph? loaded)
;; => #t

;; HP-3: Query receipts by filter
(define store (make-file-provenance-store "/tmp/prov-test"))
(provenance-store-receipt store (make-receipt "rx-1" ... timestamp=1000))
(provenance-store-receipt store (make-receipt "rx-2" ... timestamp=2000))
(provenance-store-receipt store (make-receipt "rx-3" ... timestamp=3000))
(define results (provenance-query-receipts store (make-filter after=1500)))
(length results)
;; => 2 (rx-2 and rx-3)
```

#### Edge Case Tests

```lisp
;; EC-1: Get non-existent receipt
(define store (make-file-provenance-store "/tmp/prov-test"))
(provenance-get-receipt store "nonexistent")
;; => #f

;; EC-2: Load graph when none saved
(define store (make-file-provenance-store "/tmp/prov-test-empty"))
(provenance-load-graph store)
;; => #f

;; EC-3: Prune old receipts
(define store (make-file-provenance-store "/tmp/prov-test"))
(provenance-store-receipt store (make-receipt "rx-old" ... timestamp=1000))
(provenance-store-receipt store (make-receipt "rx-new" ... timestamp=9999))
(define pruned (provenance-prune-older-than store 5000))
pruned
;; => 1 (one receipt deleted)

;; EC-4: Query with no matching results
(define results (provenance-query-receipts store (make-filter after=99999)))
(length results)
;; => 0
```

#### Error Cases

```lisp
;; ERR-1: Store to read-only location
(define store (make-file-provenance-store "/readonly/path"))
(provenance-store-receipt store receipt)
;; => Error: Permission denied

;; ERR-2: Corrupt graph file
;; (Write invalid JSON to graph.json, then try to load)
;; => Error: Failed to parse provenance graph
```

### provenance-trace Tests

```lisp
;; HP-1: Trace oracle value to itself
(define answer (oracle-infer "What is 2+2?"))
(define trace (provenance-trace answer))
(length trace)
;; => 1 (the oracle evidence itself)

;; HP-2: Trace derived value
(define a (oracle-infer "Q1"))
(define b (oracle-infer "Q2"))
(define combined (provenance-record (+ a b) (list a b) "sum"))
(define trace (provenance-trace combined))
(length trace)
;; => 2 (both oracle sources)

;; EC-1: Trace plain value
(provenance-trace 42)
;; => () (no provenance)

;; EC-2: Trace value with multiple derivation steps
(define a (oracle-infer "Q"))
(define b (provenance-record (* a 2) (list a) "double"))
(define c (provenance-record (+ b 1) (list b) "increment"))
(define trace (provenance-trace c))
;; => includes evidence from a, b derivation, and c derivation
```

### provenance-record Tests

```lisp
;; HP-1: Record simple derivation
(define a (oracle-infer "What is 2+2?"))
(define doubled (provenance-record (* a 2) (list a) "double"))
(evidence-list doubled)
;; => ((DerivedEvidence dependencies=(...) derivationExpr="double"))

;; HP-2: Record multi-source derivation
(define a (oracle-infer "Q1"))
(define b (oracle-infer "Q2"))
(define combined (provenance-record (+ a b) (list a b) "combine"))
(define ev (car (evidence-list combined)))
(length (derived-evidence-dependencies ev))
;; => 2

;; EC-1: Record derivation with empty source list
(define result (provenance-record 42 '() "constant"))
(evidence-list result)
;; => ((DerivedEvidence dependencies=() derivationExpr="constant"))

;; EC-2: Record derivation from non-evidenced values (warning?)
(define result (provenance-record (+ 1 2) (list 1 2) "add"))
;; => records but dependencies are empty (values had no evidence)
```

### Integration Tests

1. End-to-end oracle call → provenance → query
2. Multi-hop derivations
3. Persistence across restarts
4. Large graph performance

---

## Checklist

### Task 1: Evidence Types
- [ ] Add `OracleEvidence` type
- [ ] Add `TransformEvidence` type
- [ ] Add `DerivedEvidence` type
- [ ] Implement `evidenceId()` function
- [ ] Implement `computeSourceHash()` function

### Task 2: ProvenanceGraph
- [ ] Create `src/core/provenance/graph.ts`
- [ ] Implement `addNode()`
- [ ] Implement `addEdge()`
- [ ] Implement `derivedFrom()`
- [ ] Implement `getSources()` (transitive)
- [ ] Implement `checkStaleness()`
- [ ] Implement `toJSON()` / `fromJSON()`

### Task 3: Persistent Store
- [ ] Create `src/core/provenance/persistentStore.ts`
- [ ] Define `ProvenanceStore` interface
- [ ] Implement `FileProvenanceStore`
- [ ] Implement receipt query filters
- [ ] Implement pruning

### Task 4: Primitives
- [ ] Add `provenance-graph` primitive
- [ ] Add `provenance-trace` primitive
- [ ] Add `provenance-check-staleness` primitive
- [ ] Add `provenance-record` primitive
- [ ] Add all to compileText.ts

### Task 5: Oracle Integration
- [ ] Wire oracle calls to create receipts
- [ ] Wire oracle calls to create evidence
- [ ] Wire oracle calls to record in graph

### Verification
- [ ] All existing tests pass (1124+)
- [ ] New provenance tests pass
- [ ] REPL examples work

---

## Notes

### Why DAG Instead of Tree?

Multiple derivations can share sources:

```
Oracle A ──┬──▶ Derived X
           │
Oracle B ──┼──▶ Derived Y
           │
           └──▶ Derived Z (depends on X and Y)
```

A tree can't represent Z having two parents.

### Persistence Strategy

Start with `FileProvenanceStore` for simplicity. For production:
- SQLite for single-process (better-sqlite3)
- PostgreSQL for multi-process
- Consider append-only log for audit compliance

### Performance Considerations

- Provenance graphs can grow large
- Consider lazy loading of edges
- Consider bloom filters for staleness pre-check
- Consider background staleness checking

---

---

## Proof of Completion

When marking this job DONE:

1. **Prerequisite check**: `npx vitest run test/evidence/` - Job 004 Task 2 tests pass
2. **Build passes**: `npm run build` - no TypeScript errors
3. **Baseline tests pass**: `npm run test` - 1124+ tests still green
4. **New tests pass**:
   - `npx vitest run test/provenance/graph.spec.ts`
   - `npx vitest run test/provenance/store.spec.ts`
   - `npx vitest run test/provenance/staleness.spec.ts`
5. **Staleness detection verified**: Evidence correctly marked stale when source changes
6. **REPL verification**: Test examples from Verification section manually
7. **Update status**: Change `NOT STARTED` → `DONE` in this file
8. **Update README**: Mark Job 007 as DONE in [README.md](README.md)

---

*Created: 2026-01-19*
*Related: [004-IMPLEMENT-CORE-MAGIC.md](./004-IMPLEMENT-CORE-MAGIC.md)*
*Depends on: Job 004 Task 2 (evidence primitives)*
