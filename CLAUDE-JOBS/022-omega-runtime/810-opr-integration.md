# 810: OPR Integration

## Status: COMPLETE ✅
- **Completed**: 2026-01-29
- **Location**: OmegaLLM-3/src/runtime/subsystems/OPRIntegration.ts (493 lines)

## Purpose
Integrates OPR (Omega Program Representation) coprocessor for structured computation with LLMs.

## Dependencies
- 110-events.md ✅
- 120-providers.md ✅
- 210-execution.md ✅

## Source References
- OPR specification documents
- src/core/opr/ (existing OPR implementation)
- OPR-INTEGRATION-PLAN.md

---

## Deliverables

```
src/runtime/subsystems/
├── OPRIntegration.ts        # Main OPR integration
└── opr/
    ├── OPRAdapter.ts        # Adapts OPR to runtime
    ├── OPRSession.ts        # OPR session management
    └── OPRTypeMapper.ts     # Type mapping Lisp ↔ OPR
```

---

## Key Types

```typescript
export interface OPRNode {
  type: string;              // Node type
  id: string;                // Unique ID
  inputs: OPRPort[];
  outputs: OPRPort[];
  properties: Record<string, OPRValue>;
}

export interface OPRPort {
  id: string;
  name: string;
  dataType: OPRDataType;
  connected?: string;        // Connected port ID
}

export interface OPRGraph {
  id: string;
  nodes: OPRNode[];
  connections: OPRConnection[];
  inputs: OPRPort[];
  outputs: OPRPort[];
}

export interface OPRConnection {
  sourceNode: string;
  sourcePort: string;
  targetNode: string;
  targetPort: string;
}

export interface OPRExecutionResult {
  outputs: Map<string, OPRValue>;
  trace: OPRTraceEntry[];
  metadata: {
    duration: number;
    llmCalls: number;
    tokensUsed: number;
  };
}
```

---

## Key Interface

```typescript
export interface OPRIntegration {
  // ─── Graph Management ───

  /**
   * Load OPR graph from definition.
   */
  loadGraph(definition: OPRGraphDefinition): OPRGraph;

  /**
   * Compile Lisp expression to OPR graph.
   */
  compileToOPR(expr: Val): OPRGraph;

  /**
   * Execute OPR graph.
   */
  execute(graph: OPRGraph, inputs: Map<string, Val>): Promise<OPRExecutionResult>;

  // ─── Node Operations ───

  /**
   * Create OPR node.
   */
  createNode(type: string, properties?: Record<string, Val>): OPRNode;

  /**
   * Connect nodes.
   */
  connect(source: OPRPort, target: OPRPort): OPRConnection;

  // ─── Type Mapping ───

  /**
   * Convert Lisp value to OPR value.
   */
  lispToOPR(value: Val): OPRValue;

  /**
   * Convert OPR value to Lisp value.
   */
  oprToLisp(value: OPRValue): Val;

  // ─── Session ───

  /**
   * Create OPR session.
   */
  createSession(): OPRSession;

  /**
   * Get current session.
   */
  getCurrentSession(): OPRSession | undefined;
}
```

---

## OPR Node Types

| Node Type | Description | Inputs | Outputs |
|-----------|-------------|--------|---------|
| `llm.infer` | LLM inference | prompt | response |
| `llm.search` | LLM search | prompt, samples | distribution |
| `control.if` | Conditional | condition, then, else | result |
| `control.loop` | Loop | init, body, condition | result |
| `data.map` | Map operation | list, fn | list |
| `data.filter` | Filter operation | list, predicate | list |
| `data.reduce` | Reduce operation | list, fn, init | value |
| `io.read` | Read input | - | value |
| `io.write` | Write output | value | - |

---

## Compilation Example

```lisp
;; Lisp expression:
(map (lambda (x) (effect infer.op (list "Translate: " x)))
     '("hello" "goodbye"))

;; Compiles to OPR graph:
;;
;;  ┌─────────────┐
;;  │  io.read    │ ──► list
;;  └─────────────┘
;;         │
;;         ▼
;;  ┌─────────────┐
;;  │  data.map   │
;;  └─────────────┘
;;         │
;;         ▼
;;  ┌─────────────┐     ┌─────────────┐
;;  │ llm.infer   │ ◄── │ concat      │
;;  │ (per item)  │     │ "Translate: " + x
;;  └─────────────┘     └─────────────┘
;;         │
;;         ▼
;;  ┌─────────────┐
;;  │  io.write   │
;;  └─────────────┘
```

---

## Execution Trace

```typescript
interface OPRTraceEntry {
  nodeId: string;
  nodeType: string;
  timestamp: number;
  inputs: Map<string, OPRValue>;
  outputs: Map<string, OPRValue>;
  duration: number;
  metadata?: Record<string, unknown>;
}

// Execution emits trace events
emitter.emit('opr-node-start', { nodeId, inputs });
emitter.emit('opr-node-complete', { nodeId, outputs, duration });
emitter.emit('opr-graph-complete', { graphId, outputs, trace });
```

---

## Lisp Interface

```lisp
;; Load and execute OPR graph
(opr.load "path/to/graph.opr")
(opr.execute graph :inputs '((x . 10) (y . 20)))

;; Compile expression to OPR
(opr.compile '(+ 1 2))
; => OPR graph

;; View execution trace
(opr.trace last-result)
; => List of trace entries

;; Interactive graph building
(define node1 (opr.node 'llm.infer :prompt "..."))
(define node2 (opr.node 'data.map :fn ...))
(opr.connect node1.output node2.input)
```

---

## Test Requirements

### Unit Tests: `tests/runtime/subsystems/OPRIntegration.test.ts`
- [ ] loadGraph() loads valid graph
- [ ] compileToOPR() compiles simple expressions
- [ ] execute() runs graph correctly
- [ ] createNode() creates valid nodes
- [ ] connect() creates valid connections
- [ ] lispToOPR() converts correctly
- [ ] oprToLisp() converts correctly
- [ ] Trace captures all node executions

### Integration Tests
- [ ] Full graph execution with LLM nodes
- [ ] Conditional execution works
- [ ] Loop execution works
- [ ] Map/filter/reduce work
- [ ] Large graphs execute efficiently

---

## Acceptance Criteria
1. OPR graphs can be loaded and executed
2. Lisp expressions compile to OPR
3. Type mapping is bidirectional
4. Execution trace is complete
5. LLM nodes integrate with budget system
6. Performance comparable to direct execution
