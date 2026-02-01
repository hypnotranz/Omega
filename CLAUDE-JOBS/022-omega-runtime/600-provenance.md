# 600: Provenance Manager

## Status: COMPLETE ✅
- **Completed**: 2026-01-29
- **Location**: OmegaLLM-3/src/runtime/subsystems/ProvenanceManager.ts (490 lines)

## Purpose
Tracks the origin and derivation of values - where data came from, how it was computed, and what inputs influenced the result.

## Dependencies
- 100-types.md ✅
- 120-providers.md ✅

## Source References
- ARCHITECTURE/22-PROVENANCE.md
- W3C PROV Data Model
- Scientific workflow provenance systems

---

## Deliverables

```
src/runtime/subsystems/
├── ProvenanceManager.ts     # Main provenance manager
└── provenance/
    ├── ProvenanceGraph.ts   # DAG of derivations
    ├── ProvenanceNode.ts    # Node types
    └── ProvenanceQuery.ts   # Query/trace logic
```

---

## Key Types

```typescript
export type ProvenanceNodeType =
  | 'value'      // A computed value
  | 'input'      // User input
  | 'llm-call'   // LLM operation
  | 'function'   // Function application
  | 'artifact'   // Cached artifact
  | 'fact'       // Asserted fact
  | 'external';  // External data source

export interface ProvenanceNode {
  id: string;
  type: ProvenanceNodeType;
  timestamp: number;

  // What was produced
  value?: Val;
  valueHash?: string;        // Content hash

  // Derivation info
  operation?: string;        // Function/operation name
  inputs?: string[];         // IDs of input nodes
  parameters?: Record<string, Val>;

  // LLM-specific
  prompt?: string;
  model?: string;
  temperature?: number;

  // Metadata
  source?: string;           // File/location
  confidence?: number;
  tags?: string[];
}

export interface ProvenanceEdge {
  from: string;              // Input node ID
  to: string;                // Output node ID
  relation: 'derived-from' | 'influenced-by' | 'cached-from';
}

export interface ProvenanceTrace {
  rootId: string;
  nodes: ProvenanceNode[];
  edges: ProvenanceEdge[];
  depth: number;
}
```

---

## Key Interface

```typescript
export interface ProvenanceManager {
  /**
   * Record a new provenance node.
   */
  record(node: Omit<ProvenanceNode, 'id' | 'timestamp'>): ProvenanceNode;

  /**
   * Record derivation relationship.
   */
  recordDerivation(outputId: string, inputIds: string[]): void;

  /**
   * Get provenance node by ID.
   */
  getNode(id: string): ProvenanceNode | undefined;

  /**
   * Trace provenance back to origins.
   */
  trace(valueId: string, maxDepth?: number): ProvenanceTrace;

  /**
   * Find all values derived from a source.
   */
  findDerived(sourceId: string): string[];

  /**
   * Find all LLM calls that influenced a value.
   */
  findLLMInfluences(valueId: string): ProvenanceNode[];

  /**
   * Check if value was influenced by LLM.
   */
  hasLLMInfluence(valueId: string): boolean;

  /**
   * Get nodes by type.
   */
  getNodesByType(type: ProvenanceNodeType): ProvenanceNode[];

  /**
   * Export provenance graph.
   */
  export(format: 'json' | 'dot' | 'prov-json'): string;

  /**
   * Clear provenance data.
   */
  clear(): void;

  /**
   * Attach provenance to a value.
   */
  attach(value: Val, provenanceId: string): Val;

  /**
   * Get provenance ID attached to value.
   */
  getAttached(value: Val): string | undefined;
}
```

---

## Automatic Recording

```typescript
// Wrap LLM calls to record provenance
async function llmCallWithProvenance(
  prompt: string,
  params: LLMParams
): Promise<Val> {
  const inputNode = provenanceManager.record({
    type: 'input',
    value: prompt,
    operation: 'prompt'
  });

  const result = await llmProvider.complete(prompt, params);

  const outputNode = provenanceManager.record({
    type: 'llm-call',
    value: result.content,
    operation: 'infer.op',
    prompt,
    model: params.model,
    temperature: params.temperature
  });

  provenanceManager.recordDerivation(outputNode.id, [inputNode.id]);

  return provenanceManager.attach(result.content, outputNode.id);
}

// Wrap function application
function applyWithProvenance(
  fn: Closure,
  args: Val[]
): Val {
  const argIds = args.map(a => provenanceManager.getAttached(a)).filter(Boolean);

  const result = apply(fn, args);

  const outputNode = provenanceManager.record({
    type: 'function',
    value: result,
    operation: fn.name || '<anonymous>'
  });

  if (argIds.length > 0) {
    provenanceManager.recordDerivation(outputNode.id, argIds);
  }

  return provenanceManager.attach(result, outputNode.id);
}
```

---

## Provenance Trace Visualization

```
trace(result) =>

┌─────────────────────────────────────────────────────────────┐
│ result (value)                                               │
│ "The capital of France is Paris"                            │
└───────────────────────┬─────────────────────────────────────┘
                        │ derived-from
                        ▼
┌─────────────────────────────────────────────────────────────┐
│ infer.op (llm-call)                                         │
│ prompt: "What is the capital of France?"                    │
│ model: "gpt-4"                                              │
└───────────────────────┬─────────────────────────────────────┘
                        │ derived-from
                        ▼
┌─────────────────────────────────────────────────────────────┐
│ user-input (input)                                          │
│ "What is the capital of France?"                            │
└─────────────────────────────────────────────────────────────┘
```

---

## Lisp Interface

```lisp
;; Trace a value
(provenance.trace result)
; => (:nodes ((...) (...)) :edges ((...)))

;; Check LLM influence
(provenance.llm-influenced? value)
; => #t

;; Get all LLM calls in derivation
(provenance.llm-calls value)
; => ((:prompt "..." :model "gpt-4") ...)

;; Export as DOT graph
(provenance.export :format 'dot)
; => "digraph { ... }"
```

---

## Test Requirements

### Unit Tests: `tests/runtime/subsystems/ProvenanceManager.test.ts`
- [ ] record() creates node with ID
- [ ] recordDerivation() creates edges
- [ ] getNode() retrieves by ID
- [ ] trace() follows edges correctly
- [ ] trace() respects maxDepth
- [ ] findDerived() finds descendants
- [ ] findLLMInfluences() finds LLM nodes
- [ ] hasLLMInfluence() detects influence
- [ ] attach()/getAttached() work correctly
- [ ] export() produces valid output

### Integration Tests
- [ ] LLM calls auto-record provenance
- [ ] Function applications record
- [ ] Trace shows full derivation chain
- [ ] Large graphs export correctly
- [ ] Provenance survives serialization

---

## Acceptance Criteria
1. All values can be traced to their origins
2. LLM influences are clearly identified
3. Provenance graph is complete and accurate
4. Export formats are standards-compliant
5. Performance: <10ms for typical traces
6. Storage scales with session complexity
