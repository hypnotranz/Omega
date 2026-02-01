# 920: Logic Manager

## Status: COMPLETE ✅
- **Completed**: 2026-01-29
- **Location**: OmegaLLM-3/src/runtime/subsystems/LogicManager.ts (518 lines)

## Purpose
Implements logic programming with semantic facts - Prolog-style queries using LLM for inference.

## Dependencies
- 100-types.md ✅
- 110-events.md ✅
- 410-facts.md ✅
- 800-llm-integration.md ✅

## Source References
- SICP Chapter 4.4 (Logic Programming)
- docs/USER-MANUAL--27--Logic-Programming-With-Semantic-Facts.md
- Prolog unification concepts

---

## Deliverables

```
src/runtime/subsystems/
├── LogicManager.ts          # Main logic manager
└── logic/
    ├── Query.ts             # Query types
    ├── Unification.ts       # Unification logic
    ├── InferenceEngine.ts   # Forward/backward chaining
    └── SemanticMatcher.ts   # LLM-based pattern matching
```

---

## Key Types

```typescript
export interface Query {
  pattern: string;           // Query pattern with ?variables
  bindings?: Map<string, Val>; // Known bindings
  limit?: number;            // Max results
}

export interface QueryResult {
  success: boolean;
  bindings: Map<string, Val>[];
  inferenceChain?: InferenceStep[];
  llmCallCount: number;
}

export interface InferenceStep {
  type: 'fact' | 'rule' | 'llm';
  source: string;            // Fact content or rule name
  bindings: Map<string, Val>;
  confidence?: number;
}

export interface LogicRule {
  id: string;
  head: string;              // Conclusion pattern
  body: string[];            // Condition patterns (AND)
  confidence?: number;
}

export type InferenceMode = 'strict' | 'semantic' | 'hybrid';
```

---

## Key Interface

```typescript
export interface LogicManager {
  // ─── Rules ───

  /**
   * Define a logic rule.
   * head :- body1, body2, ...
   */
  defineRule(head: string, body: string[]): LogicRule;

  /**
   * Get all rules.
   */
  getRules(): LogicRule[];

  /**
   * Remove a rule.
   */
  removeRule(id: string): boolean;

  // ─── Queries ───

  /**
   * Query the knowledge base.
   */
  query(pattern: string, options?: {
    limit?: number;
    inferenceMode?: InferenceMode;
  }): Promise<QueryResult>;

  /**
   * Query with initial bindings.
   */
  queryWithBindings(
    pattern: string,
    bindings: Map<string, Val>
  ): Promise<QueryResult>;

  /**
   * Check if a statement can be proved.
   */
  prove(statement: string): Promise<boolean>;

  // ─── Unification ───

  /**
   * Unify two patterns, returning bindings.
   */
  unify(pattern1: string, pattern2: string): Map<string, Val> | null;

  /**
   * Semantic unification using LLM.
   */
  unifySemanticly(pattern: string, text: string): Promise<Map<string, Val> | null>;

  // ─── Inference ───

  /**
   * Forward chaining: derive new facts from existing.
   */
  forwardChain(): Promise<string[]>;

  /**
   * Backward chaining: try to prove goal.
   */
  backwardChain(goal: string): Promise<InferenceStep[]>;

  // ─── Mode ───

  /**
   * Set inference mode.
   * - strict: Pattern matching only
   * - semantic: LLM-based matching
   * - hybrid: Pattern first, LLM fallback
   */
  setInferenceMode(mode: InferenceMode): void;
}
```

---

## Unification

```typescript
/**
 * Pattern unification.
 * ?x matches anything and binds.
 */
function unify(pattern1: string, pattern2: string): Map<string, Val> | null {
  const bindings = new Map<string, Val>();
  const words1 = tokenize(pattern1);
  const words2 = tokenize(pattern2);

  if (words1.length !== words2.length) return null;

  for (let i = 0; i < words1.length; i++) {
    const w1 = words1[i];
    const w2 = words2[i];

    if (isVariable(w1)) {
      // ?x matches anything
      const varName = w1.slice(1);
      if (bindings.has(varName)) {
        // Already bound - must match
        if (bindings.get(varName) !== w2) return null;
      } else {
        bindings.set(varName, w2);
      }
    } else if (isVariable(w2)) {
      // Same for w2
      const varName = w2.slice(1);
      if (bindings.has(varName)) {
        if (bindings.get(varName) !== w1) return null;
      } else {
        bindings.set(varName, w1);
      }
    } else if (w1 !== w2) {
      // Must match exactly
      return null;
    }
  }

  return bindings;
}

function isVariable(token: string): boolean {
  return token.startsWith('?');
}
```

---

## Semantic Unification

```typescript
async function unifySemanticly(
  pattern: string,
  text: string
): Promise<Map<string, Val> | null> {
  // Extract variables from pattern
  const variables = extractVariables(pattern);

  if (variables.length === 0) {
    // No variables - just check semantic match
    const matches = await this.llm.infer(`
Does the following text match this pattern semantically?
Pattern: ${pattern}
Text: ${text}
Answer: yes or no
    `);
    return matches.toLowerCase().includes('yes') ? new Map() : null;
  }

  // Use LLM to extract bindings
  const result = await this.llm.infer(`
Extract values for variables from the text to match the pattern.
Pattern: ${pattern}
Text: ${text}
Variables: ${variables.join(', ')}

Return as JSON: {"var1": "value1", "var2": "value2"} or "no match"
  `);

  if (result.includes('no match')) return null;

  try {
    const json = JSON.parse(result);
    return new Map(Object.entries(json));
  } catch (e) {
    return null;
  }
}
```

---

## Backward Chaining

```typescript
async function backwardChain(goal: string): Promise<InferenceStep[]> {
  const chain: InferenceStep[] = [];

  // Try to match goal against facts
  const facts = this.factsManager.getAllFacts();
  for (const fact of facts) {
    const bindings = this.inferenceMode === 'semantic'
      ? await this.unifySemanticly(goal, fact.content)
      : this.unify(goal, fact.content);

    if (bindings) {
      chain.push({
        type: 'fact',
        source: fact.content,
        bindings
      });
      return chain;
    }
  }

  // Try rules
  for (const rule of this.rules) {
    const headBindings = this.unify(goal, rule.head);
    if (headBindings) {
      // Try to prove all conditions
      let allProved = true;
      const subChain: InferenceStep[] = [];

      for (const condition of rule.body) {
        const substituted = substitute(condition, headBindings);
        const result = await this.backwardChain(substituted);

        if (result.length === 0) {
          allProved = false;
          break;
        }
        subChain.push(...result);
      }

      if (allProved) {
        chain.push({
          type: 'rule',
          source: rule.id,
          bindings: headBindings
        });
        chain.push(...subChain);
        return chain;
      }
    }
  }

  // Use LLM as last resort in semantic mode
  if (this.inferenceMode !== 'strict') {
    const canProve = await this.llm.infer(`
Given these facts:
${facts.map(f => f.content).join('\n')}

Can you prove: ${goal}
Answer: yes/no and explain
    `);

    if (canProve.toLowerCase().includes('yes')) {
      chain.push({
        type: 'llm',
        source: 'LLM inference',
        bindings: new Map(),
        confidence: 0.7
      });
    }
  }

  return chain;
}
```

---

## Lisp Interface

```lisp
;; Define rules
(logic.rule "?x is mortal" ("?x is human"))
(logic.rule "?x is grandfather of ?z" ("?x is father of ?y" "?y is parent of ?z"))

;; Query
(logic.query "?who is mortal")
; => ((:bindings ((who . "Socrates"))) ...)

;; Prove
(logic.prove "Socrates is mortal")
; => #t

;; With facts
(fact! "Socrates is human")
(fact! "All humans are mortal")

(logic.query "?x is mortal" :mode 'semantic)
; => Uses LLM to understand "All humans are mortal" implies Socrates is mortal

;; Unification
(logic.unify "?x loves ?y" "Alice loves Bob")
; => ((x . "Alice") (y . "Bob"))

;; Set mode
(logic.set-mode 'hybrid)
```

---

## Test Requirements

### Unit Tests: `tests/runtime/subsystems/LogicManager.test.ts`
- [ ] defineRule() creates rule
- [ ] unify() matches patterns
- [ ] unify() extracts bindings
- [ ] unify() fails on mismatch
- [ ] query() finds matches
- [ ] queryWithBindings() uses initial bindings
- [ ] prove() returns boolean
- [ ] backwardChain() builds inference chain
- [ ] forwardChain() derives new facts
- [ ] Semantic unification works

### Integration Tests
- [ ] Full logic programming examples
- [ ] Family tree queries
- [ ] Rule chaining works
- [ ] LLM fallback for semantic
- [ ] Performance on large fact bases

---

## Acceptance Criteria
1. Pattern unification works correctly
2. Rules enable derived facts
3. Backward chaining proves goals
4. Semantic mode uses LLM for understanding
5. Hybrid mode uses patterns first
6. Inference chains are traceable
