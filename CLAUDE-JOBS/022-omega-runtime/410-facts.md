# 410: Facts Manager

## Status: COMPLETE ✅
- **Completed**: 2026-01-29
- **Location**: OmegaLLM-3/src/runtime/subsystems/FactsManager.ts (362 lines)

## Purpose
Manages the fact store - asserting, retracting, and querying semantic facts for logic programming.

## Dependencies
- 100-types.md ✅
- 110-events.md ✅

## Source References
- ARCHITECTURE/23-FACTS.md
- docs/USER-MANUAL--27--Logic-Programming-With-Semantic-Facts.md
- SICP Chapter 4.4 (Logic Programming)

---

## Deliverables

```
src/runtime/subsystems/
├── FactsManager.ts          # Main facts manager
└── facts/
    ├── Fact.ts              # Fact types
    ├── FactIndex.ts         # Indexing for queries
    └── FactPatternMatcher.ts # Pattern matching
```

---

## Key Types

```typescript
export interface Fact {
  id: string;
  content: string;           // Natural language fact
  assertedAt: number;
  source?: string;           // Where fact came from
  confidence?: number;       // 0-1 confidence level
  tags?: string[];
  metadata?: Record<string, Val>;
}

export interface FactQuery {
  pattern?: string;          // Text pattern to match
  tags?: string[];           // Filter by tags
  minConfidence?: number;    // Minimum confidence
  limit?: number;            // Max results
}

export interface FactRule {
  id: string;
  condition: string;         // Natural language condition
  conclusion: string;        // Natural language conclusion
}

export interface QueryResult {
  facts: Fact[];
  bindings?: Map<string, Val>; // Variable bindings from pattern
  inferredFrom?: string[];     // Facts used in inference
}
```

---

## Key Interface

```typescript
export interface FactsManager {
  /**
   * Assert a new fact.
   */
  assert(content: string, options?: {
    source?: string;
    confidence?: number;
    tags?: string[];
  }): Fact;

  /**
   * Assert multiple facts.
   */
  assertAll(facts: string[]): Fact[];

  /**
   * Retract a fact by ID.
   */
  retract(id: string): boolean;

  /**
   * Retract facts matching pattern.
   */
  retractMatching(pattern: string): number;

  /**
   * Query facts.
   */
  query(query: FactQuery): QueryResult;

  /**
   * Ask a question about facts (uses LLM).
   */
  ask(question: string): Promise<string>;

  /**
   * Check if fact can be derived.
   */
  canDerive(statement: string): Promise<boolean>;

  /**
   * Define an inference rule.
   */
  defineRule(condition: string, conclusion: string): FactRule;

  /**
   * Get all facts.
   */
  getAllFacts(): Fact[];

  /**
   * Get all rules.
   */
  getAllRules(): FactRule[];

  /**
   * Clear all facts.
   */
  clearFacts(): void;

  /**
   * Clear all rules.
   */
  clearRules(): void;

  /**
   * Export facts to JSON.
   */
  export(): string;

  /**
   * Import facts from JSON.
   */
  import(json: string): void;
}
```

---

## Semantic Query Implementation

```typescript
class FactsManagerImpl implements FactsManager {
  async ask(question: string): Promise<string> {
    const facts = this.getAllFacts();

    if (facts.length === 0) {
      return 'unknown';
    }

    const factText = facts.map(f => f.content).join('\n');

    return this.llm.complete(`
Based ONLY on these facts:
${factText}

Answer: ${question}
If the answer cannot be determined from the facts, say 'unknown'.
    `.trim());
  }

  async canDerive(statement: string): Promise<boolean> {
    const answer = await this.ask(`Can you determine if this is true: ${statement}`);
    const normalized = answer.toLowerCase();
    return normalized.includes('yes') || normalized.includes('true');
  }
}
```

---

## Pattern Matching

```typescript
/**
 * Match pattern against fact text.
 * Supports variables like ?x, ?y.
 */
function matchPattern(pattern: string, fact: string): Map<string, string> | null {
  // Extract variables from pattern
  const varRegex = /\?(\w+)/g;
  const variables: string[] = [];
  let match;
  while ((match = varRegex.exec(pattern)) !== null) {
    variables.push(match[1]);
  }

  if (variables.length === 0) {
    // Simple substring match
    return fact.toLowerCase().includes(pattern.toLowerCase())
      ? new Map()
      : null;
  }

  // Build regex from pattern
  const regexPattern = pattern
    .replace(/[.*+?^${}()|[\]\\]/g, '\\$&') // Escape regex chars
    .replace(/\\\?(\w+)/g, '(.+?)');         // Replace ?var with capture

  const regex = new RegExp(regexPattern, 'i');
  const result = fact.match(regex);

  if (!result) return null;

  const bindings = new Map<string, string>();
  variables.forEach((v, i) => {
    bindings.set(v, result[i + 1]);
  });
  return bindings;
}
```

---

## Lisp Interface

```lisp
;; Assert facts
(fact! "Marie Curie won the Nobel Prize in Physics in 1903")
(fact! "Marie Curie won the Nobel Prize in Chemistry in 1911")

;; Query facts
(facts.query :pattern "Nobel Prize")
; => ((:id "f1" :content "Marie Curie won...") ...)

;; Ask questions
(facts.ask "How many Nobel Prizes did Marie Curie win?")
; => "Two - one in Physics and one in Chemistry"

;; Define rules
(facts.rule "someone wins multiple Nobel Prizes"
            "they are exceptionally accomplished")

;; Derive
(facts.derive? "Marie Curie is exceptionally accomplished")
; => #t

;; Retract
(fact- "f1")
```

---

## Event Emission

```typescript
// When fact is asserted
emitter.emit('fact-asserted', { fact });

// When fact is retracted
emitter.emit('fact-retracted', { factId, content });

// When query is performed
emitter.emit('fact-query', { query, resultCount });

// When rule fires
emitter.emit('rule-fired', { rule, matchedFacts, conclusion });
```

---

## Test Requirements

### Unit Tests: `tests/runtime/subsystems/FactsManager.test.ts`
- [ ] assert() creates fact with ID
- [ ] assert() handles options correctly
- [ ] retract() removes fact
- [ ] retractMatching() removes matching
- [ ] query() filters correctly
- [ ] Pattern matching extracts variables
- [ ] Pattern matching handles no-match
- [ ] defineRule() creates rule
- [ ] export()/import() round-trip
- [ ] Tags filter correctly

### Integration Tests
- [ ] ask() uses LLM correctly
- [ ] canDerive() infers from facts
- [ ] Rules trigger on matching facts
- [ ] Multiple rules chain together
- [ ] Closed world assumption works

---

## Acceptance Criteria
1. Facts can be asserted and queried
2. Pattern matching extracts variable bindings
3. LLM-based ask() answers from facts only
4. Rules enable derived facts
5. Closed world assumption: unknown = false
6. Events enable tracking of fact changes
