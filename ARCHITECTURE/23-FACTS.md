# ⚠️ COMPLETE REDESIGN REQUIRED

> ## Primitives Defined (Lisp Forms)
>
> | Form | Type | Current Implementation |
> |------|------|----------------------|
> | `(assert 'expr)` | Special Form | `evalExpr` case (line 279) |
> | `(assert 'expr :evidence ev)` | Special Form | `evalExpr` case (line 281) |
> | `(assert-if-absent 'expr)` | Special Form | Alias for assert |
> | `(fact? 'expr)` | Special Form | `evalExpr` case (line 290) |
> | `(facts)` | Special Form | `evalExpr` case (line 297) |
> | `(fact/get 'expr)` | FFI | TypeScript function |
> | `(facts/count)` | FFI | TypeScript function |
>
> ## Current Implementation
>
> **Special forms** in evalExpr + **FactStore in Environment** (not State):
> ```typescript
> // evalExpr cases (lines 279-300):
> if (isAssert(expr)) {
>   const factExpr = evalExpr(expr[1], env, cont, ffi);
>   const isNew = env.facts.assert(factExpr, evidence);  // ← env.facts, NOT State
>   return applyCont(cont, isNew);
> }
>
> // Environment holds factStore (lines 203-228):
> class Environment {
>   private factStore?: FactStore;  // Only at root
>   get facts(): FactStore { ... }  // Walks to root
> }
> ```
>
> ## CEKS Redesign Instructions
>
> ### 1. Move FactStore to State (not Environment):
> ```typescript
> type State = {
>   control: Control;
>   env: Ctx;
>   store: Store;
>   kont: Kont;
>   facts: FactStore;  // ← IN State, accessible everywhere
>   // ...
> };
> ```
>
> ### 2. Handle (assert) in step() function:
> ```typescript
> function step(s: State): StepOutcome {
>   if (s.control.tag === 'expr' && isAssert(s.control.expr)) {
>     const [_, factExpr, ...opts] = s.control.expr;
>     // Push AssertK frame, evaluate factExpr first
>     return {
>       ...s,
>       control: { tag: 'expr', expr: factExpr },
>       kont: [{ tag: 'AssertK', evidence: extractEvidence(opts) }, ...s.kont],
>     };
>   }
>
>   // When AssertK frame pops with value:
>   if (s.kont[0]?.tag === 'AssertK' && s.control.tag === 'value') {
>     const frame = s.kont[0] as AssertKFrame;
>     const isNew = s.facts.assert(s.control.value, frame.evidence);
>     return {
>       ...s,
>       control: { tag: 'value', value: isNew },
>       kont: s.kont.slice(1),
>     };
>   }
> }
> ```
>
> ### 3. Subeval creates snapshot, restores on exit:
> ```typescript
> // When entering subeval:
> if (isSubeval(s.control.expr)) {
>   const factsSnapshot = s.facts.clone();
>   return {
>     ...s,
>     kont: [{ tag: 'SubevalK', parentFacts: factsSnapshot }, ...s.kont],
>     facts: new FactStore(),  // Fresh isolated store
>   };
> }
>
> // When SubevalK frame pops:
> if (s.kont[0]?.tag === 'SubevalK' && s.control.tag === 'value') {
>   const frame = s.kont[0] as SubevalKFrame;
>   return {
>     ...s,
>     facts: frame.parentFacts,  // Restore parent facts (isolated didn't leak)
>     kont: s.kont.slice(1),
>   };
> }
> ```
>
> ### 4. Keep FFI for inspection helpers:
> - `fact/get` - returns fact with metadata
> - `facts/count` - returns count
>
> ## References
> - See State structure in 32-6 CEKS specification
> - See monotone lattice semantics
> - See [ARCHITECTURE-REDESIGN-ASSESSMENT.md](../docs/ARCHITECTURE-REDESIGN-ASSESSMENT.md)

---

# 23: Facts (Monotone Epistemic State)

## The Problem with Booleans

Traditional programming uses booleans for knowledge:

```javascript
if (fileExists) {
  doSomething();
}
```

Problems:
- Boolean is forgotten after the `if`
- No record that we checked
- Can't query "what do we know?"
- Can't share knowledge across phases

---

## Facts as First-Class Knowledge

Facts are **propositions that accumulate**. Once asserted, they persist.

```lisp
;; Assert a fact (never retracts)
(assert '(file-exists "auth.ts"))

;; Check if fact is known
(fact? '(file-exists "auth.ts"))  ;; => #t

;; Query all known facts
(facts)  ;; => ((file-exists "auth.ts") (module-loaded "core") ...)
```

---

## TypeScript Implementation

```typescript
interface Fact {
  // The proposition (canonicalized)
  proposition: CanonicalKey;

  // The original expression (for display)
  expr: Value;

  // Optional evidence supporting this fact
  evidence?: EvidenceId;

  // When the fact was asserted
  assertedAt: number;

  // Assertion context
  assertedIn?: string;  // Namespace or subeval ID
}

class FactStore {
  // Set of canonical keys (for efficient membership)
  private keys: Set<CanonicalKey> = new Set();

  // Map to full fact objects (for inspection)
  private facts: Map<CanonicalKey, Fact> = new Map();

  // Assert a new fact
  assert(expr: Value, evidence?: EvidenceId): boolean {
    const key = toCanonicalKey(expr);

    // Already known - idempotent
    if (this.keys.has(key)) {
      return false;  // Not newly added
    }

    const fact: Fact = {
      proposition: key,
      expr,
      evidence,
      assertedAt: Date.now(),
    };

    this.keys.add(key);
    this.facts.set(key, fact);
    return true;  // Newly added
  }

  // Assert only if not already known
  assertIfAbsent(expr: Value, evidence?: EvidenceId): boolean {
    return this.assert(expr, evidence);  // Same behavior, explicit intent
  }

  // Check if fact is known
  has(expr: Value): boolean {
    const key = toCanonicalKey(expr);
    return this.keys.has(key);
  }

  // Get all facts as expressions
  all(): Value[] {
    return Array.from(this.facts.values()).map(f => f.expr);
  }

  // Get fact with evidence
  get(expr: Value): Fact | undefined {
    const key = toCanonicalKey(expr);
    return this.facts.get(key);
  }

  // Compute signature for fixpoint detection
  signature(): string {
    const sortedKeys = Array.from(this.keys).sort();
    return sha256(JSON.stringify(sortedKeys));
  }

  // Count (for metrics)
  get size(): number {
    return this.keys.size;
  }
}
```

### Canonical Keys

Facts are compared by structure, not identity:

```typescript
// Convert expression to canonical key for comparison
function toCanonicalKey(expr: Value): CanonicalKey {
  if (typeof expr === 'number') return `n:${expr}`;
  if (typeof expr === 'string') return `s:${expr}`;
  if (typeof expr === 'boolean') return `b:${expr}`;
  if (expr === null) return 'null';
  if (typeof expr === 'symbol') return `sym:${Symbol.keyFor(expr)}`;

  if (Array.isArray(expr)) {
    const elements = expr.map(toCanonicalKey).join(',');
    return `[${elements}]`;
  }

  throw new Error(`Cannot canonicalize: ${typeof expr}`);
}

// Examples:
// toCanonicalKey([Symbol.for('file-exists'), "auth.ts"])
// => "[sym:file-exists,s:auth.ts]"
```

---

## Environment Integration

Facts are stored at the **root environment** for global visibility:

```typescript
class Environment {
  private bindings: Map<string, Value> = new Map();
  private parent?: Environment;
  private factStore?: FactStore;  // Only at root

  // Get fact store (walks to root)
  get facts(): FactStore {
    if (this.factStore) return this.factStore;
    if (this.parent) return this.parent.facts;
    throw new Error('No fact store in environment chain');
  }

  // Create root environment with fact store
  static createRoot(): Environment {
    const env = new Environment();
    env.factStore = new FactStore();
    return env;
  }

  // Extend creates child that shares parent's fact store
  extend(): Environment {
    const child = new Environment();
    child.parent = this;
    // factStore is inherited via getter
    return child;
  }

  // Compute state signature (for fixpoint)
  signature(options: SignatureOptions = {}): string {
    const parts: string[] = [];

    // Always include facts
    parts.push(`facts:${this.facts.signature()}`);

    // Optionally include scalar bindings
    if (options.includeBindings) {
      const bindingSig = this.computeBindingSignature();
      parts.push(`bindings:${bindingSig}`);
    }

    return sha256(parts.join('|'));
  }
}
```

---

## Lisp Primitives

```lisp
;; Assert fact (unconditional)
(assert '(tag value))
(assert '(tag value) :evidence ev)  ;; With evidence

;; Assert if not already known (idempotent)
(assert-if-absent '(tag value))

;; Check if fact exists
(fact? '(tag value))  ;; => #t or #f

;; Get all facts
(facts)  ;; => list of all fact expressions

;; Get fact with metadata
(fact/get '(tag value))  ;; => {:expr (...) :evidence ev :assertedAt ...}

;; Count facts (for debugging)
(facts/count)  ;; => number
```

### Implementation in Evaluator

```typescript
// In evalExpr for special forms

// (assert expr) or (assert expr :evidence ev)
if (isAssert(expr)) {
  const factExpr = evalExpr(expr[1], env, cont, ffi);
  const evidence = expr.length > 3 ? evalExpr(expr[3], env, cont, ffi) : undefined;

  const isNew = env.facts.assert(factExpr, evidence);
  metrics.increment('facts_added', isNew ? 1 : 0);

  return applyCont(cont, isNew);
}

// (fact? expr)
if (isFactQuery(expr)) {
  const factExpr = evalExpr(expr[1], env, cont, ffi);
  const exists = env.facts.has(factExpr);
  return applyCont(cont, exists);
}

// (facts)
if (isFacts(expr)) {
  const allFacts = env.facts.all();
  return applyCont(cont, allFacts);
}
```

---

## Monotonicity: Why Facts Never Retract

Facts are **monotone** - they only grow, never shrink. This is intentional:

| Mutable State | Monotone Facts |
|---------------|----------------|
| Can be inconsistent | Always consistent |
| Race conditions | No conflicts |
| Hard to reason about | Simple model |
| Retraction complexity | Just accumulation |
| "What do we know now?" | "What have we learned?" |

### Use Cases for Monotonicity

```lisp
;; Phase gating: Can't proceed until prerequisites met
(unless (fact? '(analysis-complete))
  (error "Must complete analysis before planning"))

;; Knowledge accumulation across iterations
(fixpoint
  (begin
    (let ((new-findings (analyze-code)))
      (for-each (lambda (f) (assert `(finding ,f))) new-findings))
    (analyze-code)))  ;; Keeps running until no new findings

;; Conditional logic based on accumulated knowledge
(when (fact? '(has-security-vulnerability))
  (assert '(requires-security-review)))
```

---

## Subeval Isolation

Subeval creates **isolated fact stores** - facts don't leak:

```typescript
function evalSubeval(
  expr: Value,
  parentEnv: Environment,
  cont: Continuation,
  ffi: FFI
): Value {
  // Create isolated root with fresh fact store
  const isolatedEnv = Environment.createRoot();

  // Copy bindings from parent (optional)
  copyBindings(parentEnv, isolatedEnv);

  // Evaluate in isolated context
  const result = evalExpr(expr, isolatedEnv, cont, ffi);

  // Facts asserted in subeval do NOT propagate to parent
  // This is intentional - subeval is for isolated reasoning

  return result;
}
```

```lisp
;; Facts in subeval are isolated
(assert '(in-parent))

(subeval
  (begin
    (assert '(in-subeval))
    (fact? '(in-parent))    ;; => #f (parent facts not visible)
    (fact? '(in-subeval)))) ;; => #t

(fact? '(in-subeval))  ;; => #f (subeval facts don't leak out)
(fact? '(in-parent))   ;; => #t (still there)
```

---

## Facts with Evidence

Facts can carry evidence for provenance:

```lisp
;; Capture evidence
(define ev (evidence/capture "src/auth.ts" :lines 42 50))

;; Assert fact with evidence
(assert '(bug-at "auth.ts" 42) :evidence ev)

;; Later, retrieve evidence for fact
(let ((fact-info (fact/get '(bug-at "auth.ts" 42))))
  (when fact-info
    (print (str "Evidence: " (fact-info :evidence)))))
```

---

## Pattern: Phase Gating

Use facts to enforce execution phases:

```lisp
;; Phase 1: Analysis
(define (analyze)
  (let ((files (world.list "src/**/*.ts")))
    (for-each
      (lambda (file)
        (let ((content (world.read file)))
          ;; Analyze and assert findings
          (when (contains? content "eval(")
            (assert `(security-issue ,file "eval usage")))))
      files))
  (assert '(phase-complete analysis)))

;; Phase 2: Report (requires analysis phase)
(define (report)
  (unless (fact? '(phase-complete analysis))
    (error "Analysis phase must complete first"))

  ;; Generate report from accumulated facts
  (let ((issues (filter (lambda (f) (eq? (car f) 'security-issue))
                        (facts))))
    (generate-report issues))
  (assert '(phase-complete report)))

;; Execute in order
(analyze)
(report)
```

---

## Fixpoint Integration

Facts enable fixpoint convergence detection:

```typescript
function computeStateSignature(
  env: Environment,
  world: World,
  mode: SignatureMode
): string {
  const parts: string[] = [];

  // Facts always included
  parts.push(`facts:${env.facts.signature()}`);

  // Optional components based on mode
  if (mode.includeBindings) {
    parts.push(`bindings:${env.signature({ includeBindings: true })}`);
  }

  if (mode.includeWorld) {
    parts.push(`world:${world.fingerprint()}`);
  }

  return sha256(parts.join('|'));
}

// Fixpoint loop
function evalFixpoint(expr, env, maxIters, mode): FixpointResult {
  const seen = new Set<string>();
  let lastValue: Value;

  for (let i = 0; i < maxIters; i++) {
    lastValue = evalExpr(expr, env, ...);

    const sig = computeStateSignature(env, world, mode);

    // Convergence: same signature as previous iteration
    if (seen.has(sig)) {
      return { status: 'converged', value: lastValue, iterations: i };
    }

    seen.add(sig);
  }

  return { status: 'nonconverged', value: lastValue, iterations: maxIters };
}
```

---

## Summary

Facts provide:

1. **Monotone knowledge** - Only grows, never retracts
2. **First-class propositions** - Data, not just booleans
3. **Evidence attachment** - Provenance integration
4. **Phase gating** - Enforce execution order
5. **Fixpoint detection** - State signature for convergence
6. **Subeval isolation** - Isolated reasoning without fact leakage

Facts replace ad-hoc boolean state with a principled epistemic system.
