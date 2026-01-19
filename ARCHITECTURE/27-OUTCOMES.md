# 27: Outcomes (Structured Return Types)

## The Problem with Exceptions

Traditional error handling loses information:

```javascript
try {
  result = await analyze();
} catch (e) {
  // What kind of failure? Timeout? Budget? Logic error?
  // What partial progress was made?
  // Can we recover?
}
```

**Outcomes solve this** by making success, failure, and partial results first-class.

---

## Outcome Types

Every significant computation returns a tagged outcome:

```typescript
type Outcome =
  | OkOutcome           // Successful completion
  | ProposedOutcome     // Proposed but not committed
  | NonconvergedOutcome // Budget/iteration exceeded
  | CycleOutcome        // Fixpoint oscillation detected
  | NeedsOutcome        // Blocked, needs something
  | ErrorOutcome        // Explicit failure
  ;

interface OkOutcome {
  tag: 'ok';
  value: Value;
  metadata: OutcomeMetadata;
}

interface ProposedOutcome {
  tag: 'proposed';
  value: Value;
  proposals: Proposal[];  // Pending world changes
  metadata: OutcomeMetadata;
}

interface NonconvergedOutcome {
  tag: 'nonconverged';
  value: Value;           // Last computed value
  iterations: number;
  reason: 'budget' | 'max-iterations';
  metadata: OutcomeMetadata;
}

interface CycleOutcome {
  tag: 'cycle';
  value: Value;
  cycleLength: number;
  signatures: string[];   // For debugging
  metadata: OutcomeMetadata;
}

interface NeedsOutcome {
  tag: 'needs';
  needType: NeedType;
  description: string;
  context: Value;
  metadata: OutcomeMetadata;
}

interface ErrorOutcome {
  tag: 'error';
  message: string;
  errorType: string;
  stacktrace?: string[];
  metadata: OutcomeMetadata;
}

interface OutcomeMetadata {
  iterations?: number;
  tokens?: number;
  cost?: number;
  timeMs?: number;
  signature?: string;
}
```

---

## Need Types

When computation is blocked, outcomes specify what's needed:

```typescript
type NeedType =
  | 'needs-evidence'      // Cannot proceed without facts
  | 'needs-reframing'     // Problem needs human guidance
  | 'needs-primitive'     // Missing language feature
  | 'needs-clarification' // Ambiguous requirement
  | 'needs-approval'      // Requires human sign-off
  ;

// Examples:
// { tag: 'needs', needType: 'needs-evidence', description: 'Cannot verify claim without file access' }
// { tag: 'needs', needType: 'needs-reframing', description: 'Multiple valid interpretations' }
```

---

## TypeScript Implementation

```typescript
// Outcome constructors
function ok(value: Value, metadata?: Partial<OutcomeMetadata>): OkOutcome {
  return {
    tag: 'ok',
    value,
    metadata: { ...metadata },
  };
}

function proposed(
  value: Value,
  proposals: Proposal[],
  metadata?: Partial<OutcomeMetadata>
): ProposedOutcome {
  return {
    tag: 'proposed',
    value,
    proposals,
    metadata: { ...metadata },
  };
}

function nonconverged(
  value: Value,
  iterations: number,
  reason: 'budget' | 'max-iterations',
  metadata?: Partial<OutcomeMetadata>
): NonconvergedOutcome {
  return {
    tag: 'nonconverged',
    value,
    iterations,
    reason,
    metadata: { ...metadata },
  };
}

function needs(
  needType: NeedType,
  description: string,
  context?: Value
): NeedsOutcome {
  return {
    tag: 'needs',
    needType,
    description,
    context: context ?? null,
    metadata: {},
  };
}

// Type guards
function isOk(outcome: Outcome): outcome is OkOutcome {
  return outcome.tag === 'ok';
}

function isSuccess(outcome: Outcome): boolean {
  return outcome.tag === 'ok' || outcome.tag === 'proposed';
}

function isBlocked(outcome: Outcome): boolean {
  return outcome.tag === 'needs' || outcome.tag === 'error';
}
```

---

## Lisp Integration

```lisp
;; Outcomes are tagged lists
;; (ok value metadata)
;; (proposed value proposals metadata)
;; (nonconverged value iterations reason metadata)
;; (cycle value cycle-length metadata)
;; (needs need-type description context)
;; (error message error-type)

;; Pattern matching on outcomes
(match (fixpoint/outcome body :max-iters 10)
  [(ok value _)
   (print (str "Success: " value))]

  [(proposed value proposals _)
   (print (str "Proposed " (length proposals) " changes"))
   (when (approve-proposals? proposals)
     (commit-proposals proposals))]

  [(nonconverged value iters reason _)
   (print (str "Failed to converge after " iters " iterations"))
   (if (eq? reason 'budget)
       (signal 'needs-more-budget)
       value)]

  [(cycle value len _)
   (print (str "Oscillation detected, cycle length: " len))
   (break-cycle value)]

  [(needs type desc ctx)
   (print (str "Blocked: " desc))
   (request-help type ctx)]

  [(error msg _)
   (print (str "Error: " msg))
   nil])
```

---

## Proposals (Transaction Mode)

Proposals represent uncommitted world changes:

```typescript
interface Proposal {
  type: 'write' | 'delete' | 'run';
  ref: string;
  content?: string;     // For writes
  command?: string;     // For run
  timestamp: number;
}

class StagedWorld implements World {
  private base: World;
  private proposals: Proposal[] = [];
  private staged: Map<string, string> = new Map();

  constructor(base: World) {
    this.base = base;
  }

  read(ref: string): string {
    // Check staged first
    if (this.staged.has(ref)) {
      return this.staged.get(ref)!;
    }
    return this.base.read(ref);
  }

  write(ref: string, content: string): void {
    // Stage the write, don't commit
    this.staged.set(ref, content);
    this.proposals.push({
      type: 'write',
      ref,
      content,
      timestamp: Date.now(),
    });
  }

  fingerprint(ref: string): string {
    if (this.staged.has(ref)) {
      return sha256(this.staged.get(ref)!);
    }
    return this.base.fingerprint(ref);
  }

  getProposals(): Proposal[] {
    return [...this.proposals];
  }

  commit(): void {
    for (const proposal of this.proposals) {
      if (proposal.type === 'write') {
        this.base.write(proposal.ref, proposal.content!);
      }
    }
    this.proposals = [];
    this.staged.clear();
  }

  rollback(): void {
    this.proposals = [];
    this.staged.clear();
  }
}
```

```lisp
;; Transaction mode execution
(define result
  (with-transaction
    (world.write "config.json" new-config)
    (world.write "package.json" new-package)
    (validate-changes)))

;; Result is proposed outcome with proposals
(match result
  [(proposed value proposals _)
   (print "Changes ready to commit:")
   (for-each print-proposal proposals)
   (when (user-approves?)
     (commit-proposals proposals))]
  [_ (print "No proposals generated")])
```

---

## Outcome Propagation

Outcomes propagate through computation:

```typescript
// If inner computation returns needs/error, propagate it
function evalWithPropagation(
  body: Value,
  env: Environment,
  ...
): Outcome {
  const result = evalExpr(body, env, ...);

  // If result is already an outcome, return it
  if (isOutcome(result)) {
    return result;
  }

  // Otherwise wrap in ok
  return ok(result);
}

// Monadic bind for outcomes
function bindOutcome<T>(
  outcome: Outcome,
  fn: (value: Value) => Outcome
): Outcome {
  if (outcome.tag === 'ok') {
    return fn(outcome.value);
  }
  // Non-ok outcomes propagate unchanged
  return outcome;
}
```

```lisp
;; Outcome-aware sequencing
(outcome-bind (analyze-code)
  (lambda (analysis)
    (outcome-bind (generate-fix analysis)
      (lambda (fix)
        (ok fix)))))

;; Sugar: outcome-do
(outcome-do
  (analysis <- (analyze-code))
  (fix <- (generate-fix analysis))
  (ok fix))
```

---

## Outcome Handlers

Different outcomes trigger different behaviors:

```typescript
interface OutcomeHandler {
  onOk?: (outcome: OkOutcome) => void;
  onProposed?: (outcome: ProposedOutcome) => void;
  onNonconverged?: (outcome: NonconvergedOutcome) => void;
  onCycle?: (outcome: CycleOutcome) => void;
  onNeeds?: (outcome: NeedsOutcome) => void;
  onError?: (outcome: ErrorOutcome) => void;
}

function handleOutcome(outcome: Outcome, handlers: OutcomeHandler): void {
  switch (outcome.tag) {
    case 'ok':
      handlers.onOk?.(outcome);
      break;
    case 'proposed':
      handlers.onProposed?.(outcome);
      break;
    case 'nonconverged':
      handlers.onNonconverged?.(outcome);
      break;
    case 'cycle':
      handlers.onCycle?.(outcome);
      break;
    case 'needs':
      handlers.onNeeds?.(outcome);
      break;
    case 'error':
      handlers.onError?.(outcome);
      break;
  }
}
```

---

## Integration with Session

Sessions track outcome history:

```typescript
class Session {
  private outcomeHistory: Outcome[] = [];

  async turn(action: () => Promise<Outcome>): Promise<Outcome> {
    const outcome = await action();
    this.outcomeHistory.push(outcome);

    // Auto-handle certain outcomes
    if (outcome.tag === 'needs') {
      await this.handleNeed(outcome);
    }

    return outcome;
  }

  private async handleNeed(outcome: NeedsOutcome): Promise<void> {
    switch (outcome.needType) {
      case 'needs-approval':
        // Signal to host for human approval
        this.emit('approval-needed', outcome);
        break;
      case 'needs-evidence':
        // Log for debugging
        console.log(`Blocked: ${outcome.description}`);
        break;
    }
  }

  getOutcomeStats(): OutcomeStats {
    const counts = { ok: 0, proposed: 0, nonconverged: 0, cycle: 0, needs: 0, error: 0 };
    for (const o of this.outcomeHistory) {
      counts[o.tag]++;
    }
    return counts;
  }
}
```

---

## Summary

Outcomes provide:

1. **Tagged union types** - Explicit representation of all termination states
2. **Metadata** - Iterations, tokens, time for every outcome
3. **Need types** - Specific blockers that can be addressed
4. **Proposals** - Uncommitted changes for transaction mode
5. **Pattern matching** - Clean handling of all cases
6. **Propagation** - Monadic composition of outcome-returning functions

Outcomes replace exceptions with structured, actionable return values.
