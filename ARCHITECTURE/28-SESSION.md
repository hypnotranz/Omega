# 28: Session (Host-Controlled Execution)

## The Trust Boundary Problem

Autonomous AI agents are dangerous without control:

```
┌─────────────────────────────────────────────────────────────────┐
│  UNSAFE: LLM controls the loop                                  │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│   while not done:                                               │
│     action = llm.decide()   # LLM chooses what to do           │
│     execute(action)          # Including shell commands!        │
│                                                                 │
│   Problems:                                                     │
│   - LLM can run arbitrary commands                              │
│   - No budget enforcement                                       │
│   - No human oversight                                          │
│   - Unbounded execution                                         │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘

┌─────────────────────────────────────────────────────────────────┐
│  SAFE: Host controls the loop                                   │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│   while budget.has_remaining() and not human.abort():           │
│     observation = gather_context()                              │
│     proposal = llm.analyze(observation)   # LLM only analyzes  │
│     if human.approves(proposal):                                │
│       execute_whitelisted(proposal)        # Host executes     │
│                                                                 │
│   Benefits:                                                     │
│   - Host controls execution                                     │
│   - Budget enforced                                             │
│   - Human in the loop                                           │
│   - Whitelisted operations only                                 │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

**Sessions implement the safe pattern.**

---

## Session Architecture

```typescript
interface Session {
  id: string;

  // State
  environment: Environment;
  world: World;
  stagedWorld?: StagedWorld;  // For proposal mode
  artifacts: ArtifactStore;

  // Control
  budget: Budget;
  policy: SessionPolicy;

  // History
  turns: Turn[];
  outcomeHistory: Outcome[];

  // Lifecycle
  status: SessionStatus;
  createdAt: number;
  lastActivityAt: number;
}

type SessionStatus =
  | 'active'
  | 'paused'
  | 'completed'
  | 'aborted'
  | 'budget-exceeded'
  ;

interface Turn {
  number: number;
  startTime: number;
  endTime: number;
  operations: Operation[];
  outcome: Outcome;
  tokensUsed: number;
}
```

---

## Session Policy

Policies define what operations are allowed:

```typescript
interface SessionPolicy {
  // Whitelisted operations
  allowedOperations: Set<string>;

  // Denied operations (even if in allowed)
  deniedOperations: Set<string>;

  // World access rules
  worldRules: {
    readPatterns: string[];    // Globs for allowed reads
    writePatterns: string[];   // Globs for allowed writes
    readOnly: boolean;         // No writes at all
    proposalMode: boolean;     // Writes are proposals only
  };

  // LLM rules
  llmRules: {
    allowComplete: boolean;
    allowIntent: boolean;
    maxTokensPerCall: number;
  };

  // Execution rules
  executionRules: {
    allowShellCommands: boolean;  // Almost always false
    allowNetworkCalls: boolean;
    allowSubeval: boolean;
  };
}

// Standard policies
const SAFE_ANALYSIS_POLICY: SessionPolicy = {
  allowedOperations: new Set([
    'world.read', 'world.list', 'world.search', 'world.fingerprint',
    'llm.complete', 'eval', 'subeval', 'memo', 'fixpoint',
    'assert', 'fact?', 'facts',
  ]),
  deniedOperations: new Set([
    'world.run', 'world.exec', 'shell',
  ]),
  worldRules: {
    readPatterns: ['**/*'],
    writePatterns: [],
    readOnly: true,
    proposalMode: false,
  },
  llmRules: {
    allowComplete: true,
    allowIntent: true,
    maxTokensPerCall: 4000,
  },
  executionRules: {
    allowShellCommands: false,
    allowNetworkCalls: false,
    allowSubeval: true,
  },
};

const SAFE_EDIT_POLICY: SessionPolicy = {
  ...SAFE_ANALYSIS_POLICY,
  allowedOperations: new Set([
    ...SAFE_ANALYSIS_POLICY.allowedOperations,
    'world.write',  // But in proposal mode
  ]),
  worldRules: {
    readPatterns: ['**/*'],
    writePatterns: ['src/**/*', 'test/**/*'],
    readOnly: false,
    proposalMode: true,  // Writes are proposals, not commits
  },
};
```

---

## Operation Enforcement

```typescript
class SessionEnforcer {
  constructor(private policy: SessionPolicy) {}

  checkOperation(op: string, args: Value[]): void {
    // Check whitelist
    if (!this.policy.allowedOperations.has(op)) {
      throw new PolicyViolationError(`Operation not allowed: ${op}`);
    }

    // Check denylist
    if (this.policy.deniedOperations.has(op)) {
      throw new PolicyViolationError(`Operation explicitly denied: ${op}`);
    }

    // Check specific rules
    if (op === 'world.read') {
      this.checkWorldRead(args[0] as string);
    }
    if (op === 'world.write') {
      this.checkWorldWrite(args[0] as string);
    }
    if (op === 'llm.complete') {
      this.checkLLMCall(args);
    }
  }

  private checkWorldRead(ref: string): void {
    const allowed = this.policy.worldRules.readPatterns.some(
      pattern => minimatch(ref, pattern)
    );
    if (!allowed) {
      throw new PolicyViolationError(`Read not allowed: ${ref}`);
    }
  }

  private checkWorldWrite(ref: string): void {
    if (this.policy.worldRules.readOnly) {
      throw new PolicyViolationError('World is read-only');
    }

    const allowed = this.policy.worldRules.writePatterns.some(
      pattern => minimatch(ref, pattern)
    );
    if (!allowed) {
      throw new PolicyViolationError(`Write not allowed: ${ref}`);
    }
  }
}

// Wrapped FFI that enforces policy
class EnforcedFFI implements FFI {
  constructor(
    private base: FFI,
    private enforcer: SessionEnforcer,
    private audit: AuditLog
  ) {}

  call(name: string, args: Value[]): Value {
    // Enforce policy
    this.enforcer.checkOperation(name, args);

    // Audit
    this.audit.log({ op: name, args, time: Date.now() });

    // Execute
    return this.base.call(name, args);
  }
}
```

---

## Turn-Based Execution

```typescript
class Session {
  async executeTurn(code: string): Promise<TurnResult> {
    // Check budget
    if (!this.budget.hasRemaining('iterations')) {
      return { outcome: nonconverged(null, this.turns.length, 'budget') };
    }

    // Start turn
    const turn: Turn = {
      number: this.turns.length + 1,
      startTime: Date.now(),
      endTime: 0,
      operations: [],
      outcome: null!,
      tokensUsed: 0,
    };

    // Execute with enforced FFI
    const enforcedFFI = new EnforcedFFI(
      this.ffi,
      this.enforcer,
      this.auditLog
    );

    try {
      // Use staged world if in proposal mode
      const world = this.policy.worldRules.proposalMode
        ? this.getOrCreateStagedWorld()
        : this.world;

      const result = await evalExpr(
        read(code),
        this.environment,
        initialContinuation(),
        enforcedFFI
      );

      turn.outcome = isOutcome(result) ? result : ok(result);

    } catch (e) {
      if (e instanceof PolicyViolationError) {
        turn.outcome = error(e.message, 'policy-violation');
      } else if (e instanceof BudgetExceededError) {
        turn.outcome = nonconverged(null, turn.number, 'budget');
      } else {
        turn.outcome = error((e as Error).message, 'runtime-error');
      }
    }

    // Finalize turn
    turn.endTime = Date.now();
    turn.tokensUsed = this.metrics.get('llm_total_tokens') - this.lastTokenCount;
    this.lastTokenCount = this.metrics.get('llm_total_tokens');

    this.turns.push(turn);
    this.budget.consumeIteration();

    return {
      outcome: turn.outcome,
      turnNumber: turn.number,
      budgetRemaining: this.budget.remainingAll(),
      proposals: this.stagedWorld?.getProposals() ?? [],
    };
  }
}
```

---

## Audit Trail

Every operation is logged:

```typescript
interface AuditEntry {
  timestamp: number;
  turnNumber: number;
  operation: string;
  args: Value[];
  result?: Value;
  error?: string;
  tokensUsed?: number;
  duration?: number;
}

class AuditLog {
  private entries: AuditEntry[] = [];
  private currentTurn: number = 0;

  log(entry: Omit<AuditEntry, 'timestamp' | 'turnNumber'>): void {
    this.entries.push({
      ...entry,
      timestamp: Date.now(),
      turnNumber: this.currentTurn,
    });
  }

  setTurn(turn: number): void {
    this.currentTurn = turn;
  }

  // Query audit log
  getEntriesForTurn(turn: number): AuditEntry[] {
    return this.entries.filter(e => e.turnNumber === turn);
  }

  getEntriesForOperation(op: string): AuditEntry[] {
    return this.entries.filter(e => e.operation === op);
  }

  // Export for analysis
  export(): AuditEntry[] {
    return [...this.entries];
  }

  // Statistics
  stats(): AuditStats {
    const opCounts: Record<string, number> = {};
    for (const entry of this.entries) {
      opCounts[entry.operation] = (opCounts[entry.operation] ?? 0) + 1;
    }
    return {
      totalOperations: this.entries.length,
      operationCounts: opCounts,
      totalTokens: this.entries.reduce((sum, e) => sum + (e.tokensUsed ?? 0), 0),
    };
  }
}
```

---

## Session Lifecycle

```typescript
class SessionManager {
  private sessions: Map<string, Session> = new Map();

  create(config: SessionConfig): Session {
    const session: Session = {
      id: generateId(),
      environment: Environment.createRoot(),
      world: config.world ?? new InMemoryWorld(),
      artifacts: new ArtifactStore(),
      budget: new Budget(config.budget),
      policy: config.policy ?? SAFE_ANALYSIS_POLICY,
      turns: [],
      outcomeHistory: [],
      status: 'active',
      createdAt: Date.now(),
      lastActivityAt: Date.now(),
    };

    this.sessions.set(session.id, session);
    return session;
  }

  get(id: string): Session | undefined {
    return this.sessions.get(id);
  }

  pause(id: string): void {
    const session = this.sessions.get(id);
    if (session) {
      session.status = 'paused';
    }
  }

  resume(id: string): void {
    const session = this.sessions.get(id);
    if (session && session.status === 'paused') {
      session.status = 'active';
      session.lastActivityAt = Date.now();
    }
  }

  abort(id: string): void {
    const session = this.sessions.get(id);
    if (session) {
      session.status = 'aborted';
      // Rollback any staged changes
      session.stagedWorld?.rollback();
    }
  }

  commit(id: string): void {
    const session = this.sessions.get(id);
    if (session?.stagedWorld) {
      session.stagedWorld.commit();
      session.stagedWorld = undefined;
    }
  }

  // Snapshot for persistence
  snapshot(id: string): SessionSnapshot {
    const session = this.sessions.get(id);
    if (!session) throw new Error(`Session not found: ${id}`);

    return {
      id: session.id,
      environment: session.environment.snapshot(),
      world: session.world.snapshot(),
      artifacts: session.artifacts.serialize(),
      turns: session.turns,
      status: session.status,
    };
  }

  restore(snapshot: SessionSnapshot): Session {
    // ... restore from snapshot
  }
}
```

---

## Lisp API

```lisp
;; Session info
(session/id)              ;; => "sess-abc123"
(session/turn-number)     ;; => 5
(session/status)          ;; => 'active

;; Budget queries
(session/budget-remaining 'tokens)     ;; => 5000
(session/budget-remaining 'cost)       ;; => 0.50

;; Policy queries
(session/allowed? 'world.write)        ;; => #t or #f
(session/read-only?)                   ;; => #t or #f
(session/proposal-mode?)               ;; => #t or #f

;; Audit
(session/audit-log)                    ;; => list of audit entries
(session/operations-count)             ;; => 42

;; Proposals (in proposal mode)
(session/proposals)                    ;; => list of pending proposals
(session/commit-proposals!)            ;; Host must approve first
(session/rollback-proposals!)          ;; Discard proposals
```

---

## Host Integration

The host (application using LambdaLLM) controls the session:

```typescript
// Host application
async function runAgent(task: string): Promise<AgentResult> {
  const session = sessionManager.create({
    budget: { tokens: 10000, cost: 1.0, time: 300 },
    policy: SAFE_EDIT_POLICY,
  });

  // Host-controlled loop
  while (session.canContinue()) {
    // Step 1: Gather context
    const context = await gatherContext(session);

    // Step 2: LLM analyzes (via session turn)
    const result = await session.executeTurn(`
      (analyze-and-propose ${serialize(context)})
    `);

    // Step 3: Host decides what to do
    if (result.outcome.tag === 'proposed') {
      // Show proposals to human
      const approved = await humanReview(result.proposals);

      if (approved) {
        sessionManager.commit(session.id);
      } else {
        sessionManager.abort(session.id);
        break;
      }
    }

    if (result.outcome.tag === 'ok' && isComplete(result.outcome.value)) {
      break;
    }

    if (result.outcome.tag === 'needs') {
      // Handle need (maybe ask human)
      await handleNeed(result.outcome);
    }
  }

  return {
    success: session.status === 'completed',
    turns: session.turns.length,
    tokensUsed: session.metrics.get('llm_total_tokens'),
    proposals: session.stagedWorld?.getProposals() ?? [],
  };
}
```

---

## Summary

Sessions provide:

1. **Host control** - Application controls the loop, not LLM
2. **Policy enforcement** - Whitelist/denylist operations
3. **Budget tracking** - Per-session resource limits
4. **Proposal mode** - Writes are staged, require approval
5. **Audit trail** - Complete operation history
6. **Lifecycle management** - Pause, resume, abort, commit

Sessions are the trust boundary that makes AI agents safe for production.
