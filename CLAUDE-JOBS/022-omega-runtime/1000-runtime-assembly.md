# 1000: OmegaRuntime Assembly

## Status: COMPLETE ✅
- **Completed**: 2026-01-29
- **Location**: OmegaLLM-3/src/runtime/OmegaRuntime.ts (704 lines)

## Purpose
Assembles all subsystems into the unified OmegaRuntime facade class, resolving all design flaws and providing the final public API.

## Dependencies
- ALL previous jobs (100-920) ✅

## Source References
- Design pattern: Facade (GoF)
- FLAW F8: Debugger can't inspect semantic state (FINAL RESOLUTION)
- ARCHITECTURE/01-OVERVIEW.md

---

## Deliverables

```
src/runtime/
├── index.ts                 # Public exports
├── OmegaRuntime.ts          # Main facade class
├── RuntimeFactory.ts        # Factory for creating runtime
└── RuntimePresets.ts        # Common configuration presets
```

---

## OmegaRuntime Interface

```typescript
export interface OmegaRuntime {
  // ─── Core Evaluation ───
  eval(code: string): Promise<EvalResult>;
  evalExpr(expr: Val): Promise<EvalResult>;
  step(): Promise<StepResult>;
  isTerminal(): boolean;

  // ─── Session ───
  session: SessionManager;
  getConfig(): RuntimeConfig;
  configure(updates: Partial<RuntimeConfig>): void;

  // ─── Debugging ───
  debug: DebugSubsystem;
  breakpoints: BreakpointManager;
  inspector: StateInspector;
  history: HistoryManager;
  snapshots: SnapshotManager;

  // ─── State Management ───
  stateCoordinator: StateCoordinator;
  transactions: TransactionManager;

  // ─── Data ───
  artifacts: ArtifactManager;
  facts: FactsManager;
  fixpoint: FixpointManager;

  // ─── Control ───
  conditions: ConditionsManager;
  concurrency: ConcurrencyManager;

  // ─── Governance ───
  provenance: ProvenanceManager;
  security: SecurityManager;
  budget: BudgetManager;

  // ─── LLM ───
  llm: LLMIntegration;
  opr: OPRIntegration;
  experts: ExpertsManager;

  // ─── Semantic ───
  amb: AmbManager;
  streams: StreamsManager;
  logic: LogicManager;

  // ─── Communication ───
  protocol: ProtocolServer;

  // ─── Events ───
  on<K extends keyof RuntimeEventMap>(event: K, handler: RuntimeEventHandler<K>): void;
  off<K extends keyof RuntimeEventMap>(event: K, handler: RuntimeEventHandler<K>): void;
  emit<K extends keyof RuntimeEventMap>(event: K, data: RuntimeEventMap[K]): void;

  // ─── Lifecycle ───
  dispose(): Promise<void>;
  isDisposed(): boolean;

  // ─── Semantic State Inspection (FLAW F8) ───
  getAmbState(): AmbState | null;
  getTransactionState(): TransactionState | null;
  getFixpointState(): FixpointState | null;
}
```

---

## Factory Pattern

```typescript
export interface RuntimeFactory {
  /**
   * Create runtime with default configuration.
   */
  create(): Promise<OmegaRuntime>;

  /**
   * Create runtime with custom configuration.
   */
  createWithConfig(config: Partial<RuntimeConfig>): Promise<OmegaRuntime>;

  /**
   * Create runtime with preset.
   */
  createWithPreset(preset: RuntimePreset): Promise<OmegaRuntime>;

  /**
   * Create runtime with custom providers.
   */
  createWithProviders(providers: RuntimeProviders): Promise<OmegaRuntime>;
}

export type RuntimePreset =
  | 'default'      // Full-featured runtime
  | 'minimal'      // Core evaluation only
  | 'debug'        // Full debugging enabled
  | 'production'   // Optimized, no debug
  | 'testing'      // Mock providers
  | 'repl'         // For interactive use
  | 'server';      // For server deployment
```

---

## Implementation

```typescript
class OmegaRuntimeImpl implements OmegaRuntime {
  // Subsystems
  readonly session: SessionManager;
  readonly stateCoordinator: StateCoordinator;
  readonly execution: ExecutionEngine;
  // ... all other subsystems

  private emitter: RuntimeEventEmitter;
  private disposed = false;

  private constructor(
    private config: RuntimeConfig,
    private providers: RuntimeProviders
  ) {
    this.emitter = new RuntimeEventEmitter();
    this.initializeSubsystems();
    this.wireSubsystems();
  }

  static async create(
    config: Partial<RuntimeConfig> = {},
    providers: Partial<RuntimeProviders> = {}
  ): Promise<OmegaRuntime> {
    const fullConfig = { ...DEFAULT_CONFIG, ...config };
    const fullProviders = { ...DEFAULT_PROVIDERS, ...providers };

    const runtime = new OmegaRuntimeImpl(fullConfig, fullProviders);
    await runtime.initialize();
    return runtime;
  }

  private initializeSubsystems(): void {
    // Initialize in dependency order
    this.stateCoordinator = new StateCoordinatorImpl(this.emitter);

    this.session = new SessionManagerImpl(
      this.providers.sessionProvider,
      this.emitter
    );

    this.budget = new BudgetManagerImpl(
      this.config.budget,
      this.emitter
    );

    const budgetAdapter = new BudgetAwareLLMAdapter(
      this.providers.llmProvider,
      this.budget,
      this.emitter
    );

    this.llm = new LLMIntegrationImpl(
      budgetAdapter,
      this.emitter
    );

    this.execution = new ExecutionEngineImpl(
      this.stateCoordinator,
      this.emitter
    );

    // ... initialize all other subsystems
  }

  private wireSubsystems(): void {
    // Wire event listeners (FLAW F5 resolution)

    // AMB backtrack triggers transaction rollback
    this.emitter.on('amb-backtrack', (event) => {
      this.transactions.rollbackToCheckpoint(event.checkpointId);
    });

    // LLM calls tracked by budget
    this.emitter.on('llm-call-complete', (event) => {
      this.budget.recordTokens(event.actualTokens, 'llm');
      this.budget.recordLLMCall();
    });

    // Provenance tracking
    this.emitter.on('infer-complete', (event) => {
      this.provenance.record({
        type: 'llm-call',
        value: event.result,
        operation: 'infer.op',
        prompt: event.prompt
      });
    });

    // ... wire all other listeners
  }

  async eval(code: string): Promise<EvalResult> {
    if (this.disposed) {
      throw new Error('Runtime is disposed');
    }

    this.session.recordEval();
    const expr = parse(code);
    return this.evalExpr(expr);
  }

  async evalExpr(expr: Val): Promise<EvalResult> {
    // Expand macros
    const expanded = this.macros.expandAll(expr, this.environment);

    // Initialize CESK state
    const state = this.execution.initState(expanded, this.environment);

    // Run to completion
    return this.execution.eval(expanded, this.environment);
  }

  // ─── Semantic State Inspection (FLAW F8) ───

  getAmbState(): AmbState | null {
    if (!this.amb.isSearching()) return null;
    return this.amb.getState();
  }

  getTransactionState(): TransactionState | null {
    if (!this.transactions.isInTransaction()) return null;
    return {
      active: true,
      depth: this.transactions.getDepth(),
      transaction: this.transactions.current()!,
      proposals: this.transactions.getProposals()
    };
  }

  getFixpointState(): FixpointState | null {
    if (!this.fixpoint.isComputing()) return null;
    return this.fixpoint.getState();
  }

  async dispose(): Promise<void> {
    if (this.disposed) return;

    await this.session.terminate();
    await this.protocol.stop();
    // ... cleanup all subsystems

    this.disposed = true;
  }

  isDisposed(): boolean {
    return this.disposed;
  }
}
```

---

## Presets

```typescript
const PRESETS: Record<RuntimePreset, Partial<RuntimeConfig>> = {
  default: {
    // Full-featured
  },

  minimal: {
    enableAmb: false,
    enableStreams: false,
    enableTransactions: false,
    enableDebug: false
  },

  debug: {
    enableDebug: true,
    breakOnError: true,
    traceExecution: true,
    historyEnabled: true
  },

  production: {
    enableDebug: false,
    traceExecution: false,
    historyEnabled: false,
    artifactCaching: true
  },

  testing: {
    providers: {
      llmProvider: new MockLLMProvider(),
      stateProvider: new InMemoryStateProvider(),
      sessionProvider: new InMemorySessionProvider()
    }
  },

  repl: {
    enableHistory: true,
    enableSnapshots: true,
    historyLimit: 1000
  },

  server: {
    enableProtocol: true,
    transportType: 'tcp',
    port: 7888
  }
};
```

---

## Public Exports

```typescript
// src/runtime/index.ts

export { OmegaRuntime } from './OmegaRuntime';
export { createRuntime, RuntimeFactory } from './RuntimeFactory';
export { RuntimePresets, RuntimePreset } from './RuntimePresets';

// Types
export type {
  RuntimeConfig,
  RuntimeProviders,
  EvalResult,
  StepResult,
  RuntimeEventMap,
  RuntimeEventHandler
} from './types';

// Subsystem interfaces (for advanced use)
export type { SessionManager } from './subsystems/SessionManager';
export type { DebugSubsystem } from './subsystems/DebugSubsystem';
// ... etc
```

---

## Test Requirements

### Unit Tests: `tests/runtime/OmegaRuntime.test.ts`
- [ ] create() returns valid runtime
- [ ] createWithConfig() applies config
- [ ] createWithPreset() uses preset
- [ ] eval() evaluates code
- [ ] All subsystems are accessible
- [ ] Events can be subscribed
- [ ] dispose() cleans up
- [ ] getAmbState() returns state during AMB
- [ ] getTransactionState() returns state during transaction

### Integration Tests
- [ ] Full evaluation flow
- [ ] All subsystems work together
- [ ] Event wiring works correctly
- [ ] Presets configure correctly
- [ ] Provider injection works

---

## Acceptance Criteria
1. OmegaRuntime provides unified API to all subsystems
2. Factory enables easy creation with configs/presets
3. All design flaws are resolved
4. Lifecycle management works correctly
5. Events propagate between subsystems
6. Public API is clean and well-documented
