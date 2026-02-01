# 340: Session Manager

## Status: COMPLETE ✅
- **Completed**: 2026-01-29
- **Location**: OmegaLLM-3/src/runtime/subsystems/SessionManager.ts (416 lines)

## Purpose
Manages runtime session lifecycle - initialization, configuration, persistence, and cleanup.

## Dependencies
- 100-types.md ✅
- 110-events.md ✅
- 120-providers.md ✅

## Source References
- ARCHITECTURE/08-PROTOCOL.md (session handling)
- VS Code Debug Adapter Protocol (launch/attach)

---

## Deliverables

```
src/runtime/subsystems/
├── SessionManager.ts        # Main session manager
└── session/
    ├── SessionState.ts      # Session state types
    ├── SessionPersistence.ts # Save/restore sessions
    └── SessionConfig.ts     # Session configuration
```

---

## Key Types

```typescript
export type SessionState =
  | 'uninitialized'
  | 'initializing'
  | 'ready'
  | 'running'
  | 'paused'
  | 'terminated'
  | 'error';

export interface SessionInfo {
  id: string;
  name?: string;
  state: SessionState;
  startTime: number;
  endTime?: number;

  // Configuration
  config: RuntimeConfig;

  // Statistics
  evalCount: number;
  stepCount: number;
  llmCallCount: number;
  errorCount: number;

  // Persistence
  snapshotCount: number;
  historySize: number;
}

export interface SessionConfig {
  // Execution settings
  maxSteps?: number;
  stepTimeout?: number;

  // Budget settings
  maxTokens?: number;
  maxLLMCalls?: number;
  maxCost?: number;

  // Debug settings
  breakOnError?: boolean;
  breakOnLLMCall?: boolean;
  traceExecution?: boolean;

  // Provider settings
  stateProvider?: StateProvider;
  llmProvider?: LLMProvider;
  sessionProvider?: SessionProvider;

  // Feature flags
  enableAmb?: boolean;
  enableStreams?: boolean;
  enableTransactions?: boolean;
}
```

---

## Key Interface

```typescript
export interface SessionManager {
  /**
   * Initialize a new session.
   */
  initialize(config?: Partial<SessionConfig>): Promise<void>;

  /**
   * Get current session info.
   */
  getInfo(): SessionInfo;

  /**
   * Get current session state.
   */
  getState(): SessionState;

  /**
   * Update session configuration.
   */
  configure(updates: Partial<SessionConfig>): void;

  /**
   * Pause session (for debugging).
   */
  pause(): void;

  /**
   * Resume paused session.
   */
  resume(): void;

  /**
   * Terminate session.
   */
  terminate(): Promise<void>;

  /**
   * Reset session to initial state.
   */
  reset(): Promise<void>;

  /**
   * Save session state for later restoration.
   */
  saveSession(name: string): Promise<string>;

  /**
   * Restore a saved session.
   */
  restoreSession(name: string): Promise<void>;

  /**
   * List saved sessions.
   */
  listSavedSessions(): Promise<SessionInfo[]>;

  /**
   * Delete a saved session.
   */
  deleteSavedSession(name: string): Promise<boolean>;

  /**
   * Record statistics.
   */
  recordEval(): void;
  recordStep(): void;
  recordLLMCall(): void;
  recordError(): void;
}
```

---

## Session Lifecycle

```
┌────────────────┐
│ uninitialized  │
└───────┬────────┘
        │ initialize()
        ▼
┌────────────────┐
│  initializing  │ ← Loading providers, setting up state
└───────┬────────┘
        │ success
        ▼
┌────────────────┐
│     ready      │ ← Waiting for commands
└───────┬────────┘
        │ eval()
        ▼
┌────────────────┐         pause()        ┌────────────────┐
│    running     │ ◄──────────────────►   │     paused     │
└───────┬────────┘         resume()       └────────────────┘
        │
        │ complete/error
        ▼
┌────────────────┐
│     ready      │ ← Back to ready for next command
└───────┬────────┘
        │ terminate()
        ▼
┌────────────────┐
│   terminated   │
└────────────────┘
```

---

## Session Events

```typescript
interface SessionEvents {
  'session-initialized': { id: string; config: SessionConfig };
  'session-state-changed': { oldState: SessionState; newState: SessionState };
  'session-config-changed': { changes: Partial<SessionConfig> };
  'session-terminated': { id: string; reason: string; stats: SessionInfo };
  'session-saved': { id: string; name: string };
  'session-restored': { id: string; name: string };
}
```

---

## Implementation

```typescript
class SessionManagerImpl implements SessionManager {
  private info: SessionInfo;
  private config: SessionConfig;

  async initialize(config?: Partial<SessionConfig>): Promise<void> {
    if (this.info.state !== 'uninitialized') {
      throw new Error('Session already initialized');
    }

    this.setState('initializing');

    try {
      // Merge with defaults
      this.config = { ...DEFAULT_CONFIG, ...config };

      // Initialize providers
      await this.initializeProviders();

      // Set up event listeners
      this.setupEventListeners();

      // Initialize subsystems
      await this.initializeSubsystems();

      this.info = {
        id: generateId(),
        state: 'ready',
        startTime: Date.now(),
        config: this.config,
        evalCount: 0,
        stepCount: 0,
        llmCallCount: 0,
        errorCount: 0,
        snapshotCount: 0,
        historySize: 0
      };

      this.setState('ready');

      this.emitter.emit('session-initialized', {
        id: this.info.id,
        config: this.config
      });
    } catch (error) {
      this.setState('error');
      throw error;
    }
  }

  async terminate(): Promise<void> {
    if (this.info.state === 'terminated') return;

    // Cleanup
    await this.cleanupSubsystems();
    await this.cleanupProviders();

    this.info.endTime = Date.now();
    this.setState('terminated');

    this.emitter.emit('session-terminated', {
      id: this.info.id,
      reason: 'user-requested',
      stats: this.info
    });
  }

  private setState(state: SessionState): void {
    const oldState = this.info.state;
    this.info.state = state;
    this.emitter.emit('session-state-changed', { oldState, newState: state });
  }
}
```

---

## Test Requirements

### Unit Tests: `tests/runtime/subsystems/SessionManager.test.ts`
- [ ] initialize() transitions to ready state
- [ ] initialize() fails gracefully on provider error
- [ ] getState() returns correct state
- [ ] configure() updates config
- [ ] pause()/resume() work correctly
- [ ] terminate() cleans up resources
- [ ] reset() returns to initial state
- [ ] saveSession() persists state
- [ ] restoreSession() loads state
- [ ] Statistics are recorded correctly

### Integration Tests
- [ ] Full session lifecycle works
- [ ] Session survives provider failures
- [ ] Saved sessions can be restored in new process
- [ ] Config changes take effect immediately
- [ ] Events fire at correct points

---

## Acceptance Criteria
1. Session initializes with configurable options
2. State transitions are consistent and tracked
3. Sessions can be saved/restored across processes
4. Statistics provide accurate usage info
5. Cleanup happens on termination
6. Events enable external monitoring
