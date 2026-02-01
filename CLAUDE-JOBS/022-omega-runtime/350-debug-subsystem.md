# 350: Debug Subsystem

## Status: COMPLETE ✅
- **Completed**: 2026-01-29
- **Location**: OmegaLLM-3/src/runtime/subsystems/DebugManager.ts (683 lines)

## Purpose
Unified debugging facade that coordinates breakpoints, inspection, stepping, and debug protocol messages.

## Dependencies
- 100-types.md ✅
- 110-events.md ✅
- 210-execution.md ✅
- 300-breakpoints.md ✅
- 310-inspector.md ✅
- 320-history.md ✅
- 330-snapshots.md ✅

## Source References
- VS Code Debug Adapter Protocol (DAP)
- ARCHITECTURE/08-PROTOCOL.md

---

## Deliverables

```
src/runtime/subsystems/
├── DebugSubsystem.ts        # Main debug subsystem
└── debug/
    ├── DebugAdapter.ts      # DAP message handling
    ├── StepController.ts    # Step/continue logic
    └── DebugEvents.ts       # Debug event types
```

---

## Key Types

```typescript
export type StopReason =
  | 'breakpoint'
  | 'step'
  | 'exception'
  | 'pause'
  | 'entry'
  | 'goto'
  | 'data-breakpoint';

export interface StoppedEvent {
  reason: StopReason;
  threadId: number;
  description?: string;
  text?: string;
  allThreadsStopped?: boolean;
  hitBreakpointIds?: string[];
}

export interface DebugCapabilities {
  supportsConfigurationDoneRequest: boolean;
  supportsFunctionBreakpoints: boolean;
  supportsConditionalBreakpoints: boolean;
  supportsHitConditionalBreakpoints: boolean;
  supportsEvaluateForHovers: boolean;
  supportsStepBack: boolean;
  supportsSetVariable: boolean;
  supportsRestartFrame: boolean;
  supportsGotoTargetsRequest: boolean;
  supportsCompletionsRequest: boolean;
  supportsDataBreakpoints: boolean;
  supportsLogPoints: boolean;
}
```

---

## Key Interface

```typescript
export interface DebugSubsystem {
  // ─── Lifecycle ───

  /**
   * Launch debug session.
   */
  launch(config: LaunchConfig): Promise<void>;

  /**
   * Attach to running session.
   */
  attach(config: AttachConfig): Promise<void>;

  /**
   * Disconnect from session.
   */
  disconnect(): Promise<void>;

  /**
   * Get debug capabilities.
   */
  getCapabilities(): DebugCapabilities;

  // ─── Execution Control ───

  /**
   * Continue execution.
   */
  continue(): Promise<void>;

  /**
   * Pause execution.
   */
  pause(): Promise<void>;

  /**
   * Step over (next line).
   */
  stepOver(): Promise<void>;

  /**
   * Step into (enter function).
   */
  stepInto(): Promise<void>;

  /**
   * Step out (exit function).
   */
  stepOut(): Promise<void>;

  /**
   * Step back (time travel).
   */
  stepBack(): Promise<void>;

  /**
   * Restart current frame.
   */
  restartFrame(frameId: number): Promise<void>;

  // ─── Breakpoint Management ───

  /**
   * Set breakpoints for a source file.
   * Replaces existing breakpoints.
   */
  setBreakpoints(source: string, breakpoints: BreakpointInput[]): Breakpoint[];

  /**
   * Set function breakpoints.
   */
  setFunctionBreakpoints(breakpoints: FunctionBreakpointInput[]): Breakpoint[];

  /**
   * Set exception breakpoints.
   */
  setExceptionBreakpoints(filters: string[]): void;

  // ─── Inspection ───

  /**
   * Get current stack trace.
   */
  getStackTrace(options?: { startFrame?: number; levels?: number }): StackFrame[];

  /**
   * Get scopes for a frame.
   */
  getScopes(frameId: number): Scope[];

  /**
   * Get variables.
   */
  getVariables(variablesReference: number): Variable[];

  /**
   * Evaluate expression.
   */
  evaluate(expression: string, frameId?: number, context?: string): EvaluateResult;

  /**
   * Get completions for REPL input.
   */
  getCompletions(text: string, column: number, frameId?: number): CompletionItem[];

  // ─── State Access ───

  /**
   * Get execution engine (for direct access).
   */
  getExecutionEngine(): ExecutionEngine;

  /**
   * Get breakpoint manager.
   */
  getBreakpointManager(): BreakpointManager;

  /**
   * Get state inspector.
   */
  getInspector(): StateInspector;

  /**
   * Get history manager.
   */
  getHistoryManager(): HistoryManager;
}
```

---

## Step Implementation

```typescript
class DebugSubsystemImpl implements DebugSubsystem {
  private stepMode: 'none' | 'over' | 'into' | 'out' = 'none';
  private stepStartFrame: number = 0;

  async stepOver(): Promise<void> {
    this.stepMode = 'over';
    this.stepStartFrame = this.getCurrentFrameDepth();

    await this.runUntilStop();
  }

  async stepInto(): Promise<void> {
    this.stepMode = 'into';

    await this.runUntilStop();
  }

  async stepOut(): Promise<void> {
    this.stepMode = 'out';
    this.stepStartFrame = this.getCurrentFrameDepth();

    await this.runUntilStop();
  }

  private async runUntilStop(): Promise<void> {
    while (true) {
      const state = await this.engine.step(this.engine.getState());

      if (this.engine.isTerminal(state)) {
        this.emitTerminated();
        break;
      }

      // Check breakpoints
      const breakpoint = this.breakpoints.shouldBreak(state, this.getEnv());
      if (breakpoint) {
        this.emitStopped('breakpoint', breakpoint);
        break;
      }

      // Check step completion
      if (this.isStepComplete(state)) {
        this.emitStopped('step');
        break;
      }
    }
  }

  private isStepComplete(state: CEKState): boolean {
    const currentDepth = this.getCurrentFrameDepth();

    switch (this.stepMode) {
      case 'over':
        return currentDepth <= this.stepStartFrame && this.isAtStatementBoundary();
      case 'into':
        return this.isAtStatementBoundary();
      case 'out':
        return currentDepth < this.stepStartFrame;
      default:
        return false;
    }
  }
}
```

---

## Event Emission

```typescript
// When execution stops
emitter.emit('stopped', {
  reason: 'breakpoint',
  threadId: 1,
  hitBreakpointIds: [breakpoint.id]
});

// When execution continues
emitter.emit('continued', { threadId: 1 });

// When execution terminates
emitter.emit('terminated', {});

// When output is produced
emitter.emit('output', {
  category: 'stdout',
  output: 'Result: 42\n'
});
```

---

## Test Requirements

### Unit Tests: `tests/runtime/subsystems/DebugSubsystem.test.ts`
- [ ] launch() initializes debug session
- [ ] continue() runs until breakpoint or end
- [ ] pause() stops execution
- [ ] stepOver() advances one statement
- [ ] stepInto() enters function calls
- [ ] stepOut() exits current function
- [ ] stepBack() uses history (time travel)
- [ ] setBreakpoints() configures breakpoints
- [ ] getStackTrace() shows correct frames
- [ ] getVariables() shows correct values
- [ ] evaluate() works in frame context

### Integration Tests
- [ ] Full debug session works
- [ ] Breakpoints stop at correct locations
- [ ] Stepping navigates correctly
- [ ] Time travel debugging works
- [ ] VS Code/DAP compatibility

---

## Acceptance Criteria
1. All DAP operations are supported
2. Stepping is accurate at statement level
3. Breakpoints work correctly
4. Variable inspection shows correct values
5. Time travel (stepBack) works via history
6. Performance: stepping is responsive (<100ms)
