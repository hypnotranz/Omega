# 300: Breakpoint Manager

## Status: COMPLETE ✅
- **Completed**: 2026-01-29
- **Location**: OmegaLLM-3/src/runtime/subsystems/BreakpointManager.ts (471 lines)

## Purpose
Manages debugger breakpoints - setting, removing, hit detection, and conditional breakpoints.

## Dependencies
- 100-types.md ✅
- 110-events.md ✅

## Source References
- VS Code Debug Adapter Protocol (DAP)
- ARCHITECTURE/08-PROTOCOL.md

---

## Deliverables

```
src/runtime/subsystems/
├── BreakpointManager.ts     # Main breakpoint manager
└── breakpoints/
    ├── Breakpoint.ts        # Breakpoint types
    ├── HitCounter.ts        # Hit count tracking
    └── ConditionEvaluator.ts # Conditional breakpoint logic
```

---

## Key Types

```typescript
export type BreakpointType = 'line' | 'function' | 'exception' | 'data';

export interface Breakpoint {
  id: string;
  type: BreakpointType;
  enabled: boolean;
  verified: boolean;

  // Location (for line breakpoints)
  source?: string;
  line?: number;
  column?: number;

  // Function name (for function breakpoints)
  functionName?: string;

  // Exception type (for exception breakpoints)
  exceptionType?: 'all' | 'uncaught' | 'specific';
  exceptionFilter?: string;

  // Condition (optional)
  condition?: string;
  hitCondition?: string;  // e.g., ">=5" means break on 5th hit
  logMessage?: string;    // Log instead of break

  // Runtime state
  hitCount: number;
}

export interface BreakpointEvent {
  type: 'added' | 'removed' | 'changed' | 'hit';
  breakpoint: Breakpoint;
  reason?: string;
}
```

---

## Key Interface

```typescript
export interface BreakpointManager {
  /**
   * Add a line breakpoint.
   */
  addLineBreakpoint(source: string, line: number, options?: {
    condition?: string;
    hitCondition?: string;
    logMessage?: string;
  }): Breakpoint;

  /**
   * Add a function breakpoint.
   */
  addFunctionBreakpoint(functionName: string, options?: {
    condition?: string;
  }): Breakpoint;

  /**
   * Add an exception breakpoint.
   */
  addExceptionBreakpoint(type: 'all' | 'uncaught' | 'specific', filter?: string): Breakpoint;

  /**
   * Remove a breakpoint by ID.
   */
  removeBreakpoint(id: string): boolean;

  /**
   * Enable/disable a breakpoint.
   */
  setBreakpointEnabled(id: string, enabled: boolean): void;

  /**
   * Check if execution should break at current state.
   * Returns the breakpoint if hit, null otherwise.
   */
  shouldBreak(state: CEKState, env: Environment): Breakpoint | null;

  /**
   * Get all breakpoints.
   */
  getBreakpoints(): Breakpoint[];

  /**
   * Get breakpoints for a specific source file.
   */
  getBreakpointsForSource(source: string): Breakpoint[];

  /**
   * Clear all breakpoints.
   */
  clearAll(): void;

  /**
   * Verify breakpoints against loaded source.
   * Adjusts line numbers if necessary.
   */
  verifyBreakpoints(source: string, validLines: number[]): void;
}
```

---

## Conditional Breakpoint Evaluation

```typescript
/**
 * Evaluate breakpoint condition in current environment.
 */
function evaluateCondition(
  condition: string,
  env: Environment,
  evaluator: (expr: Val, env: Environment) => Val
): boolean {
  try {
    const expr = parse(condition);
    const result = evaluator(expr, env);
    return isTruthy(result);
  } catch (e) {
    // Condition evaluation error - don't break
    console.warn(`Breakpoint condition error: ${e.message}`);
    return false;
  }
}

/**
 * Evaluate hit condition.
 * Formats: "=5", ">10", ">=5", "%3" (every 3rd hit)
 */
function evaluateHitCondition(hitCondition: string, hitCount: number): boolean {
  const match = hitCondition.match(/^(=|>|>=|%)?(\d+)$/);
  if (!match) return true;

  const [, op, numStr] = match;
  const num = parseInt(numStr, 10);

  switch (op) {
    case '=':  return hitCount === num;
    case '>':  return hitCount > num;
    case '>=': return hitCount >= num;
    case '%':  return hitCount % num === 0;
    default:   return hitCount >= num;
  }
}
```

---

## Event Emission

```typescript
// When breakpoint is added
emitter.emit('breakpoint', {
  type: 'added',
  breakpoint,
  reason: 'new'
});

// When breakpoint is hit
emitter.emit('breakpoint', {
  type: 'hit',
  breakpoint,
  reason: condition ? 'condition met' : 'line reached'
});

// Log message breakpoint (doesn't stop)
if (breakpoint.logMessage) {
  const message = interpolateLogMessage(breakpoint.logMessage, env);
  emitter.emit('output', { category: 'console', output: message });
  return null; // Don't actually break
}
```

---

## Test Requirements

### Unit Tests: `tests/runtime/subsystems/BreakpointManager.test.ts`
- [ ] addLineBreakpoint() creates valid breakpoint
- [ ] addFunctionBreakpoint() creates valid breakpoint
- [ ] removeBreakpoint() removes by ID
- [ ] setBreakpointEnabled() toggles state
- [ ] shouldBreak() detects line breakpoint hit
- [ ] shouldBreak() detects function entry breakpoint
- [ ] shouldBreak() evaluates conditions correctly
- [ ] shouldBreak() respects hit conditions
- [ ] shouldBreak() handles log messages (no break)
- [ ] getBreakpointsForSource() filters correctly
- [ ] verifyBreakpoints() adjusts invalid line numbers

### Integration Tests
- [ ] Debugger stops at line breakpoints
- [ ] Debugger stops at function entry
- [ ] Conditional breakpoints evaluate in scope
- [ ] Hit count breakpoints work across runs
- [ ] Exception breakpoints catch errors

---

## Acceptance Criteria
1. Line breakpoints stop execution at correct point
2. Function breakpoints trigger on function entry
3. Conditions are evaluated in correct scope
4. Hit conditions work correctly
5. Log message breakpoints don't stop execution
6. Invalid conditions don't crash debugger
