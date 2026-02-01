# 310: State Inspector (FLAW F3 Resolution)

## Status: COMPLETE ✅
- **Completed**: 2026-01-29
- **Location**: OmegaLLM-3/src/runtime/subsystems/StateInspector.ts (492 lines)

## Purpose
Provides introspection capabilities for debugging - examining variables, evaluating expressions in scope, and inspecting complex data structures.

## Dependencies
- 100-types.md ✅
- 110-events.md ✅
- 210-execution.md ✅

## Source References
- VS Code Debug Adapter Protocol (variables, evaluate, stackTrace)
- ARCHITECTURE/08-PROTOCOL.md

---

## Deliverables

```
src/runtime/subsystems/
├── StateInspector.ts        # Main inspector class
└── inspector/
    ├── VariablePresenter.ts # Format variables for display
    ├── StackFrameBuilder.ts # Build stack frame info
    └── ExpressionEvaluator.ts # Evaluate watch expressions
```

---

## Key Types

```typescript
export interface Variable {
  name: string;
  value: string;           // Formatted display value
  type?: string;           // Type annotation
  variablesReference: number; // Non-zero if has children
  namedVariables?: number;    // Count of named children
  indexedVariables?: number;  // Count of indexed children
  evaluateName?: string;      // Expression to get this value
}

export interface StackFrame {
  id: number;
  name: string;             // Function name or '<anonymous>'
  source?: string;          // Source file
  line?: number;
  column?: number;
  endLine?: number;
  endColumn?: number;
  moduleId?: string;
  presentationHint?: 'normal' | 'label' | 'subtle';
}

export interface Scope {
  name: string;             // 'Local', 'Closure', 'Global'
  variablesReference: number;
  expensive: boolean;       // Lazy load if true
  presentationHint?: 'arguments' | 'locals' | 'registers';
}

export interface EvaluateResult {
  result: string;
  type?: string;
  variablesReference: number;
  namedVariables?: number;
  indexedVariables?: number;
}
```

---

## Key Interface

```typescript
export interface StateInspector {
  /**
   * Get current stack trace.
   */
  getStackTrace(state: CEKState): StackFrame[];

  /**
   * Get scopes for a stack frame.
   */
  getScopes(frameId: number): Scope[];

  /**
   * Get variables in a scope.
   */
  getVariables(variablesReference: number, options?: {
    filter?: 'indexed' | 'named';
    start?: number;
    count?: number;
  }): Variable[];

  /**
   * Evaluate expression in context of a frame.
   */
  evaluate(expression: string, frameId?: number, context?: 'watch' | 'repl' | 'hover'): EvaluateResult;

  /**
   * Set variable value (for editable variables).
   */
  setVariable(variablesReference: number, name: string, value: string): Variable;

  /**
   * Get CESK-specific state info.
   */
  getCESKInfo(state: CEKState): {
    control: string;         // Current expression
    environment: Variable[]; // Env bindings
    kontinuation: string;    // Continuation chain
    store: Variable[];       // Mutable cells
  };

  /**
   * Get semantic state info (FLAW F8 resolution).
   */
  getSemanticState(): {
    ambState: AmbStateInfo | null;
    transactionState: TransactionStateInfo | null;
    fixpointState: FixpointStateInfo | null;
  };

  /**
   * Format a value for display.
   */
  formatValue(value: Val, depth?: number): string;
}
```

---

## CESK State Visualization

```typescript
function getCESKInfo(state: CEKState): CESKInfo {
  return {
    control: formatControl(state.control),
    environment: formatEnvironment(state.environment),
    kontinuation: formatKontinuation(state.kontinuation),
    store: formatStore(state.store)
  };
}

function formatControl(control: Control): string {
  // Pretty print current expression
  if (isAtom(control)) return stringify(control);
  if (isList(control)) return `(${control.slice(0, 3).map(stringify).join(' ')}${control.length > 3 ? ' ...' : ''})`;
  return stringify(control);
}

function formatKontinuation(k: Kontinuation): string {
  // Build continuation chain visualization
  const chain: string[] = [];
  let current = k;
  while (current.type !== 'halt') {
    chain.push(kontinuationLabel(current));
    current = current.k;
  }
  chain.push('halt');
  return chain.join(' → ');
}

function kontinuationLabel(k: Kontinuation): string {
  switch (k.type) {
    case 'if-cont': return 'if';
    case 'arg-cont': return `args(${k.evaluated.length}/${k.evaluated.length + k.remaining.length})`;
    case 'apply-cont': return `apply(${formatValue(k.fn)})`;
    case 'let-cont': return `let(${k.bindings.length})`;
    case 'begin-cont': return `begin(${k.remaining.length})`;
    default: return k.type;
  }
}
```

---

## Stack Frame Building

```typescript
function buildStackFrames(state: CEKState): StackFrame[] {
  const frames: StackFrame[] = [];
  let frameId = 0;

  // Current frame
  frames.push({
    id: frameId++,
    name: getCurrentFrameName(state),
    ...getSourceLocation(state.control)
  });

  // Walk continuation chain for call stack
  let k = state.kontinuation;
  while (k.type !== 'halt') {
    if (k.type === 'apply-cont' && isClosure(k.fn)) {
      frames.push({
        id: frameId++,
        name: getClosureName(k.fn) || '<anonymous>',
        ...getSourceLocation(k.fn.body)
      });
    }
    k = k.k;
  }

  return frames;
}
```

---

## Semantic State Info (FLAW F8)

```typescript
interface AmbStateInfo {
  isActive: boolean;
  depth: number;
  currentPath: number[];     // Which alternative at each level
  remainingAlternatives: number;
  checkpointId: string;
}

interface TransactionStateInfo {
  isActive: boolean;
  depth: number;
  transactionId: string;
  proposalCount: number;
  proposals: { key: string; oldValue: Val; newValue: Val }[];
}

interface FixpointStateInfo {
  isActive: boolean;
  iterationCount: number;
  changedKeys: string[];
  convergenceProgress: number; // 0-1
}
```

---

## Test Requirements

### Unit Tests: `tests/runtime/subsystems/StateInspector.test.ts`
- [ ] getStackTrace() builds correct frame chain
- [ ] getScopes() returns local, closure, global scopes
- [ ] getVariables() expands complex objects
- [ ] getVariables() handles indexed access (arrays)
- [ ] evaluate() works in correct scope
- [ ] evaluate() handles errors gracefully
- [ ] setVariable() modifies mutable variables
- [ ] getCESKInfo() shows all CESK components
- [ ] formatValue() truncates large values
- [ ] formatValue() handles circular references

### Integration Tests
- [ ] Debugger shows correct variable values
- [ ] Watch expressions update correctly
- [ ] REPL evaluation uses correct scope
- [ ] Hover shows variable info
- [ ] Semantic state is visible during AMB

---

## Acceptance Criteria
1. Stack trace shows correct call chain
2. Variables show correct values at each scope level
3. Complex structures can be expanded
4. Expression evaluation works in context
5. CESK state is clearly visualized
6. Semantic state (AMB/Transaction/Fixpoint) is inspectable
