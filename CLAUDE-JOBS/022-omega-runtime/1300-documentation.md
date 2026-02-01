# 1300: API Documentation

## Status: COMPLETE ✅

## Purpose
Create comprehensive API documentation for OmegaRuntime.

## Dependencies
- 1000-runtime-assembly.md ✅

## Implementation Notes
API documentation created in `docs/api/`:

**Core Documentation:**
- `index.md` - Overview, quick start, architecture, presets
- `getting-started.md` - Installation, basic usage, examples
- `configuration.md` - Full configuration reference
- `events.md` - Complete event reference (30+ events)
- `providers.md` - Provider interfaces and implementations

**Subsystem Guides:**
- `subsystems/execution.md` - ExecutionEngine, MacroManager, StateCoordinator
- `subsystems/debugging.md` - DebugManager, breakpoints, inspection, history, snapshots
- `subsystems/semantic.md` - AmbManager, StreamsManager, FactsManager, LogicManager

## Source References
- TypeDoc documentation generator
- Existing ARCHITECTURE docs

---

## Documentation Structure

```
docs/api/
├── index.md                 # Overview and quick start
├── getting-started.md       # Installation and basic usage
├── configuration.md         # Configuration options
├── subsystems/
│   ├── execution.md         # Evaluation and stepping
│   ├── debugging.md         # Debug features
│   ├── state-management.md  # Snapshots, history
│   ├── llm-integration.md   # LLM effects
│   ├── semantic.md          # AMB, streams, logic
│   └── governance.md        # Budget, security, provenance
├── events.md                # Event reference
├── providers.md             # Provider interfaces
├── errors.md                # Error types
└── migration.md             # Migration from old code
```

---

## Index Page

```markdown
# OmegaRuntime API Documentation

OmegaRuntime is a unified runtime for OmegaLLM, providing:

- **CESK Machine Evaluation**: Step-by-step code execution
- **Debugging**: Breakpoints, stepping, variable inspection
- **State Management**: Snapshots, history, time-travel
- **LLM Integration**: Semantic effects (infer.op, search.op)
- **Semantic Computation**: AMB, streams, logic programming
- **Governance**: Budget tracking, security, provenance

## Quick Start

```typescript
import { createRuntime } from '@omegallm/runtime';

// Create runtime
const runtime = await createRuntime();

// Evaluate code
const result = await runtime.eval('(+ 1 2)');
console.log(result.value); // 3

// Cleanup
await runtime.dispose();
```

## Installation

```bash
npm install @omegallm/runtime
```

## Presets

| Preset | Description |
|--------|-------------|
| `default` | Full-featured runtime |
| `minimal` | Core evaluation only |
| `debug` | Full debugging enabled |
| `testing` | Mock providers for tests |
| `repl` | Interactive use |
| `server` | Protocol server |

```typescript
const runtime = await createRuntime({ preset: 'debug' });
```

## Next Steps

- [Getting Started](getting-started.md)
- [Configuration](configuration.md)
- [Subsystems](subsystems/)
```

---

## Getting Started Guide

```markdown
# Getting Started

## Creating a Runtime

### Basic Creation

```typescript
const runtime = await createRuntime();
```

### With Configuration

```typescript
const runtime = await createRuntime({
  budget: { maxTokens: 10000 },
  enableDebug: true,
  enableHistory: true
});
```

### With Custom Providers

```typescript
const runtime = await createRuntime({
  providers: {
    llmProvider: new OpenAIProvider({ apiKey: '...' }),
    stateProvider: new FileStateProvider('./state')
  }
});
```

## Basic Evaluation

```typescript
// Simple expression
const result = await runtime.eval('(+ 1 2)');
console.log(result.value); // 3

// Define function
await runtime.eval('(define (square x) (* x x))');
const squared = await runtime.eval('(square 5)');
console.log(squared.value); // 25

// LLM effect
const answer = await runtime.eval(
  '(effect infer.op (list "What is 2+2?"))'
);
console.log(answer.value); // "4"
```

## Debugging

```typescript
// Enable debug mode
const runtime = await createRuntime({ preset: 'debug' });

// Set breakpoint
runtime.breakpoints.addLineBreakpoint('code.lisp', 5);

// Handle stopped event
runtime.on('stopped', (event) => {
  console.log(`Stopped: ${event.reason}`);

  // Inspect variables
  const vars = runtime.inspector.getVariables(0);
  console.log(vars);

  // Continue
  runtime.debug.continue();
});

// Start evaluation
await runtime.eval(code);
```

## Cleanup

Always dispose of the runtime when done:

```typescript
await runtime.dispose();
```
```

---

## API Reference Template

```markdown
# ExecutionEngine

The execution engine provides core evaluation capabilities.

## Interface

```typescript
interface ExecutionEngine {
  eval(expr: Val, env?: Environment): Promise<EvalResult>;
  step(state: CEKState): Promise<CEKState>;
  isTerminal(state: CEKState): boolean;
  getState(): CEKState;
  setState(state: CEKState): void;
  initState(expr: Val, env?: Environment): CEKState;
}
```

## Methods

### eval(expr, env?)

Evaluates an expression to completion.

**Parameters:**
- `expr: Val` - The expression to evaluate
- `env?: Environment` - Optional environment (defaults to global)

**Returns:** `Promise<EvalResult>`

**Example:**
```typescript
const result = await runtime.execution.eval(parse('(+ 1 2)'));
console.log(result.value); // 3
```

### step(state)

Takes a single evaluation step.

**Parameters:**
- `state: CEKState` - Current CESK state

**Returns:** `Promise<CEKState>` - New state after one step

**Example:**
```typescript
let state = runtime.execution.initState(expr);
while (!runtime.execution.isTerminal(state)) {
  state = await runtime.execution.step(state);
}
```
```

---

## Events Reference

```markdown
# Events Reference

OmegaRuntime emits events at key points. Subscribe using `runtime.on()`.

## Evaluation Events

### eval-start
Emitted when evaluation begins.
```typescript
runtime.on('eval-start', ({ expr, env }) => {
  console.log('Starting evaluation of:', expr);
});
```

### eval-complete
Emitted when evaluation completes successfully.
```typescript
runtime.on('eval-complete', ({ result, stepCount }) => {
  console.log('Result:', result.value);
});
```

### eval-error
Emitted when evaluation fails.
```typescript
runtime.on('eval-error', ({ error, state }) => {
  console.error('Error:', error.message);
});
```

## Debug Events

### stopped
Emitted when execution stops (breakpoint, step, etc.).
```typescript
runtime.on('stopped', ({ reason, threadId }) => {
  console.log('Stopped:', reason);
});
```

### breakpoint
Emitted for breakpoint events.
```typescript
runtime.on('breakpoint', ({ type, breakpoint }) => {
  if (type === 'hit') {
    console.log('Hit breakpoint:', breakpoint.id);
  }
});
```

## LLM Events

### llm-call-start
Emitted before LLM call.
```typescript
runtime.on('llm-call-start', ({ operation, estimatedTokens }) => {
  console.log('LLM call starting:', operation);
});
```

### llm-call-complete
Emitted after LLM call.
```typescript
runtime.on('llm-call-complete', ({ operation, actualTokens, duration }) => {
  console.log('LLM call completed:', operation, actualTokens, 'tokens');
});
```

## Budget Events

### budget-consumed
Emitted when budget is consumed.
```typescript
runtime.on('budget-consumed', ({ resource, amount, remaining }) => {
  console.log(`Used ${amount} ${resource}, ${remaining} remaining`);
});
```

### budget-alert
Emitted when budget threshold reached.
```typescript
runtime.on('budget-alert', ({ resource, threshold, action }) => {
  console.warn(`Budget alert: ${resource} at ${threshold * 100}%`);
});
```
```

---

## TypeDoc Configuration

```json
// typedoc.json
{
  "entryPoints": ["src/runtime/index.ts"],
  "out": "docs/api/generated",
  "theme": "default",
  "readme": "docs/api/index.md",
  "excludePrivate": true,
  "excludeProtected": true,
  "excludeInternal": true,
  "includeVersion": true,
  "categorizeByGroup": true,
  "categoryOrder": [
    "Core",
    "Debugging",
    "State Management",
    "LLM",
    "Semantic",
    "Governance",
    "Events",
    "Providers"
  ]
}
```

---

## Build Commands

```bash
# Generate TypeDoc
npm run docs:generate

# Serve locally
npm run docs:serve

# Build for deployment
npm run docs:build
```

---

## Acceptance Criteria
1. All public APIs documented
2. Examples for common use cases
3. Event reference complete
4. TypeDoc generates correctly
5. Migration guide helps users upgrade
6. Readable and well-organized
