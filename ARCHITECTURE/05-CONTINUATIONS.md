# ⚠️ SUPERSEDED BY 32-6 EXPLICIT KONT FRAMES

> **THIS SPEC DESCRIBES CPS-STYLE CONTINUATIONS - 32-6 USES EXPLICIT FRAMES**
>
> ## What Changed
>
> This spec describes continuations as captured call stacks in a recursive evaluator.
> The 32-series uses **explicit Kont (Kontinuation) frames** in the CEKS machine:
>
> ```typescript
> // THIS SPEC (implicit continuation in recursive call):
> function evalExpr(expr, env, cont, ffi) {
>   // cont is "what to do with result"
>   // Captured via call/cc as a function
> }
>
> // 32-6 (explicit Kont frames in State):
> type Frame =
>   | { tag: 'ApplyFrame'; fn: Value; args: Value[] }
>   | { tag: 'IfFrame'; conseq: Expr; alt: Expr; env: Ctx }
>   | { tag: 'LetFrame'; bindings: [string, Value][]; body: Expr }
>   | { tag: 'HandlerFrame'; handlers: Handler[] }
>   | { tag: 'FixpointFrame'; iteration: number; lastSig: string }
>   // ... each continuation type is explicit
>
> type State = {
>   control: Control;
>   env: Ctx;
>   store: Store;
>   kont: Frame[];  // ← Explicit stack of frames
>   handlers: HandlerFrame[];
> };
> ```
>
> ## Why This Matters
>
> - **Explicit frames**: Each frame type can carry provenance marks, budget tracking
> - **Serializable**: Kont stack can be saved/restored (time-travel debugging)
> - **Inspectable**: Debugger can examine frame stack without magic
> - **Governance**: Frames can enforce security at each return point
>
> ## What's Still Valid
>
> The *concepts* (call/cc, escaping, time-travel) are still valid.
> The *implementation* changes from implicit to explicit.
>
> ## References
> - [32-6 CEKS Machine - Kont Frames](32-6-CEKS.md)
> - [ARCHITECTURE-REDESIGN-ASSESSMENT.md](../docs/ARCHITECTURE-REDESIGN-ASSESSMENT.md)

---

# 05: Continuations (First-Class Control Flow)

## What Is a Continuation?

A continuation is **"what happens next"** - the rest of the computation after the current point.

```lisp
;; In this expression:
(+ 1 (* 2 3))

;; When evaluating (* 2 3), the continuation is:
;; "take the result and add 1 to it"
;; Represented as: (lambda (x) (+ 1 x))
```

**First-class continuations** mean you can capture this "what happens next" as a value, store it, and invoke it later.

## Why Continuations Matter

| Feature | Without Continuations | With Continuations |
|---------|----------------------|-------------------|
| Time-travel | Rebuild everything from scratch | Just restore continuation |
| REPL at level | Hack: recreate environment | Natural: resume continuation |
| Early return | Exceptions (unwind stack) | Invoke continuation |
| Coroutines | Threads or generators | Continuation juggling |
| Backtracking | Custom implementation | Save/restore continuations |

## The call/cc Primitive

`call/cc` (call-with-current-continuation) captures the current continuation:

```lisp
;; Simple example: early return
(+ 1 (call/cc (lambda (k)
                (* 2 (k 10)))))  ; k is the continuation
;; Returns: 11
;; Because (k 10) "returns 10 to the + 1" immediately

;; The (* 2 ...) never happens!
```

## TypeScript Implementation

### Frame Structure

```typescript
// continuation.ts

// A single stack frame
interface Frame {
  id: string;
  type: 'apply' | 'if-test' | 'if-branch' | 'let-bind' | 'begin' | 'define';

  // The expression being evaluated
  expr: Value;

  // Environment at this point
  envId: string;

  // Continuation marks (for debugging)
  marks: Map<string, Value>;

  // Source location
  sourceLoc?: SourceLoc;

  // Parent frame (forms the stack)
  parent?: Frame;

  // Frame-specific data
  data?: any;
}

// The entire captured continuation
interface Continuation {
  frames: Frame[];
  currentFrameId: string;

  // The value we're "returning" through
  // null means we're still computing
  pendingValue?: Value;
}
```

### Continuation-Passing Style Evaluator

The key insight: rewrite the evaluator to explicitly pass continuations.

```typescript
// Traditional evaluator (implicit continuation)
function eval(expr: Value, env: Environment): Value {
  if (isNumber(expr)) return expr;
  if (isSymbol(expr)) return env.lookup(expr);
  if (isIf(expr)) {
    const test = eval(expr[1], env);  // Recursive call
    return test ? eval(expr[2], env) : eval(expr[3], env);
  }
  // ...
}

// CPS evaluator (explicit continuation)
function evalCPS(
  expr: Value,
  env: Environment,
  cont: Continuation,
  ffi: FFI
): Value {
  // Push frame for this evaluation
  const frame: Frame = {
    id: generateId(),
    type: 'eval',
    expr,
    envId: env.id,
    marks: new Map(),
    parent: cont.currentFrame,
  };
  cont = { ...cont, currentFrame: frame };

  // Self-evaluating
  if (isAtom(expr)) {
    return applyCont(cont, expr);
  }

  // Variable lookup
  if (isSymbol(expr)) {
    return applyCont(cont, env.lookup(expr));
  }

  // If expression
  if (isIf(expr)) {
    const testExpr = expr[1];
    const thenExpr = expr[2];
    const elseExpr = expr[3];

    // Create continuation for after test evaluation
    const afterTest: Continuation = {
      ...cont,
      currentFrame: {
        id: generateId(),
        type: 'if-test',
        expr,
        envId: env.id,
        marks: new Map(),
        data: { thenExpr, elseExpr, env },
        parent: cont.currentFrame,
      }
    };

    // Evaluate test with new continuation
    return evalCPS(testExpr, env, afterTest, ffi);
  }

  // call/cc - THE KEY PRIMITIVE
  if (isCallCC(expr)) {
    const proc = evalCPS(expr[1], env, cont, ffi);

    // Capture current continuation as a procedure
    const capturedCont: ContinuationProc = {
      type: 'continuation',
      frames: [...cont.frames, cont.currentFrame!],
      envSnapshot: snapshotEnv(env),
    };

    // Call proc with captured continuation
    return apply(proc, [capturedCont], cont, ffi);
  }

  // ... other forms
}

// Apply continuation (return value to "what's next")
function applyCont(cont: Continuation, value: Value): Value {
  const frame = cont.currentFrame;

  if (!frame) {
    // No more frames - we're done!
    return value;
  }

  // Dispatch based on frame type
  switch (frame.type) {
    case 'if-test':
      // We just evaluated the test; now evaluate branch
      const { thenExpr, elseExpr, env } = frame.data;
      const branch = value ? thenExpr : elseExpr;
      return evalCPS(branch, env, parentCont(cont), ffi);

    case 'apply':
      // We evaluated the operator; now apply it
      return applyProc(value, frame.data.args, parentCont(cont), ffi);

    // ... other frame types
  }
}

// Get parent continuation
function parentCont(cont: Continuation): Continuation {
  return {
    frames: cont.frames.slice(0, -1),
    currentFrame: cont.currentFrame?.parent,
  };
}
```

### Invoking a Captured Continuation

```typescript
function apply(
  proc: Value,
  args: Value[],
  cont: Continuation,
  ffi: FFI
): Value {
  // Invoking a captured continuation
  if (isContinuation(proc)) {
    if (args.length !== 1) {
      throw new Error('Continuation takes exactly one argument');
    }

    // ABANDON current continuation, use captured one
    return applyCont(
      {
        frames: proc.frames,
        currentFrame: proc.frames[proc.frames.length - 1],
      },
      args[0]  // The value to "return"
    );
  }

  // Normal procedure application...
}
```

## Continuation Marks

Continuation marks let you attach metadata to stack frames:

```typescript
// Attach a mark to current frame
function withContinuationMark(
  key: Value,
  value: Value,
  body: () => Value,
  cont: Continuation
): Value {
  cont.currentFrame!.marks.set(serialize(key), value);
  return body();
}

// Get all marks with given key from stack
function continuationMarks(key: Value, cont: Continuation): Value[] {
  const marks: Value[] = [];
  let frame = cont.currentFrame;

  while (frame) {
    const serializedKey = serialize(key);
    if (frame.marks.has(serializedKey)) {
      marks.push(frame.marks.get(serializedKey)!);
    }
    frame = frame.parent;
  }

  return marks;
}
```

**Use cases for marks:**
- **Debugging**: Attach source locations
- **Profiling**: Track timing info
- **Logging**: Structured log context
- **Security**: Track privilege levels

```lisp
;; In Lisp:
(with-continuation-mark 'context "parsing config"
  (parse-config file))

;; Later, in error handler:
(continuation-marks 'context)
;; → ("parsing config" "loading file" "main")
```

## Serialization for Time-Travel

The whole point: continuations are **data** and can be serialized!

```typescript
interface SerializedContinuation {
  frames: SerializedFrame[];
  currentFrameId: string;
}

interface SerializedFrame {
  id: string;
  type: string;
  expr: SerializedValue;
  envSnapshot: EnvSnapshot;
  marks: Record<string, SerializedValue>;
  data?: any;
  parentId?: string;
}

function serializeContinuation(cont: Continuation): SerializedContinuation {
  return {
    frames: cont.frames.map(f => ({
      id: f.id,
      type: f.type,
      expr: serialize(f.expr),
      envSnapshot: snapshotEnv(f.env),
      marks: Object.fromEntries(
        Array.from(f.marks.entries()).map(([k, v]) => [k, serialize(v)])
      ),
      data: f.data,  // Assuming data is serializable
      parentId: f.parent?.id,
    })),
    currentFrameId: cont.currentFrame?.id ?? '',
  };
}

function deserializeContinuation(
  s: SerializedContinuation,
  primitives: Map<string, Primitive>
): Continuation {
  // Reconstruct frame chain...
}
```

## Practical Example: Time-Travel Snapshot

```typescript
// Capture snapshot at any point
function captureSnapshot(cont: Continuation, env: Environment, world: World): Snapshot {
  return {
    continuation: serializeContinuation(cont),
    environment: snapshotEnv(env),
    world: world.snapshot(),
    timestamp: Date.now(),
  };
}

// Restore and continue execution
function restoreSnapshot(snapshot: Snapshot, ffi: FFI): Value {
  const cont = deserializeContinuation(snapshot.continuation, ffi.primitives);
  const env = Environment.fromSnapshot(snapshot.environment);
  const world = World.fromSnapshot(snapshot.world);

  // Continue where we left off!
  return applyCont(cont, snapshot.pendingValue);
}
```

## Comparison to Other Languages

| Language | Continuations | Notes |
|----------|---------------|-------|
| **Scheme** | First-class call/cc | Full power |
| **Racket** | First-class + delimited | Even more control |
| **Clojure** | None (JVM limitation) | Uses other patterns |
| **Common Lisp** | None (historical) | Uses conditions instead |
| **JavaScript** | Generators (limited) | yield is half a continuation |
| **Python** | Generators (limited) | Same |
| **LambdaLLM** | First-class | For time-travel |

## Summary

Continuations give us:

1. **Time-travel**: Serialize continuation → restore later
2. **REPL at level**: Capture continuation → interact → resume
3. **Early return**: Invoke continuation with value
4. **Debugging**: Continuation marks for context

The cost: More complex evaluator (CPS style). But the benefits for an AI debugging system are enormous.
