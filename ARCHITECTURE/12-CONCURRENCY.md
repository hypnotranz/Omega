# 12: Concurrency Model

## Overview

LambdaLLM has a **single-threaded evaluator** with **async I/O**. True parallelism is achieved through:
- Async/await for I/O operations
- Promises for concurrent requests
- Future: Actors or workers for computation

---

## Async Operations

### FFI Calls Are Async

All FFI calls (world, LLM, network) return promises:

```lisp
;; Async call
(await (llm.complete "prompt"))

;; Fire and forget
(llm.complete "prompt")  ; Returns promise, doesn't wait
```

### Implementation

```typescript
// FFI handler returns Promise
ffi.register('llm.complete', async (prompt: string): Promise<string> => {
  const response = await fetch(LLM_URL, {
    method: 'POST',
    body: JSON.stringify({ prompt })
  });
  return response.text();
});

// In evaluator
if (isAwait(expr)) {
  const promise = evalExpr(expr[1], env, cont, ffi);
  return promise.then(value => applyCont(cont, value));
}
```

---

## Concurrent Operations

### Parallel Promises

```lisp
;; Wait for all
(await-all
  (llm.complete "prompt 1")
  (llm.complete "prompt 2")
  (llm.complete "prompt 3"))
;; Returns list of results

;; Race (first to complete)
(await-any
  (llm.complete "fast model" :model "haiku")
  (llm.complete "slow model" :model "opus"))
;; Returns first result
```

### Implementation

```typescript
// await-all primitive
ffi.register('await-all', async (...promises: Promise<Value>[]): Promise<Value[]> => {
  return Promise.all(promises);
});

// await-any primitive
ffi.register('await-any', async (...promises: Promise<Value>[]): Promise<Value> => {
  return Promise.race(promises);
});
```

---

## Timeout and Cancellation

### Timeout

```lisp
(with-timeout 5000
  (await (slow-operation)))
;; Signals 'timeout condition after 5 seconds
```

### Cancellation

```lisp
(define task (async (long-computation)))
(cancel task)
```

### Implementation

```typescript
class CancellablePromise<T> {
  private controller = new AbortController();
  readonly promise: Promise<T>;

  constructor(executor: (signal: AbortSignal) => Promise<T>) {
    this.promise = executor(this.controller.signal);
  }

  cancel(): void {
    this.controller.abort();
  }
}
```

---

## Structured Concurrency

### Scope-Based Concurrency

```lisp
(concurrent-scope
  ;; All tasks must complete before scope exits
  (spawn (task-1))
  (spawn (task-2))
  (spawn (task-3)))
;; Waits for all spawned tasks
;; Cancels remaining if one fails
```

### Nursery Pattern (from Trio)

```lisp
(with-nursery (nursery)
  (nursery-spawn nursery (lambda () (task-1)))
  (nursery-spawn nursery (lambda () (task-2)))
  ;; Nursery waits for all children
  ;; Propagates errors properly
  )
```

---

## Future: Actor Model

For true parallelism, actors run in separate workers:

```lisp
;; Define actor behavior
(defactor counter (state)
  (on :increment
    (become (counter (+ state 1))))
  (on :get
    (reply state)))

;; Create actor
(define c (spawn-actor counter 0))

;; Send messages
(send c :increment)
(send c :increment)
(await (ask c :get))  ; → 2
```

### Actor Implementation

```typescript
// Actors run in Web Workers / Worker Threads
interface Actor {
  id: string;
  worker: Worker;
  mailbox: Message[];
}

function spawnActor(behavior: Value, initialState: Value): Actor {
  const worker = new Worker('actor-runtime.js');
  worker.postMessage({ type: 'init', behavior, state: initialState });
  return { id: generateId(), worker, mailbox: [] };
}

function send(actor: Actor, message: Value): void {
  actor.worker.postMessage({ type: 'message', payload: message });
}

async function ask(actor: Actor, message: Value): Promise<Value> {
  return new Promise(resolve => {
    const id = generateId();
    actor.worker.postMessage({ type: 'ask', id, payload: message });
    actor.worker.onmessage = (e) => {
      if (e.data.id === id) resolve(e.data.result);
    };
  });
}
```

---

## Synchronization Primitives

### Atoms (Compare-and-Swap)

```lisp
(define counter (atom 0))
(swap! counter inc)       ; Atomically increment
(reset! counter 0)        ; Atomically set
(deref counter)           ; Read current value
```

### Channels (CSP-style)

```lisp
(define ch (chan))
(go (put! ch (expensive-computation)))  ; Producer
(go (println (take! ch)))               ; Consumer
```

### Locks (Low-level)

```lisp
(define lock (make-lock))
(with-lock lock
  (critical-section))
```

---

## Event Loop Integration

### Browser

Uses browser event loop directly:

```typescript
// In browser, async operations use native promises
// setTimeout, fetch, etc. all work naturally
```

### Node.js

Uses Node event loop:

```typescript
// Node's libuv handles async I/O
// Worker threads for CPU-bound tasks
```

### Single Threaded Guarantees

The evaluator is **always** single-threaded:
- No race conditions in Lisp code
- No locks needed for normal operations
- Async operations yield but don't interleave

---

## Error Handling in Async Code

### Unhandled Rejections

```lisp
(handle-async-errors
  (lambda (error)
    (log-error error)
    (cleanup)))

(await (might-fail))  ; Error goes to handler if unhandled
```

### Timeout as Condition

```lisp
(handler-bind
  ((timeout (lambda (c)
              (invoke-restart 'use-cached))))
  (with-timeout 1000
    (restart-case
      (await (slow-api-call))
      (use-cached () (get-cached-value)))))
```

---

## Performance Considerations

| Operation | Single-Threaded | Async | Worker |
|-----------|-----------------|-------|--------|
| Pure computation | ✅ Fast | N/A | ✅ Parallel |
| I/O (file, network) | ❌ Blocks | ✅ Non-blocking | N/A |
| LLM calls | ❌ Blocks | ✅ Concurrent | N/A |
| CPU-intensive | ✅ Sequential | N/A | ✅ Parallel |

### Recommendations

1. **Always await I/O**: Never block on network/file operations
2. **Use await-all**: Parallelize independent LLM calls
3. **Workers for CPU**: Offload heavy computation to workers
4. **Actors for state**: Use actors for shared mutable state

---

## Example: Parallel LLM Calls

```lisp
(define (analyze-files files)
  ;; Analyze all files in parallel
  (await-all
    (map (lambda (f)
           (llm.complete
             (str "Analyze this code:\n" (world.read f))))
         files)))

;; Usage
(analyze-files '("main.ts" "utils.ts" "config.ts"))
;; Makes 3 concurrent LLM calls
```
