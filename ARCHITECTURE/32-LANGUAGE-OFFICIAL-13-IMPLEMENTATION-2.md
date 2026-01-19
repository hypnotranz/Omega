## Ω Language Specification v1.0

### Part 15 — Code‑First Blueprint: Persistent Store, Multi‑Shot Resumptions, `amb` Search Runner (DFS/BFS/Best/Beam/Sample + Fair Scheduling), Full `syntax-rules` (Nested Ellipses + Binding‑Identity Literals), and Alpha‑Normalization for Differential Testing

This part is intentionally **implementation-shaped**: you can copy/paste these skeletons into the repo layout from Part 14 and flesh out the remaining boring glue. I’m going to be brutally explicit about the invariants, because these are the places where real Lisp systems and real nondeterministic evaluators usually go subtly wrong.

There are five deliverables:

1. **Persistent store** (or snapshot discipline) so multi-shot continuations are correct under mutation
2. **Multi-shot resumption capture** as *data*, not an accidental closure over mutable state
3. **`amb` handler suite** implemented as a **search runner** over CEKS states (supports DFS/BFS/Best/Beam/Sample, plus *fair scheduling* to avoid divergence starvation)
4. **`syntax-rules`** in TypeScript with **nested ellipses** and **binding-identity literal matching** (`free-identifier=?`)
5. **Alpha-normalization** of core AST so you can do reliable differential testing (host expander vs boot expander, etc.)

---

# 136. Persistent Store: The Memento You Must Not Fake

### The invariant you must satisfy

> If a continuation/resumption is invoked multiple times (“multi-shot”), then each invocation must start from the **same store snapshot**.

If your store is mutable and shared, your nondet evaluator becomes **unsound**: a write in one branch bleeds into another. This isn’t an “optimization detail.” It is semantics.

## 136.1 `src/core/eval/store.ts`

I’ll give you an interface that supports either:

* a true persistent map (HAMT/Trie), or
* a reference-grade copy-on-write `Map` snapshot.

```ts
// src/core/eval/store.ts
import type { Val } from "./values";

/**
 * StoreAddr is an explicit location (CESK-style). This enables mutation without
 * destroying lexical environment persistence.
 */
export type StoreAddr = number;

/**
 * Store is required to be *persistent* with respect to captured continuations.
 * "Persistent" here means: any snapshot referenced by a resumption must not
 * be affected by future writes in other branches.
 */
export interface Store {
  readonly next: number;

  /** Allocate a new cell initialized with v. Returns [newStore, addr]. */
  alloc(v: Val): [Store, StoreAddr];

  /** Read cell at addr; must throw on invalid addr in strict mode. */
  read(addr: StoreAddr): Val;

  /** Write cell; returns a new Store (or same Store if structural sharing). */
  write(addr: StoreAddr, v: Val): Store;

  /** Snapshot suitable for storing inside a multi-shot resumption. */
  snapshot(): Store;

  /** Debug: stable digest for receipts/differential tests (content-addressed). */
  digest(): string;
}

/**
 * Reference-grade Store implementation: copy-on-write Map snapshot.
 * Correct semantics, not fast. Replace with HAMT for production.
 */
export class COWStore implements Store {
  public readonly next: number;
  private readonly cells: Map<StoreAddr, Val>;

  constructor(next = 0, cells?: Map<StoreAddr, Val>) {
    this.next = next;
    this.cells = cells ?? new Map();
  }

  alloc(v: Val): [Store, StoreAddr] {
    const addr = this.next;
    const cells2 = new Map(this.cells);
    cells2.set(addr, v);
    return [new COWStore(addr + 1, cells2), addr];
  }

  read(addr: StoreAddr): Val {
    const v = this.cells.get(addr);
    if (v === undefined) {
      throw new Error(`Store.read: invalid addr ${addr}`);
    }
    return v;
  }

  write(addr: StoreAddr, v: Val): Store {
    if (!this.cells.has(addr)) {
      throw new Error(`Store.write: invalid addr ${addr}`);
    }
    const cells2 = new Map(this.cells);
    cells2.set(addr, v);
    return new COWStore(this.next, cells2);
  }

  snapshot(): Store {
    // Deep enough: Val is assumed immutable by convention; if not, you must deep-clone Val.
    return new COWStore(this.next, new Map(this.cells));
  }

  digest(): string {
    // Deterministic (slow) digest for testing.
    const entries = Array.from(this.cells.entries()).sort((a, b) => a[0] - b[0]);
    return JSON.stringify({ next: this.next, entries });
  }
}
```

### Design pattern callout

* This `Store` is a **Memento** in the GoF sense: it captures evaluation state.
* It is also the “S” of CESK: the explicit store that makes mutation manageable.

---

# 137. Multi‑Shot Resumptions as Data (Not Accidental Closures)

You want resumptions that are:

* **multi-shot** (callable multiple times)
* **referentially stable** (each call starts from the same captured snapshot)
* **serializable/digestible** (for receipts and debugging)

That means: treat a resumption like a record, not a JS closure over live objects.

## 137.1 `src/core/effects/opcall.ts`

```ts
// src/core/effects/opcall.ts
import type { Val } from "../eval/values";
import type { State } from "../eval/machine";

/**
 * A multi-shot resumption captured at an effect boundary.
 * Each invoke() must start from identical captured state.
 */
export type Resumption = {
  readonly rid: string;       // unique ID (uuid)
  readonly base: State;       // captured state snapshot (store must be persistent)
  readonly kind: "value";     // we resume by injecting a value into the suspended continuation
  invoke(v: Val): State;      // returns a new state beginning at the continuation point
  digest(): string;
};

export type OpCall = {
  readonly op: string;
  readonly args: Val[];
  readonly ctxDigest: string;     // for policies/receipts (not full ctx)
  readonly resumption: Resumption;
};
```

## 137.2 Capture function: `src/core/effects/capture.ts`

This is where you enforce snapshot semantics.

```ts
// src/core/effects/capture.ts
import type { Val } from "../eval/values";
import type { State, Control } from "../eval/machine";
import type { Resumption } from "./opcall";

/**
 * Create a multi-shot resumption.
 * The captured store MUST be persistent or snapshotted here.
 */
export function captureValueResumption(state: State, rid: string): Resumption {
  const base: State = {
    ...state,
    // hard rule: snapshot store for multi-shot use
    store: state.store.snapshot(),
    // handlers and kont should be treated as immutable structures; if you mutate them, snapshot too.
  };

  return {
    rid,
    base,
    kind: "value",
    invoke: (v: Val) => {
      // Inject value into suspended continuation by setting control = Val(v).
      // We snapshot store again if your store is NOT persistent. With persistent store, base.store is fine.
      const resumed: State = {
        ...base,
        control: { tag: "Val", v },
        // store: base.store.snapshot()  // optional extra isolation if you're paranoid
      };
      return resumed;
    },
    digest: () => {
      // Deterministic digest: used for receipts/debugging
      return JSON.stringify({
        rid,
        baseControl: base.control.tag,
        store: base.store.digest(),
        // You can add kont digest, handler stack digest, ctx digest, etc.
      });
    },
  };
}
```

### Why “resumption = base state + inject” is the right shape

* It makes resumptions **multi-shot by construction**.
* It avoids closure capture bugs where you accidentally capture a mutable `state` object by reference.
* It allows you to hash and ledger resumptions (critical for receipts and auditability).

---

# 138. `amb` as a Search Runner Over CEKS States (DFS/BFS/Best/Beam/Sample + Fair Scheduling)

This is the most correct and compositional approach if you want:

* completeness/fairness controls,
* divergence tolerance,
* integration with inference heuristics,
* and explicit budgets.

## 138.1 Key decision: `amb` is “control” → runner must schedule states

If you only run each branch “to completion” before exploring others, BFS isn’t fair and you cannot prevent divergence starvation. Therefore:

> The nondet interpreter is a **scheduler**: it interleaves evaluation steps across branches.

That’s the SICP “explicit-control evaluator” move, applied to nondeterminism.

### Essential concepts

* **Job**: a CEKS state with metadata (depth, score, constraints digest)
* **Frontier**: data structure controlling exploration order (Strategy)
* **Quantum**: max machine steps per job before yielding (fairness control)

## 138.2 `src/core/effects/nondet/types.ts`

```ts
// src/core/effects/nondet/types.ts
import type { State } from "../../eval/machine";
import type { Val } from "../../eval/values";

export type NondetMode = "first" | "all" | "best" | "sample";

export type ChoiceVec = Val[]; // amb.op passes choices as already-evaluated values (CBV). CBN can pass thunks.

export type ConstraintObs =
  | { tag: "Require"; predSyntaxHash: string; ok?: boolean }
  | { tag: "Note"; msg: string };

export type Job = {
  jid: string;
  state: State;
  depth: number;
  score: number;
  constraints: ConstraintObs[];
};

export type FrontierKind = "dfs" | "bfs" | "best" | "beam" | "sample";

export type NondetPolicy = {
  mode: NondetMode;
  frontier: FrontierKind;

  /** Fair scheduling quantum (steps per job). Required for BFS fairness under divergence. */
  quantumSteps: number;

  /** Beam width if frontier="beam". */
  beamWidth?: number;

  /** For best/sample: compute job score; can call inference (budgeted) */
  scoreChoice?: (ctx: { constraints: ConstraintObs[]; depth: number }, choice: Val) => Promise<number> | number;

  /** For pruning: return true to prune choice/job before exploring. */
  pruneChoice?: (ctx: { constraints: ConstraintObs[]; depth: number; score: number }, choice: Val) => Promise<boolean> | boolean;

  /** For best-solution aggregation: score complete results */
  scoreResult?: (v: Val) => Promise<number> | number;

  /** Optional RNG seed for sampling. */
  seed?: number;

  /** Step and wall budgets to bound explosion. */
  maxJobs?: number;
  maxTotalSteps?: number;
};

export type NondetResult =
  | { tag: "None" }
  | { tag: "One"; value: Val }
  | { tag: "Many"; values: Val[] };
```

## 138.3 Frontier implementations: `src/core/effects/nondet/frontier.ts`

Reference-grade data structures are fine; optimize later.

```ts
// src/core/effects/nondet/frontier.ts
import type { Job } from "./types";

export interface Frontier {
  push(job: Job): void;
  pop(): Job | undefined;
  size(): number;
  clear(): void;
}

export class StackFrontier implements Frontier {
  private xs: Job[] = [];
  push(job: Job) { this.xs.push(job); }
  pop() { return this.xs.pop(); }
  size() { return this.xs.length; }
  clear() { this.xs = []; }
}

export class QueueFrontier implements Frontier {
  private xs: Job[] = [];
  private head = 0;
  push(job: Job) { this.xs.push(job); }
  pop() {
    if (this.head >= this.xs.length) return undefined;
    const j = this.xs[this.head];
    this.head += 1;
    // compact occasionally
    if (this.head > 1024 && this.head * 2 > this.xs.length) {
      this.xs = this.xs.slice(this.head);
      this.head = 0;
    }
    return j;
  }
  size() { return this.xs.length - this.head; }
  clear() { this.xs = []; this.head = 0; }
}

export class PriorityFrontier implements Frontier {
  // Max-heap by job.score (reference grade)
  private heap: Job[] = [];
  push(job: Job) { heapPush(this.heap, job); }
  pop() { return heapPop(this.heap); }
  size() { return this.heap.length; }
  clear() { this.heap = []; }
}

function heapPush(heap: Job[], job: Job) {
  heap.push(job);
  let i = heap.length - 1;
  while (i > 0) {
    const p = Math.floor((i - 1) / 2);
    if (heap[p].score >= heap[i].score) break;
    [heap[p], heap[i]] = [heap[i], heap[p]];
    i = p;
  }
}

function heapPop(heap: Job[]): Job | undefined {
  if (heap.length === 0) return undefined;
  const top = heap[0];
  const last = heap.pop()!;
  if (heap.length > 0) {
    heap[0] = last;
    let i = 0;
    while (true) {
      const l = 2 * i + 1, r = 2 * i + 2;
      let m = i;
      if (l < heap.length && heap[l].score > heap[m].score) m = l;
      if (r < heap.length && heap[r].score > heap[m].score) m = r;
      if (m === i) break;
      [heap[i], heap[m]] = [heap[m], heap[i]];
      i = m;
    }
  }
  return top;
}

export class BeamFrontier implements Frontier {
  private heap: Job[] = [];
  constructor(private readonly width: number) {}
  push(job: Job) {
    heapPush(this.heap, job);
    if (this.heap.length > this.width) {
      // drop worst by rebuilding heap after removing min:
      // reference-grade: sort descending, truncate, rebuild
      this.heap.sort((a, b) => b.score - a.score);
      this.heap.length = this.width;
      this.heap = this.heap.slice();
      // rebuild heap structure
      const xs = this.heap.slice().sort((a, b) => a.score - b.score);
      this.heap = [];
      for (const j of xs) heapPush(this.heap, j);
    }
  }
  pop() { return heapPop(this.heap); }
  size() { return this.heap.length; }
  clear() { this.heap = []; }
}
```

## 138.4 The nondet runner: `src/core/effects/nondet/runner.ts`

This runner:

* maintains frontier of jobs,
* executes each job for `quantumSteps`,
* handles `amb.*` opcalls specially,
* delegates all other effects to the standard runtime dispatch (so inference/tools/etc still work).

### We need two runtime hooks

Your runtime should expose:

* `stepOnce(state): StepOutcome`
* `dispatchEffect(state, opCall): Promise<State | "Uncaught">`

So nondet runner can integrate cleanly.

Let’s define these minimal shapes.

## 138.4.1 Step outcome: `src/core/eval/machine.ts` (hook signature)

```ts
// src/core/eval/machine.ts
import type { State } from "./machine";
import type { OpCall } from "../effects/opcall";
import type { Val } from "./values";

export type StepOutcome =
  | { tag: "State"; state: State }
  | { tag: "Done"; value: Val; state: State }
  | { tag: "Op"; opcall: OpCall; state: State }; // state is the suspended state (before handling)
```

Now the nondet runner can iterate steps.

## 138.4.2 Runtime dispatch: `src/core/eval/runtime.ts` (hook signature)

```ts
// src/core/eval/runtime.ts
import type { State } from "./machine";
import type { OpCall } from "../effects/opcall";

/**
 * Dispatch an OpCall to the handler stack.
 * Returns:
 *  - new State to continue, if handled
 *  - "Uncaught" if no handler handles this op
 */
export type DispatchResult = State | "Uncaught";

export interface Runtime {
  dispatch(state: State, opcall: OpCall): Promise<DispatchResult>;
}
```

### Now the nondet runner:

```ts
// src/core/effects/nondet/runner.ts
import type { Runtime } from "../../eval/runtime";
import type { StepOutcome } from "../../eval/machine";
import { stepOnce } from "../../eval/machineStep"; // your CEKS stepper
import type { Val } from "../../eval/values";
import { captureValueResumption } from "../capture";
import type { OpCall } from "../opcall";
import type { ConstraintObs, Job, NondetPolicy, NondetResult } from "./types";
import { BeamFrontier, PriorityFrontier, QueueFrontier, StackFrontier, type Frontier } from "./frontier";

function mkFrontier(policy: NondetPolicy): Frontier {
  switch (policy.frontier) {
    case "dfs": return new StackFrontier();
    case "bfs": return new QueueFrontier();
    case "best": return new PriorityFrontier();
    case "beam": return new BeamFrontier(policy.beamWidth ?? 32);
    case "sample": return new StackFrontier(); // sample uses RNG to choose among forks, frontier trivial
    default: return new StackFrontier();
  }
}

function isVectorVal(v: Val): v is { tag: "Vector"; items: Val[] } {
  return (v as any).tag === "Vector";
}

function isRecordVal(v: Val): v is { tag: "Map"; entries: Array<[Val, Val]> } {
  return (v as any).tag === "Map";
}

function uuid(): string {
  return Math.random().toString(16).slice(2) + "-" + Date.now().toString(16);
}

export async function runNondet(
  runtime: Runtime,
  initial: State,
  policy: NondetPolicy
): Promise<NondetResult> {

  const frontier = mkFrontier(policy);
  const quantum = Math.max(1, policy.quantumSteps);
  const maxJobs = policy.maxJobs ?? 100_000;
  const maxTotalSteps = policy.maxTotalSteps ?? 10_000_000;

  let totalSteps = 0;
  let jobsCreated = 0;

  let bestScore = -Infinity;
  let bestVal: Val | null = null;
  const allVals: Val[] = [];

  // Seed frontier with initial job.
  frontier.push({
    jid: uuid(),
    state: initial,
    depth: 0,
    score: 0,
    constraints: [],
  });
  jobsCreated++;

  while (frontier.size() > 0) {
    if (jobsCreated > maxJobs) break;
    if (totalSteps > maxTotalSteps) break;

    const job = frontier.pop()!;
    const result = await runJobQuantum(runtime, job, policy, quantum);

    totalSteps += result.steps;

    // A job quantum can:
    // - finish with a value
    // - fork into multiple jobs
    // - fail (yield no job)
    // - yield (produce a continuation state to re-enqueue)
    for (const event of result.events) {
      switch (event.tag) {
        case "Yield":
          frontier.push(event.job);
          break;

        case "Fork":
          for (const j of event.jobs) {
            frontier.push(j);
            jobsCreated++;
            if (jobsCreated > maxJobs) break;
          }
          break;

        case "Success":
          if (policy.mode === "first") {
            return { tag: "One", value: event.value };
          }
          if (policy.mode === "all") {
            allVals.push(event.value);
          }
          if (policy.mode === "best") {
            const s = policy.scoreResult ? await policy.scoreResult(event.value) : 0;
            if (s > bestScore) {
              bestScore = s;
              bestVal = event.value;
            }
          }
          if (policy.mode === "sample") {
            // In sampling mode, we typically stop at first success by policy; you can make it probabilistic.
            return { tag: "One", value: event.value };
          }
          break;

        case "DeadEnd":
          // nothing
          break;
      }
    }
  }

  // finalize
  switch (policy.mode) {
    case "first": return { tag: "None" };
    case "all": return { tag: "Many", values: allVals };
    case "best": return bestVal ? { tag: "One", value: bestVal } : { tag: "None" };
    case "sample": return { tag: "None" };
  }
}

type JobQuantumEvent =
  | { tag: "Yield"; job: Job }                  // job not done, re-enqueue
  | { tag: "Fork"; jobs: Job[] }                // forked into multiple jobs
  | { tag: "Success"; value: Val }              // job completed with value
  | { tag: "DeadEnd" };                         // failure

type JobQuantumResult = { steps: number; events: JobQuantumEvent[] };

/**
 * Run a job for at most quantum steps, interpreting amb.* operations by forking/aborting,
 * and delegating other operations to the runtime's dispatch mechanism.
 */
async function runJobQuantum(
  runtime: Runtime,
  job: Job,
  policy: NondetPolicy,
  quantum: number
): Promise<JobQuantumResult> {

  let s = job.state;
  const events: JobQuantumEvent[] = [];
  let steps = 0;

  for (; steps < quantum; steps++) {
    const out: StepOutcome = stepOnce(s); // small-step CEKS

    if (out.tag === "State") {
      s = out.state;
      continue;
    }

    if (out.tag === "Done") {
      events.push({ tag: "Success", value: out.value });
      return { steps: steps + 1, events };
    }

    // effect boundary
    const op = out.opcall.op;

    // Handle amb ops at the runner level (this is the semantic interpreter for nondet).
    if (op === "amb.op") {
      const forked = await handleAmbOp(out.state, out.opcall, job, policy);
      events.push({ tag: "Fork", jobs: forked });
      return { steps: steps + 1, events };
    }

    if (op === "amb.fail") {
      // Branch failure: abort job.
      events.push({ tag: "DeadEnd" });
      return { steps: steps + 1, events };
    }

    if (op === "amb.cut") {
      // Delimited cut is more subtle: it should clear frontier *within delimiter*.
      // Since the runner owns frontier globally for this run, you implement cut by
      // emitting a special event and having the outer loop clear frontier.
      // Reference-grade: treat cut as noop in runner; implement at wrapper level.
      // We'll handle it by resuming with unit.
      const resumed = out.opcall.resumption.invoke({ tag: "Unit" } as any);
      s = resumed;
      continue;
    }

    if (op === "amb.observe") {
      // Side-channel observation: constraints telemetry for heuristics/receipts.
      // We treat it as unit and incorporate into job.constraints.
      // The opcall args likely include a record describing require predicates.
      const obs: ConstraintObs = { tag: "Note", msg: JSON.stringify(out.opcall.args) };
      job.constraints.push(obs);
      const resumed = out.opcall.resumption.invoke({ tag: "Unit" } as any);
      s = resumed;
      continue;
    }

    // Not an amb op: delegate to normal effect dispatch (oracle, tools, commits, etc.)
    const dispatched = await runtime.dispatch(out.state, out.opcall);
    if (dispatched === "Uncaught") {
      throw new Error(`Uncaught effect op: ${op}`);
    }
    s = dispatched;
  }

  // Quantum expired: yield updated job state back to frontier for fairness.
  events.push({
    tag: "Yield",
    job: { ...job, state: s },
  });
  return { steps, events };
}

async function handleAmbOp(
  suspendedState: State,
  opcall: OpCall,
  job: Job,
  policy: NondetPolicy
): Promise<Job[]> {

  // Convention: amb.op takes a single Vector of choices (CBV).
  // If you want CBN, the choices are thunks and must be forced by apply logic.
  const [choicesVal] = opcall.args;
  if (!choicesVal || !(choicesVal as any).tag) throw new Error("amb.op: malformed args");

  let choices: Val[] = [];
  if (isVectorVal(choicesVal)) {
    choices = choicesVal.items;
  } else {
    // fallback: treat args as choices list
    choices = opcall.args;
  }

  // Determine forked jobs by applying the resumption to each choice.
  // IMPORTANT: opcall.resumption must be multi-shot safe.
  const forkJobs: Job[] = [];

  for (const choice of choices) {
    const ctx = { constraints: job.constraints, depth: job.depth + 1 };

    const baseScore = policy.scoreChoice ? await policy.scoreChoice(ctx, choice) : 0;
    const prune = policy.pruneChoice ? await policy.pruneChoice({ ...ctx, score: baseScore }, choice) : false;
    if (prune) continue;

    const resumedState = opcall.resumption.invoke(choice);

    forkJobs.push({
      jid: uuid(),
      state: resumedState,
      depth: job.depth + 1,
      score: baseScore,
      constraints: job.constraints.slice(), // copy constraint log (persistent list would be nicer)
    });
  }

  // Sampling mode can choose one fork and discard the rest.
  if (policy.mode === "sample") {
    if (forkJobs.length === 0) return [];
    const idx = Math.floor(Math.random() * forkJobs.length); // replace with seeded RNG adapter
    return [forkJobs[idx]];
  }

  return forkJobs;
}
```

### What this buys you (and why it matches SICP “magic”)

* You have literally created a *new evaluator regime* (nondeterministic evaluation) without rewriting CEKS: you’re **interpreting** `amb.*` effects with a **scheduler**.
* Different search strategies are purely **Strategy** objects (frontiers + scoring functions).
* Fairness is explicit: you get dovetailing (run each branch for a bounded quantum then requeue).
* Inference becomes a scoring/pruning stage (policy-controlled) rather than ambient magic.

---

# 139. `syntax-rules` in TypeScript: Nested Ellipses + Binding‑Identity Literals

This is the other pillar: hygienic language-making.

I’m going to show you a TS implementation shape that is correct, modular, and testable.

## 139.1 Syntax objects and identifiers

You need:

* `Syntax` nodes that carry scopes
* `Ident` nodes with `name` + `scopes`
* `Env` that maps binders with stable `bid`

### `src/core/syntax/syntax.ts`

```ts
// src/core/syntax/syntax.ts
export type Scope = string;

export type Loc = { file?: string; line?: number; col?: number };

export type Syntax =
  | SAtom
  | SIdent
  | SList;

export type SAtom = { tag: "Atom"; value: number | string | boolean | null; scopes: Scope[]; loc?: Loc };
export type SIdent = { tag: "Ident"; name: string; scopes: Scope[]; loc?: Loc };
export type SList  = { tag: "List"; items: Syntax[]; scopes: Scope[]; loc?: Loc };

export const isIdent = (s: Syntax): s is SIdent => s.tag === "Ident";
export const isList  = (s: Syntax): s is SList  => s.tag === "List";
export const isAtom  = (s: Syntax): s is SAtom  => s.tag === "Atom";

export function addScope(stx: Syntax, sc: Scope): Syntax {
  const add = (scopes: Scope[]) => (scopes.includes(sc) ? scopes : [sc, ...scopes]);
  switch (stx.tag) {
    case "Atom": return { ...stx, scopes: add(stx.scopes) };
    case "Ident": return { ...stx, scopes: add(stx.scopes) };
    case "List": return { ...stx, scopes: add(stx.scopes), items: stx.items.map(i => addScope(i, sc)) };
  }
}

export function freshScope(counter: { n: number }): Scope {
  counter.n += 1;
  return `sc#${counter.n}`;
}
```

## 139.2 Binding environment and `free-identifier=?`

### `src/core/syntax/binding.ts`

```ts
// src/core/syntax/binding.ts
import type { Scope, SIdent } from "./syntax";

export type Bid = string;

export type Binding = {
  bid: Bid;
  name: string;
  scopes: Scope[];
  phase: number;
  kind: "value" | "syntax" | "module";
  value: unknown; // runtime value / transformer / module ref
};

export type Env = Binding[];

/** scopesA ⊆ scopesB */
function subset(scopesA: Scope[], scopesB: Scope[]): boolean {
  for (const sc of scopesA) if (!scopesB.includes(sc)) return false;
  return true;
}

export function resolveIdent(id: SIdent, env: Env, phase: number): Binding | null {
  const candidates = env.filter(b =>
    b.phase === phase &&
    b.name === id.name &&
    subset(b.scopes, id.scopes)
  );

  if (candidates.length === 0) return null;

  // maximal |b.scopes|
  candidates.sort((a, b) => b.scopes.length - a.scopes.length);
  const best = candidates[0];
  const second = candidates[1];
  if (second && second.scopes.length === best.scopes.length) {
    throw new Error(`resolveIdent ambiguity: ${id.name}`);
  }
  return best;
}

/**
 * free-identifier=? at a given phase.
 * If both bound: compare bid.
 * If both unbound: compare name.
 */
export function freeIdentifierEq(id1: SIdent, env1: Env, phase: number, id2: SIdent, env2: Env): boolean {
  const b1 = resolveIdent(id1, env1, phase);
  const b2 = resolveIdent(id2, env2, phase);
  if (b1 && b2) return b1.bid === b2.bid;
  if (!b1 && !b2) return id1.name === id2.name;
  return false;
}
```

---

## 139.3 `syntax-rules` representation

We want a compiled transformer that:

* captures definition environment at output phase (for literal binding identity)
* precomputes literal keys (`bid` or unbound) for fast matching
* stores rules: pattern + template

### `src/core/expand/syntaxRules.ts`

```ts
// src/core/expand/syntaxRules.ts
import type { Syntax, SIdent, Scope } from "../syntax/syntax";
import { isIdent, isList, addScope, freshScope } from "../syntax/syntax";
import type { Env, Binding } from "../syntax/binding";
import { resolveIdent, freeIdentifierEq } from "../syntax/binding";

export type SRRule = { pat: Syntax; tmpl: Syntax };

export type SRTransformer = {
  tag: "SyntaxRules";
  phaseOut: number;         // p_out
  envDefOut: Env;           // env at definition site, output phase
  literals: SIdent[];       // literal identifiers (syntax)
  litKeys: Map<string, { kind: "bound"; bid: string } | { kind: "unbound"; name: string }>;
  rules: SRRule[];
};

export function compileSyntaxRules(
  phaseOut: number,
  envDefOut: Env,
  literals: SIdent[],
  rules: SRRule[]
): SRTransformer {
  const litKeys = new Map<string, { kind: "bound"; bid: string } | { kind: "unbound"; name: string }>();
  for (const lit of literals) {
    const b = resolveIdent(lit, envDefOut, phaseOut);
    if (b) litKeys.set(lit.name, { kind: "bound", bid: b.bid });
    else litKeys.set(lit.name, { kind: "unbound", name: lit.name });
  }
  return { tag: "SyntaxRules", phaseOut, envDefOut, literals, litKeys, rules };
}
```

---

## 139.4 Pattern IR and ranked substitutions (`One`/`Many`)

This is the critical piece for nested ellipses.

```ts
// src/core/expand/syntaxRules.ts (continued)

/** Substitution value; nested Many represents nested ellipses. */
export type SubstVal =
  | { tag: "One"; stx: Syntax }
  | { tag: "Many"; items: SubstVal[] };

export type Subst = Map<string, SubstVal>;

const None: null = null;

function svOne(stx: Syntax): SubstVal { return { tag: "One", stx }; }
function svMany(items: SubstVal[]): SubstVal { return { tag: "Many", items }; }

function substGet(σ: Subst, x: string): SubstVal | null {
  return σ.get(x) ?? None;
}

function substSet(σ: Subst, x: string, v: SubstVal): Subst {
  const σ2 = new Map(σ);
  σ2.set(x, v);
  return σ2;
}

/**
 * Merge a list of substitutions σ_i into one where each variable x becomes Many([σ1(x),...,σk(x)]).
 * This is used when matching a repeated pattern element and collecting bindings for each repetition.
 */
function mergeMany(substs: Subst[]): Subst {
  const out: Subst = new Map();
  const vars = new Set<string>();
  for (const σ of substs) for (const k of σ.keys()) vars.add(k);

  for (const x of vars) {
    const items: SubstVal[] = [];
    for (const σ of substs) {
      const v = substGet(σ, x);
      if (!v) throw new Error(`mergeMany: missing var ${x} in repetition`);
      items.push(v);
    }
    out.set(x, svMany(items));
  }
  return out;
}

/**
 * Combine two substitutions with disjoint variables (reference-grade).
 * Real implementation should support consistent duplicates under certain conditions.
 */
function mergeDisjoint(σa: Subst, σb: Subst): Subst {
  const out = new Map(σa);
  for (const [k, v] of σb.entries()) {
    if (out.has(k)) throw new Error(`subst merge conflict on ${k}`);
    out.set(k, v);
  }
  return out;
}
```

---

## 139.5 Literal matching by binding identity (the real fix)

We match a pattern literal `L` against input identifier `I` at call site using:

* cached `litKeys` from definition environment
* resolution of `I` in call-site env at output phase

You must pass `envUseOut` to the matcher.

```ts
// src/core/expand/syntaxRules.ts (continued)

type MatchCtx = {
  transformer: SRTransformer;
  envUseOut: Env;     // call-site output-phase env
};

/** Is this ident a literal keyword? */
function isLiteral(ident: SIdent, tr: SRTransformer): boolean {
  return tr.litKeys.has(ident.name);
}

function literalMatches(lit: SIdent, input: SIdent, ctx: MatchCtx): boolean {
  const key = ctx.transformer.litKeys.get(lit.name);
  if (!key) return false;

  const phaseOut = ctx.transformer.phaseOut;
  const bInput = resolveIdent(input, ctx.envUseOut, phaseOut);

  if (key.kind === "bound") {
    return !!bInput && bInput.bid === key.bid;
  }
  // literal unbound at definition site: only matches unbound at use site by name
  return !bInput && input.name === key.name;
}
```

---

## 139.6 Matching algorithm with ellipses (deterministic, correct shape)

We implement the classic `syntax-rules` list pattern where `...` repeats the *preceding* element.

### Representation choice for ellipses

Use an identifier named `"..."` in syntax (or a special token) and recognize it in patterns/templates.

```ts
function isEllipsis(stx: Syntax): boolean {
  return isIdent(stx) && stx.name === "...";
}
```

### Matching

We’ll implement:

* `match(pat, stx, σ)` returns σ or null
* list matching supports repeated element `p0 ...` by trying k repetitions (max-first, then down to 0) — deterministic.

```ts
// src/core/expand/syntaxRules.ts (continued)

function matchPattern(pat: Syntax, stx: Syntax, σ: Subst, ctx: MatchCtx): Subst | null {
  // wildcard: identifier "_" matches anything
  if (isIdent(pat) && pat.name === "_") return σ;

  // identifiers
  if (isIdent(pat)) {
    if (isLiteral(pat, ctx.transformer)) {
      if (!isIdent(stx)) return null;
      return literalMatches(pat, stx, ctx) ? σ : null;
    }

    // pattern variable
    const x = pat.name;
    const bound = substGet(σ, x);
    if (!bound) {
      return substSet(σ, x, svOne(stx));
    }
    // consistency check (reference-grade: datum equality)
    if (bound.tag !== "One") return null;
    return syntaxDatumEq(bound.stx, stx) ? σ : null;
  }

  // atoms
  if (pat.tag === "Atom") {
    return (stx.tag === "Atom" && stx.value === pat.value) ? σ : null;
  }

  // lists
  if (pat.tag === "List") {
    if (stx.tag !== "List") return null;
    return matchList(pat.items, stx.items, σ, ctx);
  }

  return null;
}

function matchList(pats: Syntax[], items: Syntax[], σ: Subst, ctx: MatchCtx): Subst | null {
  // end cases
  if (pats.length === 0) return items.length === 0 ? σ : null;
  if (items.length === 0) {
    // can still match if remaining patterns are repeatable with 0 matches
    if (pats.length >= 2 && isEllipsis(pats[1])) {
      return matchRepeat(pats[0], pats.slice(2), items, σ, ctx);
    }
    return null;
  }

  // detect repetition: p0 followed by "..."
  if (pats.length >= 2 && isEllipsis(pats[1])) {
    return matchRepeat(pats[0], pats.slice(2), items, σ, ctx);
  }

  // normal one-to-one
  const σ1 = matchPattern(pats[0], items[0], σ, ctx);
  if (!σ1) return null;
  return matchList(pats.slice(1), items.slice(1), σ1, ctx);
}

/**
 * Match p0 repeated k times, then match rest against remaining items.
 * Deterministic: try maximal k down to 0.
 */
function matchRepeat(p0: Syntax, restPats: Syntax[], items: Syntax[], σ: Subst, ctx: MatchCtx): Subst | null {
  // try k = items.length down to 0
  for (let k = items.length; k >= 0; k--) {
    // match p0 against first k items, each producing its own σ_i
    const σs: Subst[] = [];
    let ok = true;

    for (let i = 0; i < k; i++) {
      const σi = matchPattern(p0, items[i], new Map(), ctx);
      if (!σi) { ok = false; break; }
      σs.push(σi);
    }
    if (!ok) continue;

    const σMany = mergeMany(σs);
    let σMerged: Subst;
    try {
      σMerged = mergeDisjoint(σ, σMany);
    } catch {
      continue;
    }

    const σRest = matchList(restPats, items.slice(k), σMerged, ctx);
    if (σRest) return σRest;
  }
  return null;
}
```

### Datum equality helper (reference-grade)

```ts
function syntaxDatumEq(a: Syntax, b: Syntax): boolean {
  if (a.tag !== b.tag) return false;
  switch (a.tag) {
    case "Atom": return (b as any).value === a.value;
    case "Ident": return (b as any).name === a.name; // reference-grade; for real you'd compare binding identity
    case "List": {
      const ax = a.items, bx = (b as any).items as Syntax[];
      if (ax.length !== bx.length) return false;
      for (let i = 0; i < ax.length; i++) if (!syntaxDatumEq(ax[i], bx[i])) return false;
      return true;
    }
  }
}
```

---

## 139.7 Template expansion with ellipses: rank‑aware zip replication

This is where nested ellipses either works or ruins your week.

### Core rules

* Template variable `x` expands to `σ(x)`:

  * must be `One` at that position (rank 0)
* If template contains `segment ...`, then `segment` is replicated:

  * number of times determined by any `Many` substitution used within `segment`
  * if multiple `Many` vars occur, their lengths must match (zip)

We implement:

* `segmentRepLen(segment, σ)` returns k
* `substProject(σ, i)` selects i-th element for all `Many` bindings, turning them into `One` (or deeper `Many` for nested)
* then expand segment under projected substitution

```ts
// src/core/expand/syntaxRules.ts (continued)

function expandTemplate(tmpl: Syntax, σ: Subst): Syntax {
  if (tmpl.tag === "Atom") return tmpl;

  if (tmpl.tag === "Ident") {
    const v = substGet(σ, tmpl.name);
    if (!v) return tmpl;               // literal identifier remains
    if (v.tag !== "One") throw new Error(`template var ${tmpl.name} expected One`);
    return v.stx;
  }

  // list template
  const outItems: Syntax[] = [];
  const items = tmpl.items;

  for (let i = 0; i < items.length; i++) {
    const t0 = items[i];
    const t1 = items[i + 1];

    if (t1 && isEllipsis(t1)) {
      const k = segmentRepLen(t0, σ);
      for (let j = 0; j < k; j++) {
        const σj = substProject(σ, j);
        outItems.push(expandTemplate(t0, σj));
      }
      i += 1; // skip ellipsis
      continue;
    }

    outItems.push(expandTemplate(t0, σ));
  }

  return { ...tmpl, items: outItems };
}

/** Determine repetition length for a template segment by finding any Many binding used within. */
function segmentRepLen(segment: Syntax, σ: Subst): number {
  const vars = new Set<string>();
  collectTemplateVars(segment, vars);

  let len: number | null = null;
  for (const x of vars) {
    const v = substGet(σ, x);
    if (!v) continue;
    if (v.tag === "Many") {
      const k = v.items.length;
      if (len === null) len = k;
      else if (len !== k) throw new Error(`ellipsis zip length mismatch for var ${x}: ${k} vs ${len}`);
    }
  }
  return len ?? 0;
}

function collectTemplateVars(t: Syntax, vars: Set<string>) {
  if (t.tag === "Ident") vars.add(t.name);
  else if (t.tag === "List") for (const it of t.items) collectTemplateVars(it, vars);
}

/**
 * Project substitution at index i:
 *  - One remains One
 *  - Many -> ith element (which may itself be One or Many for nested ellipses)
 */
function substProject(σ: Subst, i: number): Subst {
  const out: Subst = new Map();
  for (const [k, v] of σ.entries()) {
    if (v.tag === "One") out.set(k, v);
    else out.set(k, v.items[i]);
  }
  return out;
}
```

### Nested ellipses correctness

Nested ellipses work because:

* at the outer ellipsis, `Many` entries project to elements that may still be `Many`
* at the inner ellipsis, `segmentRepLen` sees the inner `Many` and replicates correctly

You’ve now implemented the “ranked substitution” discipline needed for nested ellipses.

---

## 139.8 Applying a transformer to a call site

Finally, `applySyntaxRules(tr, callStx, envUseOut)`:

* add use-site scope `U` to call syntax
* match each rule
* on match, add introducer scope `I` to template (or more precisely: to identifiers in template that are not substituted; reference-grade: scope entire template)
* expand template under substitution
* return expanded syntax
* then expander re-expands (macro chaining)

```ts
// src/core/expand/syntaxRules.ts (continued)

export function applySyntaxRules(
  tr: SRTransformer,
  callStx: Syntax,
  envUseOut: Env,
  scopeCounter: { n: number }
): Syntax {

  // use-site scope
  const U = freshScope(scopeCounter);
  const callU = addScope(callStx, U);

  for (const rule of tr.rules) {
    const σ0: Subst = new Map();
    const σ = matchPattern(rule.pat, callU, σ0, { transformer: tr, envUseOut });
    if (!σ) continue;

    // introducer scope
    const I = freshScope(scopeCounter);
    const tmplI = addScope(rule.tmpl, I); // reference-grade; production: add only to introduced identifiers
    return expandTemplate(tmplI, σ);
  }

  throw new Error("syntax-rules: no rule matched");
}
```

---

# 140. Alpha‑Normalization (Core AST): The Differential Harness That Makes Self‑Hosting Possible

To compare expanders/evaluators, you must compare **semantics**, not accidental binder names/scopes.

Alpha-normalization:

* renames bound variables deterministically (`x0`, `x1`, …)
* rewrites all bound occurrences to the canonical names
* leaves free variables unchanged

This is required for:

* host expander vs boot expander comparison
* macro receipts equivalence ignoring local binder IDs
* compiled-vs-interpreted equivalence checks

## 140.1 Core AST types (minimal)

Assume a core AST like:

```ts
// src/core/ast.ts
export type Expr =
  | { tag: "Lit"; value: unknown }
  | { tag: "Var"; name: string }
  | { tag: "Lambda"; params: string[]; body: Expr }
  | { tag: "If"; test: Expr; conseq: Expr; alt: Expr }
  | { tag: "Begin"; exprs: Expr[] }
  | { tag: "Define"; name: string; rhs: Expr }
  | { tag: "Set"; name: string; rhs: Expr }
  | { tag: "App"; fn: Expr; args: Expr[] }
  | { tag: "Quote"; datum: unknown }
  | { tag: "Effect"; op: string; args: Expr[] }
  | { tag: "Handle"; body: Expr; handler: unknown }
  | { tag: "Match"; scrut: Expr; clauses: Array<{ pat: unknown; body: Expr }>; elseExpr?: Expr };
```

Alpha normalization targets binding forms:

* `Lambda(params, body)`
* possibly `Define` (top-level bindings: treat as stable, don’t rename unless you’re comparing *modules* ignoring top-level names)
* `Match` patterns if they introduce binders (optional)
* `let` etc should already be lowered to lambda/app or explicit binder forms

## 140.2 `src/core/syntax/alpha.ts`

```ts
// src/core/syntax/alpha.ts
import type { Expr } from "../ast";

type Env = Map<string, string>;

type Supply = { n: number };

function fresh(s: Supply): string {
  const name = `x${s.n}`;
  s.n += 1;
  return name;
}

export function alphaNormalize(e: Expr): Expr {
  const supply: Supply = { n: 0 };
  return norm(e, new Map(), supply);
}

function norm(e: Expr, env: Env, supply: Supply): Expr {
  switch (e.tag) {
    case "Lit":
    case "Quote":
      return e;

    case "Var": {
      const n = env.get(e.name);
      return n ? { ...e, name: n } : e;
    }

    case "Lambda": {
      const env2 = new Map(env);
      const params2: string[] = [];
      for (const p of e.params) {
        const p2 = fresh(supply);
        env2.set(p, p2);
        params2.push(p2);
      }
      return { ...e, params: params2, body: norm(e.body, env2, supply) };
    }

    case "If":
      return { ...e, test: norm(e.test, env, supply), conseq: norm(e.conseq, env, supply), alt: norm(e.alt, env, supply) };

    case "Begin":
      return { ...e, exprs: e.exprs.map(x => norm(x, env, supply)) };

    case "App":
      return { ...e, fn: norm(e.fn, env, supply), args: e.args.map(a => norm(a, env, supply)) };

    case "Effect":
      return { ...e, args: e.args.map(a => norm(a, env, supply)) };

    case "Set":
      // set! targets an existing binding; rename if bound
      return { ...e, name: env.get(e.name) ?? e.name, rhs: norm(e.rhs, env, supply) };

    case "Define":
      // For alpha-eq of *expressions*, you often keep Define names stable.
      // For module-level comparisons, you may normalize top-level defines too, but be careful about exports.
      return { ...e, rhs: norm(e.rhs, env, supply) };

    case "Handle":
      // If handler contains expressions, normalize them too (depends on handler IR).
      return { ...e, body: norm(e.body, env, supply) };

    case "Match": {
      // If patterns bind vars, you need to extend env accordingly per clause.
      // Reference-grade: treat patterns as opaque; normalize bodies under same env.
      return {
        ...e,
        scrut: norm(e.scrut, env, supply),
        clauses: e.clauses.map(c => ({ ...c, body: norm(c.body, env, supply) })),
        elseExpr: e.elseExpr ? norm(e.elseExpr, env, supply) : undefined,
      };
    }

    default:
      // exhaustive check
      const _exhaustive: never = e;
      return _exhaustive;
  }
}

export function alphaEqual(a: Expr, b: Expr): boolean {
  const an = alphaNormalize(a);
  const bn = alphaNormalize(b);
  return JSON.stringify(an) === JSON.stringify(bn);
}
```

### Why alpha-normalization is enough

* Macro expansion differences often manifest only as different fresh names/scopes.
* Alpha-normalization collapses these differences while preserving binding structure.

This is the **Comparator** of your differential harness.

---

# 141. How These Pieces Interlock (The “Minimal Complete Circuit”)

Here’s the precise integration loop:

1. **Reader** produces `Syntax` with scopes empty
2. **Expander** (host or boot) introduces scopes and applies transformers (`syntax-rules`)
3. **Lowering** turns expanded `Syntax` into **core `Expr`**
4. **Evaluator** runs core via CEKS + effects
5. **Nondet runner** optionally executes CEKS under nondeterministic semantics by scheduling states and interpreting `amb.*` effects
6. **Differential harness** compares:

   * expanded core AST via `alphaEqual`
   * receipts via canonicalized digests
   * runtime outputs under test suites

This is the SICP “tower” realized as a CI-grade build pipeline.

---

# 142. The Two Critical “Correctness Traps” You Must Not Step Into

## Trap A — Multi-shot continuations with a mutable store

If you can do:

```scheme
(amb (begin (set! x 1) x)
     (begin (set! x 2) x))
```

and `x` is store-backed, your branches must not interfere.

You solve this only via:

* persistent store, or
* snapshotting at resumption capture, or
* store-versioning + copy-on-write

Anything else is unsound.

## Trap B — `syntax-rules` literal matching by name

If you match keywords by spelling, then importing a macro from another module and shadowing its literals will break semantics.

You solve this by:

* binding identities (`bid`)
* `free-identifier=?` comparison at the correct phase
* literal keys precomputed in the transformer’s def env

---

# 143. If you want the next continuation

If you say **continue**, Part 16 will be the *closing engineering loop*:

* a complete `stepOnce` CEKS implementation (the missing “machineStep” file) with correct `Effect` emission using `captureValueResumption`
* a concrete `Runtime.dispatch` implementation with delimited handlers (including `infer.op`, `commit.op`, and `amb.*` passthrough)
* and the full `syntax-rules` torture test suite as runnable `vitest` specs (H1–H5, E1–E4, P1–P3, N1–N5, R1–R3, D1–D2)

That will give you an end-to-end *executable* reference interpreter blueprint, including the self-hosting “promote expander” milestone.
