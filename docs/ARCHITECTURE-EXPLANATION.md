# RSR-01 — The Two-Loops Architecture

## Reentrant Semantic Runtime for LLM-augmented Lisp evaluation

> **Doc intent:** repo-ready, engineering-facing architecture spec for *what you already built* (CEKS + effects + session protocol + receipts + snapshots + adapters), with *explicit* coverage of the practical “this kinda shit” you care about: **provenance, state/context continuity, deterministic replay, checkpoint/resume, interception/mocking, and context-window management**.

---

## 0. Naming: stop saying “oracle”

You’re right to hate the term. It implies “external authority service.” What you actually have is a **reentrant effect boundary** plus a **runtime driver**.

**Recommended canonical name (conceptual):**

* **RSR — Reentrant Semantic Runtime**

**Recommended canonical name (code-facing):**

* `EffectSession` (instead of `OracleSession`)
* `EffectReq` / `EffectResp` (instead of `OracleReq` / `OracleResp`)
* `RuntimeDriver` (instead of “switchboard”)
* `EffectLedger` (instead of `ReceiptStore`, if you want the event-sourcing semantics to be explicit)

> You can keep the legacy names in types for now (rename later). This doc will refer to **legacy names in parentheses** where helpful, but the *meaning* is RSR.

---

## 1. The big picture (what you built in one diagram)

You built a **metacircular interpreter** (eval/apply) that can **suspend** at effect boundaries and be **resumed** by a driver that performs external actions (LLM calls, tools, observation), while persisting **receipts** and **snapshots** for provenance + replay.

### 1.1 Two mutually recursive loops

There are **two “fixed-point loops”** now:

1. **Language loop (SICP loop):**
   `eval(exp, env) ↔ apply(proc, args)`
   This gives you *homoiconic metacircularity* (code-as-data, procedures-as-values, lexical scope).

2. **Effect loop (RSR loop):**
   `machine.step() → yield Req … driver.handle(Req) → send Resp → machine.step()`
   This gives you **reentrancy**, **auditability**, **replay**, **checkpoint/resume**, and **mockability**.

### 1.2 System diagram

```
                 ┌────────────────────────────────────────┐
                 │              USER REPL / API            │
                 └────────────────────────────────────────┘
                                   │
                                   ▼
┌───────────────────────────────────────────────────────────────────┐
│                  CEKS / Evaluator (metacircular core)             │
│   - Expr, Val, Env                                                │
│   - eval/apply                                                    │
│   - continuations/frames                                          │
│   - algebraic effects: infer.op, tool, observe, …                 │
│                                                                   │
│   step() either:                                                  │
│     a) reduces internally                                         │
│     b) SUSPENDS with a Req (yield)                                │
└───────────────────────────────────────────────────────────────────┘
                 ▲                                   │
                 │ send Resp                         │ yield Req
                 │ (resume continuation)             │ (effect boundary)
                 │                                   ▼
┌───────────────────────────────────────────────────────────────────┐
│                      RuntimeDriver (RSR driver)                   │
│   - dispatch Req → handler                                        │
│   - enforce policy/budget (even if minimal now)                   │
│   - record receipts (EffectLedger / ReceiptStore)                 │
│   - snapshot & restore (SnapshotRepo)                             │
│   - choose adapter (Live LLM, Scripted, Replay)                   │
└───────────────────────────────────────────────────────────────────┘
                 │             │                 │
                 ▼             ▼                 ▼
        ┌────────────┐  ┌─────────────┐  ┌─────────────┐
        │ LLM Portal  │  │ Tool Router │  │ Observation │
        │ (PortalImpl)│  │ (ReqTool)   │  │ (ReqObserve)│
        └────────────┘  └─────────────┘  └─────────────┘
                 │
                 ▼
        ┌──────────────────┐
        │ Receipts / Cache  │
        │ (ReceiptStore)    │
        └──────────────────┘
```

---

## 2. Vocabulary: the minimal “top-level words” you identified

You were correct to zoom in on:

* **expression**
* **environment**
* **procedure**
* **arguments**
* **eval / apply**

RSR adds:

* **continuation** (the rest of the computation)
* **effect request/response** (external boundary)
* **receipt** (persisted effect trace)
* **snapshot** (persisted machine state)
* **adapter** (strategy for performing requests)

### 2.1 Expressions (Expr)

An `Expr` is a **syntactic object** in the object language (your Lisp). It is *data*.

Core shape is standard SICP (self-evaluating, variable, quote, if, lambda, begin, application, define, set!, …), plus your **effect forms**:

* `(effect infer.op payload...)`
* `(effect tool.call payload...)`
* `(effect observe payload...)`

> You already have the CEKS machine and effect handler machinery; the doc is describing the semantics that makes the machinery *valuable*.

### 2.2 Environment (Env)

An `Env` is a **lexically-scoped binding structure** (chain of frames) mapping identifiers → values.

Even if you currently represent it as `Map`, conceptually it’s:

* a **persistent data structure** (copy-on-write store already exists: `COWStore`)
* with stable **references/hashes** (you already have `EnvRef` / `Hash`)
* and can be snapshotted (SnapshotRepo already exists)

### 2.3 Procedures (Proc / Val)

Values include procedures:

* **Primitive procedures**: base-case realizers (e.g., deterministic string functions, JSON parsers, also **infer/tool** wrappers depending on your design)
* **Compound procedures**: closures capturing `(params, body, env)`

In SICP: primitives bottom out at machine ops.
In RSR: primitives may bottom out at **effects** that require driver mediation.

### 2.4 Continuation (CEKS frames)

This is your “breadcrumb trail” insight made concrete:

* continuation is the structured remainder of the computation
* captured explicitly as CEKS frames (you said you have this)
* snapshotting a continuation enables **checkpoint/resume**

---

## 3. The effect algebra you already have (what it *means*)

You’ve implemented a request/response protocol (legacy `OracleReq/OracleResp`) and a session type (legacy `OracleSession = AsyncGenerator<Req, Resp, Return>`).

This protocol is not cosmetic. It is the **semantic seam** where you obtain:

* provenance
* replay
* mocking
* checkpointing
* policy enforcement
* context shaping

### 3.1 Session type (conceptual)

```ts
// legacy names in parentheses
type EffectSession = AsyncGenerator<EffectReq, EffectResp, EffectReturn>;
```

Operationally:

* The evaluator runs until it either returns a value OR suspends on an effect.
* Suspension yields a `Req`.
* Driver supplies a matching `Resp` to resume.
* This repeats until termination.

### 3.2 Request algebra (subset you listed as “we have”)

You said you have at least:

* `ReqEval(qexpr, envRef)`
* `ReqApply(fn, args, envRef)`
* `ReqObserve(ctxRef, schema)` (basic)
* `ReqTool(call)` (exists; maybe stub)
* `ReqReturn(meaning/value)`
* `ReqFail(reason)`

These are **Commands** (GoF Command Pattern): they are *intent objects*.

That matters because once “intent” is a value, you can:

* persist it
* hash it
* replay it
* route it
* test it
* transform it (rewrite the request without touching the evaluator)

---

## 4. The Two-Loops operational semantics (the actual core)

This section is the “you should put this in docs and everyone will stop being confused” part.

### 4.1 Loop A — Language reduction (eval/apply)

Inside the CEKS machine:

* small-step evaluation reduces the state
* until it hits an effect frame, at which point it **does not** perform it; it **reifies** it as a request

This is the crucial design choice:

> **Effects are reified** (turned into data) rather than executed inline.

That single choice unlocks everything below.

### 4.2 Loop B — Effect handling (driver)

The driver is a deterministic interpreter over `Req` objects:

```ts
async function runSession(session: EffectSession, driver: RuntimeDriver): Promise<EffectReturn> {
  let yielded = await session.next(undefined as any);
  while (!yielded.done) {
    const req = yielded.value;              // EffectReq (legacy OracleReq)
    const resp = await driver.handle(req);  // EffectResp
    yielded = await session.next(resp);     // resume continuation
  }
  return yielded.value; // EffectReturn
}
```

That is the “switchboard” you were confused about.
**It’s not a random metaphor. It is literally the driver loop that routes effect-requests to effect-handlers.**

If you want a better name than “switchboard”:

* **RuntimeDriver**
* **EffectDispatcher**
* **ReentrancyDriver**
* **EffectKernel**

(“Kernel” is accurate: it mediates side effects for user-space evaluation.)

---

## 5. Why this solves provenance (not just “logging”)

You asked: *agents are already becoming long-running and people complain about provenance and maintaining state/context — does this help?*

**Yes.** But the key is to be precise about *what provenance means*.

### 5.1 Provenance is a dependency graph, not a transcript

In RSR, every effect boundary yields a `Req` that *names*:

* what was requested (Expr / ToolCall / Observe schema)
* under what environment (EnvRef)
* at what point in continuation (implicitly via snapshot/StateRef)

That becomes a **provenance DAG**:

* Nodes: receipts
* Edges: “receipt B depends on receipt A’s response” or “this response was computed under env hash H that was produced after earlier steps”
* Roots: initial program + initial env

### 5.2 Receipt = stable artifact

A receipt is not “a log line.” It is a **content-addressed event**.

Minimal useful receipt fields (conceptual):

```ts
type Receipt = {
  key: string;            // hash(req + resp + metadata)
  req: EffectReq;
  resp: EffectResp;
  envRef?: string;        // or embedded in req
  stateRef?: string;      // optional: snapshot ref
  meta: {
    timestamp: number;
    wallMs: number;
    model?: string;
    decoding?: object;
    toolVersions?: Record<string,string>;
  };
};
```

**Why this matters:**

* When someone asks “why did it do that?”, you can traverse the receipt chain.
* When someone asks “what did the model actually see?”, you can reconstruct the prompt artifact from the recorded request payload.
* When someone asks “did it use tool X?”, you can prove it.

This is the foundation for *audit-grade provenance*.

---

## 6. Deterministic replay (the killer feature for engineering)

You already summarized it; here is the precise model and why it’s not “overengineering.”

### 6.1 Replay is an alternate interpreter for effects

Deterministic replay is achieved by swapping the handler for `infer/tool/observe` effects:

* **Live handler:** call external API
* **Replay handler:** look up receipt by request hash and return recorded response

This is **Strategy Pattern** over effect execution.

### 6.2 Why “same program, same result” becomes possible

Without effect reification, you can’t replay, because the external calls happen inline and vanish.

With RSR:

* the `Req` is *the* canonical representation of “what the outside world was asked”
* so `Req` → `Resp` can be treated as a memoized function

### 6.3 What you can test now that was painful before

You can now write tests like:

* “This semantic program should call `infer` exactly twice.”
* “The first call should be schema-constrained and temperature 0.”
* “The second call should only happen if predicate returns false.”
* “Tool router should not be invoked in airgapped profile.”
* “Given recorded receipts, execution is deterministic.”

This is not possible with transcript-only agent frameworks without bespoke brittle mocking.

---

## 7. Mid-computation checkpointing (explicit continuation = resumable computation)

This is the second practical “this kinda shit” capability.

### 7.1 What checkpointing means in RSR

Checkpointing means persisting:

* **machine state** (including continuation stack)
* **environment reference** (or full env snapshot)
* **receipt ledger** up to that point (so replay is possible)

You said you have:

* `SnapshotRepo` (EnvRef/StateRef)
* `ReceiptStore`

That is exactly the scaffolding required.

### 7.2 Why this is not the same as “save chat history”

Chat-history persistence is not checkpointing; it’s just retaining a transcript.

Checkpointing in RSR:

* resumes at *exact continuation point*
* with the *same lexical bindings*
* and the *same evaluation context*

This is closer to:

* JVM/BEAM process snapshots
* continuation persistence
* durable workflows

### 7.3 Real use cases

* multi-hour research/refactoring agents
* long-running extraction pipelines with intermittent tool failures
* human-in-the-loop tasks where you pause and later resume without losing state
* cost control (resume from checkpoint without re-paying for earlier steps via replay)

---

## 8. Interception / mocking (the engineering superpower)

This is what turns “cool runtime” into “CI/CD viable.”

### 8.1 Why HTTP mocking is the wrong layer

If you mock at HTTP, you tie tests to:

* exact vendor API formats
* nonsemantic incidental parameters
* request ordering artifacts

RSR lets you mock at the **semantic boundary**: `Req → Resp`.

That boundary is stable and under your control.

### 8.2 Scripted adapter (you said you already have it)

A `ScriptedOracleAdapter` / scripted portal is a deterministic handler that:

* returns canned responses for known requests
* optionally asserts expected request shapes

This is essentially:

* **Test Double** for effect execution
* plus **Contract Testing** of semantic intent

---

## 9. Context window overflow: “making implicit state explicit”

You explicitly asked about maintaining state/context; here’s the honest story with what you have now.

### 9.1 What RSR already improves today

Even before you implement full compression/hydration, RSR already helps because:

* the evaluator’s continuation is explicit
* environments are explicit (and snapshot-able)
* you do not need to stuff *everything* into the LLM prompt to preserve computation state

In other words:

> **Not all “state” must be in the token context if the runtime carries state structurally.**

This is the crucial distinction between:

* **semantic state** (bindings, continuation, intermediate values)
* **linguistic context** (what you show the model)

Most agent frameworks conflate them and pay the price.

### 9.2 The operational trick you can do right now

Because you have receipts, you can reconstruct “what happened” without always re-sending everything to the model.

A pragmatic pattern:

* only pass the *minimal* necessary context to the model
* include references (receipt keys) for audit
* store full provenance out-of-band

This yields cheaper, more controlled prompts while preserving traceability.

### 9.3 What’s missing (but now implementable cleanly)

You correctly noted missing:

* `ReqCompress(ctxRef)`
* `ReqHydrate(receiptRef)`
* VOI-driven selection
* formal “context economics”

But the key point is: **RSR makes this implementable as a library**, not as a fragile prompt trick.

Because “compress” and “hydrate” can be:

* semantic procedures in Lisp (composed, tested)
* plus driver-level enforcement (budgets, caps)

---

## 10. The honest boundaries (what this does *not* magically fix)

This doc must be explicit about non-goals, or people will project fantasy.

RSR does **not**:

* make the LLM transparent
* make inference deterministic without replay
* turn “string in/string out” into formal proofs (unless you add proof regime tooling)
* eliminate prompt sensitivity

RSR **does**:

* make *the computation around inference* principled, inspectable, and engineering-grade

That distinction matters.

---

## 11. “Mind-blowing” usage examples (real, not 5+3)

The point is not arithmetic; the point is **semantic composition + reentrancy + traceability**.

### Example A — “Mechanical vs Semantic Strategy” (your slur-removal scenario)

Goal: remove offensive language without breaking grammar/meaning, choosing strategy dynamically.

Program sketch:

1. `FindOffensiveSpans(text)` — semantic classifier (infer)
2. `RemoveSpansMechanically(text, spans)` — deterministic transform
3. `MeaningPreserved?(before, after)` — semantic equivalence check (infer)
4. fallback to `RewriteToRemoveOffense(text)` if needed (infer)

Why RSR matters:

* receipts capture each semantic judgment
* replay yields deterministic behavior in tests
* you can diff “why did it choose rewrite instead of mechanical removal?”

### Example B — “Reentrant inference that calls back into Lisp”

This is the *metacircular extension* to LLMs you’ve been asking for.

You want the model to be able to say (implicitly or explicitly):

* “evaluate this expression to get a helper value”
* “apply this function to that argument”
* then continue its own generation

That is exactly what `ReqEval` / `ReqApply` enable **as reentrancy**.

This is not “tools.” This is:

> **LLM is a primitive procedure that can invoke the evaluator to compute subresults mid-inference.**

The runtime becomes a *bidirectional* evaluator:

* evaluator calls LLM (infer effect)
* LLM calls evaluator (reentrant eval/apply requests)
* everything is receipted

This is the real “strange loop” extension.

### Example C — “Synthesize a new semantic function, install it, then use it as an HOF”

The SICP/Hofstadter move:

1. Use semantic inference to synthesize code (AST)
2. `eval` the AST into the environment
3. now it’s a normal closure usable with `map/filter/fold`

RSR makes it non-handwavy because:

* the synthesis step is a receipted effect
* the installed code is an artifact
* replay makes it testable and repeatable

---

## 12. Where the “value” lands in real systems (not research demos)

You asked for *real value*. Here’s the concrete, operational list.

### 12.1 Provenance & audit

* “Why did the agent do X?” → traverse receipts.
* “What did it see?” → reconstruct prompts/tools/observations from stored requests.
* “Did it call tool Y?” → prove via receipt chain.

### 12.2 Cost control & regression testing

* replay means you can run full semantic test suites in CI without paying API costs
* you can bisect semantic regressions by comparing receipt graphs

### 12.3 Reliability

* checkpointing means long workflows survive crashes
* you can resume from the last stable continuation state

### 12.4 Maintainability (this is underrated)

* semantic functions are real procedures with lexical scope
* refactorable like normal FP code
* compositional by default (map/filter/fold, closures, partial application)

This is a massive difference from “prompt spaghetti.”

---

## 13. FAQ: “is this infrastructure too early?”

This is the 1975 transaction-log argument you quoted. Here’s the correct framing:

* If you are building **single-shot** summarizers, yes, it’s overkill.
* If you are building **long-running, tool-using, stateful, auditable** agents (which is already happening), then the missing piece is *exactly*:

  * stable provenance
  * reentrancy
  * replay
  * checkpointing

Most of the complaints in the field (“it forgot”, “it drifted”, “we can’t reproduce”, “we can’t test”, “we can’t explain”) are symptoms of **implicit state** and **unreified effects**.

RSR’s core move is: *make the effects and state explicit.*

---

## 14. Implementation alignment: what you already have maps cleanly

From your inventory:

✅ **CEKS machine** → provides explicit continuation and step semantics
✅ **COWStore** → persistent-ish state foundation
✅ **Effects + handlers** → suspension semantics
✅ **AsyncGenerator session** → the reentrancy conduit
✅ **PortalImpl / REPL re-entry** → interactive continuation manipulation
✅ **Scripted adapter** → test doubles at semantic boundary
✅ **SnapshotRepo** → checkpoint/resume foundation
✅ **ReceiptStore** → event sourcing / provenance foundation
✅ **Debugger stack nav** → inspection of continuations

So yes: you already have the minimum needed to claim:

* deterministic replay (via receipts)
* checkpoint/resume (via snapshots)
* interception/mocking (via adapter)
* provenance (via structured receipts)

Even without the “full spec” governance layer.

---

## 15. What should be documented next (but not in this doc)

This doc intentionally explains the **architecture and value**.

The next docs should be:

* **RSR-02 — Reentrancy Walkthrough:** one complete end-to-end trace showing nested `ReqApply/ReqEval` initiated by the LLM mid-inference, with receipts.
* **RSR-03 — Receipt Ledger & Replay Semantics:** hashing, lookup keys, cache invalidation, golden tests, metamorphic tests.
* **RSR-04 — Checkpointing Semantics:** snapshot format, state refs, resumability invariants, failure recovery.
* **RSR-05 — Context Economics (Pragmatic):** even before full compress/hydrate, establish best practices for “runtime state vs prompt state”.

---

## 16. Minimal “spec clauses” you must include to prevent confusion

To keep people from thinking it’s “just TS glue,” your spec must explicitly state:

1. **Effects are reified as request objects (Req).**
2. **The evaluator does not execute effects; it suspends.**
3. **A driver interprets requests and resumes continuations with responses.**
4. **Receipts persist the boundary to enable provenance and replay.**
5. **Snapshots persist the machine state to enable checkpoint/resume.**
6. **Adapters allow execution substitution (live vs scripted vs replay).**

That is the “core claim set” that makes the system legible.

---

If you reply **“continue”**, I’ll write **RSR-02** as a full walkthrough document with a *single, concrete, semantic* program (slur removal / policy gate / compliance rewrite) and a *fully spelled-out request/receipt trace* showing:

* where the CEKS machine suspends,
* exactly which `Req*` objects are emitted,
* how the driver handles them,
* how receipts are persisted,
* how a replay adapter makes the run deterministic in CI,
* and where checkpointing would occur mid-computation.
------
# RSR-02 — Reentrancy Walkthrough

## A single end-to-end trace that proves: **semantic procedures + eval/apply reentrancy + receipts + replay + checkpointing**

*(One concrete program; no arithmetic toys.)*

---

## 0. What you are about to see

We will execute **one** Ω/Lisp program that:

1. **Deterministically** redacts mechanically-detectable PII (emails/phones).
2. Uses **semantic inference** to detect “sensitive entities” that are not regex-detectable (names, internal project codenames, customer IDs written in prose, etc.).
3. Uses **reentrancy**: during semantic inference, the LLM **asks the runtime to evaluate/apply Lisp helpers** (to compute string spans, offsets, normalizations) instead of hallucinating indices.
4. Emits a **receipt ledger** proving exactly what happened.
5. Re-runs in **Replay mode** with **zero external calls** yet identical output.
6. Shows where you would place a **checkpoint** and how **resume** works.

This is the “value core”:

* **Provenance**: every external effect is captured as structured data.
* **Reentrancy**: the inference procedure can call back into eval/apply *mid-flight*.
* **Determinism on demand**: replay makes a stochastic boundary behave like a pure function in CI.

---

## 1. Names (so we stop saying “oracle”)

In this walkthrough:

* **Evaluator session**: `EffectSession` *(legacy: OracleSession)*
* **Request/response**: `EffectReq` / `EffectResp` *(legacy: OracleReq/OracleResp)*
* **Driver**: `RuntimeDriver` *(what you called “switchboard”; it’s the dispatcher loop)*
* **Receipt store**: `EffectLedger` *(legacy: ReceiptStore)*
* **Snapshot store**: `SnapshotRepo` *(as you already have)*
* **LLM boundary**: `Portal` *(legacy: PortalImpl)*

---

## 2. The demo program (Ω/Lisp)

### 2.1 Domain: policy redaction with semantic closure

We want: “remove PII/sensitive entities but preserve meaning.”

We will **not** rely on the LLM to compute string indices—because that’s exactly where agent frameworks get brittle. Instead we do:

* LLM identifies *what* is sensitive (semantic classification),
* runtime computes *where* it occurs (deterministic span-finding),
* runtime performs deterministic redaction,
* LLM is used again only if necessary to repair fluency.

### 2.2 Ω/Lisp code

This is the *object language* program—what runs on the CEKS machine.

```lisp
;; ------------------------------------------------------------
;; Deterministic helpers (pure, replayable without LLM).
;; ------------------------------------------------------------

(define (pii/find-basic-spans s)
  ;; returns list of spans like: ((start end kind) ...)
  ;; email/phone regex are deterministic.
  (append
    (regex/find-spans s EMAIL_REGEX "email")
    (regex/find-spans s PHONE_REGEX "phone")))

(define (spans/redact s spans)
  ;; redact by replacing each span with "[REDACTED:<kind>]"
  (string/redact-spans s spans))

(define (string/span-of s needle)
  ;; deterministic: returns (start end) of the first match or #f
  (string/find-span s needle))

(define (spans/from-needles s needles kind)
  ;; convert list of sensitive substrings -> deterministic spans
  (filter truthy?
    (map (lambda (needle)
           (let ((span (string/span-of s needle)))
             (if span
                 (list (car span) (cadr span) kind)
                 #f)))
         needles)))

;; ------------------------------------------------------------
;; Semantic procedures (LLM-backed) expressed as effects.
;; ------------------------------------------------------------

(define (semantic/find-sensitive-needles s)
  ;; returns a list of substrings that *should* be redacted.
  ;; IMPORTANT: returns substrings, not offsets.
  ;; Offsets are computed deterministically via reentrant calls.
  (effect infer.op
    (list
      "Task: Identify sensitive entities in the text that should be redacted "
      "(personal names, internal project codenames, secrets described in prose). "
      "Return ONLY a JSON array of exact substrings to redact. No explanation.\n\n"
      "TEXT:\n" s)))

(define (semantic/meaning-preserved? before after)
  ;; semantic equivalence check (coarse but useful):
  ;; "Did we preserve the substantive meaning, modulo redaction?"
  (effect infer.op
    (list
      "Answer ONLY true or false.\n"
      "Question: Does AFTER preserve the essential meaning of BEFORE, "
      "except that sensitive details may be removed or replaced with placeholders?\n\n"
      "BEFORE:\n" before "\n\n"
      "AFTER:\n" after "\n")))

(define (semantic/rewrite-preserving-meaning s)
  ;; last-resort: rewrite to regain fluency after mechanical redaction
  (effect infer.op
    (list
      "Rewrite the text so it is fluent and natural, while keeping all facts "
      "except replacing sensitive details with placeholders like [REDACTED]. "
      "Return ONLY the rewritten text.\n\n"
      "TEXT:\n" s)))

;; ------------------------------------------------------------
;; The composed semantic-mechanical pipeline.
;; ------------------------------------------------------------

(define (sanitize-message s)
  (let* ((basic (pii/find-basic-spans s))
         (s1 (spans/redact s basic))
         (needles (semantic/find-sensitive-needles s1))           ;; LLM
         (extra (spans/from-needles s1 needles "sensitive"))      ;; deterministic
         (s2 (spans/redact s1 extra))
         (ok? (semantic/meaning-preserved? s s2)))                ;; LLM
    (if (truthy? ok?)
        s2
        (semantic/rewrite-preserving-meaning s2))))               ;; LLM (rare)
```

### 2.3 Why this is already “language building”

Even without macros, you’ve effectively created a **new library language**:

* “semantic procedure” is now a first-class notion: it’s just a function whose body is an `infer.op` effect.
* composition is ordinary FP: `sanitize-message` is a pure pipeline with controlled effects.
* you can now build embedded DSLs around *semantic* functions exactly like SICP’s picture language or circuit language—closure property is:
  **Text → Text**, **Text → Bool**, **Text → List**, etc.

---

## 3. Where reentrancy happens (the part people miss)

In the above program, `(semantic/find-sensitive-needles s1)` returns substrings. But how does the LLM produce them robustly?

**The key move:** during `infer.op`, the runtime driver may allow the model to request **eval/apply** calls back into the Lisp runtime to compute deterministic structure (span offsets, normalized variants, regex candidates, etc.).

This is not “tool use” in the generic sense. This is:

> **The inference procedure is allowed to invoke eval/apply on the object language mid-inference.**
> That is the metacircular fixed point extension.

Concretely:

* evaluator suspends with `ReqTool(infer.op …)`
* driver calls the LLM
* LLM replies with either:

  * `Done(result)` OR
  * `CallEval(expr)` / `CallApply(fn,args)` (reentrant)
* driver fulfills that by calling the **Portal** (re-entering the evaluator)
* driver feeds the result back to the LLM, which continues until `Done`

---

## 4. The RuntimeDriver loop (actual TypeScript)

Below is a minimal driver that demonstrates:

* the `AsyncGenerator` request/response loop (your session protocol),
* receipt capture (event sourcing),
* **reentrant eval/apply** inside an `infer.op` handler,
* replayability by swapping adapters.

> This is deliberately “core-loop code,” not framework fluff.

### 4.1 Types (minimal)

```ts
export type Hash = string;

export type Expr = unknown; // your AST type
export type Val  = unknown; // your value type

export type ToolCall =
  | { tool: "infer.op"; prompt: string; schema?: unknown }
  | { tool: "observe"; schema: unknown; ctxRef: Hash }
  | { tool: string; args: unknown };

export type EffectReq =
  | { tag: "ReqEval"; qexpr: Expr; envRef: Hash }
  | { tag: "ReqApply"; fn: Val; args: Val[]; envRef: Hash }
  | { tag: "ReqTool"; call: ToolCall; envRef: Hash }
  | { tag: "ReqReturn"; val: Val }
  | { tag: "ReqFail"; reason: string };

export type EffectResp =
  | { tag: "RespVal"; val: Val }
  | { tag: "RespFail"; reason: string };

export type EffectSession = AsyncGenerator<EffectReq, Val, EffectResp>;
```

### 4.2 Receipt ledger (Event Sourcing / Memento-ish)

```ts
export type Receipt = {
  key: string;     // content-addressed hash of req+resp (plus stable metadata)
  req: EffectReq;
  resp: EffectResp;
  meta: {
    t0: number;
    dtMs: number;
    mode: "live" | "replay" | "script";
  };
};

export interface EffectLedger {
  get(req: EffectReq): Promise<Receipt | null>;
  put(r: Receipt): Promise<void>;
}
```

### 4.3 Portal interface (reentrant bridge)

```ts
export interface Portal {
  eval(qexpr: Expr, envRef: Hash): Promise<Val>;
  apply(fn: Val, args: Val[], envRef: Hash): Promise<Val>;
}
```

### 4.4 LLM adapter that can request reentrant eval/apply

```ts
export type LLMContinuation =
  | { tag: "Done"; value: unknown }                            // final output
  | { tag: "CallEval"; qexpr: Expr }                           // ask runtime
  | { tag: "CallApply"; fn: Val; args: Val[] };                // ask runtime

export interface LLMAdapter {
  step(prompt: string, scratch: Record<string, unknown>): Promise<LLMContinuation>;
}
```

### 4.5 The driver (the “second eval/apply loop”)

```ts
import { createHash } from "crypto";

function stableKey(req: EffectReq, resp: EffectResp): string {
  const h = createHash("sha256");
  h.update(JSON.stringify({ req, resp }));
  return `sha256:${h.digest("hex")}`;
}

export class RuntimeDriver {
  constructor(
    private readonly ledger: EffectLedger,
    private readonly portal: Portal,
    private readonly llm: LLMAdapter,
    private readonly mode: "live" | "replay" | "script",
  ) {}

  async handle(req: EffectReq): Promise<EffectResp> {
    // --- Receipt replay boundary -------------------------------------------
    const cached = await this.ledger.get(req);
    if (cached) return cached.resp;

    const t0 = Date.now();
    let resp: EffectResp;

    try {
      switch (req.tag) {
        case "ReqTool":
          resp = await this.handleTool(req);
          break;

        // In many implementations ReqEval/ReqApply are used by the LLM-side portal.
        // If your session can also yield these outward, handle them similarly.
        case "ReqEval":
          resp = { tag: "RespVal", val: await this.portal.eval(req.qexpr, req.envRef) };
          break;

        case "ReqApply":
          resp = { tag: "RespVal", val: await this.portal.apply(req.fn, req.args, req.envRef) };
          break;

        case "ReqReturn":
          resp = { tag: "RespVal", val: req.val };
          break;

        case "ReqFail":
          resp = { tag: "RespFail", reason: req.reason };
          break;

        default:
          resp = { tag: "RespFail", reason: `Unhandled req tag: ${(req as any).tag}` };
      }
    } catch (e: any) {
      resp = { tag: "RespFail", reason: e?.stack ?? String(e) };
    }

    const dtMs = Date.now() - t0;
    const receipt: Receipt = {
      key: stableKey(req, resp),
      req,
      resp,
      meta: { t0, dtMs, mode: this.mode },
    };
    await this.ledger.put(receipt);
    return resp;
  }

  private async handleTool(req: Extract<EffectReq, { tag: "ReqTool" }>): Promise<EffectResp> {
    if (req.call.tool !== "infer.op") {
      // dispatch other tools / observe here
      return { tag: "RespFail", reason: `Unsupported tool: ${req.call.tool}` };
    }

    // --- The reentrant inference loop --------------------------------------
    // This is the "LLM apply" that can call back into Lisp eval/apply.
    const prompt = req.call.prompt;
    const scratch: Record<string, unknown> = {};

    for (;;) {
      const k = await this.llm.step(prompt, scratch);

      switch (k.tag) {
        case "Done":
          return { tag: "RespVal", val: k.value as Val };

        case "CallEval": {
          const v = await this.portal.eval(k.qexpr, req.envRef);
          scratch["lastEval"] = v;
          continue;
        }

        case "CallApply": {
          const v = await this.portal.apply(k.fn, k.args, req.envRef);
          scratch["lastApply"] = v;
          continue;
        }

        default:
          return { tag: "RespFail", reason: `Unknown LLMContinuation tag: ${(k as any).tag}` };
      }
    }
  }
}
```

### 4.6 Session runner (the canonical loop)

```ts
export async function runSession(session: EffectSession, driver: RuntimeDriver): Promise<Val> {
  let it = await session.next(undefined as any);
  while (!it.done) {
    const req = it.value;
    const resp = await driver.handle(req);
    it = await session.next(resp);
  }
  return it.value;
}
```

> **This is the entire architectural thesis in executable form.**
> The CEKS evaluator interprets Lisp.
> The driver interprets effects.
> Reentrancy is simply: the effect interpreter is permitted to call back into the language interpreter.

---

## 5. The walk-through trace (step-by-step, with reentrancy)

We now run:

```lisp
(sanitize-message "Hi, I'm Alex. Email: alex@example.com. Please escalate Project Nightfall ASAP.")
```

### 5.1 What happens (high-level)

1. `pii/find-basic-spans` finds the email span deterministically.
2. `spans/redact` replaces it with `[REDACTED:email]`.
3. `semantic/find-sensitive-needles` runs `infer.op`:

   * model identifies `"Alex"` and `"Project Nightfall"` as sensitive needles.
   * **but** to avoid mistakes, the model asks runtime to confirm spans or normalized variants via reentrant eval/apply if needed.
4. runtime turns needles into spans deterministically and redacts.
5. `semantic/meaning-preserved?` checks meaning preservation.
6. finish without rewrite (usually).

### 5.2 Concrete request/response sequence (representative)

**(A)** Evaluator hits first `infer.op`:

**Req #1**

```ts
{
  tag: "ReqTool",
  envRef: "env:17",
  call: {
    tool: "infer.op",
    prompt: "Task: Identify sensitive entities... TEXT:\nHi, I'm Alex. Email: [REDACTED:email]. Please escalate Project Nightfall ASAP."
  }
}
```

Driver handles it in **live mode**. LLM replies, but in our reentrancy-enabled protocol it can ask:

**LLMContinuation #1**

```ts
{ tag: "Done", value: ["Alex", "Project Nightfall"] }
```

*(If you want to demonstrate reentrancy more aggressively, here’s the exact kind of thing you enable:)*

**LLMContinuation variant (reentrant)**

```ts
{ tag: "CallEval", qexpr: ["string/find-span", 
                           "Hi, I'm Alex. Email: [REDACTED:email]...", 
                           "Project Nightfall"] }
```

Driver executes:

* `portal.eval(qexpr, envRef)` → deterministic `(start,end)` span
* feeds result back to model via scratch
* model completes with `Done([...])`

That “CallEval” is the moment where:

* LLM is *not* merely generating text,
* it is participating in a **metacircular computation** (it is asking the interpreter to compute).

**(B)** Evaluator then hits second `infer.op` (`meaning-preserved?`):

**Req #2**

```ts
{
  tag: "ReqTool",
  envRef: "env:17",
  call: {
    tool: "infer.op",
    prompt: "Answer ONLY true or false...\nBEFORE:\n...\nAFTER:\n..."
  }
}
```

Resp:

```ts
{ tag: "RespVal", val: true }
```

### 5.3 Receipts (what gets persisted)

For Req #1 you store:

* exact prompt
* exact response
* envRef
* timing
* key = sha256(req+resp)

This is what makes “why did it do that?” answerable without handwaving.

---

## 6. Replay mode: zero external calls, identical output

Now the point:

1. Run once in **live** mode, store receipts.
2. Switch driver to **replay** mode (adapter replaced by ledger lookup).
3. Run again: every `ReqTool(infer.op …)` is answered from receipts.

### 6.1 What this buys you

* **CI determinism**: semantic programs become testable.
* **Cost control**: reruns do not re-spend tokens.
* **Regression analysis**: compare receipt graphs across versions.

---

## 7. Checkpoint/resume: where it fits in this run

The natural checkpoint boundary is:

* right after deterministic PII redaction (`s1`)
* before semantic inference (`semantic/find-sensitive-needles`)

Because that’s:

* a stable point in the continuation,
* and it prevents redoing deterministic preprocessing on resume (minor),
* but more importantly: it lets you pause *before costly model work*.

Conceptually:

* snapshot = `StateRef` for CEKS + `EnvRef` + ledger cursor
* resume = restore machine state and continue stepping

This is classic **Memento Pattern** (GoF) applied to an interpreter state machine.

---

## 8. Tests that are actually about semantic value (not arithmetic)

Below are tests that would sit in `vitest` or `jest`. They are “semantic-programming” tests, not toy math tests.

### 8.1 Deterministic replay test (Golden Receipt Pattern)

```ts
import { expect, test } from "vitest";

test("sanitize-message is deterministic under replay", async () => {
  const input = "Hi, I'm Alex. Email: alex@example.com. Please escalate Project Nightfall ASAP.";

  // 1) Live run (scripted or real)
  const ledger = new InMemoryLedger();
  const live = new RuntimeDriver(ledger, portal, scriptedLLM, "live");
  const out1 = await runSession(makeSessionForSanitize(input), live);

  // 2) Replay run
  const replayLLM = new ReplayLLMAdapter(ledger); // returns cached
  const replay = new RuntimeDriver(ledger, portal, replayLLM, "replay");
  const out2 = await runSession(makeSessionForSanitize(input), replay);

  expect(out2).toEqual(out1);
});
```

### 8.2 Policy invariant test (Metamorphic Testing)

Invariant: output must not contain email/phone patterns.

```ts
test("output contains no obvious PII patterns", async () => {
  const out = await runSession(makeSessionForSanitize(INPUT), driver);
  expect(out).not.toMatch(/[A-Z0-9._%+-]+@[A-Z0-9.-]+\.[A-Z]{2,}/i);
  expect(out).not.toMatch(/\b\d{3}[-)\s]*\d{3}[-\s]*\d{4}\b/);
});
```

### 8.3 Provenance test (Event Sourcing query)

Invariant: exactly two `infer.op` calls happened on the happy path.

```ts
test("provenance: infer.op called expected number of times", async () => {
  await runSession(makeSessionForSanitize(INPUT), driver);
  const inferReceipts = ledger.all().filter(r =>
    r.req.tag === "ReqTool" && (r.req as any).call?.tool === "infer.op"
  );
  expect(inferReceipts.length).toBe(2); // find-sensitive + meaning-preserved
});
```

These are *the* tests agent frameworks usually cannot do cleanly because they don’t reify effects at the right boundary.

---

## 9. The metacircular “fixed point” claim (explicit, not mystical)

SICP fixed point:

* A language can interpret itself because `eval/apply` are expressed in the language itself.

RSR extension:

* A **semantic primitive** (`infer.op`) can participate in evaluation, **and** can call back into `eval/apply` reentrantly.

So the closed loop is:

1. Lisp evaluates program
2. Program triggers semantic primitive
3. Semantic primitive requests deterministic computation from Lisp
4. Lisp supplies result
5. Semantic primitive completes
6. Lisp continues

That is the new strange loop:

> **inference-as-primitive + eval/apply reentrancy + receipts**
> → a runtime where “meaning” can be computed compositionally, audited, replayed, and resumed.

---

If you say **“continue”**, I’ll produce **RSR-03**: *Receipt Ledger & Replay Semantics*, including:

* canonical hashing / stable keys
* schema for receipts (prompt artifacts vs execution artifacts)
* cache invalidation/versioning strategy (Fowler-style “versioned artifacts” without needing the full Ω governance layer)
* property-based test strategy for semantic programs
* how to diff two runs as provenance DAGs (not log diff)
-----

# RSR-03 — Receipt Ledger & Replay Semantics

## Making LLM effects behave like **referentially transparent** computations for testing, debugging, provenance, and long‑running reentrant agents

---

## 0. Terminology (so we never say “oracle” again)

I’m going to use the following stable vocabulary (and map it to what you already have):

| Concept                         | Name in this doc                        | What it *is*                                                        | Legacy label                       |
| ------------------------------- | --------------------------------------- | ------------------------------------------------------------------- | ---------------------------------- |
| “The suspended effect protocol” | **EffectSession**                       | `AsyncGenerator<EffectReq, Val, EffectResp>`                        | OracleSession                      |
| “A request from evaluator”      | **EffectReq**                           | `ReqEval / ReqApply / ReqTool / ReqReturn / ReqFail …`              | OracleReq                          |
| “A response into evaluator”     | **EffectResp**                          | `RespVal / RespFail …`                                              | OracleResp                         |
| “Effect interpreter loop”       | **RuntimeDriver** (or **EffectRouter**) | Dispatch loop that interprets EffectReq and returns EffectResp      | “switchboard”                      |
| “LLM boundary”                  | **Portal** + **LLMAdapter**             | The thing that can run `infer.op` and optionally reenter eval/apply | PortalImpl / ScriptedOracleAdapter |
| “Receipt store”                 | **EffectLedger**                        | Content-addressed, queryable event store for effect calls           | ReceiptStore                       |
| “Snapshots”                     | **SnapshotRepo**                        | Store for `EnvRef/StateRef` checkpoints                             | SnapshotRepo                       |

> **Core thesis (RSR):**
> The evaluator interprets your object language (Ω/Lisp).
> The `RuntimeDriver` interprets effects (LLM/tool/observe).
> **EffectLedger** makes that interpretation *auditable and replayable*.
> Reentrancy is just: the effect interpreter is allowed to call back into eval/apply **mid-flight**.

---

## 1. What the EffectLedger *actually* buys you (non-toy value)

### 1.1 Four concrete capabilities

1. **Deterministic replay (Golden Master for semantic programs)**
   Treat `(infer.op prompt)` as a *pure function* during CI by replaying receipts.

2. **Provenance with causality (Why did it do that?)**
   Not logs. Not “thoughts”. Actual structured inputs/outputs for each effect, linked into a run graph.

3. **Reentrancy-safe auditability**
   If the LLM called back into Lisp (via `ReqEval`/`ReqApply`), those subcalls are captured too, so you can reconstruct *the exact interactive computation*.

4. **Cost and stability control**
   Cached receipts + request hashing become *the runtime equivalent of memoization* (but across process boundaries and across time).

### 1.2 Why this isn’t “just caching”

Because it’s not keyed on “prompt string”; it’s keyed on:

* **the request algebra** (tag + payload),
* **the effective environment reference** (what semantic closures were in scope),
* **the execution profile** (decoder params, safety profile, model id),
* and optionally **the interpreter state fingerprint** (stack/continuation).

That’s the difference between:

* “a cache of responses”
  and
* **event sourcing for an effectful interpreter**.

This is Fowler’s **Event Sourcing** pattern applied to the boundary where meaning becomes action.

---

## 2. The spec: receipt model and invariants

### 2.1 Separate keys: `reqKey` vs `receiptKey`

Your earlier sketch hashed `req+resp`. That’s good for integrity, but **lookup** must be keyed by request alone.

We want:

* **Request key**
  `reqKey = H( Canon(req) )`
* **Receipt key**
  `receiptKey = H( Canon(req) || Canon(resp) || Canon(meta) )`

Why:

* `reqKey` is your *memoization key*.
* `receiptKey` is your *tamper-evident content address*.

### 2.2 Receipt schema (minimal but correct)

```ts
export type SessionId = string;
export type StepId = number;

export type ArtifactRef =
  | { kind: "model"; id: string }          // e.g., "gpt-5.2-pro" or vendor id
  | { kind: "decoder"; id: string }        // e.g., "greedy", "temp=0.7"
  | { kind: "policy"; id: string }         // governance/profile id
  | { kind: "promptView"; id: string }     // structured prompt artifact hash
  | { kind: "toolSpec"; id: string };      // tool router spec hash/version

export type ReceiptMeta = {
  sessionId: SessionId;
  stepId: StepId;

  mode: "live" | "replay" | "script" | "validate";

  t0: number;
  dtMs: number;

  // Optional but high leverage:
  envRef?: string;         // the envRef at suspension point
  stateRef?: string;       // CEKS state snapshot ref (if you checkpointed)
  stackRef?: string;       // hash/fingerprint of continuation stack
  parents?: string[];      // receiptKeys of causally prior receipts
  artifacts?: ArtifactRef[];
};

export type Receipt = {
  reqKey: string;
  receiptKey: string;

  req: EffectReq;
  resp: EffectResp;
  meta: ReceiptMeta;

  // Optional integrity:
  sig?: { alg: "ed25519" | "hmac-sha256"; value: string; keyId: string };
};
```

### 2.3 Invariants the ledger must enforce

These are *runtime laws* you can actually test:

1. **Idempotent receiver law** (Enterprise Integration Patterns)
   `put(receipt)` is idempotent by `receiptKey`.

2. **Deterministic lookup law**
   `getByReq(req)` returns a receipt whose `reqKey == H(Canon(req))`.

3. **Replay purity law**
   In replay mode, no external execution is performed for requests that have receipts.
   (This is how you guarantee CI determinism.)

4. **Causal ordering law (per session)**
   `stepId` is a total order within a `sessionId`.
   `parents` creates a DAG that is consistent with `stepId`.

---

## 3. Canonicalization (C14N): the real “sharp edge” that makes or breaks replay

### 3.1 Why JSON.stringify is not sufficient

Because:

* object key ordering is not guaranteed in all JS producers,
* `Map`, `Set`, `BigInt`, `Uint8Array`, and custom AST nodes serialize inconsistently,
* floating params (temperature, top_p) may have rounding noise,
* your `Expr` and `Val` likely contain cyclic structures or tagged unions.

Replay only works if the request key is stable across:

* machines,
* processes,
* versions (within a defined compatibility policy).

### 3.2 Define a StableCodec

You need a single canonical encoding for any value that participates in `reqKey`:

```ts
export interface StableCodec {
  canon(x: unknown): string;          // canonical UTF-8 string
  hashCanon(x: unknown): string;      // sha256 over canon(x)
}
```

Implementation rules (the minimum):

* Objects: keys sorted lexicographically; no undefined.
* Arrays: preserve order.
* Map/Set: encode as sorted arrays of canonical entries.
* Floats: normalized decimal representation (or fixed rounding policy).
* Tagged unions: always include `tag`.
* Buffers: base64 with explicit tag.
* Dates: ISO string.

A common pattern is a “canonical JSON” subset with explicit tagging for non-JSON types.

### 3.3 Canonicalizing `EffectReq` safely

**Important design choice:** your canonicalization boundary should include exactly what you consider part of the semantic call.

For `ReqTool(infer.op)` you generally want **structured prompt artifacts**, not raw, because you need to:

* prove which wrapper/transformers were applied,
* keep a stable representation across whitespace changes (optional).

So consider this split:

```ts
type InferToolCall = {
  tool: "infer.op";
  promptView: PromptView;          // structured AST of the prompt/messages
  decode: DecodeParams;            // temp, top_p, max_tokens, seed...
  schema?: unknown;                // output schema if any
};

type PromptView =
  | { kind: "text"; text: string }
  | { kind: "chat"; messages: { role: "system"|"user"|"assistant"; content: string }[] }
  | { kind: "template"; parts: PromptPart[] }; // holes, delimiters, etc.
```

**Why:** this is where your “prompt algebra vs execution algebra” distinction becomes real in the runtime.

---

## 4. Replay semantics (modes, policies, and correctness)

### 4.1 Execution modes (Strategy pattern)

Treat replay policy as a Strategy:

```ts
export type ReplayPolicy =
  | { mode: "replayOnly" }                              // hard deterministic
  | { mode: "replayThenLive" }                          // cache miss -> live
  | { mode: "validateAgainstLive"; tolerance?: number } // compare replay vs live
  | { mode: "recordOnly" };                             // always live, store receipts
```

* **replayOnly**
  For CI, unit tests, and debugging.
  Cache miss is an error.

* **replayThenLive**
  For development: deterministic where possible, flexible where needed.

* **validateAgainstLive**
  For migration / model upgrades: run live, compare to replayed.
  (This is how you quantify drift.)

* **recordOnly**
  For generating new golden receipts.

### 4.2 Correctness contract of replay

Replay gives you:

> **Observational equivalence** at the evaluator boundary, not mathematical equivalence of the LLM.

Meaning:

* The evaluator sees the *same* `EffectResp` for the *same* `EffectReq`.
* Therefore the CEKS machine follows the same control flow.
* Therefore you can reproduce bugs.

This is exactly the same contract as “mocking” but with:

* better provenance,
* better determinism,
* no fragile HTTP mocking.

---

## 5. Cache invalidation (the non-handwavy version)

You asked for an actual spec and justification. Here is the honest rule:

> **A receipt is reusable iff its request is bitwise-identical under canonicalization**
> *in the compatibility domain you choose.*

### 5.1 The “artifact closure” principle

Your request key must include artifact identity for anything that influences semantics:

* model id / version
* decoder parameters
* system prompt wrapper versions
* tool router versions
* policy profile (strict/pragmatic/etc) if present
* any retrieval corpora version if you add RAG later

Therefore: **cache invalidation becomes automatic** because changing any artifact changes the request canonical form → changes `reqKey` → cache miss.

This is literally the **Content Addressable Store** principle applied to semantic computations.

### 5.2 Compatibility domains (when you *want* reuse across changes)

Sometimes you *do* want to reuse receipts across “minor refactors”.

You can support **explicit compatibility transforms**:

* `PromptViewNormalizer`: strip whitespace-only differences, canonicalize delimiters, etc.
* `ArtifactAlias`: map “gpt-5.2-pro@patch3” → “gpt-5.2-pro@patch4” only if you accept drift risk.

This is a disciplined version of “approximate caching”.

In PL terms:

* strict mode is referential transparency in the *operational* semantics,
* compatibility domains introduce a controlled observational refinement.

---

## 6. Provenance DAG (not logs): building the “why did it do that?” graph

### 6.1 Why a DAG (and not a list)

Because effects cause more effects:

* LLM call → requests eval/apply reentrantly → may trigger observe/tool calls → returns → continues.

That’s a **causal structure**, not just time order.

So represent a run as:

* Nodes: receipts
* Edges: `parents` / `children` causal relation

This is essentially a **Merkle DAG** if you hash parents into child keys (optional).

### 6.2 How to assign parents concretely

A practical rule that works:

* Each `RuntimeDriver.handle(req)` knows the “current active receipt” stack.
* When it produces a new receipt:

  * `parents = [topOfReceiptStack]` (or all active parents)
* Push receipt key while handling nested reentrant calls; pop on return.

This mirrors the evaluator’s continuation stack, but for effects.

### 6.3 Queries you get “for free” once it’s a DAG

* “Show me the subcalls that happened inside this infer.op”
* “Which infer.op produced the value that later influenced this tool call?”
* “What was the minimal cut of effects that led to the final output?”

This is the conceptual jump from “agent logs” to **computation provenance**.

---

## 7. Diffing two runs (structural provenance diff, not text diff)

This is the feature that turns “this agent drifted” into an engineering problem you can solve.

### 7.1 Two runs produce two DAGs

Let:

* `G1 = (V1, E1)` receipts for run A
* `G2 = (V2, E2)` receipts for run B

### 7.2 Alignment strategy (Pragmatic unification)

You need to match “corresponding” receipts.

**Primary alignment key:** identical `reqKey`
If `reqKey` matches, you are looking at “same semantic call”.

**Secondary alignment key:** `(tag, tool, stackRef, callsiteFingerprint)`

* `tag` = ReqTool/ReqEval/ReqApply
* `tool` = infer.op/observe/…
* `stackRef` = hash of CEKS continuation skeleton (frame tags, not values)
* callsite = optional: source span if you have it, else derive from continuation layout

This is like structural matching in compilers: first exact, then heuristic.

### 7.3 Diff output (what you show a developer)

For matched nodes:

* compare `resp`:

  * changed text
  * changed JSON parse
  * changed boolean outcome
* compare meta:

  * dtMs
  * model id
  * decode params

For unmatched nodes:

* new calls introduced
* calls removed (dead branch)
* branch divergence point = earliest node whose children sets differ

This gives you:

> “Run B diverged at the second semantic check; it flipped from true to false, causing rewrite path to activate, adding an extra infer.op.”

That is the sort of explanation you simply cannot get from typical agent frameworks.

---

## 8. Testing strategy (the patterns, explicitly)

### 8.1 Golden Receipt Pattern (Approval Testing variant)

Store a “golden ledger” (or a slice) in your repo for specific tests.

* Test runs in `replayOnly`
* Any cache miss fails fast
* Output is deterministic and reviewable

This is **Golden Master testing**, but for effectful interpreters.

### 8.2 Metamorphic testing for semantic programs

Instead of expecting exact output, define invariants:

Examples for `sanitize-message`:

* **Idempotence (under replay/live if stable):**
  `sanitize(sanitize(x)) ≈ sanitize(x)`
  (You may accept “≈” if rewrite makes it not strictly idempotent; then define a normal form.)

* **Non-leak property:**
  output contains no substrings matching certain sensitive patterns.

* **Meaning preservation property:**
  `semantic/meaning-preserved?(x, sanitize(x))` should be true.

These are metamorphic relations: they test correctness without relying on brittle exact strings.

### 8.3 Property-based testing (QuickCheck style)

Generate random “message-like” text with inserted PII and random “sensitive needles”.

Then test:

* leak invariants
* termination
* bounded number of infer.op calls (cost guardrails)

### 8.4 Contract tests for Portal/LLMAdapter

The reentrancy protocol must obey laws:

* If adapter emits `CallEval`, it must be prepared to accept `lastEval` in scratch.
* If it emits `CallApply`, similarly.
* It must eventually emit `Done` (or time out).

This is a classic **Contract Test** and **Test Double** setup:

* `ScriptedLLMAdapter` is your stub.
* `ReplayLLMAdapter` is your deterministic double.

---

## 9. Putting it into your existing code (minimal deltas)

You already have:

* CEKS machine
* effect infer.op
* receipt store (minimal)
* snapshot repo (basic)
* portal re-entry mechanism (PortalImpl)
* scripted adapter

To reach “RSR-03 completeness” you need:

1. **StableCodec** for canonicalization
   (This is non-negotiable for reliable replay.)

2. **EffectLedger API upgrade**

```ts
export interface EffectLedger {
  getByReqKey(reqKey: string): Promise<Receipt | null>;
  getByReq(req: EffectReq): Promise<Receipt | null>;
  put(r: Receipt): Promise<void>;

  // Queries:
  bySession(sessionId: string): AsyncIterable<Receipt>;
  byTool(tool: string): AsyncIterable<Receipt>;
}
```

3. **RuntimeDriver policy injection** (Strategy)

* allow `replayOnly`, `replayThenLive`, `validate`

4. **Parent linkage** for provenance DAG

* push/pop receipt stack in driver during nested handling

That’s it. No new grand theory required.

---

## 10. Why the “switchboard” is actually the metacircular extension point

You said: “I still don’t see how this exploits the fixed point.”

Here is the crisp statement:

* In SICP, the evaluator is metacircular because it’s an interpreter written in the language it interprets.
* In your system, you have **two interpreters**:

  1. The Lisp evaluator (CEKS)
  2. The effect interpreter (RuntimeDriver)

The new fixed point is:

* the Lisp evaluator can invoke the effect interpreter via `(effect infer.op …)`
* and the effect interpreter can invoke the Lisp evaluator **reentrantly** via `portal.eval/apply`

That mutual recursion is the *new* strange loop.

**The “switchboard” (RuntimeDriver) is literally the second interpreter.**
It is not UI plumbing. It’s the semantic equivalent of `apply`—the place where potential external meaning becomes an actual response that continues computation.

---

## 11. Concrete “mind-blowing” application class enabled by RSR-03

Not “5+3”. Here are categories that become *engineering-possible*:

### 11.1 Long-running agent workflows with deterministic postmortems

Think:

* incident response assistants
* compliance auditors
* customer-support triage systems
* “week-long research agents”

They all fail today because you can’t reliably answer:

* what did it see,
* what did it call,
* why did it branch,
* can we reproduce it.

With receipts + replay + provenance DAG:

* failures become reproducible artifacts, not anecdotes.

### 11.2 Semantic pipelines with *mechanical correctness boundaries*

The redaction demo is a prototype of a general pattern:

> Use LLM for classification/meaning
> Use Lisp for deterministic computation and transformations
> Use receipts to bind them into a replayable program

This is how you prevent “LLM wrote bad indices” classes of bugs.

### 11.3 Building embedded languages over semantic primitives

Once `infer.op` is a primitive in the evaluator, you can build:

* a mini “policy language”
* a “review language”
* a “requirements language”
* a “style language”
  … all in Lisp, with semantic operators but deterministic replay.

This is exactly SICP’s embedded language lesson, but with a new primitive that bottoms out in meaning.

---

If you say **continue**, I’ll deliver **RSR-04 — Prompt Artifacts vs Execution Algebra (the two algebras you’re feeling)** with:

* a minimal prompt AST + transformer combinators (artifact algebra)
* an execution algebra (monadic) for chains/branch/retry/search
* a disciplined way to express “chat”, “completion”, “tool protocol”, “REPL” as *typed sublanguages*
* and a full example: a “Semantic SOP DSL” that compiles into your effectful evaluator and produces receipts you can replay and diff.
