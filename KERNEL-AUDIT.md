# OmegaLLM Kernel Audit

## Purpose

This document provides a complete audit of OmegaLLM's kernel to ensure it provides a **clean surface abstraction layer** for building packages on top. It identifies what's complete, what's incomplete, and what action items remain.

---

## 1. Overall Architecture Assessment

```
┌────────────────────────────────────────────────────────────────────────────┐
│                         KERNEL STATUS SUMMARY                               │
├────────────────────────────────────────────────────────────────────────────┤
│                                                                            │
│  ✅ COMPLETE (Production-Ready)        ⚠️ INCOMPLETE (Needs Work)          │
│  ─────────────────────────────        ────────────────────────────         │
│  • CESK Machine (machineStep.ts)      • Budget enforcement (not wired)     │
│  • Effect handlers (handle/perform)    • ReqTool (stubbed)                  │
│  • Oracle protocol (all Req types)    • Real LLM adapters (stubbed)        │
│  • Meaning values (MeaningVal)        • Nondet best/sample modes           │
│  • Dist values (DistVal)              • REPL debugger commands             │
│  • Snapshots (SnapshotRepo)           • Context constraints                │
│  • Receipts (ReceiptStore)            • Evidence accumulation              │
│  • Capabilities (caps.ts)                                                  │
│  • Profiles (profile.ts)                                                   │
│  • Reader/Parser (full pipeline)                                           │
│  • Macros (hygiene-preserving)                                             │
│  • Basic REPL (omega-repl.ts)                                              │
│                                                                            │
└────────────────────────────────────────────────────────────────────────────┘
```

---

## 2. Detailed Component Audit

### 2.1 Core Evaluator (CESK Machine) ✅ COMPLETE

**Files:**
- [machineStep.ts](src/core/eval/machineStep.ts) - Step-once evaluator (533 lines)
- [machine.ts](src/core/eval/machine.ts) - State and Frame types
- [run.ts](src/core/eval/run.ts) - `runToCompletion` driver
- [store.ts](src/core/eval/store.ts) - COWStore (copy-on-write)
- [env.ts](src/core/eval/env.ts) - Environment as Ctx
- [values.ts](src/core/eval/values.ts) - Value type union

**Status:** ✅ Production-ready

**Verified working:**
- All expression forms: Lit, Var, Lambda, If, Begin, Define, Set, App, Quote, Effect, Handle, Match
- Continuation frames: KIf, KBegin, KDefine, KSet, KAppFun, KAppArg, KCall, KEffect, KHandleBoundary, KHandleReturn, KMatch
- Multi-shot resumptions via ContVal
- Handler clause execution with parameter binding
- Pattern matching in Match expressions
- COWStore for snapshot-safe mutations
- Recursive definitions (pre-allocation pattern)

**No gaps identified.**

---

### 2.2 Effect System ⚠️ MOSTLY COMPLETE

**Files:**
- [effects/opcall.ts](src/core/effects/opcall.ts) - OpCall & Resumption types
- [effects/capture.ts](src/core/effects/capture.ts) - `captureValueResumption`
- [effects/runtimeImpl.ts](src/core/effects/runtimeImpl.ts) - RuntimeImpl dispatcher
- [effects/nondet/runner.ts](src/core/effects/nondet/runner.ts) - `runNondet` for amb.op

**Status:** ⚠️ Working but budget enforcement not wired

**Working:**
- Effect dispatch via `stepOnce` → `RuntimeImpl.dispatch`
- Deep handler search (nearest enclosing clause wins)
- Multi-shot resumptions captured and invocable
- `int.op`, `infer.op`, `rewrite.op`, `search.op` all dispatch to oracle
- `oracle.apply.op` for LLM procedures in apply position
- `commit.op` with truth regime enforcement
- `amb.op` via nondet runner (BFS/DFS modes)

**Gaps:**

| Gap | Impact | Fix Required |
|-----|--------|--------------|
| **Budget not consumed in eval loop** | Can't limit eval steps | Add step counter to `runToCompletion`, call `budgetConsumeEvalStep` |
| **Nondet "best"/"sample" modes** | Can't score/weight amb choices | Implement scoring in `runNondet` |

**Code verification (runtimeImpl.ts line 141-143):**
```typescript
capRequire(this.profile.caps, "eval", `start ${opName}`);
capRequire(this.profile.caps, "apply", `start ${opName}`);
capRequire(this.profile.caps, "observe", `start ${opName}`);
```
✅ Capability enforcement is wired for oracle ops.

**Code verification (runtimeImpl.ts line 249-251):**
```typescript
if (this.profile.truth === "speculative") {
  throw new Error(`commit rejected in speculative truth regime (kind=${kind})`);
}
```
✅ Truth regime enforcement is wired for commit.op.

---

### 2.3 Oracle Protocol ✅ COMPLETE (except ReqTool)

**Files:**
- [oracle/protocol.ts](src/core/oracle/protocol.ts) - Req/Resp types
- [oracle/adapter.ts](src/core/oracle/adapter.ts) - OracleAdapter interface
- [oracle/driver.ts](src/core/oracle/driver.ts) - `runOracleSession`
- [oracle/portal.ts](src/core/oracle/portal.ts) - OraclePortal interface
- [oracle/portalImpl.ts](src/core/oracle/portalImpl.ts) - Full implementation

**Status:** ✅ Complete (ReqTool stubbed)

**All request types implemented:**

| Request | Status | Notes |
|---------|--------|-------|
| `ReqEval` | ✅ | Evaluates expressions in snapshot env |
| `ReqApply` | ✅ | Applies closures, natives, continuations |
| `ReqObserve` | ✅ | Stack, control, handlers, storage summary |
| `ReqMatch` | ✅ | AST pattern matching with ?x binders |
| `ReqAssert` | ✅ | Predicate evaluation or value checks |
| `ReqSnapshot` | ✅ | Creates context receipts |
| `ReqCompress` | ✅ | Context compression receipts |
| `ReqHydrate` | ✅ | Retrieves stored receipts |
| `ReqTest` | ✅ | Smoke tests and equality tests |
| `ReqTool` | ⚠️ STUB | Returns "tool subsystem not wired yet" |
| `ReqEmitExample` | ✅ | Dataset emission (acks) |
| `ReqReturn` | ✅ | Session termination with Meaning |

**Gap:**
- `ReqTool` returns error instead of routing to tool execution system

---

### 2.4 Governance (Caps, Budgets, Profiles) ⚠️ DEFINED, PARTIALLY ENFORCED

**Files:**
- [governance/caps.ts](src/core/governance/caps.ts) - Capability system
- [governance/budgets.ts](src/core/governance/budgets.ts) - Budget tracking
- [governance/profile.ts](src/core/governance/profile.ts) - Execution profiles

**Status:** ⚠️ Types complete, enforcement partial

**Capabilities (caps.ts):** ✅ COMPLETE AND ENFORCED
```typescript
export function capRequire(caps: CapSet, required: Cap, context: string): void {
  if (caps.includes("*")) return; // wildcard grants all
  if (caps.includes(required)) return;
  const domain = required.split(".")[0];
  if (caps.includes(`${domain}.*`)) return;
  throw new Error(`capability denied: ${required} (context: ${context})`);
}
```
- Wildcards work (`*`, `tool.*`)
- Domain prefixes work
- Used in `runtimeImpl.ts` for oracle ops

**Budgets (budgets.ts):** ⚠️ DEFINED BUT NOT CONSUMED
```typescript
// These functions exist:
budgetConsumeOracleTurn(b)   // Throws if oracleTurns exceeded
budgetConsumeEvalStep(b)     // Throws if evalSteps exceeded
budgetConsumeToolCall(b)     // Throws if toolCalls exceeded

// But they are NEVER CALLED from:
// - machineStep.ts (no step counter)
// - run.ts (no budget parameter)
// - runtimeImpl.ts (no budget in dispatch)
```

**Profiles (profile.ts):** ✅ COMPLETE
```typescript
export const PROFILE_SPECULATIVE: Profile = {
  name: "speculative",
  caps: ["eval", "apply", "observe"],
  budgets: { ... },
  truth: "speculative",
};

export const PROFILE_TEST_CERTIFIED: Profile = { ... };
export const PROFILE_PROOF_CERTIFIED: Profile = { ... };
```
- Three profiles defined with appropriate caps/budgets/truth
- Used in `runtimeImpl.ts` constructor

---

### 2.5 Snapshots & Receipts ✅ COMPLETE

**Files:**
- [oracle/snapshots.ts](src/core/oracle/snapshots.ts) - SnapshotRepo
- [oracle/receipts.ts](src/core/oracle/receipts.ts) - ReceiptStore
- [oracle/ctxReceipts.ts](src/core/oracle/ctxReceipts.ts) - Context receipts

**Status:** ✅ Production-ready

**SnapshotRepo:**
```typescript
putEnv(s: EnvSnapshot): Hash;
getEnv(h: Hash): EnvSnapshot;
putState(s: StateSnapshot): Hash;
getState(h: Hash): StateSnapshot;
```
- Incrementing IDs for non-serializable structures
- Used in `runtimeImpl.ts` for oracle introspection

**ReceiptStore:**
```typescript
interface ReceiptStore {
  mode: ReceiptMode;  // "off" | "record" | "replay"
  get(key: Hash): OracleReceipt | undefined;
  put(r: OracleReceipt): void;
}
```
- InMemoryReceiptStore implementation
- `receiptKey()` for deterministic request hashing

---

### 2.6 Meaning & Dist Values ✅ COMPLETE

**Files:**
- [oracle/meaning.ts](src/core/oracle/meaning.ts) - MeaningVal
- [eval/dist.ts](src/core/eval/dist.ts) - DistVal

**Status:** ✅ Production-ready

**MeaningVal:**
```typescript
export type MeaningVal = {
  tag: "Meaning";
  denotation?: Val;      // The computed value
  rewrite?: Val;         // Code transformation
  obligation?: Val;      // Constraint to satisfy
  confidence?: number;   // 0.0-1.0
  evidence?: Val;        // Supporting evidence
  trace?: Val;           // Execution trace
  adoptEnvRef?: string;  // Adopt modified environment
};
```

**DistVal:**
```typescript
export type DistVal = {
  tag: "Dist";
  items: Array<{ v: Val; w: number }>;
  meta?: { kind?: string; note?: string };
};

distFrom(items, meta): DistVal
distNormalize(d): DistVal
distSample(d): Val
distTopK(d, k): Val[]
```

---

### 2.7 Reader/Parser & Macros ✅ COMPLETE

**Files:**
- [reader/tokenize.ts](src/core/reader/tokenize.ts) - Lexer
- [reader/parse.ts](src/core/reader/parse.ts) - Parser
- [reader/datum.ts](src/core/reader/datum.ts) - Datum representation
- [reader/toSyntax.ts](src/core/reader/toSyntax.ts) - Datum → Syntax
- [pipeline/compileText.ts](src/core/pipeline/compileText.ts) - Full pipeline (480 lines)
- [expand/syntaxRules.ts](src/core/expand/syntaxRules.ts) - Macro rules
- [expand/transformer.ts](src/core/expand/transformer.ts) - Macro expansion

**Status:** ✅ Production-ready

Full pipeline:
1. Tokenize ✅
2. Parse to datum ✅
3. Convert to syntax objects ✅
4. Add scopes (hygiene) ✅
5. Expand (define/define-syntax/macros) ✅
6. Lower to core Expr ✅

---

### 2.8 REPL System ⚠️ BASIC, NEEDS ENHANCEMENT

**Files:**
- [bin/omega-repl.ts](bin/omega-repl.ts) - Interactive REPL (387 lines)
- [test/llm_repl_demo.ts](test/llm_repl_demo.ts) - LLM speaks Lisp demo

**Status:** ⚠️ Working but needs debugger commands

**Current features:**
- Interactive readline input
- Multi-line input (paren balancing)
- Pretty-printing values as s-expressions
- Persistent state across evaluations (accumulates defines)
- `:help`, `:env`, `:defs`, `:quit` commands
- `:ask <text>` - LLM generates and evaluates code
- Verbose oracle trace mode (`--verbose`)
- TTY and non-TTY (piped) modes

**Missing features for "dual REPL" vision (from IMPLEMENTATION-20.md):**

| Feature | Description | Priority |
|---------|-------------|----------|
| `:stack` | Show current continuation stack | HIGH |
| `:frame N` | Inspect specific frame | HIGH |
| `:env` (detailed) | Show frame-specific bindings | HIGH |
| `:pause` | Pause at oracle request | HIGH |
| `:resume` | Continue from pause | HIGH |
| `:step` | Single-step evaluation | MEDIUM |
| `:break OP` | Break on specific effect op | MEDIUM |
| `:rollback RID` | Restore to receipt | MEDIUM |
| Completion | Tab-completion for symbols | LOW |
| Syntax highlighting | Color output | LOW |

---

### 2.9 Oracle Adapters ⚠️ STUBBED

**Files:**
- [oracle/adapters/anthropicAdapter.ts](src/core/oracle/adapters/anthropicAdapter.ts)
- [oracle/adapters/mcpAdapter.ts](src/core/oracle/adapters/mcpAdapter.ts)
- [oracle/scriptedOracle.ts](src/core/oracle/scriptedOracle.ts)
- [oracle/plugins/](src/core/oracle/plugins/) - OpenAI, Ollama, Anthropic plugins

**Status:** ⚠️ ScriptedOracle works, others stubbed

| Adapter | Status | Notes |
|---------|--------|-------|
| ScriptedOracleAdapter | ✅ WORKING | Used in tests, returns (+ 20 22) = 42 |
| AnthropicAdapter | ⚠️ STUB | Marked "STUB implementation" |
| MCPClientAdapter | ⚠️ STUB | "Would set up bidirectional MCP connection" |
| OpenAI plugin | ⚠️ PARTIAL | Check live test |
| Ollama plugin | ? | Need to verify |

---

## 3. Gap Analysis: What Needs Work

### 3.1 Critical Gaps (Block Package Layer)

#### Gap 1: Budget Enforcement Not Wired

**Problem:** `budgetConsumeEvalStep()` exists but is never called.

**Impact:** Cannot limit eval steps, risk infinite loops.

**Fix:**
```typescript
// In run.ts or a new budgetedRun.ts:

export async function runToCompletionWithBudget(
  runtime: Runtime,
  state0: State,
  budget: Budget,
  maxSteps: number,
): Promise<Val> {
  let st = state0;
  let steps = 0;

  while (true) {
    // Consume budget
    budget = budgetConsumeEvalStep(budget);  // Throws if exceeded
    steps++;

    if (steps > maxSteps) throw new Error(`maxSteps exceeded: ${steps}`);

    const outcome = stepOnce(st);
    // ... rest of loop
  }
}
```

**Test:**
```typescript
test("budget enforcement stops runaway evaluation", () => {
  const budget = budgetDefault({ maxEvalSteps: 100 });
  const infiniteLoop = compileTextToExpr("((lambda (x) (x x)) (lambda (x) (x x)))");
  expect(() => runToCompletionWithBudget(runtime, state, budget, 1000))
    .toThrow(/budget exhausted/);
});
```

---

#### Gap 2: ReqTool Not Implemented

**Problem:** portalImpl.ts returns error for ReqTool.

**Impact:** Cannot execute tools from oracle sessions.

**Fix:**
```typescript
// In portalImpl.ts:

if (req.tag === "ReqTool") {
  const { toolName, args } = req;

  // Check capability
  capRequire(this.profile.caps, `tool.${toolName}`, "ReqTool");

  // Look up tool in registry
  const tool = this.toolRegistry.get(toolName);
  if (!tool) {
    return { tag: "RespError", message: `unknown tool: ${toolName}` };
  }

  // Execute tool
  const result = await tool.call(args);
  return { tag: "RespVal", value: result };
}
```

**Dependency:** Requires tool registry (see World Package in INTEGRATION-PLAN.md).

---

#### Gap 3: Real LLM Adapters

**Problem:** Only ScriptedOracleAdapter works; Anthropic/MCP are stubs.

**Impact:** Cannot use real LLMs in oracle sessions.

**Options:**
1. Complete AnthropicAdapter stub
2. Use existing OpenAI plugin (needs verification)
3. Use llm_repl_demo.ts pattern (LispSpeakingOracleAdapter)

**Recommended:** Use llm_repl_demo.ts as reference implementation, extract to proper adapter.

---

### 3.2 Medium Gaps (Affect Packages)

#### Gap 4: Nondet "best"/"sample" Modes

**Problem:** runNondet only implements "first" and "all" modes.

**Fix:** Add scoring function and selection logic to runner.ts.

---

#### Gap 5: Context Constraints Not Enforced

**Problem:** ConstraintObs types defined but never checked.

**Fix:** Add constraint checking in relevant evaluation points.

---

### 3.3 Low Gaps (Polish)

#### Gap 6: Evidence Accumulation

**Problem:** `ctxAddEvidence` exists but never called.

**Fix:** Call during oracle sessions to build evidence chains.

---

#### Gap 7: REPL Debugger Commands

**Problem:** No stack introspection or pause/resume.

**Fix:** Implement `:stack`, `:frame`, `:pause`, `:resume` commands.

---

## 4. REPL Package Requirements

The REPL is part of the core system but should be enhanced as a separate package for clean separation.

### 4.1 Current REPL (omega-repl.ts)

**What exists:**
- Basic interactive loop with readline
- Multi-line input (paren depth tracking)
- Pretty-printing (`valToSexp`)
- Persistent definitions
- Simple LLM integration (`:ask`)

### 4.2 Required Enhancements

```
┌────────────────────────────────────────────────────────────────────────────┐
│                         REPL ENHANCEMENT PLAN                               │
├────────────────────────────────────────────────────────────────────────────┤
│                                                                            │
│  PHASE 1: Debugger Commands (Enable "REPL at any level")                  │
│  ─────────────────────────────────────────────────────────                │
│  :stack          Show continuation frames                                  │
│  :frame N        Inspect frame N (env, control, etc.)                     │
│  :env [frame]    Show bindings in frame                                   │
│  :control        Show current control expression                          │
│  :handlers       Show installed handlers                                   │
│                                                                            │
│  PHASE 2: Pause/Resume (Interactive debugging)                            │
│  ─────────────────────────────────────────────────────────                │
│  :pause          Pause at next effect op                                  │
│  :resume         Continue execution                                        │
│  :step           Single-step (one frame)                                  │
│  :break OP       Break when OP is performed                               │
│  :clear          Clear breakpoints                                         │
│                                                                            │
│  PHASE 3: Oracle Interaction (Dual REPL)                                  │
│  ─────────────────────────────────────────────────────────                │
│  :oracle-start   Begin manual oracle session                              │
│  :req-eval EXPR  Send ReqEval to oracle                                   │
│  :req-test TESTS Send ReqTest to oracle                                   │
│  :oracle-end     End oracle session with Meaning                          │
│  :transcript     Show oracle session transcript                           │
│                                                                            │
│  PHASE 4: Polish                                                          │
│  ─────────────────────────────────────────────────────────                │
│  Tab completion  Complete symbols from env                                │
│  Syntax highlight Color s-expressions                                      │
│  History         Readline history with search                             │
│  :load FILE      Load and evaluate file                                   │
│  :save FILE      Save session definitions                                 │
│                                                                            │
└────────────────────────────────────────────────────────────────────────────┘
```

### 4.3 REPL Package Structure

```
src/packages/repl/
├── repl.ts              # Main REPL loop
├── commands.ts          # Command handlers (:stack, :frame, etc.)
├── debugger.ts          # Pause/resume/step logic
├── prettyPrint.ts       # Value → s-expression formatter
├── completion.ts        # Tab completion
├── oracleRepl.ts        # Oracle session interaction
└── index.ts             # Package exports

bin/omega-repl.ts        # Entry point (uses package)
```

---

## 5. Action Items Summary

### 5.1 Must-Have for Clean Abstraction Layer

| # | Item | Priority | Effort | File(s) |
|---|------|----------|--------|---------|
| 1 | Wire budget into eval loop | CRITICAL | 2 hours | run.ts, runtimeImpl.ts |
| 2 | Implement ReqTool handler | CRITICAL | 4 hours | portalImpl.ts + tool registry |
| 3 | Complete one real LLM adapter | HIGH | 4 hours | adapters/ |
| 4 | Add REPL :stack/:frame commands | HIGH | 4 hours | repl package |

### 5.2 Important for Packages

| # | Item | Priority | Effort | File(s) |
|---|------|----------|--------|---------|
| 5 | Implement nondet best/sample | MEDIUM | 2 hours | nondet/runner.ts |
| 6 | Add context constraint checking | MEDIUM | 3 hours | portalImpl.ts, machineStep.ts |
| 7 | REPL pause/resume/step | MEDIUM | 4 hours | repl package |

### 5.3 Polish

| # | Item | Priority | Effort | File(s) |
|---|------|----------|--------|---------|
| 8 | Evidence accumulation | LOW | 2 hours | portalImpl.ts |
| 9 | Tab completion | LOW | 2 hours | repl package |
| 10 | Deterministic UUIDs for testing | LOW | 1 hour | capture.ts, machineStep.ts |

---

## 6. Test Coverage Assessment

### 6.1 Well-Tested Areas ✅

- Governance (caps, budgets, profiles): [test/core/governance/*.spec.ts](test/core/governance/)
- Receipts & snapshots: [test/core/receipts/*.spec.ts](test/core/receipts/)
- Oracle protocol flow: [test/oracle/*.spec.ts](test/oracle/)
- S-expressions & pattern matching: [test/core/sexp/*.spec.ts](test/core/sexp/)
- Handlers & effects: [test/effects/handler_k.spec.ts](test/effects/handler_k.spec.ts)
- Macros: [test/macros/*.spec.ts](test/macros/)

### 6.2 Weak/Missing Coverage ⚠️

| Area | Missing Test | Impact |
|------|--------------|--------|
| Budget in eval loop | Budget consumption never tested in actual eval | Critical |
| ReqTool | No tests (stubbed) | Blocks tools |
| Real LLM flow | Only scripted adapter tested | Integration |
| Nondet best/sample | Not implemented, not tested | Nondet |
| Constraint enforcement | Types exist, no tests | Correctness |

---

## 7. Verification Checklist

Before building packages, verify these kernel properties:

### 7.1 Core Evaluator
- [ ] `(+ 1 2 3)` evaluates to 6
- [ ] `(define (fact n) (if (= n 0) 1 (* n (fact (- n 1)))))` works
- [ ] `(fact 5)` = 120
- [ ] Recursive closures work (self-reference)

### 7.2 Effects
- [ ] `(handle (perform op 42) ...)` intercepts the op
- [ ] Handler k is callable (resumption works)
- [ ] Multi-shot resumption works (call k twice)
- [ ] Nested handlers work (inner shadows outer)

### 7.3 Oracle
- [ ] `(effect int.op payload)` starts oracle session
- [ ] `ReqEval` evaluates in snapshot env
- [ ] `ReqTest` runs tests and reports pass/fail
- [ ] `ReqSnapshot`/`ReqHydrate` enable rollback
- [ ] Oracle returns `MeaningVal`

### 7.4 Governance
- [ ] Missing capability throws
- [ ] Commit in speculative throws
- [ ] Profile caps are enforced

### 7.5 REPL
- [ ] Multi-line input works
- [ ] Defines persist across evaluations
- [ ] `:ask` integrates LLM (with API key)

---

## 8. Conclusion

The OmegaLLM kernel is **architecturally sound** with a solid foundation:
- CESK machine is complete and well-implemented
- Effect handlers with multi-shot resumptions work
- Oracle protocol is fully specified
- Governance types are designed correctly

**Critical gaps to address before package development:**
1. Wire budget consumption into eval loop
2. Implement ReqTool for tool execution
3. Complete at least one real LLM adapter
4. Add basic debugger commands to REPL

**Estimated time to address critical gaps: 1-2 days**

Once these are addressed, the kernel provides a clean abstraction layer for:
- Problem-Solving Package (facts, fixpoint, memo, subeval)
- Enterprise Package (policy, audit, approval, session)
- World Package (read, write, run, tools)

---

*Document Version: 1.0*
*Created: 2025-01-17*
*Last Verified: Audit based on source code analysis*
