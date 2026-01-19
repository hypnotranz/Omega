# Ω Language Official Compliance Checklist

**Generated**: 2026-01-15
**Source**: ARCHITECTURE/32-LANGUAGE-OFFICIAL-*.md (Files 1-11, Files 12-14 are blank)

## HOW TO FIND SPECS

```bash
# Find spec for a feature:
grep -n "KEYWORD" ARCHITECTURE/32-LANGUAGE-OFFICIAL-*.md

# Examples:
grep -n "int" ARCHITECTURE/32-LANGUAGE-OFFICIAL-1.md        # int special form
grep -n "Req\." ARCHITECTURE/32-LANGUAGE-OFFICIAL-2.md      # Oracle protocol
grep -n "omega\." ARCHITECTURE/32-LANGUAGE-OFFICIAL-3.md    # SICP modules
grep -n "profile:" ARCHITECTURE/32-LANGUAGE-OFFICIAL-4.md   # Governance profiles
grep -n "type State" ARCHITECTURE/32-LANGUAGE-OFFICIAL-6.md # CEKS machine
grep -n "amb" ARCHITECTURE/32-LANGUAGE-OFFICIAL-10.md       # Nondeterminism
grep -n "syntax-rules" ARCHITECTURE/32-LANGUAGE-OFFICIAL-11.md # Macro system
```

Legend: ✅ Implemented | ⚠️ Partial | ❌ Missing

---

## FILE 1: FOUNDATIONS, KERNEL, DUAL-PLANE SEMANTICS

### Kernel Forms (lines 251-268)

| Form | Line | Status | Implementation |
|------|------|--------|----------------|
| quote | 253 | ✅ | src/core/eval.ts |
| var | 254 | ✅ | src/core/eval.ts |
| lambda | 255 | ✅ | src/core/eval.ts |
| apply | 256 | ✅ | src/core/eval.ts |
| if | 257 | ✅ | src/core/eval.ts |
| define | 258 | ✅ | src/core/eval.ts |
| set! | 259 | ✅ | src/core/eval.ts |
| begin | 260 | ✅ | src/core/eval.ts |
| ctx | 261 | ❌ | NOT IMPLEMENTED |
| extend | 261 | ❌ | NOT IMPLEMENTED |
| seal | 261 | ❌ | NOT IMPLEMENTED |
| eval | 262 | ⚠️ | src/core/ffi.ts |
| **int** | 263 | ❌ | **CRITICAL MISSING** |
| **infer** | 264 | ❌ | **CRITICAL MISSING** |
| **rewrite** | 265 | ❌ | **CRITICAL MISSING** |
| handle | 266 | ⚠️ | src/core/conditions.ts |
| effect | 266 | ⚠️ | src/core/conditions.ts |
| match | 267 | ❌ | NOT IMPLEMENTED |

### Value Types (lines 161-179)

| Type | Line | Status | Implementation |
|------|------|--------|----------------|
| Atom | 162 | ✅ | src/core/types.ts |
| Pair | 163 | ✅ | arrays |
| Vector | 164 | ✅ | arrays |
| Map | 165 | ⚠️ | JS objects |
| Ctx | 166 | ❌ | NOT IMPLEMENTED |
| Closure | 167 | ✅ | src/core/types.ts |
| **Meaning** | 168 | ❌ | **CRITICAL MISSING** |
| Dist<Val> | 169 | ❌ | NOT IMPLEMENTED |
| Engine | 170 | ❌ | NOT IMPLEMENTED |
| Prompt | 171 | ❌ | NOT IMPLEMENTED |
| Policy | 172 | ❌ | NOT IMPLEMENTED |
| Receipt | 173 | ⚠️ | src/runtime/journal.ts |
| Evidence | 174 | ❌ | NOT IMPLEMENTED |
| Capability | 175 | ⚠️ | src/security/capability-ref.ts |
| Effect | 176 | ❌ | NOT IMPLEMENTED (as value) |
| Contract | 177 | ❌ | NOT IMPLEMENTED |
| Continuation | 178 | ✅ | src/core/types.ts |

### Meaning Structure (lines 200-216)

| Field | Line | Status |
|-------|------|--------|
| denotation | 202 | ❌ |
| residual | 203 | ❌ |
| rewrite | 204 | ❌ |
| invariants | 205 | ❌ |
| effects | 206 | ❌ |
| cost | 207 | ❌ |
| paths | 208 | ❌ |
| deps | 209 | ❌ |
| memo | 210 | ❌ |
| evidence | 211 | ❌ |
| obligation | 212 | ❌ |
| confidence | 213 | ❌ |
| trace | 214 | ❌ |

### Engine/Prompt/Policy (lines 220-235)

| Feature | Line | Status |
|---------|------|--------|
| Engine structure | 229 | ❌ |
| Prompt as typed AST | 230 | ❌ |
| Policy as strategy | 231 | ❌ |

---

## FILE 2: ORACLE PROTOCOL, TRUTH, LEARNING

### Oracle Protocol Request Types (lines 70-100)

| Request | Line | Status | Description |
|---------|------|--------|-------------|
| Req.Eval | 73 | ❌ | LLM calls REPL to evaluate code |
| Req.Apply | 74 | ❌ | LLM calls REPL to apply function |
| Req.Observe | 75 | ❌ | LLM sees context/env |
| Req.Match | 76 | ❌ | Pattern matching request |
| Req.Tool | 77 | ❌ | Tool call request |
| Req.Test | 78 | ❌ | Test execution request |
| Req.Assert | 79 | ❌ | Invariant check |
| Req.Snapshot | 80 | ❌ | Context snapshot |
| Req.Compress | 81 | ❌ | Context compression |
| Req.Hydrate | 82 | ❌ | Restore from receipt |
| Req.EmitExample | 83 | ❌ | Training data emission |
| Req.Return | 84 | ❌ | Session end with result |
| Req.Fail | 85 | ❌ | Session abort |

### Truth Governance (lines 161-240)

| Feature | Line | Status |
|---------|------|--------|
| Claim type | 176 | ❌ |
| Certificate type | 177 | ❌ |
| Obligation type | 178 | ❌ |
| speculative regime | 195 | ❌ |
| test-certified | 199 | ❌ |
| proof-certified | 205 | ❌ |
| consensus-certified | 210 | ❌ |
| commit barrier | 223 | ❌ |
| commit/rewrite | 233 | ❌ |
| commit/memo | 234 | ❌ |
| commit/invariants | 235 | ❌ |
| commit/prompt | 236 | ❌ |
| commit/policy | 237 | ❌ |

### Learning Plane (lines 360-500)

| Feature | Line | Status |
|---------|------|--------|
| define-engine | 382 | ❌ |
| define-prompt | 375 | ❌ |
| define-policy | 376 | ❌ |
| specialize | 395 | ❌ |
| emit-example | 415 | ❌ |
| episode | 417 | ❌ |
| reward | 418 | ❌ |
| train/prompt | 452 | ❌ |
| train/policy | 459 | ❌ |
| train/weights | 466 | ❌ |
| promote | 496 | ❌ |

---

## FILE 3: SICP TOWER MODULES (lines 33-47)

| Module | Line | Status | Purpose |
|--------|------|--------|---------|
| omega.kernel | 34 | ✅ | src/core/*.ts |
| omega.meta | 35 | ❌ | Meta-circular evaluator |
| omega.intension | 36 | ❌ | Intensional evaluation |
| omega.identity | 37 | ❌ | Identity/persistence |
| omega.test | 38 | ⚠️ | tests/*.test.ts |
| omega.stream | 39 | ❌ | Lazy streams |
| omega.nondet | 40 | ❌ | amb/backtracking |
| omega.generic | 41 | ❌ | Generic operations |
| omega.constraints | 42 | ❌ | Propagators |
| omega.concurrent | 43 | ❌ | Concurrency |
| omega.compiler | 44 | ❌ | Compilation passes |
| omega.match | 45 | ❌ | Pattern matching |
| omega.tool | 46 | ⚠️ | src/core/ffi.ts |
| omega.ml | 47 | ❌ | ML/training integration |

---

## FILE 4: GOVERNANCE PROFILES (lines 20-100)

| Profile | Line | Status |
|---------|------|--------|
| Profile type | 22 | ❌ |
| profile:explore | 57 | ❌ |
| profile:pragmatic | 68 | ❌ |
| profile:strict | 77 | ❌ |
| profile:airgap | 88 | ❌ |
| with-profile | 99 | ❌ |

---

## FILE 5: IMPLEMENTATION BLUEPRINT

### System Architecture (lines 20-48)

| Component | Line | Status |
|-----------|------|--------|
| Reader/AST | 31 | ✅ |
| Macro expander | 32 | ⚠️ |
| CEKS machine | 33 | ❌ |
| Effect runtime | 34 | ⚠️ |
| Oracle session driver | 35 | ❌ |
| Trust/obligations | 36 | ❌ |
| Receipts/compression | 37 | ❌ |
| Artifact registry | 38 | ❌ |

### Adapters (lines 39-45)

| Adapter | Line | Status |
|---------|------|--------|
| LLM adapter (OpenAI/other) | 41 | ⚠️ |
| Tool adapters (bash, git, etc) | 42 | ⚠️ |
| Ledger store | 43 | ⚠️ |
| Receipt store | 43 | ❌ |
| Artifact registry store | 43 | ❌ |
| Dataset store | 43 | ❌ |
| Tokenizer/cost estimator | 44 | ❌ |

### Ledger Σ Event Types (lines 150-166)

| Event | Line | Status |
|-------|------|--------|
| EvalEnter | 153 | ❌ |
| EvalExit | 154 | ❌ |
| EffectEmit | 155 | ❌ |
| EffectHandle | 156 | ❌ |
| OracleStart | 157 | ❌ |
| OracleReq | 158 | ❌ |
| OracleResp | 159 | ❌ |
| ToolCall | 160 | ⚠️ |
| ToolResult | 161 | ⚠️ |
| Snapshot | 162 | ❌ |
| Commit | 163 | ❌ |
| Promote | 164 | ❌ |
| TrainEmit | 165 | ❌ |

### CEKS Machine Components (lines 185-205)

| Component | Line | Status |
|-----------|------|--------|
| State type | 186 | ❌ |
| expr field | 187 | ✅ |
| ctx field | 188 | ✅ |
| kont field | 189 | ❌ |
| store field | 190 | ❌ |
| handlers field | 191 | ❌ |
| ledger field | 192 | ❌ |

### Continuation Types (lines 198-204)

| Kont Type | Line | Status |
|-----------|------|--------|
| IfK | 199 | ❌ |
| BeginK | 200 | ❌ |
| ApplyFnK | 201 | ❌ |
| ApplyArgsK | 201 | ❌ |
| DefineK | 202 | ❌ |
| SetK | 203 | ❌ |
| HandleK | 204 | ❌ |

### Oracle Protocol Runtime (lines 242-294)

| Type | Line | Status |
|------|------|--------|
| InferRequest | 259-270 | ❌ |
| OracleSession | 272 | ❌ |
| OracleReq.ReqEval | 275 | ❌ |
| OracleReq.ReqApply | 276 | ❌ |
| OracleReq.ReqObserve | 277 | ❌ |
| OracleReq.ReqTool | 278 | ❌ |
| OracleReq.ReqTest | 279 | ❌ |
| OracleReq.ReqSnapshot | 280 | ❌ |
| OracleReq.ReqCompress | 281 | ❌ |
| OracleReq.ReqHydrate | 282 | ❌ |
| OracleReq.ReqEmitExample | 283 | ❌ |
| OracleReq.ReqReturn | 284 | ❌ |
| OracleReq.ReqFail | 285 | ❌ |
| OracleResp.RespVal | 288 | ❌ |
| OracleResp.RespMeaning | 289 | ❌ |
| OracleResp.RespEvidence | 290 | ❌ |
| OracleResp.RespError | 291 | ❌ |

### Prompt AST (lines 325-331)

| Variant | Line | Status |
|---------|------|--------|
| System | 326 | ❌ |
| Rules | 327 | ❌ |
| Schema | 328 | ❌ |
| Tools | 329 | ❌ |
| Examples | 330 | ❌ |
| Compose | 331 | ❌ |

### Policy Structure (lines 346-355)

| Field | Line | Status |
|-------|------|--------|
| id | 347 | ❌ |
| inferenceStrategy | 348 | ❌ |
| truthRegime | 349 | ❌ |
| confidenceAlgebra | 350 | ❌ |
| budgets | 351 | ❌ |
| critics | 352 | ❌ |
| escalation | 353 | ❌ |

### Engine Structure (lines 362-371)

| Field | Line | Status |
|-------|------|--------|
| weightsRef | 363 | ❌ |
| promptRef | 364 | ❌ |
| memoryRef | 365 | ❌ |
| policyRef | 366 | ❌ |
| version | 367 | ❌ |
| digest | 368 | ❌ |
| trustLevel | 369 | ❌ |

### Commit Operations (lines 381-388)

| Operation | Line | Status |
|-----------|------|--------|
| commit/rewrite | 383 | ❌ |
| commit/memo | 384 | ❌ |
| commit/invariants | 385 | ❌ |
| commit/prompt | 386 | ❌ |
| commit/policy | 387 | ❌ |
| commit/engine | 388 | ❌ |

### Receipt Structure (lines 411-421)

| Field | Line | Status |
|-------|------|--------|
| rid | 413 | ❌ |
| summary | 414 | ❌ |
| schema | 415 | ❌ |
| deps | 416 | ❌ |
| replay | 417 | ❌ |
| checks | 418 | ❌ |
| cost | 419 | ❌ |
| voi | 420 | ❌ |

### Tool Call Structure (lines 455-465)

| Field | Line | Status |
|-------|------|--------|
| name | 457 | ⚠️ |
| argv | 458 | ⚠️ |
| cwd | 459 | ⚠️ |
| env | 460 | ❌ |
| stdin | 461 | ❌ |
| timeoutMs | 462 | ⚠️ |
| cacheKey | 463 | ❌ |

### Training Example Schema (lines 506-519)

| Field | Line | Status |
|-------|------|--------|
| engineDigest | 507 | ❌ |
| promptDigest | 508 | ❌ |
| policyDigest | 509 | ❌ |
| envDigest | 510 | ❌ |
| qexprHash | 511 | ❌ |
| goal | 512 | ❌ |
| predictedMeaning | 513 | ❌ |
| extensionalChecks | 514 | ❌ |
| reward | 515 | ❌ |
| cost | 516 | ❌ |
| timestamp | 517 | ❌ |

---

## FILE 6: TYPESCRIPT REFERENCE IMPLEMENTATION

### Repository Layout (lines 31-82)

| Path | Line | Status |
|------|------|--------|
| src/ast.ts | 36 | ⚠️ |
| src/reader.ts | 37 | ✅ |
| src/values.ts | 38 | ⚠️ |
| src/context.ts | 39 | ⚠️ |
| src/store.ts | 40 | ❌ |
| src/hash.ts | 41 | ❌ |
| src/ledger.ts | 42 | ⚠️ |
| src/machine/kont.ts | 45 | ❌ |
| src/machine/state.ts | 46 | ❌ |
| src/machine/step.ts | 47 | ❌ |
| src/machine/runtime.ts | 48 | ❌ |
| src/effects/handler.ts | 51 | ⚠️ |
| src/effects/builtins.ts | 52 | ⚠️ |
| src/oracle/protocol.ts | 55 | ❌ |
| src/oracle/engine.ts | 56 | ❌ |
| src/oracle/oracleHandler.ts | 57 | ❌ |
| src/oracle/mockEngines.ts | 58 | ❌ |
| src/receipts/receipt.ts | 61 | ❌ |
| src/receipts/receiptStore.ts | 62 | ❌ |
| src/receipts/compress.ts | 63 | ❌ |
| src/receipts/hydrate.ts | 64 | ❌ |
| src/artifacts/registry.ts | 67 | ❌ |
| src/artifacts/promote.ts | 68 | ❌ |
| src/artifacts/semver.ts | 69 | ❌ |
| src/training/example.ts | 72 | ❌ |
| src/training/datasetStore.ts | 73 | ❌ |

### AST Expr Types (lines 120-139)

| Type | Line | Status |
|------|------|--------|
| Lit | 121 | ✅ |
| Sym | 122 | ✅ |
| Quote | 123 | ✅ |
| Lambda | 124 | ✅ |
| If | 125 | ✅ |
| Begin | 126 | ✅ |
| Define | 127 | ✅ |
| Set | 128 | ✅ |
| Apply | 129 | ✅ |
| Effect | 130 | ⚠️ |
| Handle | 131 | ⚠️ |
| Eval | 132 | ⚠️ |
| Int | 133 | ❌ |
| Infer | 134 | ❌ |
| Rewrite | 135 | ❌ |
| Match | 136 | ❌ |
| Ctx | 137 | ❌ |
| Extend | 138 | ❌ |
| Seal | 139 | ❌ |

### Value Types (lines 170-244)

| Type | Line | Status |
|------|------|--------|
| SyntaxVal | 170 | ❌ |
| PairVal | 172 | ✅ |
| VectorVal | 173 | ✅ |
| MapVal | 174 | ⚠️ |
| ClosureVal | 176 | ✅ |
| DistVal | 184 | ❌ |
| MeaningVal | 192 | ❌ |
| CapVal | 209 | ⚠️ |
| ReceiptVal | 211 | ❌ |
| EvidenceVal | 222 | ❌ |
| EngineVal | 224 | ❌ |
| PromptVal | 225 | ❌ |
| PolicyVal | 226 | ❌ |
| NativeFnVal | (File 7) | ⚠️ |

### Handler Interface (lines 462-477)

| Method | Line | Status |
|--------|------|--------|
| id | 463 | ⚠️ |
| canHandle(op) | 464 | ⚠️ |
| handle(call, runtime) | 465 | ⚠️ |
| onReturn(v, runtime) | 466 | ❌ |
| onFinally(runtime) | 467 | ❌ |

### Built-in Handlers (lines 1096-1141)

| Handler | Line | Status |
|---------|------|--------|
| CommitHandler | 1096 | ❌ |
| ToolHandler | 1108 | ⚠️ |
| TrainEmitHandler | 1119 | ❌ |
| ReceiptHandler | 1131 | ❌ |

### Mock Engines (lines 1026-1072)

| Engine | Line | Status |
|--------|------|--------|
| ConstantMeaningEngine | 1026 | ❌ |
| EvalAsksEngine | 1047 | ❌ |

---

## FILE 7: COMPLETING THE CORE RUNTIME

### Control Type (lines 40-44)

| Component | Line | Status |
|-----------|------|--------|
| Control = Expr ∪ Val | 40-42 | ❌ |
| Proper value injection | 54-57 | ❌ |

### Additional Continuations (lines 100-111)

| Kont | Line | Status |
|------|------|--------|
| MatchK | 109 | ❌ |
| RestoreCtxK | 110 | ❌ |

### Native Functions (lines 320-333)

| Feature | Line | Status |
|---------|------|--------|
| NativeFnVal type | 325 | ⚠️ |
| native record | 352 | ❌ |
| native get | 362 | ❌ |
| deepEqual | 371 | ❌ |

### Expansion/Lowering (lines 380-457)

| Feature | Line | Status |
|---------|------|--------|
| read → expand → eval | 381 | ⚠️ |
| Special form recognition | 393-410 | ⚠️ |
| int lowering | 420-449 | ❌ |
| infer lowering | 443 | ❌ |
| rewrite lowering | 447 | ❌ |

### Handle Semantics (lines 460-494)

| Feature | Line | Status |
|---------|------|--------|
| Delimited handler | 462-478 | ⚠️ |
| Handler compilation | 482-494 | ❌ |

### Match Semantics (lines 496-529)

| Feature | Line | Status |
|---------|------|--------|
| Pattern compilation | 500-512 | ❌ |
| Scoped bindings | 514-529 | ❌ |

### First-class Contexts (lines 530-540)

| Form | Line | Status |
|------|------|--------|
| ctx | 536 | ❌ |
| extend | 537 | ❌ |
| seal | 538 | ❌ |

### Obligations (lines 559-605)

| Feature | Line | Status |
|---------|------|--------|
| Obligation schema | 565-580 | ❌ |
| tests obligation | 570 | ❌ |
| eq-ext obligation | 574 | ❌ |
| Commit barrier | 586-605 | ❌ |

---

## FILE 9: SICP TOWER RECONSTRUCTION

### omega.nondet (lines 30-129)

| Feature | Line | Status |
|---------|------|--------|
| amb | 39 | ❌ |
| require | 45 | ❌ |
| all-solutions | 50 | ❌ |
| best-solution | 51 | ❌ |
| amb lowering to effect | 56 | ❌ |
| DFS handler | 79-85 | ❌ |
| Heuristic scoring | 86-89 | ❌ |
| amb as Dist | 92-98 | ❌ |
| all-solutions handler | 104 | ❌ |
| best-solution handler | 108-114 | ❌ |
| amb with critic | 116-128 | ❌ |

### omega.stream (lines 131-243)

| Feature | Line | Status |
|---------|------|--------|
| delay | 148 | ❌ |
| force | 155 | ❌ |
| cons-stream | 157 | ❌ |
| stream-car | 158 | ❌ |
| stream-cdr | 159 | ❌ |
| stream-map | 170 | ❌ |
| stream-filter | 175 | ❌ |
| stream.strictness | 186-196 | ❌ |
| stream fusion macro | 203-222 | ❌ |
| productivity analysis | 224-243 | ❌ |

### omega.generic (lines 245-332)

| Feature | Line | Status |
|---------|------|--------|
| attach-tag | 256 | ❌ |
| type-tag | 257 | ❌ |
| contents | 258 | ❌ |
| op-table | 261 | ❌ |
| put | 263 | ❌ |
| get | 266 | ❌ |
| apply-generic | 270-284 | ❌ |
| coercion tower | 286-294 | ❌ |
| adapter synthesis | 296-332 | ❌ |

### omega.constraints (lines 334-386)

| Feature | Line | Status |
|---------|------|--------|
| connectors/cells | 347-350 | ❌ |
| propagators | 351-359 | ❌ |
| int over networks | 362-370 | ❌ |
| diagnose | 375 | ❌ |
| repair | 376 | ❌ |

### omega.compiler (lines 388-465)

| Feature | Line | Status |
|---------|------|--------|
| Explicit-control evaluator | 399-410 | ❌ |
| ANF pass | 416 | ❌ |
| CPS pass | 417 | ❌ |
| Closure conversion | 419 | ❌ |
| Defunctionalization | 420 | ❌ |
| Bytecode emission | 422 | ❌ |
| Verify pass | 424-429 | ❌ |
| Commit pass | 430-432 | ❌ |
| Learned optimization | 442-464 | ❌ |

### omega.meta (lines 467-516)

| Feature | Line | Status |
|---------|------|--------|
| eval0 (meta-circular) | 475-495 | ❌ |
| int0 (meta-intensional) | 496-516 | ❌ |

### Self-Hosting Boot Plan (lines 518-582)

| Stage | Line | Status |
|-------|------|--------|
| Stage 0: Host kernel | 536-541 | ✅ |
| Stage 1: Stdlib in Ω | 543-545 | ❌ |
| Stage 2: Macro expander in Ω | 547-551 | ❌ |
| Stage 3: Meta-circular eval | 553-557 | ❌ |
| Stage 4: Compiler passes in Ω | 559-563 | ❌ |
| Stage 5: Replace host eval | 565-568 | ❌ |
| Stage 6: Inference in Ω | 570-573 | ❌ |

---

## FILE 10: AMB + SYNTAX-RULES IMPLEMENTATION

### amb Surface Forms (lines 27-57)

| Form | Line | Status |
|------|------|--------|
| (amb e1 e2 ... en) | 29 | ❌ |
| (require p) | 35 | ❌ |
| (cut) | 43 | ❌ |
| (all-solutions expr) | 49 | ❌ |
| (first-solution expr) | 50 | ❌ |
| (best-solution :score f expr) | 51 | ❌ |
| (sample-solution :seed s expr) | 52 | ❌ |

### amb Lowering (lines 59-99)

| Feature | Line | Status |
|---------|------|--------|
| amb to effect | 64-67 | ❌ |
| amb/cbv | 77 | ❌ |
| amb/cbn | 78 | ❌ |
| require to if/fail | 84-87 | ❌ |
| cut to effect | 93-96 | ❌ |

### Handler Semantics (lines 101-186)

| Feature | Line | Status |
|---------|------|--------|
| ChoicePoint type | 112-118 | ❌ |
| Frontier interface | 130-136 | ❌ |
| AmbFail error | 144-149 | ❌ |
| DFS exploration loop | 158-183 | ❌ |

### Search Collectors (lines 188-217)

| Collector | Line | Status |
|-----------|------|--------|
| first-solution | 194 | ❌ |
| all-solutions | 197-204 | ❌ |
| best-solution | 206-216 | ❌ |

### Inference Augmentation (lines 219-267)

| Feature | Line | Status |
|---------|------|--------|
| Heuristic scoring | 225-251 | ❌ |
| Critic pruning | 253-266 | ❌ |

### RL Integration (lines 269-294)

| Feature | Line | Status |
|---------|------|--------|
| Episode emission | 271-285 | ❌ |
| Reward shaping | 277-285 | ❌ |

### syntax-rules Expander (lines 296-461)

| Feature | Line | Status |
|---------|------|--------|
| Env0 (phase 0) | 304 | ❌ |
| Env1 (phase 1) | 305 | ❌ |
| Syntax objects | 322-338 | ❌ |
| Use-site scope | 329 | ❌ |
| Introducer scope | 330 | ❌ |
| Binding scope | 331 | ❌ |
| Pattern types (PVar, PLit, etc) | 360-368 | ❌ |
| Subst/SubstVal | 382-387 | ❌ |
| match(pat, stx) | 371-378 | ❌ |
| Template expansion | 389-398 | ❌ |
| Macro expansion algorithm | 400-421 | ❌ |
| Lowering after expansion | 423-435 | ❌ |

### Hermetic Semantic Macros (lines 437-461)

| Feature | Line | Status |
|---------|------|--------|
| Macro receipt | 448-456 | ❌ |
| compile-strict mode | 455 | ❌ |
| compile-airgap mode | 456 | ❌ |
| compile-explore mode | 457 | ❌ |

### Reference Programs (lines 463-609)

| Program | Line | Status |
|---------|------|--------|
| multiple-dwelling | 473-501 | ❌ |
| sqrt-stream | 516-550 | ❌ |
| complex numbers | 554-568 | ❌ |
| propagators | 572-594 | ❌ |
| const-fold macro | 596-608 | ❌ |

---

## FILE 11: DEEP HARDENING

### nondet Effect Algebra (lines 19-45)

| Effect | Line | Status |
|--------|------|--------|
| amb.op | 21 | ❌ |
| amb.fail | 22 | ❌ |
| amb.cut | 23 | ❌ |
| amb.observe | 24 | ❌ |
| amb.score | 25 | ❌ |

### ChoicePoint Structure (lines 54-68)

| Field | Line | Status |
|-------|------|--------|
| id | 56 | ❌ |
| state | 57 | ❌ |
| score | 58 | ❌ |
| bound | 59 | ❌ |
| depth | 60 | ❌ |
| meta | 61 | ❌ |
| traceRef | 62 | ❌ |

### Frontier Strategies (lines 69-88)

| Strategy | Line | Status |
|----------|------|--------|
| DFS (LIFO stack) | 80 | ❌ |
| BFS (FIFO queue) | 81 | ❌ |
| Best-first (priority queue) | 82 | ❌ |
| Beam (bounded PQ) | 83 | ❌ |
| Branch-and-bound | 84 | ❌ |
| Sampling | 85 | ❌ |

### Search Loop Algorithm (lines 92-152)

| Step | Line | Status |
|------|------|--------|
| Initialize frontier | 115 | ❌ |
| Loop until done | 116-148 | ❌ |
| Handle success | 127-132 | ❌ |
| Handle amb.op | 133-140 | ❌ |
| Handle amb.fail | 141-144 | ❌ |
| Handle amb.cut | 145-148 | ❌ |

### Meta-properties (lines 153-180)

| Property | Line | Status |
|----------|------|--------|
| completeness | 168 | ❌ |
| soundness | 169 | ❌ |
| fairness | 170 | ❌ |
| Policy guarantees | 173-178 | ❌ |

### Constraint Context (lines 186-196)

| Feature | Line | Status |
|---------|------|--------|
| Constraint log | 190-196 | ❌ |

### Heuristic Hooks (lines 198-238)

| Feature | Line | Status |
|---------|------|--------|
| Scoring hook | 200-223 | ❌ |
| Critic pruning hook | 225-238 | ❌ |

### Disagreement Logging (lines 240-254)

| Feature | Line | Status |
|---------|------|--------|
| Engine vs evaluator | 248-250 | ❌ |
| Event logging | 252-254 | ❌ |

### RL Episode Schema (lines 256-290)

| Field | Line | Status |
|-------|------|--------|
| Trajectory | 264-266 | ❌ |
| Reward shaping | 269-276 | ❌ |
| Policy artifacts | 282-290 | ❌ |

### Syntax Objects (lines 304-324)

| Feature | Line | Status |
|---------|------|--------|
| Syntax = datum + scopes + srcloc + phase | 308 | ❌ |
| Ident type | 312 | ❌ |
| addScope | 318 | ❌ |
| flipScope | 319 | ❌ |
| stripScope | 320 | ❌ |

### Three Scopes (lines 325-338)

| Scope | Line | Status |
|-------|------|--------|
| use-site U | 329 | ❌ |
| introducer I | 330 | ❌ |
| binder B | 331 | ❌ |

### Resolution Algorithm (lines 339-355)

| Feature | Line | Status |
|---------|------|--------|
| Best match | 341-352 | ❌ |
| Ambiguity handling | 350-354 | ❌ |

### Pattern Representation (lines 359-383)

| Type | Line | Status |
|------|------|--------|
| PVar | 370 | ❌ |
| PLit | 371 | ❌ |
| PWild | 372 | ❌ |
| PAtom | 373 | ❌ |
| PList | 374 | ❌ |
| PEllipsis | 375 | ❌ |

### SubstVal with Ranks (lines 384-399)

| Type | Line | Status |
|------|------|--------|
| One(Syntax) | 390 | ❌ |
| Many(List) | 391 | ❌ |
| Rank system | 394-399 | ❌ |

### Matching Invariants (lines 400-442)

| Invariant | Line | Status |
|-----------|------|--------|
| Shape consistency | 406-408 | ❌ |
| Rank consistency | 410-412 | ❌ |
| Literal identity | 414-420 | ❌ |
| Matching procedure | 422-442 | ❌ |

### Template Expansion (lines 443-459)

| Feature | Line | Status |
|---------|------|--------|
| Zip semantics | 449-458 | ❌ |

### Module/Phase Interaction (lines 460-482)

| Feature | Line | Status |
|---------|------|--------|
| require | 471 | ❌ |
| require-for-syntax | 472 | ❌ |
| provide | 473 | ❌ |
| provide-for-syntax | 474 | ❌ |
| Phase separation | 478-480 | ❌ |

### Expansion Receipt (lines 484-502)

| Field | Line | Status |
|-------|------|--------|
| input syntax hash | 492 | ❌ |
| transformer digest | 493 | ❌ |
| macro-time env digest | 494 | ❌ |
| output syntax hash | 495 | ❌ |
| deterministic params | 496 | ❌ |
| engine/policy versions | 497 | ❌ |
| obligations | 498 | ❌ |
| tool evidence | 499 | ❌ |

### Conformance Tests (lines 504-539)

| Category | Line | Status |
|----------|------|--------|
| Hygiene tests | 510-516 | ❌ |
| Ellipsis tests | 518-525 | ❌ |
| Phase tests | 527-531 | ❌ |
| Hermeticity tests | 533-538 | ❌ |

### Self-Hosting Milestones (lines 542-623)

| Milestone | Line | Status |
|-----------|------|--------|
| A: Nondet in host/Ω | 579-581 | ❌ |
| B: Expander in host | 583-585 | ❌ |
| C: Expander core in Ω | 587-594 | ❌ |
| D: Module compiler in Ω | 596-599 | ❌ |
| E: Meta-circular eval | 601-605 | ❌ |
| F: Oracle in Ω | 607-611 | ❌ |

### Guardrails (lines 614-621)

| Guardrail | Line | Status |
|-----------|------|--------|
| strict profile | 615 | ❌ |
| receipts pinned | 616 | ❌ |
| promotion required | 617 | ❌ |
| capability narrowing | 618 | ❌ |
| no-new-facts | 619 | ❌ |

---

## GRAND SUMMARY

| Category | File | Required | Have | Missing |
|----------|------|----------|------|---------|
| Kernel forms | 1 | 18 | 10 | 8 |
| Value types | 1 | 17 | 7 | 10 |
| Meaning fields | 1 | 13 | 0 | 13 |
| Oracle protocol | 2 | 13 | 0 | 13 |
| Truth governance | 2 | 13 | 0 | 13 |
| Learning plane | 2 | 11 | 0 | 11 |
| SICP modules | 3 | 14 | 2 | 12 |
| Governance profiles | 4 | 6 | 0 | 6 |
| System architecture | 5 | 8 | 1 | 7 |
| Adapters | 5 | 7 | 2 | 5 |
| Ledger events | 5 | 13 | 2 | 11 |
| CEKS machine | 5 | 7 | 2 | 5 |
| Continuations | 5 | 7 | 0 | 7 |
| Oracle types | 5 | 17 | 0 | 17 |
| Prompt AST | 5 | 6 | 0 | 6 |
| Policy structure | 5 | 7 | 0 | 7 |
| Engine structure | 5 | 7 | 0 | 7 |
| Commit operations | 5 | 6 | 0 | 6 |
| Receipt structure | 5 | 8 | 0 | 8 |
| Tool call fields | 5 | 7 | 3 | 4 |
| Training example | 5 | 11 | 0 | 11 |
| Repository layout | 6 | 25 | 3 | 22 |
| AST expr types | 6 | 19 | 10 | 9 |
| Value types (impl) | 6 | 14 | 5 | 9 |
| Handler interface | 6 | 5 | 2 | 3 |
| Built-in handlers | 6 | 4 | 1 | 3 |
| Mock engines | 6 | 2 | 0 | 2 |
| Control type | 7 | 2 | 0 | 2 |
| Additional konts | 7 | 2 | 0 | 2 |
| Native functions | 7 | 4 | 1 | 3 |
| Expansion/lowering | 7 | 5 | 1 | 4 |
| Handle semantics | 7 | 2 | 1 | 1 |
| Match semantics | 7 | 2 | 0 | 2 |
| First-class ctx | 7 | 3 | 0 | 3 |
| Obligations | 7 | 4 | 0 | 4 |
| omega.nondet | 9 | 12 | 0 | 12 |
| omega.stream | 9 | 10 | 0 | 10 |
| omega.generic | 9 | 9 | 0 | 9 |
| omega.constraints | 9 | 5 | 0 | 5 |
| omega.compiler | 9 | 9 | 0 | 9 |
| omega.meta | 9 | 2 | 0 | 2 |
| Boot plan | 9 | 7 | 1 | 6 |
| amb forms | 10 | 7 | 0 | 7 |
| amb lowering | 10 | 5 | 0 | 5 |
| Handler semantics | 10 | 4 | 0 | 4 |
| Search collectors | 10 | 3 | 0 | 3 |
| Inference augment | 10 | 2 | 0 | 2 |
| RL integration | 10 | 2 | 0 | 2 |
| syntax-rules | 10 | 12 | 0 | 12 |
| Hermetic macros | 10 | 4 | 0 | 4 |
| Reference programs | 10 | 5 | 0 | 5 |
| nondet algebra | 11 | 5 | 0 | 5 |
| ChoicePoint | 11 | 7 | 0 | 7 |
| Frontier strategies | 11 | 6 | 0 | 6 |
| Search loop | 11 | 6 | 0 | 6 |
| Meta-properties | 11 | 4 | 0 | 4 |
| Heuristic hooks | 11 | 2 | 0 | 2 |
| RL schema | 11 | 3 | 0 | 3 |
| Syntax objects | 11 | 6 | 0 | 6 |
| Three scopes | 11 | 3 | 0 | 3 |
| Resolution | 11 | 2 | 0 | 2 |
| Pattern types | 11 | 6 | 0 | 6 |
| SubstVal | 11 | 3 | 0 | 3 |
| Matching | 11 | 4 | 0 | 4 |
| Template expansion | 11 | 1 | 0 | 1 |
| Module/phase | 11 | 5 | 0 | 5 |
| Expansion receipt | 11 | 8 | 0 | 8 |
| Conformance tests | 11 | 4 | 0 | 4 |
| Self-host milestones | 11 | 6 | 0 | 6 |
| Guardrails | 11 | 5 | 0 | 5 |
| **TOTAL** | | **~450** | **~54** | **~396** |

---

## COMPLIANCE RATE

**Implemented: ~54 features (~12%)**
**Missing: ~396 features (~88%)**

---

## CRITICAL PATH (P0 - Must Have for Core Purpose)

1. **`int` / `infer` / `rewrite`** special forms (File 1, lines 263-265)
2. **`Meaning`** value type with all fields (File 1, lines 168, 200-216)
3. **Oracle Protocol** - ALL request types (File 2, lines 70-85; File 5, lines 274-285)
4. **CEKS Machine** with Control = Expr ∪ Val (File 6, 7)
5. **Oracle Handler** implementing interactive session (File 5, lines 296-314; File 6, lines 930-995)
6. **Commit barrier** enforcement (File 2, lines 223; File 7, lines 586-605)

Without these, the "intensional plane" doesn't exist and LambdaLLM is just a Lisp interpreter.

---

## FILES 12-14: BLANK

Files 12, 13, and 14 are empty (1 line each). No additional features to implement from these files.
