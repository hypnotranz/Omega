# Job 016: Quick Reference Manual & Demo Suite

## Executive Summary

OmegaLLM is a Lisp dialect where **LLM calls are language primitives**. The expression `(effect infer.op "What is 2+2?")` calls an LLM and returns its response as a string value. This means you can use LLM calls anywhere you'd use any other expression: in `if` conditions, as function arguments, inside `map` over a list, etc.

The codebase has 27 documentation chapters that explain how to use this capability, structured as a progression from "here's how to call an LLM" to "here's how to build a logic programming system with semantic predicates." These chapters are thorough but dense. A user looking for "how do I run 10 LLM calls in parallel?" has to read through Chapter 22's full explanation of SICP concurrency theory.

**This job creates two things:**

1. **A Quick Reference** (`docs/USER-MANUAL--00--Quick-Reference.md`) — A single document with 27 sections, one per chapter. Each section is 5-10 lines: a one-sentence explanation of what the chapter enables, a minimal code example, and links to the full chapter and a runnable demo.

2. **A Demo Suite** (`demo/by-chapter/`) — 27 runnable TypeScript files, one per chapter. Each demo uses the existing demo harness to run actual Lisp code with scripted oracle responses (so no real LLM calls needed). A user can run `npx tsx demo/by-chapter/ch05-nondeterministic.ts` and see the feature working.

**Why this matters:** Right now, to understand what OmegaLLM can do, you have to read thousands of lines of documentation. After this job, you can scan a single page, find the feature you need, and immediately run a working example.

---

## Background: Key Concepts

Before implementing, understand these OmegaLLM primitives that the demos will exercise:

### Core LLM Effect
```lisp
(effect infer.op "prompt")  ; Returns LLM response as string
(effect infer.op (list "Translate " word " to French"))  ; Dynamic prompts
```
The `effect` keyword invokes a runtime effect. `infer.op` is the effect that calls the LLM.

### Semantic Predicates
```lisp
(define (is-food? x)
  (equal? "yes" (effect infer.op (list "Is " x " food? yes/no"))))

(filter is-food? (list "apple" "car" "pizza"))
=> ("apple" "pizza")
```
Functions that use LLM calls to make semantic judgments.

### AMB (Nondeterministic Choice)
```lisp
(amb 1 2 3)  ; Choose one of these values
(require (> x 1))  ; If false, backtrack and try another choice
(effect amb.fail "reason")  ; Explicitly trigger backtracking
```
`amb` is a Lisp construct for exploring multiple possibilities. When `require` fails, the system automatically backtracks to try the next `amb` choice.

### Streams (Lazy Sequences)
```lisp
(list->stream (list 1 2 3 4 5))  ; Convert to lazy stream
(stream->list s 3)  ; Force first 3 elements
(stream-map f s)  ; Lazy map (doesn't compute until forced)
```
Streams compute elements on demand. With LLM calls, this means you only pay for elements you actually use.

### Fibers (Concurrency)
```lisp
(effect fiber.spawn (lambda () (effect infer.op "Q1")))  ; Start async task
(effect fiber.join fib)  ; Wait for result
```
Fibers enable parallel LLM calls. 100 documents × 2 seconds each = 200 seconds sequential, but 20 seconds with 10 parallel workers.

### Search Effect (Multi-Shot Sampling)
```lisp
(effect search.op "Pick a random color")  ; Returns distribution of samples
```
Instead of one LLM response, get multiple samples to see the distribution of answers.

---

## Dependencies

**No blockers.** This is documentation and demo creation. All the primitives (`infer.op`, `amb`, streams, fibers) already exist and are tested.

---

## What Already Exists

### 1. The 27 Documentation Chapters

Located at `docs/USER-MANUAL--XX--*.md`. Here's what each covers:

| Ch | Title | What It Enables |
|----|-------|-----------------|
| 1 | Getting Started | Running the REPL, basic expressions |
| 2 | LLM Calls as Functions | `(effect infer.op ...)`, wrapping in functions |
| 3 | Functional Composition | `map`, `filter` with LLM functions |
| 4 | Higher-Order LLM Functions | Factories: `(make-classifier category)` returns a classifier |
| 5 | Nondeterministic Search | `amb` + semantic predicates for constraint search |
| 6 | Multi-Shot Sampling | `search.op` for distributions |
| 7 | Lazy Streams | `stream-map`, `stream-filter` for on-demand LLM calls |
| 8 | The Debugger | `:debug`, `:step`, `:goto` for time-travel debugging |
| 9 | The Agentic REPL | `:ask` mode where LLM can call back into runtime |
| 10 | Full API Reference | Table of all effects and primitives |
| 11 | Semantic Procedures as Black Boxes | LLM functions hide understanding, not algorithms |
| 12 | Inference Processes | Recursive vs iterative LLM call patterns, cost model |
| 13 | Higher-Order Inference | `fold` as semantic synthesis, prompt factories |
| 14 | Semantic Data Abstraction | Validators: `(is-haiku? text)` |
| 15 | Sequences as Semantic Interfaces | Pipelines, `flatmap` for idea expansion |
| 16 | Symbolic Semantic Data | `same-meaning?`, `implies?`, `contradicts?` |
| 17 | Multiple Representations of Meaning | Style conversion: formal/casual/poetic |
| 18 | Generic Semantic Operations | Domain-specific operations (email/legal/medical) |
| 19 | Conversational State and Memory | Chat history, knowledge bases |
| 20 | The Semantic Environment Model | Context affects interpretation |
| 21 | Mutable Semantic Structures | Entity graphs, temporal state |
| 22 | Concurrent Inference | Parallel LLM calls, voting, singleflight |
| 23 | Streams of Inference | Infinite lazy LLM sequences |
| 24 | Metalinguistic Abstraction | The oracle protocol, LLM-in-the-eval-loop |
| 25 | Lazy Semantic Evaluation | Memoized thunks, compute-once |
| 26 | The AMB Inference Engine | Semantic constraint satisfaction |
| 27 | Logic Programming with Semantic Facts | Natural language fact databases, inference |

### 2. Demo Harness

The `demo/harness/` folder provides infrastructure for runnable demos:

- `types.ts` — Defines `DemoDefinition` (id, name, run function, invariants)
- `runner.ts` — `runDemo()` executes a demo and checks invariants
- `oracle-adapter.ts` — Scripted oracle that returns predetermined responses (no real LLM needed)
- `ledger.ts` — Records events for verification

### 3. Existing Demos (8)

Located at `demo/omega-wow/`. These can be adapted or referenced:

| Demo | Covers |
|------|--------|
| demo1-oracle-repl.ts | Oracle protocol (ReqEval, ReqApply, ReqObserve) |
| demo2-backtracking.ts | AMB with multiple strategies |
| demo3-concurrency.ts | Parallel fiber execution |
| demo4-generic-synthesis.ts | Generic operations |
| demo5-constraint-repair.ts | Validate/repair loops |
| demo6-semantic-macros.ts | Macro expansion |
| demo7-compilation.ts | Compilation pipeline |
| demo8-meta-circular.ts | Metacircular evaluator |

### 4. Tests (Source Material)

Tests contain working code examples that can be extracted:

| Feature | Test File |
|---------|-----------|
| LLM calls | `test/oracle/protocol.spec.ts` |
| AMB | `test/amb/amb.spec.ts` |
| Streams | `test/prompt16-stream/stream.spec.ts` |
| Concurrency | `test/prompt13-concurrency/concurrency.spec.ts` |
| Generic | `test/prompt14-generic/generic.spec.ts` |
| Debugger | `test/repl/debugger.spec.ts` |
| Metacircular | `test/metacircular/metacircular.spec.ts` |
| Solver/Logic | `test/solver/*.spec.ts` |

---

## Implementation Plan

### Task 1: Create Quick Reference Document

Create `docs/USER-MANUAL--00--Quick-Reference.md` with this structure.

**CRITICAL FORMATTING RULES:**

1. **Semantic-only examples**: Code snippets must use content that *requires* an LLM to process — natural language text, sentiment analysis, tone detection, meaning comparison. Never use trivial numbers or single-word strings that could be processed by regular code.

2. **Define new operations**: When an example introduces an operation the reader hasn't seen before (like `parallel-map`, `stream->list`, `same-meaning?`), include a brief one-line definition before or after the code snippet explaining what it does.

3. **One section per chapter**: Each section should have:
   - A one-sentence explanation of what the chapter enables
   - Brief definitions of any new operations used
   - A minimal code example (semantic content only)
   - Links to full chapter and runnable demo

```markdown
# OmegaLLM Quick Reference

A Lisp where LLM calls are expressions. Use `(effect infer.op "prompt")` to call an LLM.

This reference has one section per chapter of the full manual. Each shows the key pattern with a minimal example.

---

## 2. LLM Calls as Functions

**Call an LLM with `(effect infer.op "prompt")`. The response is a string value.**

```lisp
(define (analyze-sentiment text)
  (effect infer.op (list "What is the sentiment of this text? positive/negative/neutral: " text)))

Ω> (analyze-sentiment "I absolutely love how responsive your support team has been!")
=> "positive"

Ω> (analyze-sentiment "This product broke after two days and nobody will help me.")
=> "negative"
```

📘 [Full Chapter](./USER-MANUAL--02--Llm-Calls-As-Functions.md) | 🎮 [Demo](../demo/by-chapter/ch02-llm-calls.ts)

---

## 3. Functional Composition

**Use `map` and `filter` with LLM functions. Each element triggers an LLM call.**

`filter` keeps only elements where the predicate returns true. Here, the predicate asks an LLM.

```lisp
(define (is-complaint? text)
  (equal? "yes" (effect infer.op
    (list "Is this customer message a complaint? yes/no: " text))))

(define messages
  (list "Your product is amazing, thank you!"
        "I've been waiting 3 weeks for my refund"
        "Quick question about sizing"
        "This is the worst service I've ever experienced"))

Ω> (filter is-complaint? messages)
=> ("I've been waiting 3 weeks for my refund"
    "This is the worst service I've ever experienced")
```

📘 [Full Chapter](./USER-MANUAL--03--Functional-Composition.md) | 🎮 [Demo](../demo/by-chapter/ch03-composition.ts)

---

## 5. Nondeterministic Search (AMB)

**Try multiple options automatically; backtrack on failure.**

`amb` picks one option from a list. If `require` fails later, it backtracks and tries the next option automatically.

```lisp
(define tones (list "formal" "friendly" "apologetic" "enthusiastic"))

(define (matches-tone? response desired-tone)
  (equal? "yes" (effect infer.op
    (list "Does this response have a " desired-tone " tone? yes/no: " response))))

(let ((tone (amb tones)))
  (let ((response (effect infer.op
          (list "Write a " tone " reply to: 'I want a refund'"))))
    (require (matches-tone? response "apologetic"))
    response))
; Tries "formal", checks tone, backtracks...
; Tries "friendly", checks tone, backtracks...
; Tries "apologetic", checks tone, succeeds!
=> "I sincerely apologize for any inconvenience. Let me process your refund right away."
```

📘 [Full Chapter](./USER-MANUAL--05--Nondeterministic-Search.md) | 🎮 [Demo](../demo/by-chapter/ch05-nondeterministic.ts)

---

## 22. Concurrent Inference

**Run multiple LLM calls in parallel.**

`parallel-map` applies a function to each element concurrently using fibers, returning results in order. 100 sequential calls taking 2 seconds each = 200 seconds. With 10 parallel workers = 20 seconds.

```lisp
(define tickets
  (list "Cannot login to my account after password reset"
        "When will the new feature be available?"
        "Your app crashed and deleted my data"
        "How do I export my reports to PDF?"))

(define (classify-ticket ticket)
  (effect infer.op
    (list "Classify this support ticket. Return exactly one of: bug/feature-request/question/complaint: " ticket)))

Ω> (parallel-map classify-ticket tickets)
=> ("bug" "feature-request" "bug" "question")  ; All 4 LLM calls ran concurrently
```

📘 [Full Chapter](./USER-MANUAL--22--Concurrent-Inference.md) | 🎮 [Demo](../demo/by-chapter/ch22-concurrent.ts)

---

[... continue for all 27 chapters ...]
```

### Task 2: Create Demo Directory

```bash
mkdir -p demo/by-chapter
```

Create `demo/by-chapter/index.ts`:
```typescript
// Barrel export for all chapter demos
export * from "./ch01-getting-started";
export * from "./ch02-llm-calls";
// ... all 27
```

### Task 3: Create Each Chapter Demo

**STEP 0: Find Existing Tests First**

Before writing any demo, search for existing tests that cover the feature:

```bash
# Example: Looking for AMB tests before writing ch05 demo
grep -r "amb" test/ --include="*.spec.ts" -l
# Found: test/amb/amb.spec.ts
```

Use the existing test as your starting point. The test already has:
- Working Lisp code that exercises the feature
- Scripted oracle responses
- Assertions that verify correct behavior

**STEP 1: Update the Existing Test**

Add documentation references to the existing test file header:

```typescript
// test/amb/amb.spec.ts
/**
 * ═══════════════════════════════════════════════════════════════════════════
 * AMB (Nondeterministic Choice) Tests
 * ═══════════════════════════════════════════════════════════════════════════
 *
 * Quick Reference: docs/USER-MANUAL--00--Quick-Reference.md#5-nondeterministic-search-amb
 * Full Chapter:    docs/USER-MANUAL--05--Nondeterministic-Search.md
 * Demo:            demo/by-chapter/ch05-nondeterministic.ts
 * ═══════════════════════════════════════════════════════════════════════════
 */
```

**STEP 2: Create Demo Based on Test**

The demo adapts the test's working code but with semantic examples (not trivial values).

**File Naming Convention:**
- `ch01-getting-started.ts`
- `ch02-llm-calls.ts`
- `ch03-functional-composition.ts`
- etc. (lowercase, hyphenated, matches chapter topic)

**Required Header Comments:**
Every demo file MUST include a header block that references:
1. The Quick Reference section
2. The full User Manual chapter
3. A brief description of what the demo shows

Each demo follows this pattern:

```typescript
// demo/by-chapter/ch02-llm-calls.ts
/**
 * ═══════════════════════════════════════════════════════════════════════════
 * CHAPTER 2: LLM Calls as Functions
 * ═══════════════════════════════════════════════════════════════════════════
 *
 * Quick Reference: docs/USER-MANUAL--00--Quick-Reference.md#2-llm-calls-as-functions
 * Full Chapter:    docs/USER-MANUAL--02--Llm-Calls-As-Functions.md
 *
 * DEMONSTRATES:
 *   (effect infer.op "prompt") - Call an LLM and get response as string
 *   Wrapping LLM calls in reusable functions
 *
 * KEY PATTERN:
 *   LLM calls are just expressions. Use them anywhere you'd use a value.
 * ═══════════════════════════════════════════════════════════════════════════
 */

import type { DemoDefinition, DemoContext, DemoResult } from "../harness/types";

async function run(ctx: DemoContext): Promise<DemoResult> {
  // Script the oracle to return predictable responses
  ctx.oracle.addScript({
    match: (req, type) => type === "InferOp",
    respond: (req) => {
      const prompt = String((req as any).args?.[0] ?? "");

      if (prompt.includes("capital of France")) {
        return { value: "Paris", evidence: "demo-capital" };
      }
      if (prompt.includes("Translate") && prompt.includes("Spanish")) {
        return { value: "hola", evidence: "demo-translate" };
      }
      return { value: "unknown", evidence: "demo-fallback" };
    }
  });

  // The actual Lisp code from the chapter
  const result1 = await ctx.eval(`(effect infer.op "What is the capital of France?")`);
  const result2 = await ctx.eval(`
    (begin
      (define (translate text lang)
        (effect infer.op (list "Translate to " lang ": " text)))
      (translate "hello" "Spanish"))
  `);

  return {
    outputs: [result1, result2],
    success: result1 === "Paris" && result2 === "hola",
    metrics: { inferCalls: ctx.oracle.getCount("InferOp") },
    transcript: ctx.oracle.getTranscript()
  };
}

export const ch02Demo: DemoDefinition = {
  id: "ch02-llm-calls",
  name: "Chapter 2: LLM Calls as Functions",
  description: "Shows (effect infer.op ...) returning LLM response as string",
  tags: ["chapter02", "infer", "basic"],
  run,
  invariants: [
    {
      name: "capital-is-paris",
      check: (r) => ({ name: "capital-is-paris", ok: r.outputs[0] === "Paris", detail: "" })
    }
  ]
};

// Direct execution
if (require.main === module) {
  import("../harness").then(h => h.runDemo(ch02Demo));
}
```

### Task 4: Chapter-Specific Demo Details

Here's what each demo should show. **All examples must use semantic content only** — text that requires LLM understanding, not trivial values.

| Ch | Demo Focus | Semantic Example |
|----|------------|------------------|
| 1 | REPL basics | Basic Lisp — only chapter that doesn't need LLM content |
| 2 | infer.op | `(analyze-sentiment "I love this product!")` → "positive" |
| 3 | map/filter | `(filter is-complaint? customer-messages)` → filtered list |
| 4 | Factories | `(define classify-tech (make-classifier "technology category"))` then apply to product descriptions |
| 5 | AMB | Try tone options until response matches "apologetic" — backtrack on mismatch |
| 6 | search.op | Sample 5 different rewrites of a formal email; see distribution of tones |
| 7 | Streams | Lazy stream of follow-up questions; only generate what you need |
| 8 | Debugger | Step through sentiment analysis; see how LLM processed each message |
| 9 | Agentic | LLM asks runtime to evaluate `(length support-tickets)` to decide next action |
| 10 | Reference | Comprehensive demo showing infer, amb, streams, fibers together |
| 11 | Black box | `(is-professional-tone? email-draft)` — hides the judgment logic |
| 12 | Cost model | Recursive summarization vs iterative; show token usage difference |
| 13 | Synthesis | `(fold-left merge-perspectives "" stakeholder-opinions)` → consensus |
| 14 | Validators | `(is-haiku? poem)`, `(has-proper-greeting? email)` |
| 15 | Pipelines | Filter complaints → extract key issues → prioritize by severity |
| 16 | Meaning ops | `(same-meaning? "I'm upset" "I am dissatisfied")` → true |
| 17 | Style | `(convert-register angry-complaint 'formal 'empathetic)` |
| 18 | Domains | Legal summarization vs email summarization — different domain rules |
| 19 | Memory | Multi-turn customer support conversation with context |
| 20 | Context | "Bank" means finance vs riverbank depending on environment |
| 21 | Mutation | Build knowledge graph of customer issues; add relations over time |
| 22 | Parallel | `(parallel-map classify-ticket tickets)` — 4 tickets, 4 concurrent LLM calls |
| 23 | Infinite | Infinite stream of increasingly detailed explanations; take only 3 |
| 24 | Oracle | LLM inspects environment, evaluates subexpressions, applies functions |
| 25 | Lazy | Expensive analysis computed once, cached for multiple uses |
| 26 | Constraints | Find email tone from options where LLM confirms it matches criteria |
| 27 | Logic | Natural language fact database: "Alice is Bob's parent" → query grandchildren |

---

## Verification Steps

### 1. All demos run
```bash
cd OmegaLLM
for f in demo/by-chapter/ch*.ts; do
  npx tsx "$f" && echo "✓ $f" || echo "✗ FAILED: $f"
done
```

### 2. Quick Reference has 27 sections
```bash
grep -c '^## [0-9]' docs/USER-MANUAL--00--Quick-Reference.md
# Should output: 27
```

### 3. All links resolve
```bash
grep -oP '\]\(([^)]+)\)' docs/USER-MANUAL--00--Quick-Reference.md | \
  sed 's/](\(.*\))/\1/' | \
  while read link; do
    if [[ "$link" == ../* ]]; then
      [ -f "docs/../$link" ] || echo "BROKEN: $link"
    else
      [ -f "docs/$link" ] || echo "BROKEN: $link"
    fi
  done
```

---

## Checklist

### Setup
- [ ] Create `demo/by-chapter/` directory
- [ ] Create `demo/by-chapter/index.ts` barrel export
- [ ] Create `docs/USER-MANUAL--00--Quick-Reference.md` skeleton

### Part I Demos (Chapters 1-10)
- [ ] ch01-getting-started.ts
- [ ] ch02-llm-calls.ts
- [ ] ch03-composition.ts
- [ ] ch04-higher-order.ts
- [ ] ch05-nondeterministic.ts
- [ ] ch06-multi-shot.ts
- [ ] ch07-lazy-streams.ts
- [ ] ch08-debugger.ts
- [ ] ch09-agentic-repl.ts
- [ ] ch10-api-reference.ts

### Part II Demos (Chapters 11-27)
- [ ] ch11-semantic-procedures.ts
- [ ] ch12-inference-processes.ts
- [ ] ch13-higher-order-inference.ts
- [ ] ch14-semantic-data.ts
- [ ] ch15-sequences.ts
- [ ] ch16-symbolic-semantic.ts
- [ ] ch17-multiple-representations.ts
- [ ] ch18-generic-semantic.ts
- [ ] ch19-conversational-state.ts
- [ ] ch20-semantic-environment.ts
- [ ] ch21-mutable-semantic.ts
- [ ] ch22-concurrent-inference.ts
- [ ] ch23-streams-of-inference.ts
- [ ] ch24-metacircular.ts
- [ ] ch25-lazy-semantic.ts
- [ ] ch26-amb-inference.ts
- [ ] ch27-logic-programming.ts

### Integration
- [ ] Quick Reference links to all chapters
- [ ] Quick Reference links to all demos
- [ ] All demos pass verification
- [ ] README updated if needed

---

## Effort Estimate

**Total: 1-2 days**

- Quick Reference document: 2-3 hours
- Demo setup: 30 min
- Part I demos (10): 3-4 hours
- Part II demos (17): 6-8 hours
- Verification: 1 hour
