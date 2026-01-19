# OmegaLLM User Manual

[← Table of Contents](USER-MANUAL--00--Table-Of-Contents.md)

---

## Chapter 24: Metalinguistic Abstraction — The Oracle in the Evaluator
### 24.1 The Metacircular Revelation

SICP Chapter 4 opens with one of the most profound ideas in computer science: the metacircular evaluator. Abelson and Sussman show us that a Lisp interpreter can be written *in Lisp*—the language describes its own meaning. This isn't a trick; it's a revelation about the nature of language and computation.

The metacircular evaluator has two core procedures:
- `eval` takes an expression and environment, and produces a value
- `apply` takes a procedure and arguments, and produces a value

These call each other recursively. `eval` may need to `apply`; `apply` may need to `eval`. The dance between them *is* computation.

Now we add something new: the **oracle**. An oracle is a computational entity that can answer questions we cannot answer algorithmically. In OmegaLLM, the LLM is our oracle.

### 24.2 The Oracle Effect

When the evaluator encounters an effect expression:

```lisp
(effect infer.op "What is the capital of France?")
```

It doesn't try to compute the answer. How could it? The answer requires *understanding*—something algorithms don't have. Instead, the evaluator *suspends*, packages the question, and consults the oracle.

The oracle—the LLM—returns: "Paris"

The evaluator *resumes* with this answer, incorporating it into ongoing computation.

This is the key insight: **the oracle is part of the eval/apply cycle**. It's not an external service called from Lisp; it's an extension of evaluation itself.

```
Traditional:
  eval(expr, env) → value
  apply(proc, args) → value

With Oracle:
  eval(expr, env) → may suspend for oracle → value
  apply(proc, args) → may suspend for oracle → value
  oracle(question) → semantic answer (from LLM)
```

### 24.3 The Oracle Can Evaluate Code

Here is where the architecture becomes recursive in a deeper sense. The oracle—the LLM—has access to the Lisp runtime. When the LLM needs to answer a question, it can:

- Evaluate Lisp expressions
- Observe the environment
- Examine the call stack
- Run computations to inform its reasoning

This creates a fascinating loop: the evaluator asks the oracle a question, and the oracle may ask the evaluator to run code.

```
User: (effect infer.op "What is factorial of 10?")
       ↓
Evaluator: "I have a semantic question" → suspends
       ↓
Oracle (LLM): "I should compute this" → calls eval("(factorial 10)")
       ↓
Evaluator: Runs factorial computation → returns 3628800
       ↓
Oracle (LLM): "The answer is 3628800" → returns to evaluator
       ↓
Evaluator: Resumes with "3628800"
```

The oracle is not passive. It is an active participant in evaluation.

### 24.4 Tools as Reified Effects

In OmegaLLM's agentic mode (`:ask`), the oracle's ability to call back into the evaluator is made explicit through *tools*. The LLM sees:

```
Available tools:
- eval(expr): Evaluate a Lisp expression
- observe(what): Examine runtime state
- apply(fn, args): Apply a function
- return(value): Provide final answer
```

These tools are reified effect operations. When the LLM calls `eval("(+ 2 3)")`, it triggers a `ReqEval` effect that the runtime handles.

The conversation between oracle and evaluator becomes visible:

```
Oracle: I'll compute this.
Oracle: [calls eval("(define (fact n) (if (< n 2) 1 (* n (fact (- n 1)))))")]
Runtime: (returns: fact defined)
Oracle: [calls eval("(fact 10)")]
Runtime: (returns: 3628800)
Oracle: [calls return("3628800")]
```

### 24.5 The Oracle Protocol

OmegaLLM defines a formal protocol for oracle communication:

**Requests (from evaluator to oracle):**
- `ReqEval`: "Evaluate this expression"
- `ReqApply`: "Apply this function to these arguments"
- `ReqObserve`: "Tell me about the runtime state"
- `ReqMatch`: "Match this expression against a pattern"
- `ReqAssert`: "Verify this condition holds"
- `ReqReturn`: "Here is the final answer"

**Responses (from oracle to evaluator):**
- `RespVal`: A computed value
- `RespObs`: Observed state
- `RespAck`: Acknowledgment
- `RespError`: Something went wrong

This protocol is the interface between computation and comprehension.

### 24.6 Meaning as Value

The oracle doesn't return raw strings—it returns *meanings*. A `Meaning` is a value that carries:

```typescript
interface Meaning {
  denotation: Val;      // The semantic content
  confidence: number;    // How certain (0.0 to 1.0)
  trace: Val;           // Provenance information
}
```

When you write `(effect infer.op "Is this text professional?")`, the returned meaning might be:

```
{
  denotation: "yes",
  confidence: 0.92,
  trace: "inferred by gpt-4o, 1 turn"
}
```

The meaning captures not just *what* the oracle said, but *how confident* it was and *how* it arrived at the answer.

### 24.7 The Evaluator as Conversation

Traditional evaluators are monologues: they reduce expressions silently until a value emerges. OmegaLLM's evaluator is a dialogue. It talks to the oracle, and the oracle talks back.

This dialogue is recorded. Every question asked, every answer received, every tool call made—all preserved in a trace. You can replay, debug, and inspect the conversation between algorithm and understanding.

### 24.8 The Philosophical Implications

What does it mean for an evaluator to have an oracle?

In classical computation, the evaluator is complete—it can reduce any expression to a value using only its rules. But semantic questions ("Is this professional?") have no algorithmic answer. They require judgment, context, knowledge of human conventions.

By adding an oracle, we acknowledge that some questions require *understanding*, and understanding is not (currently) algorithmic. The oracle is a window into a form of intelligence different from the evaluator's mechanical reduction.

The evaluator is logic. The oracle is intuition. Together, they form something new: a system that can both compute and comprehend.

### 24.9 Exercises

**Exercise 24.1:** The oracle protocol includes `ReqObserve` for inspecting runtime state. Implement a semantic function that asks the LLM to suggest optimizations by observing the current call stack.

**Exercise 24.2:** The `Meaning` type includes a confidence score. Write a wrapper function `(confident-infer threshold prompt)` that retries the inference if confidence is below the threshold, up to 3 attempts.

**Exercise 24.3:** The oracle can evaluate code. Design a "self-improving prompt" system where the oracle evaluates a test case, and if it fails, asks the oracle to revise the prompt and try again.

**Exercise 24.4:** Consider the philosophical question: Is the oracle part of the language, or external to it? Write an essay arguing each position. What are the implications for program semantics?

**Exercise 24.5:** The metacircular evaluator in SICP can interpret itself. Could OmegaLLM's oracle-augmented evaluator theoretically describe its own meaning? What would happen if the oracle were asked to explain how it works?