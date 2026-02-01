# OmegaLLM User Manual

[← Table of Contents](USER-MANUAL--00--Table-Of-Contents.md)

---

## Appendix B: Design Philosophy
### Why Lisp?

Lisp's homoiconicity (code as data) means LLM prompts can be constructed, transformed, and inspected as ordinary data structures. The language doesn't distinguish between "prompt templates" and "code"—they're the same thing.

### Why Effects?

Algebraic effects make LLM calls explicit and controllable. The runtime can:
- Intercept calls for caching/mocking
- Manage budgets and rate limits
- Record provenance for auditing
- Enable time-travel debugging

### Why AMB?

Nondeterministic search with backtracking is a natural fit for LLM-guided exploration. When the LLM's judgment acts as a constraint, AMB automatically explores the solution space until all semantic requirements are satisfied.

---

# Part II: Structure and Interpretation of Inference Programs

*In the spirit of Abelson and Sussman's classic, adapted for the age of language models.*

---

## Preface to Part II

In 1985, Harold Abelson and Gerald Jay Sussman published *Structure and Interpretation of Computer Programs*, a book that changed how we think about programming. Its central insight was that programs are not merely instructions for machines—they are expressions of ideas, and the act of programming is the act of building languages to express those ideas.

Four decades later, we find ourselves at a similar inflection point. Language models have given computers something they never had before: the ability to understand meaning. Not perfectly, not reliably, but genuinely. When you ask a language model "Is this text professional?", it doesn't pattern-match against rules—it *comprehends*.

This section explores what happens when we take the principles of SICP—abstraction, composition, higher-order programming, streams, nondeterminism—and apply them to programs that can invoke semantic understanding. The result is a new kind of programming, one where the distinction between computation and comprehension dissolves.

We call it the **Structure and Interpretation of Inference Programs**.

---

## Epilogue: The Structure of Understanding

We began this journey with a simple observation: language models can understand meaning. We end with a new kind of programming—one where computation and comprehension intertwine.

The lessons of SICP—abstraction, composition, laziness, nondeterminism, metalinguistic power—all apply, but transformed. Where Abelson and Sussman built evaluators for algorithms, we build evaluators for inference. Where they abstracted over procedures, we abstract over understanding.

The programs in this book are not just instructions for computation. They are invocations of meaning. They ask questions that no algorithm could answer, and they compose those answers into new knowledge.

This is programming for the age of AI: precise where precision matters, open to understanding where understanding matters, and always, always compositional.

The journey continues. Every new capability of language models becomes a new primitive for inference programming. Every insight into evaluation becomes a way to structure semantic computation.

Welcome to the structure and interpretation of inference programs.

*—The Authors, in the spirit of Abelson and Sussman*

---