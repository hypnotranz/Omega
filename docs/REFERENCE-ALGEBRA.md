You’re pointing at a **real stratification** that most “prompt engineering” talk collapses:

* there is an algebra of **prompt artifacts** (strings / message-lists / templates / prompt-views), and
* there is an algebra of **executions** (LM calls, chains, loops, validators, search, tool-calls), and
* those two algebras are related, but **not isomorphic**.

Currying shows up exactly at the seam: once prompts become *first-class values*, you can partially apply / compose them *as programs* (higher‑order prompting), not merely concatenate them as bytes.

Below is (1) what’s already been formalized in the literature/systems, and (2) a clean formalization that makes the distinction you’re feeling mathematically explicit.

---

## 1) First: pin down semantics of an LM call as a function (so “algebra” is meaningful)

A (decoder-equipped) language model is well-modeled as a **stochastic transducer**:

* Let Σ be a vocabulary of tokens.
* Let Σ* be the set of finite token sequences.
* A “prompt” (at the lowest level) is a token sequence `p ∈ Σ*`.
* The model defines a conditional distribution over continuations.

A very standard view (and explicitly described in the LMQL/LMP paper) is: given a tokenized prompt, the LM assigns probabilities to next tokens; a decoding procedure calls the model repeatedly to produce a completion. ([arXiv][1])

So semantically, an LM+decoder is a **Markov kernel**:

[
K : \Sigma^* \to \mathrm{Dist}(\Sigma^*) \quad\text{where }K(p)\text{ is a distribution over completions.}
]

If you prefer “programming language” notation, treat it as a primitive effect:

* `infer : Prompt -> M Text`
* where `M` is a probability monad / sampling effect.

That single choice (LM inference as a kernel / monadic effect) is what makes “prompt algebra” and “execution algebra” separable and composable.

---

## 2) Two different compositions you’re mixing (correctly): “compose prompts” vs “compose runs”

### 2.1 Prompt concatenation (artifact-level composition)

At the raw string/token level, prompts form a **free monoid**:

* operation: concatenation `⊕ : Σ* × Σ* -> Σ*`
* identity: empty string `ε`

This is purely syntactic:

[
(p_1 \oplus p_2) \oplus p_3 = p_1 \oplus (p_2 \oplus p_3)
]

If you “just concatenate skater + write-a-song before inference”, you are computing:

[
K(p_{\text{skater}} \oplus p_{\text{song}})
]

That’s one inference call, conditioned on a bigger context.

### 2.2 Prompt chaining as execution (run-level composition)

What you described as:

> “write a song” and then passing it to a “skater dude” prompt

is usually **two LM calls**, i.e. *Kleisli composition* (monadic `bind`):

1. sample `x ~ K(p_song)`
2. sample `y ~ K(p_skater ⊕ x)`  (or some wrapper like “rewrite x in skater voice”)

Semantically:

[
\Pr(y) = \sum_x \Pr(x \mid p_{\text{song}})\cdot \Pr(y \mid p_{\text{skater}}\oplus x)
]

This is not remotely equal to `K(p_skater ⊕ p_song)` in general.

### 2.3 Why they differ (the intuition that matters)

* **Concatenation preconditions content selection**: persona/style instructions can change *what* gets said (topic selection, metaphors, structure), not only surface form.
* **Two-step rewriting postconditions**: it can often only reshape the already-produced artifact (unless the second step is allowed to hallucinate new content / “improve” it).

This is the same “push computation into the generator vs keep it outside” decision—exactly the kind of operator-placement concern SPEAR raises, including the fact that **operator fusion (combining prompts) can hurt accuracy or latency**. ([VLDB][2])

So: you’re not just noticing a nuance—you’re discovering two distinct semantic operators.

---

## 3) “Systemprompt-of-systemprompt” = higher-order prompting (prompt transformers)

Now the currying / “function of a function” angle.

If you treat prompts as values, then a “persona prompt” is naturally a **prompt transformer**:

[
T_{\text{skater}} : \Sigma^* \to \Sigma^* \quad\text{defined by}\quad T_{\text{skater}}(p)=p_{\text{skater}}\oplus p
]

This is a *function on prompts*. The set of all prompt transformers `End(Σ*) = (Σ*)^(Σ*)` forms a monoid under composition:

* identity: `id(p)=p`
* composition: `(T1 ∘ T2)(p)=T1(T2(p))`

So “systemprompt of systemprompt” becomes literally:

[
(T_{\text{sys1}} \circ T_{\text{sys2}})(p)=p_{\text{sys1}}\oplus p_{\text{sys2}}\oplus p
]

### Crucial subtlety

At the **string level**, that’s just concatenation.
At the **behavioral level**, it is *not* a commutative or even well-behaved “merge”, because instruction conflicts are not resolved by a formal logic; they’re resolved by the LM’s learned heuristics + recency + formatting cues.

So you get:

* algebraic certainty about the artifact (`⊕` is associative),
* but only empirical/heuristic behavior about meanings.

SPEAR’s thesis is precisely “stop treating prompts as opaque strings; treat them as structured objects with operators and metadata,” and it explicitly defines a prompt algebra closed under composition over a system state triple `(P, C, M)` and operators like `GEN` (generate), `RET` (retrieve), `SWITCH` (control flow), plus refinements that can manipulate prompts themselves. ([VLDB][2])

That’s “prompt-of-prompt” made first-class and executable.

---

## 4) There actually *are* formalizations of “prompt algebra / prompt calculus” now

You asked “can you see if there are formalizations; if not, formalize.”

There are several *partial* formalizations—each picks a different layer as “primitive”:

### 4.1 SPEAR: algebra of prompt operators over pipeline state

SPEAR models prompts as structured, versioned “prompt views,” supports parameterization, and compiles pipelines into an **executable prompt algebra** that is explicitly closed under composition on `(P, C, M)`. Core operators include `GEN`, `RET`, and `SWITCH`, and refinements (`REF`) can modify prompt logic at runtime based on signals. ([VLDB][2])

This is extremely close to what you want if your object language is “LLM pipelines” rather than “bare strings.”

### 4.2 LMQL / LMP: prompts + scripting + constraints with runtime semantics

LMQL frames “prompting as programming”: mix natural language prompting with scripting and **constraints over output**, compiled to an inference procedure. ([arXiv][1])

This is a formalization of *execution algebra* (control flow, constraints, efficient decoding) that treats prompt fragments as components inside a program.

### 4.3 Guidance: interleave generation and control; constrain via regex/CFG

Guidance explicitly supports **constraints** (regex / CFG) and interleaving logical control and generation “in a single continuous flow.” ([GitHub][3])

This is effectively an embedded DSL whose primitives are “emit literal tokens” vs “ask the LM to generate tokens satisfying constraints,” i.e. an operational semantics close to token-level.

### 4.4 “Prompt calculus” (type-theoretic): dependent types + probabilistic refinements

A 2025 extended abstract explicitly proposes a **Prompt calculus**: a dependently typed calculus for prompt programs with refinements encoding constraints; it treats interaction with the LLM as an effectful computation and discusses type-preserving optimization. ([arXiv][4])

That is very directly aligned with your “currying / composition / abstraction” framing, because typed λ-calculus is literally the canonical substrate for that.

### 4.5 DSPy: declarative modules compiled into optimized LM pipelines

DSPy defines modules/signatures and a compiler (teleprompters) that optimizes compositions of prompting/augmentation/reasoning across pipelines. 

This is not “prompt algebra” at the string level; it’s a programming model for the execution graph where prompt selection/structure becomes compilable.

### 4.6 “Prompts are Programs” (PL community lens)

SIGPLAN’s PL Perspectives piece explicitly pushes the framing that prompts share properties with programs and should inherit SE/PL tooling concerns. ([SIGPLAN Blog][5])

Not a calculus itself, but a statement of the research agenda you’re independently reconstructing.

### 4.7 Soft-prompt algebra (adjacent but different): linear combinations of learned prompt embeddings

There’s also “prompt algebra” in the *prompt tuning* literature: prompts are vectors, and algebra means linear combination to compose tasks. 
This is not your “prompt strings as programs,” but it’s conceptually rhyming: composition operators over “prompt representations.”

---

## 5) A clean formalization that matches your distinction: Prompt Artifact Algebra vs Prompt Execution Algebra

Here’s a compact formal model that makes your question precise.

### 5.1 Layer A: prompt artifacts (pure, deterministic)

Define a datatype for prompts (don’t commit to “string”; use a structured form like chat messages):

* `Prompt` could be:

  * raw text
  * OR a list of role-tagged messages
  * OR a template AST with holes

Define primitive operators on prompt artifacts:

1. **Concatenation / sequencing of fragments**
   `⊕ : Prompt × Prompt → Prompt`

2. **Parameter substitution (templates / holes)**
   If `Prompt[α]` is a template expecting an `α`, then:
   `apply : Prompt[α] × α → Prompt`

3. **Scoping / quoting / delimiting** (prevents “prompt injection” style reinterpretation)

* `quote : Text → Prompt` (render as data block)
* `delimit : Text → Text` (escape delimiters, etc.)

4. **Prompt transformers**
   `T : Prompt → Prompt`
   These include persona/system wrappers, schema wrappers, safety wrappers.

These operations are deterministic; they compose like normal functions.

**This is where currying lives.**
A persona wrapper is an endomorphism; partial application builds specialized wrappers.

### 5.2 Layer B: executions (effectful, stochastic)

Introduce a primitive effect:

* `infer : Prompt → M Output`

  * `M` is a probabilistic effect (distribution / sampler)
  * `Output` could be text, tokens, or structured values

Now define the canonical execution combinators:

1. **Map**: apply deterministic post-processing
   `map : (a→b) → M a → M b`

2. **Bind / chain**: use output to choose next step
   `bind : M a → (a → M b) → M b`
   This is *exactly* the semantics of multi-call prompt chaining.

3. **Alternative / choice** (mixture of strategies)
   `choose : M a × M a → M a` (with some selection policy)

4. **Validate / retry**
   `retryUntil : (a→Bool) → (Prompt→M a) → Prompt → M a`
   (the generate–validate–repair pattern)

This “execution algebra” is where DSPy/LMQL/Guidance largely live: building structured programs around `infer`. ([arXiv][1])

### 5.3 The bridge: semantics of prompt transformers vs postprocessors

Now we can precisely express your “skater dude” example with two different compositions:

#### (A) Preconditioning (prompt transformer, single call)

Let `Skater : Prompt → Prompt`.

[
\text{oneShot} = infer(Skater(p_{\text{songTask}}))
]

#### (B) Postconditioning (execution chain, two calls)

Let `RewriteSkater : Text → Prompt` produce “rewrite this in skater voice” prompts.

[
\text{twoShot} = infer(p_{\text{songTask}}) ;\texttt{bind}; (\lambda x.; infer(RewriteSkater(x)))
]

Those are different programs in the execution algebra; no reason they should match.

---

## 6) Where “prompt algebra” gets interesting: prompts as constraints (not just context)

Your uppercase/profanity example is a perfect doorway into a more formal treatment.

There are two distinct ways to interpret a “prompt”:

### View 1: prompt as *conditioning context*

Prompt adds evidence; you get `K(output | prompt)`.
Composition is “more context” (concatenate).

### View 2: prompt as *constraint specification*

Prompt specifies a set of admissible outputs; decoding enforces it.

* Uppercase constraint = a regular-language constraint (all tokens match `[A-Z ]*` in some encoding)
* Profanity exclusion = another regular-language constraint (forbid a set of substrings)
* Intersection of regular languages is regular ⇒ constraints compose by intersection.

This is exactly why **constraint-aware runtimes** (Guidance, LMQL) matter: they move from “hoping the model follows instructions” to “enforcing a language of outputs.” ([GitHub][3])

In that constraint view, you can define an honest-to-God algebra:

* constraint conjunction: `C1 ∧ C2` (intersection)
* constraint disjunction: `C1 ∨ C2` (union)
* sequencing constraints: `C1 ; C2` (concatenate languages)
* refinement types encode constraints: exactly what the Prompt calculus proposal argues—constraints as refinements, some syntactic, some probabilistic/semantic. ([arXiv][4])

This is the cleanest route to something that deserves the name “algebra,” because you can prove closure properties (e.g., closure of regular languages under intersection) and then implement them via automata/CFG constrained decoding.

---

## 7) Currying, formally: “infer” is multi-argument, and personas are partial applications

In chat APIs, “infer” is not really `Prompt -> Output`; it’s closer to:

[
infer : System \times History \times User \times Params \to M(Assistant)
]

Currying gives you:

* Fix system persona:

[
infer_{\text{skater}} = \lambda (history,user,params).; infer(system=\text{skater}, history, user, params)
]

That is literally: “a system prompt is a partially applied inference function.”

Now “systemprompt-of-systemprompt” becomes function composition on these wrappers:

* `withSystem(s) : Infer → Infer`
* `withSystem(s1) ∘ withSystem(s2)` yields a composite wrapper (though behavioral resolution is heuristic).

This is the exact same move as `Reader`-style effect stacking in FP: the system prompt behaves like a *read-only environment* threaded into the inference effect.

---

## 8) Laws you might *want* from a “prompt algebra” (and which ones actually hold)

This is the acid test: what algebraic equalities do we expect?

### 8.1 Laws that hold at artifact level

* Associativity of concatenation (strings/messages)
* Associativity of transformer composition
* Identity elements (`ε`, `id`)

### 8.2 Laws that hold at execution level (idealized probability monad)

For distributions, the monad laws hold:

* left identity: `return a >>= f = f a`
* right identity: `m >>= return = m`
* associativity: `(m >>= f) >>= g = m >>= (λx. f x >>= g)`

### 8.3 Laws that emphatically do *not* hold semantically for real LMs

Even if artifact concatenation is associative, you typically **cannot** treat:

[
K(p_1 \oplus p_2) \stackrel{?}{=} \text{some compositional combination of } K(p_1),K(p_2)
]

because:

* LMs are not logical reasoners; they’re learned conditional models with strong sensitivity to ordering/formatting. ([arXiv][1])
* Context windows truncate; “older” parts of the prompt may effectively vanish.
* Instruction conflicts aren’t resolved by a formal entailment system.

SPEAR even notes an optimization analogue: **operator fusion** (combining prompts/operators) can degrade accuracy or increase latency, unlike classical DB operator fusion which is usually beneficial. ([VLDB][2])

So the right stance is:

* **algebra on artifacts**: crisp, deterministic
* **algebra on executions**: crisp, as a probabilistic program
* **link between them**: empirical + partially formal (e.g., constraints)

---

## 9) Practical “prompt combinators” taxonomy: prompt-level vs execution-level

Here’s a high-signal split, using PL vocabulary.

### 9.1 Prompt-level combinators (no LM call)

These are endomorphisms / constructors in the prompt AST:

* `prepend(fragment)` / `append(fragment)` / `surround(prefix,suffix)`
* `withPersona(personaSpec)`
* `withSchema(schemaSpec)`
* `withExamples(demos)`
* `withDelimiters(escapePolicy)`
* `substitute(slot, value)` (templates)
* `reorder(components)` (SPEAR explicitly finds component reordering impactful) ([VLDB][2])
* `version(prompt)` / `annotate(metadata)` (SPEAR prompt objects carry metadata/version history) ([VLDB][2])

### 9.2 Execution-level combinators (require LM call)

These are your “program algebra” operators:

* `chain` / `bind` (multi-call)
* `branchOn(outputClassifier)`
* `retryWith(refiner)` (generate–validate–repair)
* `selfConsistency(n, vote)`
* `retrieveAugment(queryFn)` (RET + GEN style; SPEAR has RET/GEN operators) ([VLDB][2])
* `toolUse(toolRouter)`
* `cache(prefix)` (SPEAR discusses prefix caching & reuse / KV cache) ([VLDB][2])
* `fuse(operators)` with cost/quality tradeoffs ([VLDB][2])

These are where frameworks like LMQL, Guidance, DSPy live. ([arXiv][1])

---

## 10) If you wanted to “name it” as a calculus

If I were to propose a minimal formal object that matches what you’re after, I’d define:

### **λᴾ**: a lambda calculus of prompts with an inference effect

**Types**

* `Prompt`
* `Text`
* `A -> B`
* `M A` (stochastic/inference effect)
* refinement types `A{φ}` for constraints (as in the Prompt calculus direction) ([arXiv][4])

**Terms**

* prompt literals, message constructors
* `p1 ⊕ p2`
* `λx. t`, `t u` (currying lives here)
* `infer(tPrompt) : M Text`
* `bind : M A -> (A -> M B) -> M B`
* `validate : Spec -> Text -> Bool` (or `M Bool` if probabilistic)
* optional: constraint constructors that compile to constrained decoding (Guidance/LMQL style) ([GitHub][3])

**Semantics**

* `infer` is the only primitive that touches the LM kernel `K`
* everything else is pure syntax / control

Then:

* “prompt algebra” is the equational theory of the **Prompt** fragment (monoid + templates + transformers)
* “execution algebra” is the equational theory of the **M** fragment (monad + validators + routers)
* “prompt-of-prompt” is higher-order programming in λᴾ (prompts as first-class, transformers as functions)

This matches what SPEAR/LMQL/Guidance/DSPy are each independently approximating at different layers. ([VLDB][2])

---

## 11) Where your earlier SICP analogy plugs in

In your SICP framing:

* `infer` (LM call) is the analogue of a **primitive procedure**—the base case.
* prompt transformers / templates are like building **compound procedures**—they don’t “run” until applied to arguments and ultimately passed to `infer`.

And the key meta-insight:

* composing prompt artifacts is like composing **expressions**,
* composing executions is like composing **evaluations**,
* and you need both algebras because “code vs run” is exactly the `quote/eval` duality you were already orbiting.

---

## 12) If you want a concrete next step: define operators + laws you care about, then test which are “approximate equalities”

A very “PL + SE” way to proceed (and very aligned with the “prompts are programs” agenda ([SIGPLAN Blog][5])) is:

1. **Specify** a small set of prompt combinators (`withPersona`, `withSchema`, `withExamples`, `withSafetyGuard`, `quoteUserData`, …)

2. Define two implementations for each:

   * *artifact-level* (pure prompt rewrite)
   * *execution-level* (multi-call wrapper, validate/repair)

3. For each pair, characterize:

   * do we have an algebraic law? (e.g., idempotence, associativity, distributivity over choice)
   * if not, do we have an *approximate equivalence* under constraints? (e.g., rewrite-only should preserve semantics under a test oracle)

4. Build a test harness / oracle (unit tests, metamorphic tests, approval tests). This connects to emerging SE work around prompts as artifacts; e.g., people are building testing and reliability tooling (typed outputs, repair loops, etc.)—TypeChat/BAML-style ecosystems are exactly this direction. ([Microsoft GitHub][6])

---

If you want, I can take your exact “skater dude / write a song / jump in a lake” trio and:

1. define a tiny prompt-DSL with **prompt transformers** vs **execution transformers**,
2. write out the semantics as equations (kernel composition), and
3. propose a small set of algebraic laws worth adopting (idempotence of persona wrappers, fusion vs chaining equivalences under validators, etc.), including where they fail and why.

That would make the “prompt algebra” feel as crisp as `cons/car/cdr` vs `eval/apply`, but for inference-driven systems.

[1]: https://arxiv.org/pdf/2212.06094 "Prompting Is Programming: A Query Language for Large Language Models"
[2]: https://vldb.org/cidrdb/papers/2026/p26-cetintemel.pdf "Making Prompts First-Class Citizens for Adaptive LLM Pipelines"
[3]: https://github.com/guidance-ai/guidance?utm_source=chatgpt.com "A guidance language for controlling large language models."
[4]: https://arxiv.org/html/2508.12475v1 "Type-Driven Prompt Programming: From Typed Interfaces to a Calculus of Constraints"
[5]: https://blog.sigplan.org/2024/10/22/prompts-are-programs/?utm_source=chatgpt.com "Prompts are Programs"
[6]: https://microsoft.github.io/TypeChat/blog/introducing-typechat/?utm_source=chatgpt.com "Introducing TypeChat"
--------

# FrameLisp: A Lisp‑Embedded Language and Runtime for Inference‑Driven Programs

**Version:** 0.9 (proposal)
**Status:** Buildable specification (kernel + standard library + runtime contracts)
**Target host:** Any serious Lisp with macros, conditions, and concurrency (Common Lisp, Scheme/Racket, Clojure). The concrete examples below use **Common Lisp conventions** (`&key`, conditions, CLOS). The design is *portable by construction* because the core is a set of algebraic interfaces + macros.

---

## 0. Executive summary

This proposal specifies **FrameLisp**, a *Lisp-embedded* language for building programs whose computational substrate includes **LLM inference**, **tool invocation**, and **retrieval**, while remaining:

* **Compositional** (closed under combination)
* **Abstraction-friendly** (higher‑order prompt and workflow definitions)
* **Stream-native** (token/event streams are first-class)
* **Parallel-native** (structured concurrency + bounded parallelism)
* **Inspectable/optimizable** (plans are data; macros generate IR)
* **Testable** (record/replay, deterministic harnesses, oracles)
* **Operationally serious** (caching, tracing, budgets, idempotence, retries)

The key design move is a disciplined separation between:

1. **Prompt Artifact Algebra**: pure operations on *prompt values* (structured messages, templates, transformers).
2. **Execution Algebra**: effectful operations (infer/retrieve/tool) with combinators for sequencing, branching, looping, parallelism, and streaming.
3. **Handlers/Runtime**: interpreters for the effects (real provider, mock, recorder, simulator), analogous to “primitive procedures” in SICP.

In short: we keep the *kernel* minimal and orthogonal, and we exploit Lisp’s strengths—**macros, homoiconicity, closures, generic functions, conditions, streams, and concurrency**—to take “prompt/program algebra” to the limit without building a second language you have to babysit.

---

## 1. Normative terms, scope, and non-goals

### 1.1 Normative terms

* **MUST / MUST NOT**: required for conformance.
* **SHOULD / SHOULD NOT**: recommended; deviations require justification.
* **MAY**: optional.

### 1.2 Scope

FrameLisp defines:

* A **data model**: prompts, messages, templates, tool specs, artifacts, events.
* A **kernel effect algebra**: `infer`, `embed`, `retrieve`, `call-tool`, `emit`, `observe`, `validate`, `commit`.
* A **combinator library**: sequential composition, parallel composition, streaming transducers, loops, routers, splitters/aggregators (EIP), retries, timeouts, budgets.
* A **runtime protocol**: pluggable backends, caching, tracing, concurrency control.
* A **macro layer**: prompt DSL, workflow DSL, graph DSL, typed/schematic output DSL.

### 1.3 Non-goals (explicitly)

* FrameLisp is **not** a new general-purpose Lisp. It is an embedded DSL and runtime.
* It does **not** require a dependent type system; it supports *contracts/schemas* and optional gradual typing.
* It does **not** assume any single LLM vendor API; inference is an effect with multiple handlers.

---

## 2. Conceptual model

### 2.1 Two algebras + one runtime boundary

**Prompt Artifact Algebra** answers: “What can I do to prompts *without running the model*?”

* Concatenate message lists
* Compose persona/system wrappers
* Partially apply templates (“currying” prompts)
* Quote/delimit user data (data vs instruction)
* Normalize/optimize prompt structure (dedupe, reorder, canonicalize)

**Execution Algebra** answers: “What programs can I build out of inference and other effects?”

* Chain calls (Kleisli composition / monadic bind)
* Validate/repair loops
* Content-based routing
* Tool invocation interleaved with inference
* Streaming token handling
* Parallel fan-out/fan-in
* Bounded concurrency, cancellation, timeouts

**Runtime boundary** answers: “Where do we bottom out?”

* `infer` is a primitive effect implemented by a backend handler.
* Everything else is definable as compound compositions/macros.

This is the SICP move, generalized: `eval/apply` become “interpret framed artifact / realize effectful step,” and `infer` (plus tool/retrieval) are the “primitives.”

### 2.2 Why prompt concatenation ≠ chaining (your currying intuition)

FrameLisp makes this a *type-level* distinction:

* **Prompt composition**: `prompt+ : Prompt × Prompt → Prompt` (pure)
* **Execution chaining**: `bind : Flow[A] × (A → Flow[B]) → Flow[B]` (effectful)

A persona prompt `skater` is a **prompt transformer**:

* `skater : Prompt → Prompt`

Preconditioning:

* `infer(skater(taskPrompt))` (one call)

Postconditioning:

* `bind(infer(taskPrompt), λdraft. infer(skater(rewrite(draft))))` (two calls)

FrameLisp exposes both explicitly and gives you combinators to intentionally pick the semantics you want.

---

## 3. Core data model (normative)

### 3.1 Message

A **Message** MUST be a persistent object with fields:

* `role` ∈ `(:system :developer :user :assistant :tool :context)`
  *(Role sets can be extended, but these are baseline.)*
* `content` : one of

  * `string`
  * `bytes`
  * structured payload (e.g., plist/alist/JSON object) for tool results
* Optional metadata:

  * `name` (speaker/tool name)
  * `tool-call-id`
  * `mime-type`
  * `timestamp`
  * `tags` (set of keywords)
  * `provenance` (see 3.6)

### 3.2 Prompt

A **Prompt** MUST be an object containing:

* `messages : (vector message)` or `(list message)` (order-preserving)
* `vars : map` (template bindings, optional)
* `policy : policy-spec` (tool permissions, safety constraints, budgets)
* `meta : map` (arbitrary metadata)

A Prompt MUST be treated as **immutable**; “modifications” return a new prompt.

### 3.3 PromptTemplate

A **PromptTemplate** is a callable value:

* `render : Bindings → Prompt`

Templates MUST support **partial application**:

* `partial : PromptTemplate × Bindings → PromptTemplate`

Templates SHOULD be definable via macros (see §6).

### 3.4 Artifact

An **Artifact[A]** is a value with provenance:

* `value : A`
* `provenance : provenance`
* `diagnostics : list` (optional)
* `metrics : map` (token counts, latency, cache hit, cost)

Artifacts allow you to preserve “what did we do?” without stringly-typed logging.

### 3.5 ToolSpec

A **ToolSpec** MUST describe:

* `name : string`
* `doc : string`
* `input-schema : schema`
* `output-schema : schema`
* `fn : function` (host Lisp callable)
* `purity : (:pure | :idempotent | :effectful)`
* `timeout-ms` (optional)
* `retry-policy` (optional)
* `authz-policy` (optional)

### 3.6 Provenance

Provenance MUST support:

* Source identifiers (prompt id, message ids)
* Tool call lineage
* Retrieval doc ids and spans
* Model id/config used
* Hashes of inputs/outputs for caching and replay

---

## 4. Kernel execution model

FrameLisp introduces a single abstraction for effectful computations:

### 4.1 Flow

A **Flow[A]** is a first-class program that, when executed by a runtime, yields:

* either a final `Artifact[A]`
* and/or a stream of `Event` (for streaming inference / progress)

Concretely:

* `run : Runtime × Flow[A] → Artifact[A]`
* `run/stream : Runtime × Flow[A] → (values EventStream ArtifactPromise)`

A Flow is *not* required to be lazy, but it MUST be representable as data (an IR) for inspection, tracing, optimization, and testing.

### 4.2 EventStream

An **EventStream** is a bounded or unbounded stream of events supporting:

* backpressure (consumer can slow producer)
* cancellation
* fanout (optional)

Events include:

* `(:token "...")`
* `(:delta "...")` (text chunk)
* `(:tool-call spec args)`
* `(:tool-result spec result)`
* `(:retrieval docs)`
* `(:trace span)`
* `(:error condition)`
* `(:done artifact)`

---

## 5. Kernel primitives (the “LLM Lisp instruction set”)

These are the **only** operations that require a runtime handler. Everything else is definable in Lisp using these.

I’m giving both **signatures** and **semantic contracts**.

### 5.1 `infer`

**Signature (conceptual):**

```lisp
(infer prompt &key model decoding tools response-format policy) => Flow[Completion]
```

**Contract:**

* MUST submit `prompt` (a Prompt) to the active model backend with `decoding`.
* MUST respect tool permissions in `policy` and the provided `tools` set.
* MUST return a `Completion` artifact containing at least:

  * `text` (string) OR `messages` (assistant message(s)) OR both
  * optional `tool-calls` extracted from model output
* SHOULD include metrics: token usage, latency, cache metadata.

**Streaming variant:**

```lisp
(infer/stream prompt &key ...) => Flow[Completion]
```

* MUST emit token/delta events on the Flow’s EventStream.

### 5.2 `embed`

```lisp
(embed items &key model) => Flow[VectorEmbeddings]
```

* `items` may be strings or documents.
* MUST return embeddings with provenance.

### 5.3 `retrieve`

```lisp
(retrieve query &key index top-k filter rerank) => Flow[DocSet]
```

* MUST support at least keyword search or vector search via `embed`.
* SHOULD return document spans and provenance.

### 5.4 `call-tool`

```lisp
(call-tool tool-name args &key timeout) => Flow[ToolResult]
```

* MUST validate `args` against the tool’s input schema before invocation.
* MUST return result validated against output schema.
* MUST honor tool purity/idempotence for retries/caching.

### 5.5 `emit` / `observe`

These are the I/O bridges for streaming systems.

```lisp
(emit sink item &key format) => Flow[Ok]
(observe source &key format) => Flow[Item]
```

* SHOULD be usable to integrate with Lisp streams, sockets, queues.

### 5.6 `validate`

```lisp
(validate spec value &key explain) => Flow[Validation]
```

* MUST return `(:ok t)` or `(:ok nil :diagnostics ...)`.
* `spec` MAY be schema, regex, predicate function, or contract object.

### 5.7 `commit`

```lisp
(commit store key value &key ttl) => Flow[Ok]
```

* Persistence boundary for stateful agents, caches, long-running workflows.

---

## 6. Prompt Artifact Algebra (pure operators)

These operators manipulate Prompts/Templates *without inference*. They are the “cons/car/cdr” layer for framed artifacts.

### 6.1 Prompt constructors

```lisp
(system "text" &key name tags meta) => Message
(user "text" &key ...) => Message
(assistant "text" &key ...) => Message
(tool name payload &key tool-call-id ...) => Message

(prompt &rest messages-or-prompts) => Prompt
```

Rules:

* `(prompt ...)` MUST flatten nested prompts into a single message sequence.

### 6.2 Concatenation

```lisp
(prompt+ p1 p2 &rest ps) => Prompt
```

Law:

* associative, identity is empty prompt
* not commutative

### 6.3 Transformers

A **PromptTransformer** is `Prompt → Prompt`.

Core constructors:

```lisp
(with-system text) => PromptTransformer
(with-policy policy-spec) => PromptTransformer
(with-tools toolset) => PromptTransformer
(with-examples examples) => PromptTransformer
(with-context docs) => PromptTransformer
(with-guard guard-spec) => PromptTransformer
```

Composition:

```lisp
(compose-transformers t1 t2 ...) => PromptTransformer
```

### 6.4 Templates and partial application (currying)

Templates are just functions, but we standardize a representation:

```lisp
(defprompt name (vars...) template-body...)  ;; macro
(render name &key bindings...) => Prompt
(partial name &key bindings...) => PromptTemplate
```

**Template body** uses Lisp quasiquote/unquote to embed computations safely.

Example:

```lisp
(defprompt write-song (topic)
  (prompt
    (system "You are a professional songwriter.")
    (user (format nil "Write a song about ~a." topic))))
```

Partial application:

```lisp
(defparameter *song-about-winter*
  (partial #'write-song :topic "winter"))
(render *song-about-winter*)  ;; -> Prompt
```

### 6.5 Quoting / delimiting (data vs instruction)

A core security primitive:

```lisp
(as-data x &key fence mime) => string
```

* MUST escape or fence content so that it is treated as **data payload**, not instruction.
* SHOULD support multiple fence strategies (triple backticks, length-prefix, JSON string escaping, etc.)
* SHOULD be used by default when embedding user-controlled text into system/developer instructions.

---

## 7. Execution Algebra: means of combination

This is where FrameLisp “becomes a language.”

### 7.1 Sequential composition (Pipeline)

A Flow combinator set:

```lisp
(pure x) => Flow[X]
(mapf f flow) => Flow[Y]
(bind flow f) => Flow[Y]          ;; f: X -> Flow[Y]
(tap flow side-effect) => Flow[X] ;; for tracing/logging
```

Macro sugar (Lisp-native “do” notation):

```lisp
(flow
  (let* ((x (<! (infer ...)))
         (y (<! (retrieve ...))))
    (pure (combine x y))))
```

Where `<!` is a macro that expands into `bind` with lexical binding (like monadic `let*`).

### 7.2 Branching

```lisp
(branch flow
  (:when pred1 then-flow1)
  (:when pred2 then-flow2)
  (:else else-flow)) => Flow[A]
```

* `pred` MAY inspect `Artifact` and diagnostics.
* MUST be deterministic at the host level (the nondeterminism is inside `infer`).

### 7.3 Loops / fixpoints

Canonical “validate/repair” loop:

```lisp
(retry-until flow
  :validate (lambda (artifact) ...)
  :repair   (lambda (artifact) (infer ...diagnostics...))
  :max-tries n
  :backoff backoff-spec) => Flow[A]
```

This is the first-class encoding of the **Generate–Validate–Repair** pattern (testing oracle + refactoring loop).

### 7.4 Parallelism (structured concurrency)

FrameLisp MUST provide structured parallel combinators that obey **bounded concurrency**, **cancellation**, and **failure aggregation**.

```lisp
(all &rest flows) => Flow[(values ...)]   ;; wait all
(race &rest flows) => Flow[A]             ;; first successful
(par-map f items &key max-par) => Flow[list]
(par-flatmap f items &key max-par) => Flow[list]
```

Failure semantics:

* `all` MUST collect errors (as conditions) and may fail fast optionally.
* `race` MUST cancel losers unless configured otherwise.

### 7.5 Timeouts, budgets, cancellation

```lisp
(with-timeout ms flow) => Flow[A]
(with-budget budget-spec flow) => Flow[A]
(cancel flow) => Flow[Ok]
```

Budget spec MAY include:

* max tokens
* max wall time
* max cost
* max tool invocations
* max retrieval docs

### 7.6 Streaming combinators (token/event pipelines)

A streaming inference is a Flow with an EventStream. We standardize stream ops:

```lisp
(stream-map f event-stream) => event-stream
(stream-filter pred es) => es
(stream-take n es) => es
(stream-merge &rest es) => es
(stream-zip es1 es2) => es
(stream-reduce f init es) => Flow[result]
```

And a bridge to Lisp streams:

```lisp
(write-events-to-stream es out-stream &key format) => Flow[Ok]
```

Backpressure:

* EventStream SHOULD be implemented as a bounded channel/queue where producers block or drop based on policy.

---

## 8. Standard library: “protocol EDSLs” (chat, completion, tools, RAG, REPL, logs)

The kernel primitives are intentionally small; the “magical subtypes” live here.

### 8.1 Chat protocol as a library, not a built-in

Define a **Conversation** object:

* `history : list<Message>`
* `memory : map`
* `policy`

Operations:

```lisp
(chat-turn conversation user-message &key persona tools) => Flow[(values reply updated-conversation)]
```

Implementation is definable from primitives:

* build prompt from system+history+new user msg
* `infer/stream` for streaming UI
* update history
* detect tool calls and invoke tool loop (below)

### 8.2 Tool loop protocol (ReAct / tool calling without magic)

Define:

```lisp
(tool-loop initial-prompt &key tools max-steps) => Flow[Completion]
```

Semantics:

1. infer
2. if tool-call(s) present:

   * validate tool args
   * call tool(s) (parallel if safe)
   * append tool result messages
   * repeat
3. else return completion

This is a standard **Interpreter + Command** architecture:

* model outputs “commands” (tool calls)
* runtime executes and feeds back “results”
* the loop is explicit, testable, traceable.

### 8.3 Retrieval-Augmented Generation (RAG) as combinators

```lisp
(rag query
  :retrieve (lambda (q) (retrieve q :top-k 8))
  :compose-prompt (lambda (docs q) (prompt ...))
  :answer (lambda (p) (infer p))) => Flow[Answer]
```

Support standard EIP shapes:

* **Splitter** for chunking
* **Aggregator** for combining summaries
* **Content Enricher** for injecting docs
* **Content-Based Router** for choosing strategy based on query

### 8.4 Completion protocol (single-shot)

```lisp
(complete prefix &key system decoding) => Flow[string]
```

This is just:

* `(infer (prompt (system ...) (user prefix)) ...)` with a standard extraction.

### 8.5 REPL protocol

Define a host-side REPL that streams tokens and interleaves tool calls:

* `repl-source` → `observe`
* `emit` to output stream
* `chat-turn` loop

This leverages Lisp’s own REPL idioms and stream model.

### 8.6 Log/data stream protocols

A stream processor that:

* observes events (logs, market ticks, telemetry)
* uses bounded concurrency to classify/aggregate with `infer`
* emits alerts/summaries

This is where Lisp’s streaming and concurrency strengths pay off: you write *real* stream programs, and inference is “just another effect.”

---

## 9. Abstraction mechanisms: how you “take it to the limit”

Lisp gives you:

* `lambda`, `labels`, closures (lexical scoping)
* macros (syntactic abstraction)
* generic functions (open extension)
* condition system (restarts, recovery)
* packages (namespace architecture)

FrameLisp leverages all of them.

### 9.1 `defprompt` (prompt templates)

A prompt template is a function returning a Prompt. This is the base abstraction.

### 9.2 `deftransform` (prompt transformers as higher-order functions)

```lisp
(deftransform skater ()
  (lambda (p)
    (prompt+ (prompt (system "You are a skater dude.")) p)))
```

Now you can do:

* `(compose-transformers skater polite concise)`
  This is “systemprompt-of-systemprompt” made explicit.

### 9.3 `defop` (named Flow operators)

```lisp
(defop summarize-doc (doc)
  (infer (prompt
           (system "Summarize precisely.")
           (user (as-data doc)))))
```

Operators are just functions returning Flow, but `defop` attaches:

* schemas/contracts
* tracing tags
* caching policy
* resource budget defaults

### 9.4 Macro-based workflow DSL

A professional-grade library MUST include a macro that compiles a readable workflow syntax into the Flow IR:

```lisp
(defmacro workflow (&body forms) ...)
```

Supporting:

* `let/let*` bindings of `<!`
* `if/case` routing
* `loop` with `retry-until`
* `par` blocks
* `with-*` decorators (policy, cache, trace)

This gives you a *language* without leaving Lisp.

### 9.5 Graph DSL (optional, but specified)

For long-running agents with explicit state machines:

```lisp
(defgraph my-agent (state)
  (node :classify (lambda (st) ...))
  (node :retrieve (lambda (st) ...))
  (node :answer   (lambda (st) ...))
  (edge :classify :retrieve :when #'needs-context?)
  (edge :classify :answer   :when #'can-answer?)
  (edge :retrieve :answer)
  (edge :answer :classify :when #'needs-iteration?))
```

Graph compilation:

* Graph MUST compile into a Flow that executes nodes with explicit state transitions.
* MUST support checkpointing via `commit`.
* MUST support streaming events per node.

This is the “LangGraph layer,” but kept as a library/EDSL.

---

## 10. Runtime specification (handlers, caching, tracing, parallel execution)

### 10.1 Runtime object

A Runtime MUST provide:

* `model-backend` (implements infer/embed)
* `retriever-backend`
* `tool-registry`
* `cache` (multi-tier)
* `scheduler` (thread pool / async executor)
* `tracer` (spans/events)
* `policy-engine` (guardrails, permissions)
* `clock` (for timeouts, backoff)

### 10.2 Effect handlers

Each kernel primitive is implemented by a handler:

* `handle-infer`
* `handle-retrieve`
* `handle-call-tool`
* etc.

This is the **Microkernel + Plug-in** architecture:

* Kernel defines interfaces
* Backends plug in

### 10.3 Caching

FrameLisp MUST specify:

* `cache-key` derivation based on:

  * normalized prompt representation
  * model id + decoding params
  * toolset/policy
* `cache tiers`:

  * in-memory LRU
  * persistent store
* `cache semantics`:

  * MUST be safe-by-default: only cache when deterministic enough or explicitly allowed
  * MUST record provenance and TTL

### 10.4 Tracing/observability

Every Flow run MUST emit:

* start/end span with unique id
* per-step metrics
* tool invocations with arguments redacted by policy
* retrieval doc ids
* model usage and decode params

Lisp’s condition system should be used to surface structured failures with restarts like:

* `retry`
* `fallback-model`
* `skip-tool`
* `return-default`

That’s not “nice to have.” It’s how you prevent production mud.

### 10.5 Parallel scheduler

The scheduler MUST support:

* bounded parallelism
* cancellation propagation
* structured resource scoping (`with-budget`, `with-timeout`)
* streaming fanout

Implementation options:

* native threads + queues
* green threads/fibers
* core.async-style channels (Clojure)
* Racket places/futures

The interface is abstract; the implementation is host-specific.

---

## 11. Safety and policy (first-class, not bolted on)

FrameLisp MUST include a policy object that controls:

* tool allowlist/denylist
* max tool calls per run
* whether user content MUST be fenced when included in system instructions
* data redaction rules for tracing/caching
* whether model outputs can trigger execution (tool calls) automatically or require confirmation gates

A safe default:

* all user-controlled content is `as-data` fenced when embedded into instruction contexts
* tool calls require schema validation + authz policy
* caching is off unless explicitly enabled

---

## 12. Concrete “full language” surface: packages, macros, and canonical operators

Below is the **canonical API surface** you’d ship as a serious library.

### 12.1 Packages (namespaces)

* `framelisp.core`
  Flow, runtime, handlers, run, run/stream, conditions.
* `framelisp.prompt`
  Message, Prompt, templates, transformers, quoting.
* `framelisp.ops`
  infer/embed/retrieve/call-tool/validate/commit + combinators.
* `framelisp.stream`
  EventStream, transducers, bridges to Lisp streams.
* `framelisp.graph`
  Graph DSL + compiler to Flow.
* `framelisp.test`
  recorder/replayer, oracles, golden master harness.

### 12.2 Canonical macros

* `defprompt`, `deftransform`, `defop`, `deftool`, `defschema`, `defgraph`
* `flow` (monadic do)
* `<!` (bind extract)
* `with-runtime`, `with-model`, `with-policy`, `with-cache`, `with-trace`, `with-timeout`, `with-budget`
* `par`, `all`, `race`, `par-map`
* `retry-until`, `repairing`, `guarded`

### 12.3 Canonical conditions and restarts

Conditions:

* `inference-error`, `tool-error`, `retrieval-error`, `validation-error`, `timeout-error`, `budget-exhausted`

Restarts:

* `retry-step`, `fallback`, `skip`, `use-value`, `abort-run`, `reduce-scope`

This is “Common Lisp done right”: operational resilience is language-native.

---

## 13. Worked examples (showing the “limit”)

### 13.1 Persona preconditioning vs postconditioning (your exact concern)

```lisp
(deftransform skater ()
  (lambda (p)
    (prompt+ (prompt (system "You are a skater dude. Use skate slang.")) p)))

(defprompt write-song (topic)
  (prompt
    (system "You are a meticulous songwriter. Include verse/chorus/bridge.")
    (user (format nil "Write a song about ~a." topic))))

;; (A) Precondition: one inference
(defop preconditioned-song (topic)
  (infer ((skater) (write-song topic))))

;; (B) Postcondition: generate then rewrite (two calls)
(defop postconditioned-song (topic)
  (flow
    (let* ((draft (<! (infer (write-song topic))))
           (rewrite-prompt
             (prompt
               (system "Rewrite the user's text in skater persona.")
               (user (as-data (completion-text (artifact-value draft)))))))
      (infer ((skater) rewrite-prompt)))))
```

FrameLisp makes them different programs by construction.

### 13.2 Streaming: print tokens as they arrive, while collecting final artifact

```lisp
(defop stream-answer (q)
  (infer/stream
    (prompt
      (system "Answer precisely.")
      (user q))))

(let* ((rt (make-runtime ...))
       (flow (stream-answer "Explain lexical scoping.")))
  (multiple-value-bind (events done) (run/stream rt flow)
    (run rt (write-events-to-stream events *standard-output*))
    (artifact-value (await done))))
```

This is “LLM call as a stream,” integrated into Lisp’s I/O model.

### 13.3 Parallel fanout / fanin: map-reduce summarization

```lisp
(defop summarize-docs (docs)
  (flow
    (let* ((partials
             (<! (par-map
                  (lambda (d)
                    (infer (prompt (system "Summarize concisely.")
                                  (user (as-data d)))))
                  docs
                  :max-par 8))))
      ;; aggregate
      (infer (prompt
              (system "Combine these into a single coherent summary.")
              (user (as-data (mapcar #'completion-text (mapcar #'artifact-value partials)))))))))
```

This is enterprise-grade *Splitter + Aggregator* with bounded concurrency.

### 13.4 Tool loop protocol (explicit, testable)

```lisp
(deftool weather
  :input-schema '(:object (:properties (:location :string)) (:required :location))
  :output-schema '(:object (:properties (:forecast :string)) (:required :forecast))
  :purity :idempotent
  (lambda (args) ...))

(defop answer-with-tools (q)
  (tool-loop
    (prompt
      (system "Use tools when needed. Otherwise answer directly.")
      (user q))
    :tools '(weather)
    :max-steps 6))
```

No magical agent spaghetti: a single explicit protocol combinator.

---

## 14. Why this belongs “on top of Lisp” specifically

FrameLisp is deliberately designed to exploit Lisp’s strengths:

1. **Homoiconicity**: prompts and workflows are S-expressions; macros can compile them into IR with zero ceremony.
2. **Macros**: prompt DSL and workflow DSL are compiled, not interpreted ad hoc.
3. **Closures**: prompt transformers and templates are plain higher-order functions. Currying is literal partial application.
4. **Conditions + Restarts**: robust production error handling without “exceptions all the way down.”
5. **Generic Functions**: open extension points (new prompt renderers, new validators, new tool backends).
6. **Streams**: token/event streams integrate with existing stream APIs (files, sockets, pipes).
7. **Concurrency**: structured parallelism and backpressure can be implemented using the host’s best primitives, while the API remains stable.

This is why you *don’t* want a Pythonic stringly-typed chain library if your goal is “take it to the limit.” Lisp lets the EDSL become a real language without leaving the language.

---

## 15. Implementation plan (still fully specified, not hand-wavy)

### Phase 1: Kernel + prompt model (2–3 weeks of real engineering)

* Implement Prompt/Message/Template objects
* Implement runtime protocol (handlers)
* Implement `infer`, `infer/stream`, `validate`, `call-tool`
* Implement Flow IR + `run` interpreter
* Implement basic tracing + recorder

### Phase 2: Combinators + streaming + parallel (2–4 weeks)

* `bind/map/flow/<!`
* `all/race/par-map`
* EventStream channel + transducers
* timeouts, cancellation, budgets

### Phase 3: Protocol libraries (4–8 weeks)

* chat protocol
* tool-loop protocol
* rag protocol
* graph DSL compiler
* caching policies + prefix caching hooks

### Phase 4: Test + optimization toolchain (ongoing)

* record/replay harness
* golden master test utilities
* prompt normalization passes
* plan visualization (S-expr pretty printer; DOT export)
* cost/latency profiling

---

## 16. The “full proposal” in one sentence

Build a **Lisp-native microkernel** whose only primitives are *inference and external effects*, plus a rigorously separated **prompt artifact algebra** and **execution algebra**, and then express all “magical subtypes” (chat, completion, tools, RAG, workflows, streaming agents) as **libraries and macros**—so we get maximal compositionality, maximal abstraction power, real concurrency/streams, and production-grade operational semantics.

---

If you want the next step to be even more “spec-like,” I can write:

1. a **formal grammar** of the workflow macro surface (as an s-expression BNF),
2. a **small-step operational semantics** for Flow evaluation (including streaming + cancellation), and
3. a **reference implementation skeleton** (packages, structs, generic functions, conditions) that you can drop into a Common Lisp system and start iterating on immediately.

But the core proposal above is already the complete language: **the primitives**, **the combination operators**, **the abstraction mechanisms**, and the **runtime contracts** that make it real rather than folklore.

