## Ω Language Specification v1.0

### Part 19 — Modules as First-Class Compilation Units (Phase 0/1 Tower, `provide/require/require-for-syntax/begin-for-syntax`) + Hermetic Artifacts + The Inference Plane Proper (Infer Requests, Meaning Values, Receipts, Obligations, Replay) + Inference‑Guided Search as a Strategy Algebra

You now have a correct evaluator, a correct hygienic macro expander, scan‑out‑defines, derived expressions, and a stdlib macro story. The next step is where real systems either become a language or devolve into “scripts”: **compilation units** and **governed inference**.

This part does two things:

1. **Completes the module system in implementation terms** (TS reference) with a real phase tower (0 runtime, 1 macro/compile‑time), exports/imports, and deterministic artifact hashing.
2. **Begins the inference plane “as semantics,” not as an API**:

   * `infer` is an effect with a formal request schema and formal result schema (`Meaning`)
   * semantic macro expansion is receipted (replayable), obligation‑bearing (verifiable), and promotable (gated)
   * inference integrates with nondeterministic search as a **Strategy Algebra** (best‑first/beam/sample policies with oracle‑scored frontiers)

Everything here is pinned to concrete data structures and code‑shaped interfaces.

---

# 170. Global Uniqueness and Determinism for `bid` and Internal Names

Your single‑file harness (Part 17) used local `bid#N` counters. That becomes **unsound** as soon as you have more than one module, because internal names collide:

* module A binds `x$bid#1`
* module B binds `x$bid#1`
* runtime linker thinks they are the same symbol → catastrophic aliasing

## 170.1 Normative rule

> A `bid` must be globally unique across the entire compilation universe.
> A scope id must be globally unique across the entire compilation universe.
> Both must be deterministic under strict compilation.

## 170.2 Practical deterministic construction

Let `ModuleId` be a stable identifier for the compilation unit (module path, package + name, etc.). For the reference interpreter, use `moduleName` as `ModuleId`.

Then:

* module scopes:

  * `M0 = "M0::<ModuleId>"`
  * `M1 = "M1::<ModuleId>"`
* fresh binder scopes inside module:

  * `B::<ModuleId>::<n>` (n from deterministic counter)
* binding identity:

  * `bid::<ModuleId>::<n>` (n from deterministic counter)
* internal name:

  * `<origName>$<bid>` (now globally unique because bid is module‑qualified)

This is the same idea as compiler “gensym” with a compilation‑unit prefix.

---

# 171. Module Surface Syntax and Phases

## 171.1 `module` form (baseline)

```scheme
(module <name>
  (provide ...exports...)
  (require ...imports...)
  (require-for-syntax ...imports...)
  (begin-for-syntax ...compile-time forms...)
  ...body forms...)
```

**Body forms** may include:

* `define` (phase 0 value binding)
* `define-syntax` (phase 1 transformer binding, expands phase 0)
* expressions (phase 0)
* stdlib macro invocations

## 171.2 Phase discipline (normative)

* phase 0:

  * runtime variables and runtime code
* phase 1:

  * transformers (macros) and compile-time code
* A phase 1 transformer **rewrites** phase 0 syntax.

### `require` semantics

* imports phase 0 values into phase 0 (via alias bindings)
* imports phase 1 syntax into phase 1 (via alias bindings)
* additionally imports the imported module’s binding tables into the **compilation universe** (for def-site resolution of identifiers introduced by imported macros)

### `require-for-syntax` semantics

* imports phase 0 values into phase 1 (phase shift +1) for compile-time evaluation support

### `begin-for-syntax`

* compile-time evaluation context (phase 1)
* can compute and `define-syntax` macros procedurally (later), or compute compile-time constants

For now (reference implementation baseline), we support:

* `begin-for-syntax` containing `define` (phase 1 values) and `define-syntax` (phase 1 transformers)
* and we evaluate its phase‑1 value `define`s (so later compile-time code can use them)
* procedural macros are introduced in Part 20+ (but the scaffolding is here).

---

# 172. Module Artifacts as Hermetic Units (CAS + Registry)

This is where you cross the boundary from “interpreter” to “compiler pipeline.”

## 172.1 `ModuleArtifact` format (reference-grade)

```ts
// src/core/modules/artifact.ts
import type { Binding } from "../syntax/binding";
import type { Expr } from "../ast";
import type { SRTransformer } from "../expand/syntaxRules";

export type ExportEntry0 = { name: string; bid: string; internal: string };
export type ExportEntry1 = { name: string; bid: string; transformer: SRTransformer };

export type ModuleArtifact = {
  moduleName: string;
  moduleId: string;                 // stable, used for prefixing bids/scopes
  scopeM0: string;
  scopeM1: string;

  // Full binding table closure for def-site resolution of imported macros
  bindings: Binding[];

  // The runtime init expression (phase 0)
  init: Expr;

  exports0: ExportEntry0[];
  exports1: ExportEntry1[];

  deps: string[];                   // module names or module ids (depends on loader)
  sourceHash: string;
  expandedHash: string;
  coreHash: string;

  // Hermetic inference receipts produced during compilation (Part 19 §178)
  receipts: unknown[];
};
```

### Why store full `bindings`?

Because imported macros introduce identifiers with their own scope sets (e.g., `M0::<A>`). Those identifiers must still resolve to their originating bindings even in another module’s compilation session. Exports alone are insufficient.

This is a classic **Linker Symbol Table** requirement.

## 172.2 Content-addressed storage (CAS)

A CAS stores blobs by digest. Minimal interface:

```ts
// src/core/artifacts/cas.ts
export type Hash = string;

export interface CAS {
  putJSON(x: unknown): Hash;
  getJSON(h: Hash): unknown;
  putText(s: string): Hash;
  getText(h: Hash): string;
}
```

In strict mode, module compilation produces deterministic hashes:

* `sourceHash = H(sourceText)`
* `expandedHash = H(expandedSyntaxCanonical)`
* `coreHash = H(coreAstCanonical)`

This is the hermetic “build artifact identity.”

## 172.3 Registry (trusted vs candidate)

```ts
// src/core/artifacts/registry.ts
export type Registry = {
  candidates: Record<Hash, { name: string; time: number }>;
  trusted: Record<Hash, { name: string; time: number }>;
  pointers: { defaultExpander?: Hash };
};
```

Promotion (Part 14) can now operate at the module artifact level, not just expander.

---

# 173. Phase Environments: One Binding Table, Phase‑Tagged Bindings

You already have:

```ts
export type Binding = { bid; name; scopes; phase; kind; value; }
export type Env = Binding[];
```

This works for phases 0 and 1. The only additional structure you need is **convention**:

* `phase=0, kind="value"` binds internal runtime names
* `phase=1, kind="syntax"` binds transformers used to expand phase 0 syntax
* `phase=1, kind="value"` binds compile-time values evaluated in begin-for-syntax

The resolver stays unchanged: filter by phase then subset+max specificity.

---

# 174. Parsing and Compiling a Module Form (TS Reference)

We now build the actual module compiler that:

* reads source
* recognizes `(module ...)`
* compiles dependencies
* imports env closures
* expands body with sequential defines and macro expansion
* computes exports
* lowers to `Expr` init
* stores artifact (CAS)

## 174.1 Module reader helper: recognize `(module name ...)`

We use your datum reader (Part 17) and then interpret the top datum.

```ts
// src/core/modules/compileModule.ts
import { tokenize } from "../reader/tokenize";
import { parseAll } from "../reader/parse";
import type { Datum } from "../reader/datum";
import { isSym } from "../reader/datum";
import { datumToSyntax } from "../reader/toSyntax";
import { addScope } from "../syntax/syntax";
import type { Syntax, SIdent, SList } from "../syntax/syntax";
import { isIdent, isList } from "../syntax/syntax";
import type { Env, Binding } from "../syntax/binding";
import { resolveIdent } from "../syntax/binding";
import { lowerSyntax } from "../pipeline/lower";
import { compileSyntaxRules, type SRTransformer } from "../expand/syntaxRules";

// loader provides module source by name
export interface ModuleLoader {
  loadSource(moduleName: string): string;
  // optional: loadArtifact by hash
}

// compilation context to memoize compiled artifacts
export type CompileCtx = {
  compiled: Map<string, ModuleArtifact>;
  // moduleName -> artifact
};

export type ModuleArtifact = import("./artifact").ModuleArtifact;

// deterministic hashes (stub; use crypto hash in real code)
function hashText(s: string): string { return `H(${s.length}:${s.slice(0,64)})`; }

function expectList(stx: Syntax, msg: string): SList {
  if (!isList(stx)) throw new Error(msg);
  return stx;
}
function expectIdent(stx: Syntax, msg: string): SIdent {
  if (!isIdent(stx)) throw new Error(msg);
  return stx;
}
```

## 174.2 Deterministic ids: module scopes + bid generator

```ts
type Counters = { scopeN: number; bidN: number };

function mkModuleIds(moduleId: string) {
  return {
    M0: `M0::${moduleId}`,
    M1: `M1::${moduleId}`,
  };
}

function freshScope(moduleId: string, c: Counters): string {
  c.scopeN += 1;
  return `B::${moduleId}::${c.scopeN}`;
}

function freshBid(moduleId: string, c: Counters): string {
  c.bidN += 1;
  return `bid::${moduleId}::${c.bidN}`;
}

function internalName(orig: string, bid: string): string {
  return `${orig}$${bid}`;
}
```

This patches the global uniqueness problem at the root.

---

# 175. Imports: Universe Import + Alias Import (Two-Layer Linking)

## 175.1 Universe import (def-site closure)

You merge imported module bindings wholesale into the current `Env` (append). Because resolution is scope‑based and maximal, this does not “pollute” call‑site names unless their scopes match.

```ts
function importUniverse(env: Env, imported: Binding[]): Env {
  return env.concat(imported);
}
```

## 175.2 Alias import for exports

You create *new binding entries* in the importing module’s scopes, **reusing the same bid**.

Why reuse bid?

* `free-identifier=?` and literal matching depend on bid identity.
* linking is by internal name derived from bid (phase 0), so reuse preserves runtime linkage.

```ts
import type { ExportEntry0, ExportEntry1 } from "./artifact";

function aliasImportPhase0(env: Env, exports0: ExportEntry0[], scopeM0: string): Env {
  let Γ = env;
  for (const ex of exports0) {
    Γ = Γ.concat([{
      bid: ex.bid,
      name: ex.name,
      scopes: [scopeM0],
      phase: 0,
      kind: "value",
      value: ex.internal,
    }]);
  }
  return Γ;
}

function aliasImportPhase1(env: Env, exports1: ExportEntry1[], scopeM0: string): Env {
  // macros used to expand phase 0 syntax typically appear in phase 0 lexical scope,
  // but are resolved at phase 1. So we bind phase=1 with scopes=[scopeM0].
  let Γ = env;
  for (const ex of exports1) {
    Γ = Γ.concat([{
      bid: ex.bid,
      name: ex.name,
      scopes: [scopeM0],
      phase: 1,
      kind: "syntax",
      value: ex.transformer,
    }]);
  }
  return Γ;
}

function aliasImportRequireForSyntax(env: Env, exports0: ExportEntry0[], scopeM1: string): Env {
  // phase shift: runtime values imported into compile-time value env (phase=1)
  let Γ = env;
  for (const ex of exports0) {
    Γ = Γ.concat([{
      bid: ex.bid,
      name: ex.name,
      scopes: [scopeM1],
      phase: 1,
      kind: "value",
      value: ex.internal,
    }]);
  }
  return Γ;
}
```

This is effectively the “import stub generation” stage in a compiler/linker.

---

# 176. Compile a Module: End-to-End Algorithm (TS Skeleton)

We now define:

* `compileModule(moduleName)` returns `ModuleArtifact`
* it compiles dependencies recursively (memoized)
* imports binding universes
* expands module body sequentially
* computes exports
* lowers runtime init to core `Expr`

## 176.1 Module body directives parsing

We treat these forms specially:

* `(provide ...)`
* `(require ...)`
* `(require-for-syntax ...)`
* `(begin-for-syntax ...)`

Everything else is ordinary top-level body: `define`, `define-syntax`, expressions.

### Provide syntax (baseline)

* `(provide x y)` → export phase 0 values
* `(provide (for-syntax m n))` → export phase 1 macros

### Require syntax (baseline)

* `(require A B)` imports modules by name

### Require-for-syntax

* `(require-for-syntax C)` imports runtime exports into phase 1 values

### Begin-for-syntax

* `(begin-for-syntax forms...)`

  * forms are expanded at phase 1 (macros at phase 2 if you later add)
  * defines inside install phase 1 value bindings; define-syntax installs phase 1 macro bindings

In this baseline, we **expand begin-for-syntax forms** but we do not yet provide full procedural macro evaluation; we prepare the env structure so it can be added without redesign.

---

## 176.2 Core compile entry point

```ts
export function compileModule(
  loader: ModuleLoader,
  ctx: CompileCtx,
  moduleName: string
): ModuleArtifact {

  // memoization
  const cached = ctx.compiled.get(moduleName);
  if (cached) return cached;

  const sourceText = loader.loadSource(moduleName);
  const sourceHash = hashText(sourceText);
  const moduleId = moduleName;             // reference-grade; include package path in real world
  const { M0, M1 } = mkModuleIds(moduleId);

  // parse to datum; require single module form
  const ds = parseAll(tokenize(sourceText));
  if (ds.length !== 1) throw new Error("module file must contain exactly one top-level form");
  const top = ds[0];

  // convert to syntax and scope with module scope M0 initially
  let stx = datumToSyntax(top);
  stx = addScope(stx, M0);

  // validate module form
  const modList = expectList(stx, "module: expected list");
  if (modList.items.length < 2) throw new Error("module: missing name");
  const head = expectIdent(modList.items[0], "module: head must be ident");
  if (head.name !== "module") throw new Error("expected (module ...) at top level");
  const nameIdent = expectIdent(modList.items[1], "module: name must be ident");
  const bodyForms = modList.items.slice(2);

  // deterministic gensym counters per module
  const counters: Counters = { scopeN: 0, bidN: 0 };

  // Initialize Γ with primitives in module scope
  let Γ: Env = initialEnvForModule(moduleId, M0, M1);

  // Stage 1: compile-time directives pass (gather requires, etc.)
  const directives = scanDirectives(bodyForms);

  // Stage 2: compile deps (topological by recursion)
  const depArtifacts = directives.requires.map(r => compileModule(loader, ctx, r))
    .concat(directives.requiresForSyntax.map(r => compileModule(loader, ctx, r)));

  // Stage 3: import universes + alias exports
  for (const art of depArtifacts) {
    Γ = importUniverse(Γ, art.bindings);
  }
  for (const req of directives.requires) {
    const art = ctx.compiled.get(req)!;
    Γ = aliasImportPhase0(Γ, art.exports0, M0);
    Γ = aliasImportPhase1(Γ, art.exports1, M0);
  }
  for (const reqS of directives.requiresForSyntax) {
    const art = ctx.compiled.get(reqS)!;
    Γ = aliasImportRequireForSyntax(Γ, art.exports0, M1);
    // also import macros for syntax if you want begin-for-syntax to have macro helpers; optional:
    // Γ = aliasImportPhase1(Γ, art.exports1, M1) ... (if you introduce phase2)
  }

  // Stage 4: expand module body (sequential defines)
  const expandedRes = expandModuleBody(bodyForms, Γ, { moduleId, M0, M1 }, counters);
  Γ = expandedRes.env;

  // Stage 5: compute exports by resolving provide names in Γ
  const exports = computeExports(directives.provides, Γ, M0);

  // Stage 6: lower expanded runtime forms to init Expr
  const initStx = expandedRes.runtimeInitSyntax; // a (begin ...) expression
  const init = lowerSyntax(initStx, Γ);

  // Stage 7: compute digests (expanded/core)
  const expandedHash = hashText(JSON.stringify(canonicalizeSyntax(initStx)));
  const coreHash = hashText(JSON.stringify(init));

  const artifact: ModuleArtifact = {
    moduleName: nameIdent.name,
    moduleId,
    scopeM0: M0,
    scopeM1: M1,
    bindings: Γ,
    init,
    exports0: exports.exports0,
    exports1: exports.exports1,
    deps: Array.from(new Set([...directives.requires, ...directives.requiresForSyntax])),
    sourceHash,
    expandedHash,
    coreHash,
    receipts: expandedRes.receipts, // semantic macro receipts (Part 19 §178)
  };

  ctx.compiled.set(moduleName, artifact);
  return artifact;
}
```

### Supporting functions we referenced:

* `initialEnvForModule`
* `scanDirectives`
* `expandModuleBody`
* `computeExports`
* `canonicalizeSyntax`

We define them next.

---

# 177. Module Directives Scan (`require/provide/...`) and Body Expansion

## 177.1 `scanDirectives`

```ts
type Directives = {
  requires: string[];
  requiresForSyntax: string[];
  provides: Syntax[]; // raw provide specs for later resolution
};

function scanDirectives(forms: Syntax[]): Directives {
  const requires: string[] = [];
  const requiresForSyntax: string[] = [];
  const provides: Syntax[] = [];

  for (const f of forms) {
    if (!isList(f) || f.items.length === 0 || !isIdent(f.items[0])) continue;
    const tag = (f.items[0] as SIdent).name;

    if (tag === "require") {
      for (let i = 1; i < f.items.length; i++) {
        const m = f.items[i];
        const id = expectIdent(m, "require: module name must be ident");
        requires.push(id.name);
      }
      continue;
    }

    if (tag === "require-for-syntax") {
      for (let i = 1; i < f.items.length; i++) {
        const m = f.items[i];
        const id = expectIdent(m, "require-for-syntax: module name must be ident");
        requiresForSyntax.push(id.name);
      }
      continue;
    }

    if (tag === "provide") {
      provides.push(f);
      continue;
    }
  }

  return { requires, requiresForSyntax, provides };
}
```

---

## 177.2 Initialize primitives in module scope

```ts
function initialEnvForModule(moduleId: string, M0: string, _M1: string): Env {
  const prims = ["+", "-", "=", "not", "unit", "__uninit", "effect", "handle"]; // "effect/handle" are syntax, but leave here if you want
  let env: Env = [];
  for (const p of prims) {
    env = env.concat([{
      bid: `prim::${p}`,
      name: p,
      scopes: [M0],
      phase: 0,
      kind: "value",
      value: p, // internal name is primitive name
    }]);
  }
  return env;
}
```

---

## 177.3 Expanding the module body

We reuse your expander logic (Part 17/18), but we must:

* ignore directives at runtime (provide/require)
* process `begin-for-syntax` separately at phase 1 scope `M1`
* keep receipts from semantic macros (Part 19 §178)

```ts
type ModuleIds = { moduleId: string; M0: string; M1: string };

type ExpandModuleRes = {
  env: Env;
  runtimeInitSyntax: Syntax;   // usually (begin ...)
  receipts: unknown[];
};

function expandModuleBody(
  forms: Syntax[],
  env: Env,
  ids: ModuleIds,
  counters: Counters
): ExpandModuleRes {

  let Γ = env;
  const runtimeForms: Syntax[] = [];
  const receipts: unknown[] = [];

  for (const f of forms) {
    if (isList(f) && f.items.length > 0 && isIdent(f.items[0])) {
      const tag = (f.items[0] as SIdent).name;

      // directives disappear from runtime init
      if (tag === "require" || tag === "require-for-syntax" || tag === "provide") {
        continue;
      }

      if (tag === "begin-for-syntax") {
        // Expand compile-time forms under M1 scope; install phase=1 value bindings and phase=1 macros.
        const bf = expectList(f, "begin-for-syntax must be list");
        const ctForms = bf.items.slice(1).map(x => addScope(x, ids.M1));

        const res = expandBeginForSyntax(ctForms, Γ, ids, counters);
        Γ = res.env;
        receipts.push(...res.receipts);
        continue;
      }

      // otherwise: normal top-level form expanded under phase 0; sequential define/define-syntax semantics
      if (tag === "define-syntax") {
        const r = expandDefineSyntaxModule(f as any, Γ, ids, counters);
        Γ = r.env;
        receipts.push(...r.receipts);
        continue; // not part of runtime init
      }

      if (tag === "define") {
        const r = expandDefineModule(f as any, Γ, ids, counters);
        Γ = r.env;
        runtimeForms.push(r.stx);
        receipts.push(...r.receipts);
        continue;
      }
    }

    // ordinary expression at top-level
    const r = expandExprModule(f, Γ, ids, counters);
    Γ = r.env;
    runtimeForms.push(r.stx);
    receipts.push(...r.receipts);
  }

  // runtime init expression is (begin <forms...>)
  const initStx: Syntax = {
    tag: "List",
    scopes: [ids.M0],
    items: [{ tag: "Ident", name: "begin", scopes: [ids.M0] }, ...runtimeForms],
  };

  return { env: Γ, runtimeInitSyntax: initStx, receipts };
}
```

### What changed vs single-unit harness

* module directives are compile-time only
* begin-for-syntax modifies phase 1 environment
* receipts are threaded through expansion results

---

# 178. The Inference Plane: Formal `infer` Effect, `InferRequest` Schema, `Meaning` Schema, Receipts, and Obligations

Now we stop speaking in metaphors and define the actual calculus.

## 178.1 Surface-level `infer` is a derived form

In Ω surface syntax:

```scheme
(infer <request>)
```

is desugared to:

```scheme
(effect infer.op <request>)
```

So `infer` is not “a function call,” it is an **effectful operation** subject to the handler stack, receipts, policies, budgets, and replay.

### Macro definition (stdlib)

```scheme
(define-syntax infer
  (syntax-rules ()
    ((_ req) (effect infer.op req))))
```

Now inference is as first-class as nondeterminism (`amb`) and commits (`commit`).

---

## 178.2 `InferRequest` record schema (normative)

An `InferRequest` is a pure data value (serializable), typically a Map/Record. Required fields:

* `kind` : string (e.g., `"eval"`, `"rewrite"`, `"critic"`, `"rank"`, `"explain"`)
* `input` : value (often quoted syntax datum or a `SyntaxValue` once you add that)
* `constraints` : list of contracts/obligations/predicates (also values)
* `expected` : contract/type spec (optional)
* `context` : *explicit* context descriptor (never implicit “whatever is in the LLM head”)

  * `ctxDigest` (mandatory)
  * optionally: `ctxReceipt` pointer hash
* `envelope` : determinism envelope (seed, temperature, maxTokens, maxToolCalls, etc.)
* `policy` : policy id (engine, prompt version, safety mode, etc.)
* `evidence` : evidence references (artifact hashes, file digests, etc.)

### TS structural shape

```ts
export type InferRequest = {
  kind: string;
  input: unknown;
  constraints: unknown[];
  expected?: unknown;
  context: { ctxDigest: string; ctxReceipt?: string };
  envelope: { seed: number; temperature: number; maxTokens: number; maxToolCalls: number; timeBudgetMs: number };
  policy: { engine: string; promptDigest: string; mode: "strict" | "explore" };
  evidence: { refs: string[] };
};
```

You can embed this inside `Val` as a `Map` value; the oracle adapter interprets it.

---

## 178.3 `Meaning` schema (normative)

Inference returns not raw text, but a **Meaning** structure:

* `value` : optional value result (classification, number, list, etc.)
* `rewrite` : optional syntax rewrite (for semantic macros or refactoring)
* `alternatives` : optional list of alternative meanings with scores
* `confidence` : numeric in [0,1]
* `cost` : structured (tokens, tool calls, time)
* `evidence` : references supporting the meaning
* `obligations` : required checks to promote/commit (tests, equivalence, contracts)
* `transcriptHash` : digest of the oracle transcript

### TS structural shape

```ts
export type Obligation =
  | { tag: "UnitTest"; name: string; expr: unknown; expected: unknown }
  | { tag: "Property"; name: string; generator: unknown; predicate: unknown; samples: number }
  | { tag: "EqExt"; left: unknown; right: unknown; samples: number }
  | { tag: "TypeCheck"; expr: unknown; typeSpec: unknown }
  | { tag: "HumanReview"; reason: string };

export type Meaning = {
  value?: unknown;
  rewrite?: unknown;
  alternatives?: Array<{ score: number; meaning: Meaning }>;
  confidence: number;
  cost: { tokens: number; toolCalls: number; ms: number };
  evidence: { refs: string[] };
  obligations: Obligation[];
  transcriptHash: string;
};
```

This is the semantic “unit” you can reason about, receipt, replay, gate, and audit.

---

## 178.4 Hermetic receipts for inference

Whenever `infer` runs in strict compilation/evaluation mode, it must be receipted.

Receipt key:

[
K = H(\text{requestCanonical} \parallel \text{policyDigest} \parallel \text{envelopeCanonical})
]

Receipt payload:

* request hash
* response (meaning) hash
* transcriptHash
* evidence refs
* obligations summary

```ts
export type InferReceipt = {
  tag: "InferReceipt";
  key: string;
  requestHash: string;
  policyDigest: string;
  envelope: unknown;
  responseHash: string;
  meaning: Meaning;
  transcriptHash: string;
  timeMs: number;
};
```

Strict mode rule:

* if receipt not present and calling inference is disallowed → compilation fails
* if inference allowed → store receipt and proceed
  Replay rule:
* in replay mode, oracle adapter must not be called; use receipt

This is the same reproducibility principle as content-addressed compilation artifacts.

---

# 179. Semantic Macros (Inference at Expansion Time) Without Non‑Reproducible Builds

You now add **procedural transformers** in addition to `syntax-rules`. This is where the inference plane can become a language feature rather than a bolt-on.

## 179.1 New transformer kind: `SemanticTransformer`

A transformer becomes:

```ts
export type Transformer =
  | { tag: "SyntaxRules"; tr: SRTransformer }
  | { tag: "Semantic"; bid: string; fnRef: unknown; policy: unknown };
```

In the TS reference, `Semantic` can be a host function pointer (Adapter) initially; in the self-hosted version, `fnRef` is an Ω closure evaluated at phase 1.

## 179.2 Semantics of semantic macro expansion

Given a macro invocation syntax `callStx`:

1. Compute `H_in = hash(canonical(callStx))`
2. Compute `D_tr = hash(transformer identity + module artifact digest)`
3. Compute `D_policy = hash(policy + promptDigest + engine)`
4. Compute `E_env = deterministic envelope`
5. Key `K = H(H_in || D_tr || D_policy || E_env)`

Then:

* if receipt exists: output = receipt.output
* else:

  * call `infer` (or other oracle) to produce `Meaning` with a `rewrite` field that is syntax
  * store MacroReceipt with `oracleTranscriptHash` and obligations

### MacroReceipt (expansion-time)

```ts
export type MacroReceipt = {
  tag: "MacroReceipt";
  key: string;
  inputHash: string;
  transformerBid: string;
  policyDigest: string;
  envelope: unknown;
  outputHash: string;
  outputSyntax: unknown;      // syntax JSON
  meaning: Meaning;           // includes obligations
  oracleTranscriptHash: string;
};
```

## 179.3 Governance rule

Expansion-time inference is allowed only if:

* it produces obligations (unless confidence=1 and policy says “no obligations,” which should be rare)
* it is receipted
* promotion gates discharge obligations before trusting the resulting artifact

This is exactly how you prevent “macro expansion as hallucination.”

---

# 180. Inference‑Guided Nondeterministic Search as a Strategy Algebra

The `amb` runner from Part 15 already supports:

* frontier = dfs/bfs/best/beam/sample
* scoring hooks: `scoreChoice`, `pruneChoice`, `scoreResult`

Now we formalize the integration of inference as a semantic plane:

## 180.1 Scoring is itself an effectful oracle query (but governed)

Define a request kind:

* `kind: "rank"` or `"heuristic"`
* `input`: choice + constraint context + partial evaluation state digest
* output: meaning.value = score (numeric) and optional evidence and obligations (for strict mode)

In strict mode:

* heuristic receipts must be stored so that best-first exploration order is replayable.

### HeuristicReceipt

```ts
export type HeuristicReceipt = {
  tag: "HeuristicReceipt";
  key: string;
  contextHash: string;     // constraints + depth + maybe partial state digest
  choiceHash: string;
  policyDigest: string;
  score: number;
  transcriptHash: string;
};
```

## 180.2 Policy objects (Strategy pattern)

You already have:

* `Frontier` as Strategy over job scheduling
* `scoreChoice` as Strategy over node evaluation
* `pruneChoice` as Strategy over cutoffs
* `quantumSteps` as fairness control (dovetailing)

Inference adds:

* `oraclePolicy` selecting engine, prompt digest, determinism envelope
* receipt store injected (Repository pattern)

This becomes a composable policy algebra:

* `BestFirst(OracleScore(policy))`
* `Beam(OracleScore(policy), width=k)`
* `Sample(OracleDistribution(policy), temperature=t)`
* `BFS(quantum=q)` (oracle-free, completeness oriented)
* `A*(g + h)` where `h` is oracle score and `g` is depth or cost

You can express this as constructors rather than ad-hoc flags.

---

# 181. Commit Barriers and Obligations: Promotion is Not Optional

Inference produces obligations; commits and promotions discharge them.

## 181.1 `commit` is an effect like `infer`

In Ω:

```scheme
(commit <commit-request>)
```

desugars to `(effect commit.op <commit-request>)`.

Commit requests typically include:

* proposed rewrite patch
* or external side-effect description
* plus attached obligations and evidence

## 181.2 Strict rule: no commit without discharged obligations

A commit handler must:

* verify obligations (run tests, equivalence checks, etc.)
* record discharge receipts
* then apply side effect / patch

This is the same “two-phase” discipline we described earlier:

* compile produces candidates
* promote/commit makes them trusted/real

---

# 182. What’s Still Missing (and What Part 20 Will Do)

At this point you have:

* A real module compiler shape with phase 0 and phase 1 binding tables
* A correct linking story (universe import + alias import reusing bids)
* A hermetic artifact model with digests and receipts
* The inference plane formalized as:

  * `infer` effect with request/meaning schemas
  * receipts for strict replay
  * obligations for governance
* A clean integration path between inference and nondeterministic search (oracle-scored strategies)

What is still missing for “full tower completeness” is:

1. **Actual begin-for-syntax evaluation** (phase 1 evaluator session) so compile-time `define` produces values usable in later compile-time computations, and procedural macros can be expressed in Ω, not TS.
2. **Transformer serialization** (SRTransformer maps/structures to JSON and back) so artifacts can persist beyond memory.
3. **A proper module loader/runtime linker** with topological init and cycle diagnostics.
4. **The procedural macro interface** (syntax-case / syntax-parse / or a minimal syntax-value API) so semantic macros can be authored in Ω (and call `infer`).

If you say **continue**, Part 20 will implement those missing pieces in code:

* a separate phase‑1 evaluation run (using the same CEKS) with its own Env/Store
* `begin-for-syntax` semantics (evaluate compile-time forms, install bindings)
* a minimal procedural macro facility (transformer as Ω closure) plus a small syntax‑API in the runtime
* full module graph initialization (toposort + SCC cycle errors)
* transformer serialization and CAS persistence

And then we’ll do an end-to-end demonstration:

* Module A exports a semantic macro that uses `infer` to rewrite a program fragment
* Module B imports it and compiles under strict mode
* Receipts are stored and replayed
* Obligations are discharged and the artifact is promoted
