# Prompt 15: Hygienic Macros + Phase Separation + Semantic Macros

This module implements SICP-style hygienic macros with phase separation and Ω's semantic macros (inference-driven expansion under governance).

## Overview

Macros in Ω extend traditional Scheme macros to support:
- **Hygienic Expansion**: Set-of-scopes model prevents variable capture
- **Phase Separation**: Compile-time (macro env) vs runtime (value env)
- **Semantic Macros**: Expansion can invoke inference under governance
- **Commit Barriers**: Semantic expansions require obligations before promotion
- **Replay Support**: Expansions produce receipts for reproducibility

## Architecture

```
omega.macro/
  types.ts      - Core types (MacroTransformer, MacroEnv, ExpansionResult, etc.)
  hygiene.ts    - Set-of-scopes hygiene implementation
  expander.ts   - Expansion engine (expand, macroexpand-1, macroexpand)
  semantic.ts   - Semantic macro support with inference and commit barriers
```

## Key Concepts

### Syntax Objects (First-Class)

Syntax objects carry hygiene metadata:

```typescript
type Syntax =
  | { tag: "Atom"; value: any; scopes: Scope[] }
  | { tag: "Ident"; name: string; scopes: Scope[] }
  | { tag: "List"; items: Syntax[]; scopes: Scope[] };
```

Each identifier's binding is determined by its scope set.

### Phase Separation

Two environments:
- **Runtime Env (phase 0)**: Value bindings for evaluation
- **Macro Env (phase 1)**: Macro transformers for expansion

The pipeline is:
```
read → datum→syntax → expand(phase=1) → lower(core AST) → evaluate(phase=0)
```

### Macro Transformers

Three kinds of transformers:

```typescript
type MacroTransformer =
  | { tag: "SyntaxRules"; ... }      // Pure syntactic (deterministic)
  | { tag: "ProcMacro"; ... }        // Procedural (phase-1 evaluation)
  | { tag: "SemanticMacro"; ... };   // Inference-capable (governed)
```

### Hygiene (Set-of-Scopes Model)

Each macro invocation:
1. Generates a fresh `useScope` and adds it to the input
2. Applies the transformer
3. Generates a fresh `introScope` for macro-introduced identifiers
4. Flips `useScope` on the output (removes from substituted, keeps on introduced)

Resolution finds the binding whose scope set is the most specific subset.

### Semantic Macros

Semantic macros can invoke inference:

```scheme
(define-syntax/semantic defsemantic
  (lambda (stx)
    (let ([spec (parse-spec stx)])
      (infer/synthesize spec)))
  :obligations (test metamorphic))
```

But expansion yields obligations that must be satisfied before commit:

```typescript
type ExpansionResult = {
  expanded: Syntax;
  obligations: ObligationRef[];
  evidence: EvidenceRef[];
  semantic: boolean;
};
```

### Profile Integration

Profiles govern what's allowed during expansion:

| Profile | Pure | Semantic | Commit | Infer |
|---------|------|----------|--------|-------|
| explore | ✓ | ✓ | ✗ | ✓ |
| pragmatic | ✓ | ✓ | ✓ | ✓ |
| strict | ✓ | ✓ | ✓ | ✓ |
| airgap | ✓ | ✗ | ✗ | ✗ |
| macro/pure | ✓ | ✗ | ✗ | ✗ |
| macro/semantic | ✓ | ✓ | ✗ | ✓ |

## API

### Expansion Functions

```typescript
// Single expansion step
macroexpand1(stx: Syntax, ctx: ExpansionContext): ExpansionResult;

// Expand to fixed point
macroexpand(stx: Syntax, ctx: ExpansionContext): ExpansionResult;

// Full expansion (including subforms)
expand(stx: Syntax, ctx: ExpansionContext): ExpansionResult;
```

### Hygiene Utilities

```typescript
// Generate fresh scopes
freshScopeId(): ScopeId;
makeDefScope(name: string): ScopeId;
makeUseScope(name: string): ScopeId;
makeIntroducerScope(name: string): ScopeId;

// Scope operations
addScopeToSyntax(stx: Syntax, scope: Scope): Syntax;
removeScopeFromSyntax(stx: Syntax, scope: Scope): Syntax;
flipScopeOnSyntax(stx: Syntax, scope: Scope): Syntax;

// Identifier resolution
resolveMacroIdent(id: SIdent, env: MacroEnv, phase: number): MacroBinding | null;
freeIdentifierEqual(id1, env1, id2, env2, phase): boolean;
boundIdentifierEqual(id1, id2): boolean;
```

### Semantic Macro Support

```typescript
// Create semantic state
createSemanticState(profileName: string): SemanticExpansionState;

// Obligation management
createObligation(kind: ObligationKind, description?: string): ObligationRef;
satisfyObligation(obligation: ObligationRef, evidence: EvidenceRef): ObligationRef;
allObligationsSatisfied(obligations: ObligationRef[]): boolean;

// Commit barriers
commitSemanticExpansion(request: CommitRequest, caps: Set<string>): CommitResult;
```

## Oracle Protocol

The oracle can request expansions:

```typescript
type ReqExpand = {
  tag: "ReqExpand";
  qstx: Syntax;
  envRef: Hash;
  mode: "1" | "*";  // single step or full expansion
};

type RespSyntax = {
  tag: "RespSyntax";
  stx: Syntax;
  obligations?: ObligationRef[];
};
```

This enables:
- Interactive expansion debugging
- REPL-assisted macro development
- Oracle-driven DSL construction

## Example: Hygienic Swap Macro

```scheme
(define-syntax swap!
  (syntax-rules ()
    ((_ a b)
     (let ((t a))
       (set! a b)
       (set! b t)))))

;; User code
(let ((t 1) (x 2))
  (swap! t x)
  (list t x))
;; => (2 1) — macro's t is distinct from user's t
```

## Example: Semantic Function Definition

```scheme
(define-syntax/semantic defsemantic
  (lambda (stx)
    (match-let ([(defsemantic (name params ...) :goal goal :contract c) stx])
      (synthesize-semantic-function name params goal c)))
  :obligations (test metamorphic))

;; Usage
(defsemantic (sanitize-text txt)
  :goal { kind "sanitize" forbid [tool] }
  :contract contract:safety)

;; Expands to OracleProc with embedded goal
;; but requires test obligations before commit
```

## Integration with Other Prompts

- **Prompt 9 (Profiles)**: Expansion governed by profile constraints
- **Prompt 10 (Contexts)**: Expansion contexts are snapshottable
- **Prompt 11 (Search)**: Semantic macros can use `amb` for exploration
- **Prompt 14 (Generics)**: Macros can define generic operations

## Replay and Receipts

Expansions produce receipts for reproducibility:

```typescript
type ExpansionReceipt = {
  id: string;
  inputHash: Hash;
  outputHash: Hash;
  profileName: string;
  semantic: boolean;
  obligations: string[];
};
```

Non-semantic expansions are deterministically replayable.
Semantic expansions require cached oracle responses for exact replay.
