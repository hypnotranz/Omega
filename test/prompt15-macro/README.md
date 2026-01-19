# Prompt 15: Hygienic Macros Tests

This directory contains tests for SICP-style hygienic macros with phase separation and semantic macro support.

## Overview

Prompt 15 implements the macro system:
- First-class syntax objects with scope sets for hygiene
- Phase separation (macro env vs runtime env)
- Pure syntactic macros (syntax-rules)
- Semantic macros (with inference under governance)
- Commit barriers for semantic expansions

## Test Coverage

### Test 15.1: Hygiene - Macro-Introduced Identifier Does NOT Capture User Identifiers
Tests that macro's internal bindings don't capture user bindings.
- Define macro `with-temp` that introduces `tmp` internally
- User code also has `tmp`
- Verifies: user `tmp` remains unchanged, macro `tmp` is distinct

### Test 15.2: Hygiene - User Binding Does NOT Capture Macro-Introduced Binding
Tests that user bindings don't capture macro-introduced bindings.
- Define macro `swap!` with introduced binder `t`
- User has binder `t` in scope
- Verifies: expansion uses fresh `t#scope`, no collision with user `t`

### Test 15.3: Phase Separation - Runtime Define Cannot Create Macro
Tests that runtime and macro environments are separate.
- Runtime `(define foo ...)`
- Compile-time `(define-syntax foo ...)`
- Verifies: they live in different namespaces/phases

### Test 15.4: Macroexpansion is Observable and Replayable
Tests receipt-based replay of macro expansion.
- Run program with macros under strict profile
- Verifies: `macroexpand` produces identical result on replay, receipt includes expansion digest

### Test 15.5: Semantic Macro - Define Semantic Function via Macro
Tests semantic function definition and composition.
- Define `defsemantic` macro that expands to OracleProc
- Compose semantic functions
- Verifies: macro expands to real definitions, composable, obligations exist

### Test 15.6: Semantic Macro Governance - Explore vs Pragmatic
Tests profile-based governance of semantic macros.
- Run semantic macro under `explore` (no commit) and `pragmatic` (commit with tests)
- Verifies: explore can expand but not commit, pragmatic can commit after tests

### Test 15.7: Oracle-Driven Macro Debugging
Tests oracle REPL integration with macroexpansion.
- Program pauses inside semantic function
- Oracle requests expansion, inspects stack, evaluates expanded code
- Verifies: REPL + macroexpansion + eval-at-frame works coherently

### Test 15.8: Hygiene + Semantic Rewrite - Pipeline Macro
Tests semantic macro with rewrite that preserves bindings.
- Define `pipeline` macro that expands DSL into staged phases
- Macro uses `rewrite` internally to propose expansion variants
- Verifies: expansion preserves lexical meaning, obligations include regression tests

## Key Types

- `Syntax`: First-class syntax object with scopes for hygiene
- `MacroTransformer`: Pure (syntax-rules) or semantic macro transformer
- `MacroEnv`: Compile-time environment mapping names to transformers
- `ExpansionResult`: Expanded syntax + obligations + evidence
- `ObligationRef`: Obligation for commit barrier

## Architecture

```
omega.macro/
  types.ts      - Core types
  hygiene.ts    - Set-of-scopes implementation
  expander.ts   - Expansion engine
  semantic.ts   - Semantic macro support
```
