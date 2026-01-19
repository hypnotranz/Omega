# Prompt 14: Generic Operations Tests

This directory contains tests for SICP-style generic operations with data-directed programming, coercion towers, and method synthesis.

## Overview

Prompt 14 implements the generic operations layer:
- Tagged values with semantic type tags
- Method tables for op × signature → procedure dispatch
- Coercion tables for type conversions
- Method synthesis via inference when methods are missing
- Commit barriers for installing synthesized methods

## Test Coverage

### Test 14.1: Basic Data-Directed Dispatch
Tests that explicit methods dispatch correctly without inference.
- Define methods for `sanitize` on `Doc/Email` and `Doc/Ticket`
- Dispatch to each method based on argument type
- Verifies: no `generic.miss`, no oracle calls, deterministic output

### Test 14.2: Coercion Path Resolution
Tests coercion-based dispatch without synthesis.
- Define `Doc/Email -> Text/Plain` coercion
- Define `sanitize(Text/Plain)` method
- Call `sanitize` on `Doc/Email`
- Verifies: coercion path found, method applied after coercion

### Test 14.3: Missing Method Triggers Synthesis
Tests inference-based method synthesis.
- New type `Doc/ChatTranscript` with no sanitize method
- Call `sanitize` on chat document
- Miss handler synthesizes method
- Verifies: `generic.miss` emitted, method synthesized, tests run, commit performed

### Test 14.4: Inferred Coercion Synthesis
Tests coercion synthesis when coercion path is missing.
- `policy-check(Text/Plain, Policy/Strict)` method exists
- Input is `Text/Markdown` (no coercion to `Text/Plain`)
- Synthesis proposes `strip-markdown` coercion
- Verifies: coercion synthesized and installed

### Test 14.5: Coercion Ambiguity Detection
Tests that multiple equally-good coercion paths are detected.
- Two paths: `Doc/Email -> Text/Plain` and `Doc/Email -> Text/Markdown -> Text/Plain`
- Both have same cost
- Verifies: ambiguity detected, disambiguation required

### Test 14.6: Profile Enforcement
Tests that profiles constrain synthesis and commit.
- `explore` profile: can synthesize, cannot commit
- `pragmatic` profile: can synthesize and commit with tests
- Verifies: capabilities enforced correctly

### Test 14.7: Singleflight for Concurrent Misses
Tests that concurrent misses are deduplicated.
- Two fibers call `sanitize` on unknown type simultaneously
- Verifies: only one synthesis session, both get result

### Test 14.8: Cost-Aware Candidate Selection
Tests scoring and selection among multiple candidates.
- Oracle proposes multiple candidates with different costs
- Selection based on confidence, cost, complexity weights
- Verifies: best candidate selected deterministically

## Key Types

- `TaggedVal`: Value with semantic type tag
- `GenericRegistryVal`: Reference to method/coercion registry
- `GenericMissVal`: First-class miss value for synthesis
- `MethodEntry`: Registered method with metadata
- `CoercionEntry`: Registered type coercion

## Architecture

```
omega.generic/
  types.ts      - Core types
  registry.ts   - Method and coercion tables
  coercion.ts   - Coercion graph and path finding
  dispatch.ts   - apply-generic implementation
  synthesis.ts  - Method synthesis and commit barriers
```
