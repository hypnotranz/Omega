# Prompt 12: Constraint Propagation Networks

This directory contains tests for SICP-style constraint propagation networks adapted for semantic pipelines.

## Overview

Prompt 12 implements the "Constraint (C)" layer that composes with search and inference:
- Constraints prune `amb` branches early (CSP/CP-SAT style)
- Contradictions generate explanation graphs (why it failed)
- Repairs are synthesized as `amb` choices (drop/relax/replace constraints)
- Oracles participate as propagators but remain governed

## Test Coverage

### Test 12.1: Basic Semantic Propagator Network
Tests that a propagator network reaches quiescence and produces expected output.
- Network: rawText -> normalize -> redact -> safe?
- Constraint: safe? = true
- Verifies: quiescence, connector values, explanation graphs

### Test 12.2: Contradiction Explanation Graphs
Tests that contradictions produce first-class explanation graphs.
- Scripted oracle returns unsafe redaction
- Verifies: contradiction value (not exception), explanation structure

### Test 12.3: Repair via `amb` Search
Tests constraint-directed backtracking with repair synthesis.
- Two redactors: mechanical (unsafe) and semantic (safe)
- BFS search explores repair options
- Verifies: successful repair found, ledger shows exploration

### Test 12.4: Unsat Core Extraction
Tests diagnosis of conflicting constraints.
- Multiple constraints that cannot all be satisfied
- Verifies: unsat core identifies minimal conflict set

### Test 12.5: Constraint-Pruned Beam Search
Tests that constraints prune search early.
- 10 candidate policies, most violate constraints
- Beam search with scoring
- Verifies: violating branches pruned, oracle calls minimized

### Test 12.6: Governance Profile Enforcement
Tests that airgap profiles forbid oracle-based propagators.
- Profile restricts infer.op
- Oracle propagators cannot run
- Verifies: deterministic denial, denied capability in explanation

### Test 12.7: Receipt-Based State Management
Tests snapshot/compress/hydrate for network state.
- Run to contradiction, snapshot, compress
- Later hydrate and verify explanations identical
- Verifies: reproducibility across compression

## Key Types

- `ConnRefVal`: Reference to a connector (store-backed cell)
- `NetRefVal`: Reference to a constraint network
- `ExplanationVal`: First-class explanation graph (assumption/derived/conflict/denied)
- `ContradictionVal`: First-class contradiction value

## Semantic Propagators

Unlike arithmetic SICP propagators, these handle semantic operations:
- `normalize`: rawText -> normalized
- `redact`: normalized + labels -> redacted
- `safe?`: redacted -> boolean
- `classify`: tokens -> sensitivity labels

## Architecture

```
omega.constraints/
  types.ts      - Core types (Connector, Propagator, Network, etc.)
  network.ts    - Network creation and connector operations
  engine.ts     - Propagation engine with scheduling
  diagnosis.ts  - Unsat core extraction, explanation traversal
  repair.ts     - Repair synthesis via amb search
```
