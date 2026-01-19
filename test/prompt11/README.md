# Prompt 11: Nondeterministic Evaluation with `amb` and Search

This directory contains tests for nondeterministic evaluation using `amb` and systematic search strategies (DFS/BFS/Beam/MCTS).

## Overview

Prompt 11 adds the Search (S) layer to OmegaLLM:
- `amb.choose` effect with weighted choices
- Search strategies as effect handlers (DFS, BFS, Beam, MCTS)
- Multi-shot continuations using COWStore
- `DistVal` as search result carrier
- Deterministic replay for reproducibility
- Search-specific budget tracking

## Test Coverage

### Test 11.1: Backtracking Semantic Redaction
Tests that DFS backtracks from failed branches to successful ones.
- Ledger records backtrack decisions
- Solutions collected in DistVal

### Test 11.2: BFS Fairness vs DFS Starvation
Tests breadth-first search explores fairly across branches.
- Compares BFS and DFS exploration patterns

### Test 11.3: Cost-Aware Search
Tests weighted choices affect distribution weights.
- `distBest` returns highest weighted solution

### Test 11.4: Beam Search with Bounded Width
Tests beam search limits frontier size.
- Pruning of low-scoring branches
- Beam width parameter

### Test 11.5: MCTS Policy Search
Tests Monte Carlo Tree Search with UCB1 selection.
- Exploration vs exploitation balance
- Custom score functions

### Test 11.6: Deterministic Replay
Tests reproducibility via recorded ledger.
- Same seed produces same search ordering
- Replay produces identical results

### Test 11.7: Budget Enforcement
Tests search resource limits.
- `searchNodesLeft` limits node expansions
- `solutionsLeft` limits collected solutions

## Key Types

- `AmbPayload`: Parsed `amb.choose` arguments
- `WeightedChoice`: Choice with weight for scoring
- `SearchBudget`: Resource limits (nodes, depth, solutions)
- `SearchConfig`: Strategy and configuration
- `SearchLedger`: Recorded decisions for replay
- `SearchResult`: Final distribution with stats

## Search Strategies

| Strategy | Data Structure | Use Case |
|----------|---------------|----------|
| DFS | Stack | Deep exploration, memory efficient |
| BFS | Queue | Fair exploration, shortest path |
| Beam | Priority Queue | Bounded width, pruning |
| MCTS | UCB1 Selection | Explore/exploit balance |

## Architecture

```
effects/search/
  types.ts    - Core types (AmbPayload, SearchBudget, etc.)
  runner.ts   - Search runner with strategy implementations
  index.ts    - Public exports
```

## Dist Algebra Extensions

Extended distribution operations for search:
- `distFilter`: Filter by predicate
- `distTake`: Take top n by weight
- `distBest`: Get highest weighted item
- `distBestK`: Get top k with custom scoring
- `distConcat`: Combine distributions
- `distUniform`: Equal-weight distribution
- `distWeighted`: Custom-weight distribution
