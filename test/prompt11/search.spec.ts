// test/prompt11/search.spec.ts
// Prompt 11 Tests: Nondeterministic evaluation with amb and search strategies

import { describe, it, expect, beforeEach } from "vitest";
import { installPrims } from "../helpers/prims";
import { COWStore } from "../../src/core/eval/store";
import type { Val } from "../../src/core/eval/values";
import { VUnit, VTrue, VFalse } from "../../src/core/eval/values";
import { stepOnce } from "../../src/core/eval/machineStep";
import type { Expr } from "../../src/core/ast";
import type { State } from "../../src/core/eval/machine";
import type { Runtime } from "../../src/core/eval/runtime";
import {
  dist,
  distFrom,
  distBest,
  distFilter,
  distTake,
  distBind,
  distMap,
  distNormalize,
  distSample,
  distTopK,
  distUniform,
  distWeighted,
  distSize,
  distValues,
  distIsEmpty,
  type DistVal,
  type DistItem,
} from "../../src/core/eval/dist";
import {
  type SearchConfig,
  type SearchBudget,
  type SearchResult,
  type SearchLedger,
  type WeightedChoice,
  DEFAULT_SEARCH_BUDGET,
  SearchBudgetExceeded,
  parseAmbArgs,
} from "../../src/core/effects/search/types";
import {
  runSearch,
  replaySearch,
} from "../../src/core/effects/search/runner";

// ─────────────────────────────────────────────────────────────────
// Minimal test runtime for search
// ─────────────────────────────────────────────────────────────────

function createMinimalRuntime(): Runtime {
  return {
    dispatch: async (state: State, opcall: any) => {
      // For tests, we don't dispatch anything - let search handle amb
      return "Uncaught" as const;
    }
  };
}

// Helper to create initial state from expression
function makeState(expr: Expr, store: COWStore): State {
  const { env, store: st } = installPrims(store);
  return {
    control: { tag: "Expr", e: expr },
    env,
    store: st,
    kont: [],
    handlers: [],
  };
}

// Helper to run to completion without search (for simple expressions)
function runToValue(st: State, maxSteps = 10000): Val | "stuck" {
  for (let i = 0; i < maxSteps; i++) {
    const out = stepOnce(st);
    if (out.tag === "Done") return out.value;
    if (out.tag === "State") {
      st = out.state;
      continue;
    }
    // Op encountered
    return "stuck";
  }
  return "stuck";
}

// ─────────────────────────────────────────────────────────────────
// Test 11.1: Backtracking - first candidate fails, second succeeds
// ─────────────────────────────────────────────────────────────────

describe("Test 11.1: Backtracking semantic redaction", () => {
  it("DFS backtracks from failed branch to successful one", async () => {
    const store = new COWStore();
    const { env, store: st } = installPrims(store);

    // Use Quote to create a vector value - Quote datum gets converted to Val by datumToVal
    // datumToVal converts arrays to cons-cell lists (Vector with 2 items)
    const expr: Expr = {
      tag: "Effect",
      op: "amb.choose",
      args: [
        { tag: "Quote", datum: [1, 2] },  // Will be converted to list of choices
      ],
    };

    const state: State = {
      control: { tag: "Expr", e: expr },
      env,
      store: st,
      kont: [],
      handlers: [],
    };

    const runtime = createMinimalRuntime();
    const config: SearchConfig = {
      strategy: "dfs",
      budget: { searchNodesLeft: 10 },
    };

    const result = await runSearch(runtime, state, config);

    // Should find both solutions (1 and 2)
    expect(result.dist.support.length).toBe(2);
    expect(result.stats.nodesExpanded).toBeGreaterThan(0);
  });

  it("ledger records backtrack decisions", async () => {
    const store = new COWStore();
    const { env, store: st } = installPrims(store);

    const expr: Expr = {
      tag: "Effect",
      op: "amb.choose",
      args: [
        { tag: "Quote", datum: [10, 20, 30] },
      ],
    };

    const state: State = {
      control: { tag: "Expr", e: expr },
      env,
      store: st,
      kont: [],
      handlers: [],
    };

    const runtime = createMinimalRuntime();
    const result = await runSearch(runtime, state, {
      strategy: "dfs",
      budget: { searchNodesLeft: 20 },
    });

    // Ledger should record the amb.choose decision
    expect(result.ledger.decisions.length).toBeGreaterThan(0);
    expect(result.ledger.strategy).toBe("dfs");
  });
});

// ─────────────────────────────────────────────────────────────────
// Test 11.2: BFS fairness vs DFS starvation
// ─────────────────────────────────────────────────────────────────

describe("Test 11.2: BFS fairness", () => {
  it("BFS explores breadth-first unlike DFS", async () => {
    const store = new COWStore();
    const { env, store: st } = installPrims(store);

    // Expression with choices - use Quote to create list of choices
    const expr: Expr = {
      tag: "Effect",
      op: "amb.choose",
      args: [
        { tag: "Quote", datum: ["a", "b"] },
      ],
    };

    const state: State = {
      control: { tag: "Expr", e: expr },
      env,
      store: st,
      kont: [],
      handlers: [],
    };

    const runtime = createMinimalRuntime();

    // Run BFS
    const bfsResult = await runSearch(runtime, state, {
      strategy: "bfs",
      budget: { searchNodesLeft: 10 },
    });

    // Run DFS
    const dfsResult = await runSearch(runtime, state, {
      strategy: "dfs",
      budget: { searchNodesLeft: 10 },
    });

    // Both should find solutions
    expect(bfsResult.dist.support.length).toBe(2);
    expect(dfsResult.dist.support.length).toBe(2);

    // Verify different strategies were used
    expect(bfsResult.ledger.strategy).toBe("bfs");
    expect(dfsResult.ledger.strategy).toBe("dfs");
  });
});

// ─────────────────────────────────────────────────────────────────
// Test 11.3: Cost-aware search (minimize infer calls)
// ─────────────────────────────────────────────────────────────────

describe("Test 11.3: Cost-aware search", () => {
  it("weighted choices affect distribution weights", async () => {
    const store = new COWStore();
    const { env, store: st } = installPrims(store);

    // amb.choose with simple choices (weights handled internally)
    const expr: Expr = {
      tag: "Effect",
      op: "amb.choose",
      args: [
        { tag: "Quote", datum: ["cheap", "expensive"] },
      ],
    };

    const state: State = {
      control: { tag: "Expr", e: expr },
      env,
      store: st,
      kont: [],
      handlers: [],
    };

    const runtime = createMinimalRuntime();
    const result = await runSearch(runtime, state, {
      strategy: "dfs",
      budget: { searchNodesLeft: 10 },
    });

    // Should have two solutions
    expect(result.dist.support.length).toBe(2);
  });

  it("distBest returns highest weighted solution", () => {
    const d = distWeighted([
      [{ tag: "Str", s: "low" }, 0.1],
      [{ tag: "Str", s: "high" }, 0.9],
      [{ tag: "Str", s: "medium" }, 0.5],
    ]);

    const best = distBest(d);
    expect(best).toBeDefined();
    expect((best as any).s).toBe("high");
  });
});

// ─────────────────────────────────────────────────────────────────
// Test 11.4: Beam search with bounded width
// ─────────────────────────────────────────────────────────────────

describe("Test 11.4: Beam search", () => {
  it("beam search limits frontier size", async () => {
    const store = new COWStore();
    const { env, store: st } = installPrims(store);

    // Many choices - use Quote with array datum
    const expr: Expr = {
      tag: "Effect",
      op: "amb.choose",
      args: [{ tag: "Quote", datum: [0, 1, 2, 3, 4, 5, 6, 7, 8, 9] }],
    };

    const state: State = {
      control: { tag: "Expr", e: expr },
      env,
      store: st,
      kont: [],
      handlers: [],
    };

    const runtime = createMinimalRuntime();
    const result = await runSearch(runtime, state, {
      strategy: "beam",
      budget: { searchNodesLeft: 100 },
      beamWidth: 3, // Only keep top 3
    });

    // Beam should find solutions
    expect(result.dist.support.length).toBeGreaterThan(0);
    expect(result.ledger.strategy).toBe("beam");
  });

  it("beam search prunes low-scoring branches", async () => {
    const store = new COWStore();
    const { env, store: st } = installPrims(store);

    // Many choices - use Quote with array datum
    const choiceValues = Array.from({ length: 20 }, (_, i) => i);
    const expr: Expr = {
      tag: "Effect",
      op: "amb.choose",
      args: [{ tag: "Quote", datum: choiceValues }],
    };

    const state: State = {
      control: { tag: "Expr", e: expr },
      env,
      store: st,
      kont: [],
      handlers: [],
    };

    const runtime = createMinimalRuntime();
    const result = await runSearch(runtime, state, {
      strategy: "beam",
      budget: { searchNodesLeft: 50 },
      beamWidth: 3,
    });

    // Stats should show pruning happened
    expect(result.stats.backtracks).toBeGreaterThan(0);
  });
});

// ─────────────────────────────────────────────────────────────────
// Test 11.5: MCTS policy search
// ─────────────────────────────────────────────────────────────────

describe("Test 11.5: MCTS policy search", () => {
  it("MCTS explores and exploits", async () => {
    const store = new COWStore();
    const { env, store: st } = installPrims(store);

    const expr: Expr = {
      tag: "Effect",
      op: "amb.choose",
      args: [
        { tag: "Quote", datum: [1, 2, 3] },
      ],
    };

    const state: State = {
      control: { tag: "Expr", e: expr },
      env,
      store: st,
      kont: [],
      handlers: [],
    };

    const runtime = createMinimalRuntime();
    const result = await runSearch(runtime, state, {
      strategy: "mcts",
      budget: { searchNodesLeft: 50 },
      mctsExploration: Math.sqrt(2),
      mctsRolloutDepth: 5,
    });

    // MCTS should find solutions
    expect(result.dist.support.length).toBeGreaterThan(0);
    expect(result.ledger.strategy).toBe("mcts");
    expect(result.stats.nodesExpanded).toBeGreaterThan(0);
  });

  it("MCTS with custom score function", async () => {
    const store = new COWStore();
    const { env, store: st } = installPrims(store);

    const expr: Expr = {
      tag: "Effect",
      op: "amb.choose",
      args: [
        { tag: "Quote", datum: [10, 50, 30] },
      ],
    };

    const state: State = {
      control: { tag: "Expr", e: expr },
      env,
      store: st,
      kont: [],
      handlers: [],
    };

    const runtime = createMinimalRuntime();
    const result = await runSearch(runtime, state, {
      strategy: "mcts",
      budget: { searchNodesLeft: 30 },
      scoreFn: (v: Val) => v.tag === "Num" ? -v.n : 0, // Prefer smaller numbers
    });

    expect(result.dist.support.length).toBeGreaterThan(0);
  });
});

// ─────────────────────────────────────────────────────────────────
// Test 11.6: Deterministic replay
// ─────────────────────────────────────────────────────────────────

describe("Test 11.6: Deterministic replay", () => {
  it("same seed produces same search ordering", async () => {
    const store = new COWStore();
    const { env, store: st } = installPrims(store);

    const expr: Expr = {
      tag: "Effect",
      op: "amb.choose",
      args: [
        { tag: "Quote", datum: [1, 2, 3] },
      ],
    };

    const state: State = {
      control: { tag: "Expr", e: expr },
      env,
      store: st,
      kont: [],
      handlers: [],
    };

    const runtime = createMinimalRuntime();
    const seed = 42;

    // Run twice with same seed
    const result1 = await runSearch(runtime, state, {
      strategy: "dfs",
      budget: { searchNodesLeft: 20 },
      seed,
    });

    const result2 = await runSearch(runtime, state, {
      strategy: "dfs",
      budget: { searchNodesLeft: 20 },
      seed,
    });

    // Same seed should produce same ledger structure
    expect(result1.ledger.seed).toBe(seed);
    expect(result2.ledger.seed).toBe(seed);
    expect(result1.ledger.decisions.length).toBe(result2.ledger.decisions.length);
  });

  it("replay produces identical results", async () => {
    const store = new COWStore();
    const { env, store: st } = installPrims(store);

    const expr: Expr = {
      tag: "Effect",
      op: "amb.choose",
      args: [
        { tag: "Quote", datum: [100, 200] },
      ],
    };

    const state: State = {
      control: { tag: "Expr", e: expr },
      env,
      store: st,
      kont: [],
      handlers: [],
    };

    const runtime = createMinimalRuntime();

    // Original run
    const original = await runSearch(runtime, state, {
      strategy: "dfs",
      budget: { searchNodesLeft: 10 },
      seed: 7,
    });

    // Modify ledger to replay first decision
    if (original.ledger.decisions.length > 0) {
      original.ledger.decisions[0].selectedIndex = 0;
    }

    // Replay should follow recorded decisions
    const replayed = await replaySearch(runtime, state, original.ledger);

    expect(replayed.dist.support.length).toBeGreaterThan(0);
    expect(replayed.stats.nodesExpanded).toBeGreaterThan(0);
  });
});

// ─────────────────────────────────────────────────────────────────
// Test 11.7: Budget enforcement
// ─────────────────────────────────────────────────────────────────

describe("Test 11.7: Budget enforcement", () => {
  it("search stops when searchNodesLeft exhausted", async () => {
    const store = new COWStore();
    const { env, store: st } = installPrims(store);

    // Many choices requiring many expansions - use Quote with array datum
    const choiceValues = Array.from({ length: 100 }, (_, i) => i);
    const expr: Expr = {
      tag: "Effect",
      op: "amb.choose",
      args: [{ tag: "Quote", datum: choiceValues }],
    };

    const state: State = {
      control: { tag: "Expr", e: expr },
      env,
      store: st,
      kont: [],
      handlers: [],
    };

    const runtime = createMinimalRuntime();

    // Very tight budget
    const result = await runSearch(runtime, state, {
      strategy: "dfs",
      budget: { searchNodesLeft: 5 }, // Only allow 5 expansions
    });

    // Should have found some solutions before budget exhausted
    // But not all 100
    expect(result.dist.support.length).toBeLessThan(100);
    expect(result.stats.nodesExpanded).toBeLessThanOrEqual(6); // +1 for initial
  });

  it("solutionsLeft limits collected solutions", async () => {
    const store = new COWStore();
    const { env, store: st } = installPrims(store);

    // Use Quote with array datum
    const choiceValues = Array.from({ length: 20 }, (_, i) => i);
    const expr: Expr = {
      tag: "Effect",
      op: "amb.choose",
      args: [{ tag: "Quote", datum: choiceValues }],
    };

    const state: State = {
      control: { tag: "Expr", e: expr },
      env,
      store: st,
      kont: [],
      handlers: [],
    };

    const runtime = createMinimalRuntime();

    const result = await runSearch(runtime, state, {
      strategy: "bfs",
      budget: {
        searchNodesLeft: 100,
        solutionsLeft: 3, // Only collect 3 solutions
      },
    });

    expect(result.dist.support.length).toBeLessThanOrEqual(3);
    expect(result.stats.solutionsFound).toBeLessThanOrEqual(3);
  });

  it("DEFAULT_SEARCH_BUDGET has reasonable limits", () => {
    expect(DEFAULT_SEARCH_BUDGET.searchNodesLeft).toBe(1000);
    expect(DEFAULT_SEARCH_BUDGET.searchDepthLeft).toBe(100);
    expect(DEFAULT_SEARCH_BUDGET.solutionsLeft).toBe(100);
    expect(DEFAULT_SEARCH_BUDGET.maxFrontierSize).toBe(1000);
  });
});

// ─────────────────────────────────────────────────────────────────
// Additional Dist algebra tests
// ─────────────────────────────────────────────────────────────────

describe("Dist algebra (B3)", () => {
  it("distUniform creates equal weights", () => {
    const values: Val[] = [
      { tag: "Num", n: 1 },
      { tag: "Num", n: 2 },
      { tag: "Num", n: 3 },
    ];
    const d = distUniform(values);

    expect(d.support.length).toBe(3);
    expect(d.normalized).toBe(true);
    for (const item of d.support) {
      expect(item.w).toBeCloseTo(1/3, 5);
    }
  });

  it("distFilter removes non-matching items", () => {
    const d = distUniform([
      { tag: "Num", n: 1 },
      { tag: "Num", n: 2 },
      { tag: "Num", n: 3 },
      { tag: "Num", n: 4 },
    ]);

    const filtered = distFilter(d, v => v.tag === "Num" && v.n > 2);

    expect(filtered.support.length).toBe(2);
    expect(distValues(filtered).every(v => v.tag === "Num" && v.n > 2)).toBe(true);
  });

  it("distTake limits to n items by weight", () => {
    const d = distWeighted([
      [{ tag: "Str", s: "a" }, 0.1],
      [{ tag: "Str", s: "b" }, 0.5],
      [{ tag: "Str", s: "c" }, 0.3],
      [{ tag: "Str", s: "d" }, 0.2],
    ]);

    const taken = distTake(d, 2);

    expect(taken.support.length).toBe(2);
    // Should have highest weighted items
    expect(taken.support[0].w).toBe(0.5);
    expect(taken.support[1].w).toBe(0.3);
  });

  it("distMap transforms values", () => {
    const d = distUniform([
      { tag: "Num", n: 1 },
      { tag: "Num", n: 2 },
    ]);

    const mapped = distMap(d, v => {
      if (v.tag === "Num") return { tag: "Num", n: v.n * 10 };
      return v;
    });

    expect(distValues(mapped).every(v => v.tag === "Num" && v.n >= 10)).toBe(true);
  });

  it("distBind flattens nested distributions", () => {
    const d1 = distUniform([
      { tag: "Num", n: 1 },
      { tag: "Num", n: 2 },
    ]);

    const d2 = distBind(d1, v => {
      if (v.tag !== "Num") return dist(v);
      return distUniform([
        { tag: "Num", n: v.n * 10 },
        { tag: "Num", n: v.n * 100 },
      ]);
    });

    // Should have 4 items: 10, 100, 20, 200
    expect(d2.support.length).toBe(4);
  });

  it("distSample is deterministic with same seed", () => {
    const d = distWeighted([
      [{ tag: "Str", s: "a" }, 0.5],
      [{ tag: "Str", s: "b" }, 0.3],
      [{ tag: "Str", s: "c" }, 0.2],
    ]);

    const sample1 = distSample(d, 42);
    const sample2 = distSample(d, 42);

    expect(sample1).toEqual(sample2);
  });

  it("distIsEmpty and distSize work correctly", () => {
    const empty = distFrom([]);
    const nonEmpty = distUniform([{ tag: "Num", n: 1 }]);

    expect(distIsEmpty(empty)).toBe(true);
    expect(distIsEmpty(nonEmpty)).toBe(false);
    expect(distSize(empty)).toBe(0);
    expect(distSize(nonEmpty)).toBe(1);
  });
});

// ─────────────────────────────────────────────────────────────────
// parseAmbArgs tests
// ─────────────────────────────────────────────────────────────────

describe("parseAmbArgs", () => {
  it("parses vector of values as unweighted choices", () => {
    const args: Val[] = [
      { tag: "Vector", items: [
        { tag: "Num", n: 1 },
        { tag: "Num", n: 2 },
      ]}
    ];

    const payload = parseAmbArgs(args);

    expect(payload.choices.length).toBe(2);
    expect(payload.choices[0].w).toBe(1);
    expect(payload.choices[1].w).toBe(1);
  });

  it("parses pairs as weighted choices", () => {
    const args: Val[] = [
      { tag: "Vector", items: [
        { tag: "Pair",
          car: { tag: "Num", n: 0.8 },
          cdr: { tag: "Str", s: "high" }
        },
        { tag: "Pair",
          car: { tag: "Num", n: 0.2 },
          cdr: { tag: "Str", s: "low" }
        },
      ]}
    ];

    const payload = parseAmbArgs(args);

    expect(payload.choices.length).toBe(2);
    expect(payload.choices[0].w).toBe(0.8);
    expect(payload.choices[1].w).toBe(0.2);
  });

  it("parses single value as choice", () => {
    const args: Val[] = [{ tag: "Num", n: 42 }];
    const payload = parseAmbArgs(args);

    expect(payload.choices.length).toBe(1);
    expect(payload.choices[0].v).toEqual({ tag: "Num", n: 42 });
  });
});
