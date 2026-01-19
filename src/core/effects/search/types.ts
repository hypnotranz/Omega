// src/core/effects/search/types.ts
// SOURCE: ARCHITECTURE/32-LANGUAGE-OFFICIAL-13-IMPLEMENTATION-16.md
// Prompt 11: Search types for amb.choose and nondeterministic evaluation

import type { Val } from "../../eval/values";
import type { State } from "../../eval/machine";
import type { Resumption } from "../opcall";
import type { DistVal, DistItem } from "../../eval/dist";
import type { Hash } from "../../artifacts/hash";

// ─────────────────────────────────────────────────────────────────
// A2) amb.choose payload
// ─────────────────────────────────────────────────────────────────

/**
 * Weighted choice for amb.choose.
 * Weight defaults to 1 if not specified.
 */
export type WeightedChoice = {
  v: Val;
  w: number;
};

/**
 * AmbPayload: payload for the amb.choose effect.
 */
export type AmbPayload = {
  /** Choices with optional weights (default weight 1) */
  choices: WeightedChoice[];
  /** Callsite label for replay determinism */
  tag?: string;
  /** Optional metadata for scoring/heuristics */
  meta?: {
    heuristic?: (v: Val) => number;
    costEstimate?: number;
  };
};

/**
 * Convert cons-cell list to flat array.
 * A cons-cell is a 2-element Vector where items[1] is another cons or Unit.
 */
function consToArray(v: Val): Val[] {
  const result: Val[] = [];
  let cur: Val = v;
  while (cur.tag === "Vector" && cur.items.length === 2) {
    result.push(cur.items[0]);
    cur = cur.items[1];
  }
  // If terminated by Unit, it's a proper list
  // If not, the last element is the improper tail (ignore for now)
  return result;
}

/**
 * Check if value is a cons-cell list (Vector with 2 items where items[1] is cons or Unit)
 */
function isConsList(v: Val): boolean {
  if (v.tag !== "Vector") return false;
  if (v.items.length !== 2) return false;
  // Check if it terminates in Unit (proper list) or is a chain
  let cur: Val = v;
  while (cur.tag === "Vector" && cur.items.length === 2) {
    cur = cur.items[1];
  }
  return cur.tag === "Unit";
}

/**
 * Parse amb arguments into AmbPayload.
 * Supports both weighted and unweighted choices.
 * Handles both flat Vectors and cons-cell lists.
 */
export function parseAmbArgs(args: Val[]): AmbPayload {
  const choices: WeightedChoice[] = [];
  let tag: string | undefined;

  for (const arg of args) {
    if (arg.tag === "Vector") {
      // Check if it's a cons-cell list (from datumToVal of arrays)
      if (isConsList(arg)) {
        const items = consToArray(arg);
        for (const item of items) {
          if (item.tag === "Pair" && item.car.tag === "Num") {
            // Weighted: (weight . value)
            choices.push({ v: item.cdr, w: item.car.n });
          } else {
            choices.push({ v: item, w: 1 });
          }
        }
      } else {
        // Flat vector of choices
        for (const item of arg.items) {
          if (item.tag === "Pair" && item.car.tag === "Num") {
            // Weighted: (weight . value)
            choices.push({ v: item.cdr, w: item.car.n });
          } else {
            choices.push({ v: item, w: 1 });
          }
        }
      }
    } else if (arg.tag === "Map") {
      // Map with special keys
      for (const [k, v] of arg.entries) {
        if (k.tag === "Sym" && k.name === "tag" && v.tag === "Str") {
          tag = v.s;
        } else if (k.tag === "Sym" && k.name === "choices" && v.tag === "Vector") {
          const items = isConsList(v) ? consToArray(v) : v.items;
          for (const item of items) {
            choices.push({ v: item, w: 1 });
          }
        }
      }
    } else {
      // Single choice
      choices.push({ v: arg, w: 1 });
    }
  }

  return { choices, tag };
}

// ─────────────────────────────────────────────────────────────────
// A6) Search budget extensions
// ─────────────────────────────────────────────────────────────────

/**
 * SearchBudget: runtime limits specific to search.
 */
export type SearchBudget = {
  /** Maximum branch expansions */
  searchNodesLeft: number;
  /** Maximum search depth */
  searchDepthLeft: number;
  /** Maximum solutions to collect */
  solutionsLeft: number;
  /** Maximum active frontier size (for beam) */
  maxFrontierSize: number;
};

/**
 * Default search budget.
 */
export const DEFAULT_SEARCH_BUDGET: SearchBudget = {
  searchNodesLeft: 1000,
  searchDepthLeft: 100,
  solutionsLeft: 100,
  maxFrontierSize: 1000,
};

/**
 * BudgetExceeded error for search.
 */
export class SearchBudgetExceeded extends Error {
  constructor(
    public readonly kind: keyof SearchBudget,
    public readonly limit: number
  ) {
    super(`SearchBudgetExceeded: ${kind} limit (${limit}) exceeded`);
    this.name = "SearchBudgetExceeded";
  }
}

// ─────────────────────────────────────────────────────────────────
// Search strategies
// ─────────────────────────────────────────────────────────────────

/**
 * Search strategy type.
 */
export type SearchStrategy = "dfs" | "bfs" | "beam" | "mcts";

/**
 * SearchConfig: configuration for with-search.
 */
export type SearchConfig = {
  strategy: SearchStrategy;
  budget: Partial<SearchBudget>;
  /** Seed for deterministic replay */
  seed?: number;
  /** Beam width (for beam strategy) */
  beamWidth?: number;
  /** MCTS exploration constant */
  mctsExploration?: number;
  /** MCTS rollout depth */
  mctsRolloutDepth?: number;
  /** Score function for beam/mcts */
  scoreFn?: (val: Val) => number;
};

// ─────────────────────────────────────────────────────────────────
// Search job and frontier
// ─────────────────────────────────────────────────────────────────

/**
 * SearchJob: a branch in the search tree.
 */
export type SearchJob = {
  /** Unique job ID */
  id: string;
  /** Machine state for this branch */
  state: State;
  /** Depth in search tree */
  depth: number;
  /** Accumulated cost/weight */
  cost: number;
  /** Parent job ID (for tree reconstruction) */
  parentId?: string;
  /** Choice index that led here */
  choiceIndex?: number;
  /** Heuristic score (for beam/mcts) */
  score?: number;
};

/**
 * SearchDecision: recorded decision for replay.
 */
export type SearchDecision = {
  /** Callsite tag */
  tag?: string;
  /** Choice digests (for privacy) */
  choiceDigests: string[];
  /** Selected index */
  selectedIndex: number;
  /** Timestamp */
  timestamp: number;
};

/**
 * SearchLedger: records all decisions for replay.
 */
export type SearchLedger = {
  decisions: SearchDecision[];
  seed: number;
  strategy: SearchStrategy;
};

// ─────────────────────────────────────────────────────────────────
// Search result
// ─────────────────────────────────────────────────────────────────

/**
 * SearchResult: the output of with-search.
 */
export type SearchResult = {
  /** Distribution of solutions (DistVal) */
  dist: DistVal;
  /** Ledger for replay */
  ledger: SearchLedger;
  /** Statistics */
  stats: SearchStats;
};

/**
 * SearchStats: statistics about the search.
 */
export type SearchStats = {
  /** Total nodes expanded */
  nodesExpanded: number;
  /** Maximum depth reached */
  maxDepthReached: number;
  /** Solutions found */
  solutionsFound: number;
  /** Backtracks (failed branches) */
  backtracks: number;
  /** Time elapsed (ms) */
  elapsedMs: number;
};

// ─────────────────────────────────────────────────────────────────
// MCTS node (for MCTS strategy)
// ─────────────────────────────────────────────────────────────────

/**
 * MCTSNode: node in the MCTS tree.
 */
export type MCTSNode = {
  /** Node ID */
  id: string;
  /** Parent node ID */
  parentId?: string;
  /** State at this node */
  state: State;
  /** Visit count */
  visits: number;
  /** Total reward from this node */
  totalReward: number;
  /** Children keyed by choice index */
  children: Map<number, MCTSNode>;
  /** Is this a terminal node? */
  terminal: boolean;
  /** Terminal value (if terminal) */
  terminalValue?: Val;
  /** Unexplored actions */
  unexploredActions: number[];
};

/**
 * Create a new MCTS node.
 */
export function createMCTSNode(
  id: string,
  state: State,
  parentId?: string
): MCTSNode {
  return {
    id,
    parentId,
    state,
    visits: 0,
    totalReward: 0,
    children: new Map(),
    terminal: false,
    unexploredActions: [],
  };
}

/**
 * UCB1 selection formula.
 */
export function ucb1(
  node: MCTSNode,
  parentVisits: number,
  exploration: number = Math.sqrt(2)
): number {
  if (node.visits === 0) return Infinity;
  const exploitation = node.totalReward / node.visits;
  const exploration_term = exploration * Math.sqrt(Math.log(parentVisits) / node.visits);
  return exploitation + exploration_term;
}
