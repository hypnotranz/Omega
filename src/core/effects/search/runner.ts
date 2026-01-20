// src/core/effects/search/runner.ts
// SOURCE: ARCHITECTURE/32-LANGUAGE-OFFICIAL-13-IMPLEMENTATION-16.md
// Prompt 11: Search runner with DFS/BFS/Beam/MCTS strategies

import type { Runtime } from "../../eval/runtime";
import type { State } from "../../eval/machine";
import type { Val } from "../../eval/values";
import { VUnit } from "../../eval/values";
import { stepOnce } from "../../eval/machineStep";
import { dist, distFrom, type DistVal, type DistItem } from "../../eval/dist";
import { sha256JSON } from "../../artifacts/hash";
import {
  type SearchConfig,
  type SearchBudget,
  type SearchJob,
  type SearchDecision,
  type SearchLedger,
  type SearchResult,
  type SearchStats,
  type WeightedChoice,
  type MCTSNode,
  DEFAULT_SEARCH_BUDGET,
  SearchBudgetExceeded,
  parseAmbArgs,
  createMCTSNode,
  ucb1,
} from "./types";

// ─────────────────────────────────────────────────────────────────
// Deterministic PRNG for reproducible search
// ─────────────────────────────────────────────────────────────────

function mulberry32(seed: number): () => number {
  let t = seed >>> 0;
  return () => {
    t += 0x6D2B79F5;
    let x = t;
    x = Math.imul(x ^ (x >>> 15), x | 1);
    x ^= x + Math.imul(x ^ (x >>> 7), x | 61);
    return ((x ^ (x >>> 14)) >>> 0) / 4294967296;
  };
}

// ─────────────────────────────────────────────────────────────────
// Search runner
// ─────────────────────────────────────────────────────────────────

let jobCounter = 0;

function makeJobId(): string {
  return `job_${++jobCounter}`;
}

function isVectorVal(v: Val): v is { tag: "Vector"; items: Val[] } {
  return v.tag === "Vector";
}

function valDigest(v: Val): string {
  try {
    return sha256JSON(v).slice(0, 16);
  } catch {
    return `tag:${v.tag}`;
  }
}

/**
 * Run search with the specified configuration.
 * Returns a DistVal of solutions with search statistics.
 */
export async function runSearch(
  runtime: Runtime,
  initial: State,
  config: SearchConfig
): Promise<SearchResult> {
  const startTime = Date.now();
  const seed = config.seed ?? Date.now();
  const rng = mulberry32(seed);

  // Initialize budget
  const budget: SearchBudget = {
    ...DEFAULT_SEARCH_BUDGET,
    ...config.budget,
  };

  // Initialize ledger
  const ledger: SearchLedger = {
    decisions: [],
    seed,
    strategy: config.strategy,
  };

  // Initialize stats
  const stats: SearchStats = {
    nodesExpanded: 0,
    maxDepthReached: 0,
    solutionsFound: 0,
    backtracks: 0,
    elapsedMs: 0,
  };

  // Solutions with weights
  const solutions: DistItem[] = [];

  // Run appropriate strategy
  let dist: DistVal;

  switch (config.strategy) {
    case "dfs":
      dist = await runDFS(runtime, initial, budget, ledger, stats, solutions, config);
      break;
    case "bfs":
      dist = await runBFS(runtime, initial, budget, ledger, stats, solutions, config);
      break;
    case "beam":
      dist = await runBeam(runtime, initial, budget, ledger, stats, solutions, config, rng);
      break;
    case "mcts":
      dist = await runMCTS(runtime, initial, budget, ledger, stats, solutions, config, rng);
      break;
    default:
      throw new Error(`Unknown search strategy: ${config.strategy}`);
  }

  stats.elapsedMs = Date.now() - startTime;

  return { dist, ledger, stats };
}

// ─────────────────────────────────────────────────────────────────
// C1) DFS - Depth-First Search (stack frontier)
// ─────────────────────────────────────────────────────────────────

async function runDFS(
  runtime: Runtime,
  initial: State,
  budget: SearchBudget,
  ledger: SearchLedger,
  stats: SearchStats,
  solutions: DistItem[],
  config: SearchConfig
): Promise<DistVal> {
  const stack: SearchJob[] = [{
    id: makeJobId(),
    state: initial,
    depth: 0,
    cost: 0,
  }];

  while (stack.length > 0 && budget.searchNodesLeft > 0 && budget.solutionsLeft > 0) {
    const job = stack.pop()!;

    // Consume budget for each job processed (node expansion)
    budget.searchNodesLeft--;
    stats.nodesExpanded++;

    stats.maxDepthReached = Math.max(stats.maxDepthReached, job.depth);

    if (job.depth > budget.searchDepthLeft) {
      stats.backtracks++;
      continue;
    }

    const result = await runJobToChoiceNoBudget(runtime, job, ledger);

    if (result.tag === "done") {
      solutions.push({ v: result.value, w: 1 / (1 + job.cost) });
      stats.solutionsFound++;
      budget.solutionsLeft--;
    } else if (result.tag === "choice") {
      // DFS: push children in reverse order (so first choice is popped first)
      for (let i = result.children.length - 1; i >= 0; i--) {
        stack.push(result.children[i]);
      }
    } else if (result.tag === "fail") {
      stats.backtracks++;
    }
  }

  return distFrom(solutions, { kind: "dfs-search" });
}

// ─────────────────────────────────────────────────────────────────
// C2) BFS - Breadth-First Search (queue frontier)
// ─────────────────────────────────────────────────────────────────

async function runBFS(
  runtime: Runtime,
  initial: State,
  budget: SearchBudget,
  ledger: SearchLedger,
  stats: SearchStats,
  solutions: DistItem[],
  config: SearchConfig
): Promise<DistVal> {
  const queue: SearchJob[] = [{
    id: makeJobId(),
    state: initial,
    depth: 0,
    cost: 0,
  }];

  while (queue.length > 0 && budget.searchNodesLeft > 0 && budget.solutionsLeft > 0) {
    const job = queue.shift()!;

    // Consume budget for each job processed (node expansion)
    budget.searchNodesLeft--;
    stats.nodesExpanded++;

    stats.maxDepthReached = Math.max(stats.maxDepthReached, job.depth);

    if (job.depth > budget.searchDepthLeft) {
      stats.backtracks++;
      continue;
    }

    const result = await runJobToChoiceNoBudget(runtime, job, ledger);

    if (result.tag === "done") {
      solutions.push({ v: result.value, w: 1 / (1 + job.cost) });
      stats.solutionsFound++;
      budget.solutionsLeft--;
    } else if (result.tag === "choice") {
      // BFS: append children to queue
      for (const child of result.children) {
        queue.push(child);
      }
    } else if (result.tag === "fail") {
      stats.backtracks++;
    }
  }

  return distFrom(solutions, { kind: "bfs-search" });
}

// ─────────────────────────────────────────────────────────────────
// C3) Beam Search (priority queue with bounded width)
// ─────────────────────────────────────────────────────────────────

async function runBeam(
  runtime: Runtime,
  initial: State,
  budget: SearchBudget,
  ledger: SearchLedger,
  stats: SearchStats,
  solutions: DistItem[],
  config: SearchConfig,
  rng: () => number
): Promise<DistVal> {
  const beamWidth = config.beamWidth ?? 3;
  const scoreFn = config.scoreFn ?? (() => 0);

  let frontier: SearchJob[] = [{
    id: makeJobId(),
    state: initial,
    depth: 0,
    cost: 0,
    score: 0,
  }];

  while (frontier.length > 0 && budget.searchNodesLeft > 0 && budget.solutionsLeft > 0) {
    const nextFrontier: SearchJob[] = [];

    for (const job of frontier) {
      if (budget.searchNodesLeft <= 0 || budget.solutionsLeft <= 0) break;

      // Consume budget for each job processed (node expansion)
      budget.searchNodesLeft--;
      stats.nodesExpanded++;

      stats.maxDepthReached = Math.max(stats.maxDepthReached, job.depth);

      if (job.depth > budget.searchDepthLeft) {
        stats.backtracks++;
        continue;
      }

      const result = await runJobToChoiceNoBudget(runtime, job, ledger);

      if (result.tag === "done") {
        const score = scoreFn(result.value);
        solutions.push({ v: result.value, w: Math.exp(score) });
        stats.solutionsFound++;
        budget.solutionsLeft--;
      } else if (result.tag === "choice") {
        // Score children and add to next frontier
        for (const child of result.children) {
          child.score = child.cost; // Can be improved with heuristic
          nextFrontier.push(child);
        }
      } else if (result.tag === "fail") {
        stats.backtracks++;
      }
    }

    // Beam: keep only top-k by score (lower cost = better)
    nextFrontier.sort((a, b) => (a.score ?? 0) - (b.score ?? 0));
    frontier = nextFrontier.slice(0, beamWidth);

    // Record pruned jobs
    if (nextFrontier.length > beamWidth) {
      stats.backtracks += nextFrontier.length - beamWidth;
    }
  }

  return distFrom(solutions, { kind: "beam-search" });
}

// ─────────────────────────────────────────────────────────────────
// C4) MCTS - Monte Carlo Tree Search (simplified: UCB1 selection on priority queue)
// ─────────────────────────────────────────────────────────────────

async function runMCTS(
  runtime: Runtime,
  initial: State,
  budget: SearchBudget,
  ledger: SearchLedger,
  stats: SearchStats,
  solutions: DistItem[],
  config: SearchConfig,
  rng: () => number
): Promise<DistVal> {
  const exploration = config.mctsExploration ?? Math.sqrt(2);
  const scoreFn = config.scoreFn ?? (() => 0);

  // Track visit counts for UCB1 selection
  const visitCounts = new Map<string, number>();
  const totalRewards = new Map<string, number>();
  let totalVisits = 0;

  // Priority queue with UCB1-based selection
  const frontier: SearchJob[] = [{
    id: makeJobId(),
    state: initial,
    depth: 0,
    cost: 0,
  }];

  while (frontier.length > 0 && budget.searchNodesLeft > 0 && budget.solutionsLeft > 0) {
    // Select job using UCB1 (or random if no visits)
    totalVisits++;
    let selectedIdx = 0;

    if (totalVisits > frontier.length) {
      // UCB1 selection
      let bestUCB = -Infinity;
      for (let i = 0; i < frontier.length; i++) {
        const job = frontier[i];
        const visits = visitCounts.get(job.id) ?? 1;
        const reward = totalRewards.get(job.id) ?? 0;
        const exploit = reward / visits;
        const explore = exploration * Math.sqrt(Math.log(totalVisits) / visits);
        const ucb = exploit + explore;

        if (ucb > bestUCB) {
          bestUCB = ucb;
          selectedIdx = i;
        }
      }
    } else {
      // Random selection during initial exploration
      selectedIdx = Math.floor(rng() * frontier.length);
    }

    const job = frontier.splice(selectedIdx, 1)[0];

    // Consume budget
    budget.searchNodesLeft--;
    stats.nodesExpanded++;

    stats.maxDepthReached = Math.max(stats.maxDepthReached, job.depth);

    if (job.depth > budget.searchDepthLeft) {
      stats.backtracks++;
      continue;
    }

    const result = await runJobToChoiceNoBudget(runtime, job, ledger);

    if (result.tag === "done") {
      const score = scoreFn(result.value);
      solutions.push({ v: result.value, w: Math.exp(score) });
      stats.solutionsFound++;
      budget.solutionsLeft--;

      // Update visit counts and rewards for backpropagation
      const visits = visitCounts.get(job.id) ?? 0;
      visitCounts.set(job.id, visits + 1);
      const reward = totalRewards.get(job.id) ?? 0;
      totalRewards.set(job.id, reward + score);
    } else if (result.tag === "choice") {
      // Add children to frontier
      for (const child of result.children) {
        frontier.push(child);
        // Initialize visit counts
        visitCounts.set(child.id, 1);
        totalRewards.set(child.id, 0);
      }
    } else if (result.tag === "fail") {
      stats.backtracks++;
      // Negative reward for failed branches
      const visits = visitCounts.get(job.id) ?? 0;
      visitCounts.set(job.id, visits + 1);
      const reward = totalRewards.get(job.id) ?? 0;
      totalRewards.set(job.id, reward - 1);
    }
  }

  return distFrom(solutions, { kind: "mcts-search" });
}

// ─────────────────────────────────────────────────────────────────
// Core: run a job until it hits a choice point, terminal, or error
// ─────────────────────────────────────────────────────────────────

type JobResult =
  | { tag: "done"; value: Val }
  | { tag: "choice"; choices: WeightedChoice[]; children: SearchJob[]; ambTag?: string }
  | { tag: "fail"; reason: string };

const MAX_STEPS_PER_JOB = 10000;

/**
 * Run a job until it hits a choice point, terminal, or error.
 * Does not consume budget internally - budget tracking happens in the main loop.
 */
async function runJobToChoiceNoBudget(
  runtime: Runtime,
  job: SearchJob,
  ledger: SearchLedger
): Promise<JobResult> {
  let st = job.state;
  let steps = 0;

  while (steps < MAX_STEPS_PER_JOB) {
    steps++;

    try {
      const out = stepOnce(st);

      if (out.tag === "State") {
        st = out.state;
        continue;
      }

      if (out.tag === "Done") {
        return { tag: "done", value: out.value };
      }

      if (out.tag === "Op") {
        const op = out.opcall.op;

        // Handle amb.choose effect
        if (op === "amb.choose" || op === "amb.op") {
          // Parse choices
          const payload = parseAmbArgs(out.opcall.args);
          const choices = payload.choices;

          if (choices.length === 0) {
            return { tag: "fail", reason: "amb with no choices" };
          }

          // Record decision for replay
          const decision: SearchDecision = {
            tag: payload.tag,
            choiceDigests: choices.map(c => valDigest(c.v)),
            selectedIndex: -1, // Will be set by strategy
            timestamp: Date.now(),
          };
          ledger.decisions.push(decision);

          // Create child jobs for each choice
          const children: SearchJob[] = choices.map((choice, idx) => {
            const cv = choice.v;
            let childState: State;

            if (cv.tag === "Closure") {
              if (cv.params.length !== 0) {
                throw new Error("amb.choose: thunk closure must take no arguments");
              }
              // Resume continuation, then evaluate thunk body in its lexical env
              const baseState = out.opcall.resumption.invoke(VUnit);
              childState = { ...baseState, control: { tag: "Expr", e: cv.body }, env: cv.env };
            } else {
              childState = out.opcall.resumption.invoke(cv);
            }
            return {
              id: makeJobId(),
              state: childState,
              depth: job.depth + 1,
              cost: job.cost + (1 - choice.w), // Higher weight = lower cost
              parentId: job.id,
              choiceIndex: idx,
            };
          });

          return { tag: "choice", choices, children, ambTag: payload.tag };
        }

        // Handle amb.fail effect
        if (op === "amb.fail") {
          return { tag: "fail", reason: "explicit fail" };
        }

        // Dispatch other effects to runtime
        const handled = await runtime.dispatch(out.state, out.opcall);
        if (handled === "Uncaught") {
          return { tag: "fail", reason: `Uncaught op: ${op}` };
        }
        st = handled;
        continue;
      }
    } catch (e) {
      if (e instanceof SearchBudgetExceeded) {
        throw e;
      }
      return { tag: "fail", reason: String(e) };
    }
  }

  return { tag: "fail", reason: "max steps exceeded" };
}

// ─────────────────────────────────────────────────────────────────
// A5) Deterministic replay
// ─────────────────────────────────────────────────────────────────

/**
 * Replay a search from a recorded ledger.
 * Uses the same decisions to produce identical results.
 */
export async function replaySearch(
  runtime: Runtime,
  initial: State,
  ledger: SearchLedger
): Promise<SearchResult> {
  const startTime = Date.now();
  const budget = { ...DEFAULT_SEARCH_BUDGET };
  const stats: SearchStats = {
    nodesExpanded: 0,
    maxDepthReached: 0,
    solutionsFound: 0,
    backtracks: 0,
    elapsedMs: 0,
  };

  const solutions: DistItem[] = [];
  let decisionIdx = 0;

  // Simple replay: follow the recorded decisions
  let st = initial;
  let steps = 0;

  while (steps < MAX_STEPS_PER_JOB * 100) {
    steps++;

    try {
      const out = stepOnce(st);

      if (out.tag === "State") {
        st = out.state;
        continue;
      }

      if (out.tag === "Done") {
        solutions.push({ v: out.value, w: 1 });
        stats.solutionsFound++;
        break;
      }

      if (out.tag === "Op") {
        const op = out.opcall.op;

        if (op === "amb.choose" || op === "amb.op") {
          if (decisionIdx >= ledger.decisions.length) {
            throw new Error("Replay exhausted: no more recorded decisions");
          }

          const decision = ledger.decisions[decisionIdx++];
          const payload = parseAmbArgs(out.opcall.args);

          if (decision.selectedIndex < 0 || decision.selectedIndex >= payload.choices.length) {
            throw new Error(`Replay error: invalid choice index ${decision.selectedIndex}`);
          }

          const choice = payload.choices[decision.selectedIndex];
          st = out.opcall.resumption.invoke(choice.v);
          stats.nodesExpanded++;
          continue;
        }

        if (op === "amb.fail") {
          stats.backtracks++;
          break;
        }

        const handled = await runtime.dispatch(out.state, out.opcall);
        if (handled === "Uncaught") {
          throw new Error(`Uncaught op during replay: ${op}`);
        }
        st = handled;
        continue;
      }
    } catch (e) {
      throw new Error(`Replay failed: ${e}`);
    }
  }

  stats.elapsedMs = Date.now() - startTime;

  return {
    dist: distFrom(solutions, { kind: "replay" }),
    ledger,
    stats,
  };
}
