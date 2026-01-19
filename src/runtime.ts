// src/runtime.ts
// OmegaRuntime - Clean API for external integrations
//
// Usage:
//   import { OmegaRuntime, createAnthropicAdapter } from "omega-llm";
//
//   const omega = new OmegaRuntime();
//   const result = await omega.eval("(+ 1 2)");
//   console.log(result.value); // 3

import type { Val } from "./core/eval/values";
import type { OracleAdapter } from "./core/oracle/adapter";
import type { MeaningVal } from "./core/oracle/meaning";
import type { Ctx } from "./core/ctx/ctx";
import type { CapSet } from "./core/governance/caps";
import type { BudgetLimits } from "./core/governance/budgets";
import type { Profile } from "./core/governance/profile";
import type { State } from "./core/eval/machine";
import type { Expr } from "./core/ast";
import type { DistVal } from "./core/eval/dist";

import { ctxRootFromProfile, ctxSeal, ctxApplyProfile } from "./core/ctx/ctx";
import { DEFAULT_PROFILE, PROFILE_SPECULATIVE, PROFILE_TEST_CERTIFIED, PROFILE_PROOF_CERTIFIED } from "./core/governance/profile";
import { COWStore } from "./core/eval/store";
import { installPrims } from "./core/prims";
import { runToCompletion } from "./core/eval/run";
import { RuntimeImpl } from "./core/effects/runtimeImpl";
import { compileTextToExpr } from "./core/pipeline/compileText";
import { DepthTrackingAdapter } from "./core/oracle/adapters/types";
import { SnapshotRepo } from "./core/oracle/snapshots";
import { InMemoryReceiptStore } from "./core/oracle/receipts";

/**
 * Configuration for OmegaRuntime
 */
export type OmegaConfig = {
  /** Oracle adapter for LLM calls (optional - without it, infer.op will fail) */
  adapter?: OracleAdapter;

  /** Maximum Oracle recursion depth (default: 4) */
  maxOracleDepth?: number;

  /** Default capability set for new contexts */
  defaultCaps?: CapSet;

  /** Default budget limits for new contexts */
  defaultBudgets?: BudgetLimits;

  /** Truth regime profile (default: PROFILE_SPECULATIVE) */
  profile?: Profile;

  /** Maximum evaluation steps (default: 500000) */
  maxSteps?: number;
};

/**
 * Result of an eval operation
 */
export type EvalResult = {
  /** Whether evaluation succeeded */
  ok: boolean;

  /** The result value (if ok) */
  value?: Val;

  /** Error message (if not ok) */
  error?: string;
};

/**
 * Result of an infer operation
 */
export type InferResult = {
  /** Whether inference succeeded */
  ok: boolean;

  /** The Meaning returned by Oracle */
  meaning?: MeaningVal;

  /** Just the denotation (convenience accessor) */
  value?: Val;

  /** Confidence score (0-1) */
  confidence?: number;

  /** Error message (if not ok) */
  error?: string;
};

// Mock commit adapter for standalone runtime
const mockCommitAdapter = {
  async commit(_payload: Val, _ctxDigest: string): Promise<Val> {
    return { tag: "Str", s: "commit:" + Math.random().toString(16).slice(2) } as Val;
  }
};

// installPrims is now imported from ./core/prims (117 production primitives)

/**
 * OmegaRuntime - Main entry point for external integrations
 *
 * Provides a clean, promise-based API for:
 * - Evaluating Lisp expressions
 * - Running Oracle-powered inference
 * - Managing contexts with governance
 * - Integrating with VS Code, CLI tools, etc.
 */
export class OmegaRuntime {
  private adapter?: OracleAdapter;
  private config: OmegaConfig;
  private rootCtx: Ctx;
  private maxSteps: number;

  constructor(config: OmegaConfig = {}) {
    this.config = config;
    this.maxSteps = config.maxSteps ?? 500_000;

    const profile = config.profile ?? DEFAULT_PROFILE;
    this.rootCtx = ctxRootFromProfile(profile);

    // Wrap adapter with depth tracking if provided
    if (config.adapter) {
      this.adapter = new DepthTrackingAdapter(
        config.adapter,
        config.maxOracleDepth ?? 4
      );
    }

    // Apply profile restrictions if non-default caps/budgets specified
    if (config.defaultCaps || config.defaultBudgets) {
      const restrictProfile: Profile = {
        name: "custom",
        caps: config.defaultCaps ?? profile.caps,
        budgets: config.defaultBudgets ?? profile.budgets,
        truth: profile.truth,
      };
      this.rootCtx = ctxApplyProfile(this.rootCtx, restrictProfile);
    }
  }

  /**
   * Create initial machine state from expression
   */
  private initialState(expr: Expr): State {
    const store0 = new COWStore();
    const prim = installPrims(store0);
    return {
      control: { tag: "Expr", e: expr },
      env: prim.env,
      store: prim.store,
      kont: [],
      handlers: [],
    };
  }

  /**
   * Evaluate a Lisp expression
   *
   * @param code - Lisp source code
   * @param options - Optional context overrides
   * @returns EvalResult with value or error
   *
   * @example
   * const result = await omega.eval("(+ 1 2 3)");
   * console.log(result.value); // 6
   */
  async eval(
    code: string,
    options?: { sealed?: boolean }
  ): Promise<EvalResult> {
    try {
      // Compile text to expression
      const expr = compileTextToExpr(code);

      // Set up context
      let ctx = this.rootCtx;
      if (options?.sealed) {
        ctx = ctxSeal(ctx);
      }

      // Create runtime with Oracle support
      const snapshots = new SnapshotRepo();
      const receipts = new InMemoryReceiptStore("off");
      const runtime = new RuntimeImpl(
        this.adapter as OracleAdapter,  // May be undefined - infer.op will fail gracefully
        snapshots,
        receipts,
        mockCommitAdapter,
        this.config.profile
      );

      // Run to completion
      const value = await runToCompletion(runtime, this.initialState(expr), this.maxSteps);

      return { ok: true, value };
    } catch (e) {
      return { ok: false, error: String(e) };
    }
  }

  /**
   * Run Oracle-powered inference
   *
   * @param query - Natural language or structured query
   * @param options - Optional context and confidence threshold
   * @returns InferResult with meaning or error
   *
   * @example
   * const result = await omega.infer("Simplify: 2x + 3x");
   * if (result.ok && result.confidence > 0.8) {
   *   console.log(result.value);
   * }
   */
  async infer(
    query: string | Record<string, unknown>,
    options?: { minConfidence?: number; sealed?: boolean }
  ): Promise<InferResult> {
    if (!this.adapter) {
      return { ok: false, error: "No Oracle adapter configured" };
    }

    // Build the infer expression
    const payload = typeof query === "string"
      ? `"${query.replace(/"/g, '\\"')}"`
      : JSON.stringify(query);
    const code = `(effect int.op ${payload})`;

    try {
      const result = await this.eval(code, { sealed: options?.sealed });

      if (!result.ok) {
        return { ok: false, error: result.error };
      }

      const val = result.value;
      if (val && typeof val === "object" && "tag" in val && val.tag === "Meaning") {
        const meaning = val as MeaningVal;
        const confidence = meaning.confidence ?? 0;

        if (options?.minConfidence !== undefined && confidence < options.minConfidence) {
          return {
            ok: false,
            error: `Confidence ${confidence} below threshold ${options.minConfidence}`,
            meaning,
            confidence,
          };
        }

        return {
          ok: true,
          meaning,
          value: meaning.denotation as Val | undefined,
          confidence,
        };
      }

      return { ok: false, error: "Inference did not return a Meaning" };
    } catch (e) {
      return { ok: false, error: String(e) };
    }
  }

  /**
   * Multi-shot search returning a distribution of solutions
   *
   * @param query - Query for the Oracle
   * @param options - Optional sample count and threshold
   * @returns Array of candidate solutions with probabilities
   */
  async search(
    query: string | Record<string, unknown>,
    options?: { maxCandidates?: number; minProb?: number }
  ): Promise<{ candidates: Array<{ value: Val | undefined; prob: number; confidence: number }> }> {
    if (!this.adapter) {
      return { candidates: [] };
    }

    const payload = typeof query === "string"
      ? `"${query.replace(/"/g, '\\"')}"`
      : JSON.stringify(query);
    const code = `(effect search.op ${payload})`;

    const result = await this.eval(code);
    if (!result.ok || !result.value) {
      return { candidates: [] };
    }

    const val = result.value;
    if (typeof val === "object" && "tag" in val && val.tag === "Dist") {
      const distVal = val as DistVal;
      const candidates = distVal.support
        .filter((item) => options?.minProb === undefined || item.w >= options.minProb)
        .slice(0, options?.maxCandidates ?? 10)
        .map((item) => {
          const meaning = item.v as MeaningVal;
          return {
            value: meaning.denotation as Val | undefined,
            prob: item.w,
            confidence: meaning.confidence ?? 0,
          };
        });
      return { candidates };
    }

    return { candidates: [] };
  }

  /**
   * Create a sealed sandbox for untrusted inference
   *
   * @param caps - Restricted capability set
   * @returns A new OmegaRuntime with sealed context
   */
  sandbox(caps: CapSet): OmegaRuntime {
    const sandboxed = new OmegaRuntime({
      ...this.config,
      defaultCaps: caps,
    });
    sandboxed.rootCtx = ctxSeal(sandboxed.rootCtx);
    return sandboxed;
  }

  /**
   * Get the underlying adapter (for advanced use)
   */
  getAdapter(): OracleAdapter | undefined {
    return this.adapter;
  }

  /**
   * Check if Oracle is available
   */
  hasOracle(): boolean {
    return !!this.adapter;
  }
}

/**
 * Quick eval helper - creates a runtime and evaluates code
 *
 * @example
 * const result = await evalOmegaCode("(+ 1 2)");
 * if (result.ok) console.log(result.value);
 */
export async function evalOmegaCode(code: string): Promise<EvalResult> {
  const runtime = new OmegaRuntime();
  return runtime.eval(code);
}
