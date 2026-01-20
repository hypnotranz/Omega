// demo/harness/types.ts
// Ω Wow Pack - Harness types and report schema

import type { Profile } from "../../src/core/governance/profile";
import type { Hash } from "../../src/core/artifacts/hash";

// ─────────────────────────────────────────────────────────────────
// WowReport - The proof object for each demo
// ─────────────────────────────────────────────────────────────────

/**
 * Report schema for demo runs - pure Event Sourcing + CQRS.
 * Ledger Σ is the write model, reports are read models.
 */
export type WowReport = {
  /** Demo identifier */
  demoId: string;
  /** Demo description */
  description: string;
  /** Governance profile used */
  profile: string;
  /** Random seed for determinism */
  seed: number;
  /** Timestamp */
  timestamp: string;

  /** Hash of output values */
  outputsDigest: string;
  /** Hash of ledger state */
  ledgerDigest: string;

  /** Call and operation counts */
  counts: {
    /** Total infer.op calls */
    inferCalls: number;
    /** Oracle ReqEval requests */
    oracleReqEval: number;
    /** Oracle ReqApply requests */
    oracleReqApply: number;
    /** Oracle ReqObserve requests */
    oracleReqObserve: number;
    /** Oracle ReqTest requests */
    oracleReqTest: number;
    /** Oracle ReqReturn responses */
    oracleReqReturn?: number;

    /** Scheduler decisions (for concurrency) */
    scheduleDecisions?: number;
    /** Amb choice points */
    ambChoices?: number;
    /** Backtrack events */
    backtracks?: number;

    /** Value hydrations */
    hydrations?: number;
    /** Value compressions */
    compressions?: number;

    /** Commits performed */
    commits?: number;
    /** Promotions performed */
    promotions?: number;

    /** Generic.miss events */
    genericMiss?: number;
    /** Methods installed */
    methodsInstalled?: number;

    /** Macro expansions */
    macroExpansions?: number;
  };

  /** Cost metrics */
  costs: {
    /** Approximate tokens (if tracked) */
    tokens?: number;
    /** CEKS evaluation steps */
    steps: number;
    /** Wall clock time in ms */
    wallMs?: number;
  };

  /** Invariant checks */
  invariants: InvariantResult[];

  /** Artifact references */
  artifacts: {
    /** Transcript ID for replay */
    transcriptId?: string;
    /** Receipt IDs */
    receiptIds?: string[];
    /** IR digest (if compiled) */
    irDigest?: string;
    /** Source map digest */
    sourceMapDigest?: string;
  };
};

/**
 * Result of an invariant check.
 */
export type InvariantResult = {
  /** Invariant name */
  name: string;
  /** Whether it passed */
  ok: boolean;
  /** Details or error message */
  detail?: string;
};

// ─────────────────────────────────────────────────────────────────
// Demo Definition
// ─────────────────────────────────────────────────────────────────

/**
 * Demo definition - what each Ω demo provides.
 */
export type DemoDefinition = {
  /** Unique demo ID */
  id: string;
  /** Human-readable name */
  name: string;
  /** Description of what it demonstrates */
  description: string;
  /** Tags for categorization */
  tags: string[];
  /** The demo runner function */
  run: (ctx: DemoContext) => Promise<DemoResult>;
  /** Invariants to check */
  invariants: InvariantSpec[];
};

/**
 * Invariant specification.
 */
export type InvariantSpec = {
  /** Invariant name */
  name: string;
  /** Check function */
  check: (result: DemoResult, ctx: DemoContext) => InvariantResult;
};

/**
 * Context provided to demos.
 */
export type DemoContext = {
  /** Governance profile */
  profile: Profile;
  /** Random seed */
  seed: number;
  /** Seeded random number generator */
  random: () => number;
  /** Oracle adapter */
  oracle: ScriptedOracleAdapter;
  /** Ledger for recording events */
  ledger: DemoLedger;
  /** Whether this is a replay */
  isReplay: boolean;
  /** Transcript to replay (if any) */
  replayTranscript?: OracleTranscript;
  /** Configuration options */
  options: DemoOptions;
};

/**
 * Demo options.
 */
export type DemoOptions = {
  /** Enable verbose logging */
  verbose?: boolean;
  /** Maximum oracle calls allowed */
  maxOracleCalls?: number;
  /** Maximum steps allowed */
  maxSteps?: number;
  /** Custom options per demo */
  custom?: Record<string, unknown>;
};

/**
 * Result from running a demo.
 */
export type DemoResult = {
  /** Output value(s) */
  outputs: unknown[];
  /** Whether demo succeeded */
  success: boolean;
  /** Error if failed */
  error?: Error;
  /** Metrics collected */
  metrics: DemoMetrics;
  /** Transcript of oracle interactions */
  transcript: OracleTranscript;
};

/**
 * Metrics collected during demo run.
 */
export type DemoMetrics = {
  inferCalls: number;
  oracleReqEval: number;
  oracleReqApply: number;
  oracleReqObserve: number;
  oracleReqTest: number;
  oracleReqReturn: number;
  steps: number;
  wallMs: number;
  scheduleDecisions?: number;
  ambChoices?: number;
  backtracks?: number;
  commits?: number;
  genericMiss?: number;
  methodsInstalled?: number;
  macroExpansions?: number;
};

// ─────────────────────────────────────────────────────────────────
// Oracle Transcript
// ─────────────────────────────────────────────────────────────────

/**
 * Oracle interaction types.
 */
export type OracleRequestType =
  | "ReqEval"
  | "ReqApply"
  | "ReqObserve"
  | "ReqTest"
  | "ReqReturn"
  | "InferOp";

/**
 * Single oracle interaction.
 */
export type OracleInteraction = {
  /** Interaction ID */
  id: string;
  /** Sequence number */
  seq: number;
  /** Request type */
  type: OracleRequestType;
  /** Request payload */
  request: unknown;
  /** Response payload */
  response: unknown;
  /** Timestamp */
  timestamp: number;
  /** Environment snapshot ID (if applicable) */
  envSnapshotId?: string;
};

/**
 * Complete oracle transcript for replay.
 */
export type OracleTranscript = {
  /** Transcript ID */
  id: string;
  /** Demo ID */
  demoId: string;
  /** Seed used */
  seed: number;
  /** Profile used */
  profile: string;
  /** All interactions in order */
  interactions: OracleInteraction[];
  /** Hash of transcript */
  digest: string;
};

// ─────────────────────────────────────────────────────────────────
// Scripted Oracle Adapter
// ─────────────────────────────────────────────────────────────────

/**
 * Script entry for oracle.
 */
export type OracleScriptEntry = {
  /** Pattern to match request */
  match: (req: unknown, type: OracleRequestType) => boolean;
  /** Response to return */
  respond: (req: unknown, ctx: DemoContext) => unknown;
  /** Optional side effects */
  sideEffect?: (req: unknown, ctx: DemoContext) => void;
};

/**
 * Scripted oracle adapter for deterministic demos.
 */
export interface ScriptedOracleAdapter {
  /** Add a scripted response */
  addScript(entry: OracleScriptEntry): void;
  /** Handle an oracle request */
  handle(type: OracleRequestType, request: unknown): unknown;
  /** Get interaction count */
  getCount(type: OracleRequestType): number;
  /** Get all counts */
  getCounts(): Record<OracleRequestType, number>;
  /** Get transcript */
  getTranscript(): OracleTranscript;
  /** Reset for new run */
  reset(): void;
  /** Load transcript for replay */
  loadTranscript(transcript: OracleTranscript): void;
  /** Check if in replay mode */
  isReplaying(): boolean;
}

// ─────────────────────────────────────────────────────────────────
// Demo Ledger
// ─────────────────────────────────────────────────────────────────

/**
 * Ledger event types.
 */
export type LedgerEventType =
  | "demo.start"
  | "demo.end"
  | "infer.call"
  | "infer.result"
  | "oracle.request"
  | "oracle.response"
  | "amb.choose"
  | "amb.fail"
  | "amb.backtrack"
  | "commit.attempt"
  | "commit.success"
  | "commit.denied"
  | "generic.miss"
  | "generic.install"
  | "macro.expand"
  | "schedule.decision"
  | "constraint.violation"
  | "constraint.repair"
  | "snapshot.create"
  | "snapshot.restore";

/**
 * Ledger event.
 */
export type LedgerEvent = {
  id: string;
  seq: number;
  type: LedgerEventType;
  timestamp: number;
  data: unknown;
  parentId?: string;
};

/**
 * Demo ledger interface.
 */
export interface DemoLedger {
  /** Record an event */
  record(type: LedgerEventType, data: unknown, parentId?: string): string;
  /** Get all events */
  getEvents(): LedgerEvent[];
  /** Get events by type */
  getEventsByType(type: LedgerEventType): LedgerEvent[];
  /** Compute ledger digest */
  getDigest(): string;
  /** Reset ledger */
  reset(): void;
  /** Create snapshot */
  snapshot(): string;
  /** Restore from snapshot */
  restore(snapshotId: string): void;
}

// ─────────────────────────────────────────────────────────────────
// CLI Types
// ─────────────────────────────────────────────────────────────────

/**
 * CLI options.
 */
export type CLIOptions = {
  /** Demo ID or 'all' */
  demo: string;
  /** Profile name */
  profile: string;
  /** Random seed */
  seed: number;
  /** Transcript ID for replay */
  replay?: string;
  /** Output report path */
  report?: string;
  /** Verbose output */
  verbose: boolean;
  /** List demos only */
  list: boolean;
};

/**
 * Suite result.
 */
export type SuiteResult = {
  /** Total demos run */
  total: number;
  /** Passed demos */
  passed: number;
  /** Failed demos */
  failed: number;
  /** Skipped demos */
  skipped: number;
  /** Individual reports */
  reports: WowReport[];
  /** Overall success */
  success: boolean;
};
