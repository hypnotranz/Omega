// demo/harness/index.ts
// Harness module exports

export type {
  WowReport,
  InvariantResult,
  DemoDefinition,
  DemoContext,
  DemoResult,
  DemoOptions,
  DemoMetrics,
  InvariantSpec,
  OracleRequestType,
  OracleInteraction,
  OracleTranscript,
  OracleScriptEntry,
  ScriptedOracleAdapter,
  LedgerEventType,
  LedgerEvent,
  DemoLedger,
  CLIOptions,
  SuiteResult,
} from "./types";

export {
  createScriptedOracleAdapter,
  classifyScript,
  sanitizeScript,
  validateScript,
  reqEvalScript,
  reqApplyScript,
  reqObserveScript,
  reqTestScript,
} from "./oracle-adapter";

export {
  createDemoLedger,
  countEventsByType,
  findCommits,
  findBacktracks,
  findGenericMisses,
  findMethodInstalls,
  findConstraintViolations,
  findConstraintRepairs,
  buildEventTree,
  formatLedgerTrace,
} from "./ledger";

export {
  createSeededRandom,
  getProfile,
  runDemo,
  runSuite,
  formatReport,
  formatSuiteResult,
} from "./runner";
