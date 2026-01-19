// src/core/commit/index.ts
// Commit barrier exports

export {
  type ObligationNone,
  type ObligationTests,
  type Obligation,
  type CommitGateResult,
  type ApplyRewrite,
  canCommitMeaning,
  commitRewriteMeaning,
  attachTestReport,
  isRegimeAtLeast,
  strictestRegime,
} from "./commit";
