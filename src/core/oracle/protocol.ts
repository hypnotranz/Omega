// src/core/oracle/protocol.ts
// SOURCE: ARCHITECTURE/32-LANGUAGE-OFFICIAL-13-IMPLEMENTATION-16.md
// Patch Set C: Extended Oracle protocol request algebra

import type { Expr } from "../ast";
import type { Val } from "../eval/values";
import type { Hash } from "../artifacts/hash";
import type { MeaningVal } from "./meaning";

export type EnvRef = Hash;
export type StateRef = Hash;

// Re-export MeaningVal as the protocol's Meaning type
export type Meaning = MeaningVal;

export type QExpr = string | { tag: "Text"; text: string } | Expr;

export type ObserveSpec =
  | { tag: "Stack"; limit?: number }
  | { tag: "Control" }
  | { tag: "Handlers" }
  | { tag: "FrameEnv"; frameIndex: number }
  | { tag: "StoreSummary"; maxCells?: number }
  | { tag: "Env"; envRef?: EnvRef }                     // List all bindings in environment
  | { tag: "EnvLookup"; name: string; envRef?: EnvRef } // Lookup specific binding by name
  | { tag: "Defs"; envRef?: EnvRef };                   // List top-level definitions

export type ToolCall = {
  name: string;
  argv: string[];
  cwd?: string;
  stdin?: string;
  timeoutMs?: number;
};

export type TestSpec =
  | { tag: "ExprEquals"; qexpr: Expr; expected: Val; envRef: EnvRef }
  | { tag: "Smoke"; qexpr: Expr; envRef: EnvRef }
  | { tag: "Tests"; tests: QExpr[]; envRef: EnvRef }  // Batch tests: each expr should return truthy
  | { tag: "TestSuite"; name: string; tests: Array<{ name: string; qexpr: QExpr }>; envRef: EnvRef };  // Named suite

export type TrainingExample = {
  tag: "Example";
  payload: unknown;
};

export type OracleReq =
  // Core REPL re-entry
  | { tag: "ReqEval"; qexpr: QExpr; envRef: EnvRef }
  | { tag: "ReqApply"; fn: Val; args: Val[]; envRef: EnvRef }
  | { tag: "ReqObserve"; what: ObserveSpec; stateRef: StateRef }

  // Patch Set C: Extended request algebra
  | { tag: "ReqMatch"; qexpr: QExpr; pattern: QExpr; envRef: EnvRef }
  | { tag: "ReqAssert"; predicate: QExpr | Val; msg: string; severity?: "warn" | "error"; envRef: EnvRef }
  | { tag: "ReqSnapshot"; envRef: EnvRef; stateRef?: StateRef; meta?: unknown }
  | { tag: "ReqCompress"; envRef: EnvRef; meta?: unknown }
  | { tag: "ReqHydrate"; receiptId: Hash }

  // Tools and testing
  | { tag: "ReqTool"; call: ToolCall; envRef?: EnvRef }
  | { tag: "ReqTest"; spec: TestSpec }
  | { tag: "ReqEmitExample"; ex: TrainingExample }

  // Adoption and termination
  | { tag: "ReqAdoptEnv"; envRef: EnvRef }
  | { tag: "ReqReturn"; result: Meaning }
  | { tag: "ReqFail"; reason: string };

export type OracleResp =
  | { tag: "RespVal"; value: Val; envRef?: EnvRef; stateRef?: StateRef }
  | { tag: "RespObs"; data: unknown }
  | { tag: "RespTool"; result: unknown }
  | { tag: "RespTest"; passed: boolean; report: unknown }
  | { tag: "RespAck" }
  | { tag: "RespError"; message: string; details?: unknown };

export type OracleReturn = Meaning;

/**
 * Interactive oracle session:
 *   - yields OracleReq
 *   - receives OracleResp
 *   - returns OracleReturn (Meaning)
 */
export type OracleSession = AsyncGenerator<OracleReq, OracleReturn, OracleResp>;
