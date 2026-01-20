import type { Expr } from "../ast";
import type { Val } from "../eval/values";
import { sha256JSON } from "../artifacts/hash";

export type TestEvidence = { tag: "TestEvidence"; passed: number; total: number; receipt?: string };
export type NoMatchEvidence = { tag: "NoMatchEvidence"; pattern: Expr; searched: number; found: number };
export type EqExtEvidence = {
  tag: "EqExtEvidence";
  tests: number;
  allPassed: boolean;
  failures?: Array<{ input: Val; expected: Val; got: Val }>;
};

export type OracleEvidence = { tag: "OracleEvidence"; receiptId: string; sourceHash: string; timestamp: number };
export type TransformEvidence = { tag: "TransformEvidence"; operation: string; inputEvidenceIds: string[] };
export type DerivedEvidence = { tag: "DerivedEvidence"; dependencies: string[]; derivationExpr: string };

export type Evidence =
  | TestEvidence
  | NoMatchEvidence
  | EqExtEvidence
  | OracleEvidence
  | TransformEvidence
  | DerivedEvidence;

export function evidenceId(ev: Evidence | Evidence[]): string {
  return sha256JSON(ev);
}

export function computeSourceHash(content: unknown): string {
  return sha256JSON(content);
}

function str(s: string): Val {
  return { tag: "Str", s };
}

function num(n: number): Val {
  return { tag: "Num", n };
}

function vector(items: Val[]): Val {
  return { tag: "Vector", items };
}

/** Convert an Evidence record into a Map Val for user-facing inspection. */
export function evidenceToVal(ev: Evidence): Val {
  const entries: Array<[Val, Val]> = [[str("tag"), str(ev.tag)]];

  switch (ev.tag) {
    case "TestEvidence":
      entries.push([str("passed"), num(ev.passed)]);
      entries.push([str("total"), num(ev.total)]);
      if (ev.receipt) entries.push([str("receipt"), str(ev.receipt)]);
      break;
    case "NoMatchEvidence":
      entries.push([str("searched"), num(ev.searched)]);
      entries.push([str("found"), num(ev.found)]);
      entries.push([str("pattern"), str(JSON.stringify(ev.pattern))]);
      break;
    case "EqExtEvidence":
      entries.push([str("tests"), num(ev.tests)]);
      entries.push([str("allPassed"), { tag: "Bool", b: ev.allPassed }]);
      if (ev.failures) entries.push([str("failures"), num(ev.failures.length)]);
      break;
    case "OracleEvidence":
      entries.push([str("receiptId"), str(ev.receiptId)]);
      entries.push([str("sourceHash"), str(ev.sourceHash)]);
      entries.push([str("timestamp"), num(ev.timestamp)]);
      break;
    case "TransformEvidence":
      entries.push([str("operation"), str(ev.operation)]);
      entries.push([str("inputs"), vector(ev.inputEvidenceIds.map(str))]);
      break;
    case "DerivedEvidence":
      entries.push([str("derivationExpr"), str(ev.derivationExpr)]);
      entries.push([str("dependencies"), vector(ev.dependencies.map(str))]);
      break;
    default:
      break;
  }

  return { tag: "Map", entries };
}
