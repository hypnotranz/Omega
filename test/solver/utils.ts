import type { Val } from "../../src/core/eval/values";
import { listToArray } from "../monad/utils";
export { evalAll, evalOne, listToArray } from "../monad/utils";

export function expectNum(v: Val): number {
  if (v.tag !== "Num") throw new Error(`expected Num, got ${v.tag}`);
  return v.n;
}

export function expectBool(v: Val): boolean {
  if (v.tag !== "Bool") throw new Error(`expected Bool, got ${v.tag}`);
  return v.b;
}

export function expectSym(v: Val): string {
  if (v.tag !== "Sym") throw new Error(`expected Sym, got ${v.tag}`);
  return v.name;
}

export function expectStr(v: Val): string {
  if (v.tag !== "Str") throw new Error(`expected Str, got ${v.tag}`);
  return v.s;
}

export function asResult(v: Val): Extract<Val, { tag: "Result" }> {
  if ((v as any).tag !== "Result") {
    throw new Error(`expected Result, got ${(v as any).tag}`);
  }
  return v as Extract<Val, { tag: "Result" }>;
}

export function asBudget(v: Val): Extract<Val, { tag: "Budget" }> {
  if ((v as any).tag !== "Budget") {
    throw new Error(`expected Budget, got ${(v as any).tag}`);
  }
  return v as Extract<Val, { tag: "Budget" }>;
}

export function asFactStore(v: Val): Extract<Val, { tag: "FactStore" }> {
  if ((v as any).tag !== "FactStore") {
    throw new Error(`expected FactStore, got ${(v as any).tag}`);
  }
  return v as Extract<Val, { tag: "FactStore" }>;
}

export function listToJs<T>(list: Val, mapFn: (v: Val) => T): T[] {
  return listToArray(list).map(mapFn);
}
