// SOURCE: ARCHITECTURE/32-LANGUAGE-OFFICIAL-13-IMPLEMENTATION-11.md
// AUTO-EXTRACTED - Do not edit directly. Edit the source document.

import type { Expr } from "../../src/core/ast";
import { compileTextToExpr } from "../../src/core/pipeline/compileText";

export async function expandWithHost(src: string): Promise<Expr> {
  // baseline: use the same expander for both
  return compileTextToExpr(src);
}

export async function expandWithBoot(src: string): Promise<Expr> {
  // later: replace with self-hosted expander artifact
  return compileTextToExpr(src);
}