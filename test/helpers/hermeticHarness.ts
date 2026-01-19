// SOURCE: ARCHITECTURE/32-LANGUAGE-OFFICIAL-13-IMPLEMENTATION-11.md
// AUTO-EXTRACTED - Do not edit directly. Edit the source document.

import { sha256Text } from "../../src/core/artifacts/hash";
import { compileTextToExpr } from "../../src/core/pipeline/compileText";

export async function compileStrict(src: string): Promise<{ expandedHash: string; coreHash: string; receipts: any[] }> {
  // Stub: expanded hash = hash of source; core hash = hash of core AST JSON.
  const expr = compileTextToExpr(src);
  return {
    expandedHash: sha256Text(src),
    coreHash: sha256Text(JSON.stringify(expr)),
    receipts: [],
  };
}

export async function replayCompileStrict(src: string, _receipts: any[]): Promise<{ expandedHash: string; coreHash: string }> {
  const expr = compileTextToExpr(src);
  return {
    expandedHash: sha256Text(src),
    coreHash: sha256Text(JSON.stringify(expr)),
  };
}