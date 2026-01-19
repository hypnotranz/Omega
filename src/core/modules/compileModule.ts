// SOURCE: ARCHITECTURE/32-LANGUAGE-OFFICIAL-13-IMPLEMENTATION-7.md
// AUTO-EXTRACTED - Do not edit directly. Edit the source document.

import { tokenize } from "../reader/tokenize";
import { parseAll } from "../reader/parse";
import type { Datum } from "../reader/datum";
import { isSym } from "../reader/datum";
import { datumToSyntax } from "../reader/toSyntax";
import { addScope } from "../syntax/syntax";
import type { Syntax, SIdent, SList } from "../syntax/syntax";
import { isIdent, isList } from "../syntax/syntax";
import type { Env, Binding } from "../syntax/binding";
import { resolveIdent } from "../syntax/binding";
import { lowerSyntax } from "../pipeline/lower";
import { compileSyntaxRules, type SRTransformer } from "../expand/syntaxRules";

// loader provides module source by name
export interface ModuleLoader {
  loadSource(moduleName: string): string;
  // optional: loadArtifact by hash
}

// compilation context to memoize compiled artifacts
export type CompileCtx = {
  compiled: Map<string, ModuleArtifact>;
  // moduleName -> artifact
};

export type ModuleArtifact = import("./artifact").ModuleArtifact;

// deterministic hashes (stub; use crypto hash in real code)
function hashText(s: string): string { return `H(${s.length}:${s.slice(0,64)})`; }

function expectList(stx: Syntax, msg: string): SList {
  if (!isList(stx)) throw new Error(msg);
  return stx;
}
function expectIdent(stx: Syntax, msg: string): SIdent {
  if (!isIdent(stx)) throw new Error(msg);
  return stx;
}