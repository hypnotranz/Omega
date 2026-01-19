// SOURCE: ARCHITECTURE/32-LANGUAGE-OFFICIAL-13-IMPLEMENTATION-7.md
// AUTO-EXTRACTED - Do not edit directly. Edit the source document.

import type { Binding } from "../syntax/binding";
import type { Expr } from "../ast";
import type { SRTransformer } from "../expand/syntaxRules";

export type ExportEntry0 = { name: string; bid: string; internal: string };
export type ExportEntry1 = { name: string; bid: string; transformer: SRTransformer };

export type ModuleArtifact = {
  moduleName: string;
  moduleId: string;                 // stable, used for prefixing bids/scopes
  scopeM0: string;
  scopeM1: string;

  // Full binding table closure for def-site resolution of imported macros
  bindings: Binding[];

  // The runtime init expression (phase 0)
  init: Expr;

  exports0: ExportEntry0[];
  exports1: ExportEntry1[];

  deps: string[];                   // module names or module ids (depends on loader)
  sourceHash: string;
  expandedHash: string;
  coreHash: string;

  // Hermetic inference receipts produced during compilation (Part 19 §178)
  receipts: unknown[];
};