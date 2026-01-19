// SOURCE: ARCHITECTURE/32-LANGUAGE-OFFICIAL-13-IMPLEMENTATION-8.md
// AUTO-EXTRACTED - Do not edit directly. Edit the source document.

import type { SRTransformer } from "./syntaxRules";

export type TransformerVal =
  | { tag: "SyntaxRules"; tr: SRTransformer }
  | { tag: "ProcRef"; originModule: string; phase: number; internal: string }; // closure stored in module instance at phase