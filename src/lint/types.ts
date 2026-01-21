import type { IRBundle } from "../frameir/bundle";
import type { Diagnostic } from "../outcome/diagnostic";
import type { PrimitiveRegistry } from "../registry";

export interface PassResult {
  diagnostics: Diagnostic[];
  transformed?: IRBundle;
  metadata?: Record<string, unknown>;
}

export type PassPhase =
  | "parsing"
  | "lowering"
  | "normalize"
  | "lint"
  | "optimize";

export interface Pass {
  id: string;
  name: string;
  phase: PassPhase;
  dependencies?: string[];
  run(bundle: IRBundle, registry: PrimitiveRegistry): PassResult;
}

export interface PassConfig {
  enabled: boolean;
  options?: Record<string, unknown>;
  severityOverride?: "error" | "warning" | "info" | "off";
}

export interface LintConfig {
  passes: Record<string, PassConfig>;
}
