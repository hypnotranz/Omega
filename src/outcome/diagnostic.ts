import type { Span } from "../frameir/meta";

export type DiagnosticSeverity = "error" | "warning" | "info" | "hint";

export interface DiagnosticFix {
  description: string;
  replacement?: string;
  span?: Span;
}

export interface Diagnostic {
  code: string;
  severity: DiagnosticSeverity;
  message: string;
  span?: Span;
  data?: Record<string, unknown>;
  related?: Diagnostic[];
  fixes?: DiagnosticFix[];
}

type DiagnosticOpts = Partial<Omit<Diagnostic, "code" | "message" | "severity">>;

export function errorDiag(code: string, message: string, opts?: DiagnosticOpts): Diagnostic {
  return { code, message, severity: "error", ...opts };
}

export function warnDiag(code: string, message: string, opts?: DiagnosticOpts): Diagnostic {
  return { code, message, severity: "warning", ...opts };
}

export function infoDiag(code: string, message: string, opts?: DiagnosticOpts): Diagnostic {
  return { code, message, severity: "info", ...opts };
}

export function hintDiag(code: string, message: string, opts?: DiagnosticOpts): Diagnostic {
  return { code, message, severity: "hint", ...opts };
}
