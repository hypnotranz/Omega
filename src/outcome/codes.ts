import type { Span } from "../frameir/meta";
import type { Diagnostic, DiagnosticSeverity } from "./diagnostic";

interface DiagCodeDef {
  code: string;
  severity: DiagnosticSeverity;
  category: string;
  template: string;
}

export const DIAGNOSTIC_CODES: Record<string, DiagCodeDef> = {
  E0001: { code: "E0001", severity: "error", category: "Syntax", template: "Malformed expression" },
  E0002: { code: "E0002", severity: "error", category: "Syntax", template: "Unbalanced parentheses" },
  E0003: { code: "E0003", severity: "error", category: "Syntax", template: "Invalid string literal" },

  E0100: { code: "E0100", severity: "error", category: "Type", template: "Type mismatch: expected {expected}, got {actual}" },
  E0101: { code: "E0101", severity: "error", category: "Type", template: "Undefined variable: {name}" },
  E0102: { code: "E0102", severity: "error", category: "Type", template: "Wrong number of arguments: expected {expected}, got {actual}" },

  E0200: { code: "E0200", severity: "error", category: "Runtime", template: "Division by zero" },
  E0201: { code: "E0201", severity: "error", category: "Runtime", template: "Index out of bounds: {index}" },
  E0202: { code: "E0202", severity: "error", category: "Runtime", template: "Null pointer dereference" },

  E0300: { code: "E0300", severity: "error", category: "Oracle", template: "Oracle timeout after {ms}ms" },
  E0301: { code: "E0301", severity: "error", category: "Oracle", template: "Budget exhausted: {resource}" },
  E0302: { code: "E0302", severity: "error", category: "Oracle", template: "Invalid response format" },
  E0303: { code: "E0303", severity: "error", category: "Oracle", template: "Tool call failed: {tool}" },

  E0400: { code: "E0400", severity: "error", category: "Validation", template: "Schema validation failed" },
  E0401: { code: "E0401", severity: "error", category: "Validation", template: "Required field missing: {field}" },
  E0402: { code: "E0402", severity: "error", category: "Validation", template: "Invalid value for field: {field}" },

  E0500: { code: "E0500", severity: "error", category: "Capability", template: "Missing capability: {cap}" },
  E0501: { code: "E0501", severity: "error", category: "Capability", template: "Tool not allowed: {tool}" },
  E0502: { code: "E0502", severity: "error", category: "Capability", template: "Model not allowed: {model}" },

  W0001: { code: "W0001", severity: "warning", category: "Performance", template: "Large context: {tokens} tokens" },
  W0002: { code: "W0002", severity: "warning", category: "Performance", template: "Deep recursion: depth {depth}" },
  W0003: { code: "W0003", severity: "warning", category: "Style", template: "Unused variable: {name}" },
  W0004: { code: "W0004", severity: "warning", category: "Style", template: "Unreachable code" },
  W0005: { code: "W0005", severity: "warning", category: "Oracle", template: "Low confidence response: {confidence}" },
};

export function makeDiagnostic(
  code: keyof typeof DIAGNOSTIC_CODES,
  params?: Record<string, string | number>,
  span?: Span
): Diagnostic {
  const def = DIAGNOSTIC_CODES[code];
  if (!def) {
    throw new Error(`Unknown diagnostic code: ${String(code)}`);
  }

  let message = def.template;
  if (params) {
    for (const [key, value] of Object.entries(params)) {
      message = message.replace(`{${key}}`, String(value));
    }
  }

  return {
    code: def.code,
    severity: def.severity,
    message,
    span,
    data: params as Record<string, unknown> | undefined,
  };
}
