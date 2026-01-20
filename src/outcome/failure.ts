import type { Diagnostic } from "./diagnostic";

export type FailureReason =
  | "budget-exceeded"
  | "timeout"
  | "rate-limited"
  | "validation-failed"
  | "schema-mismatch"
  | "type-error"
  | "oracle-error"
  | "oracle-timeout"
  | "invalid-response"
  | "tool-error"
  | "tool-not-found"
  | "tool-contract-violation"
  | "user-cancelled"
  | "precondition-failed"
  | "invariant-violated"
  | "internal-error"
  | "not-implemented"
  | `custom:${string}`;

export interface Failure {
  reason: FailureReason;
  message: string;
  context?: Record<string, unknown>;
  diagnostics: Diagnostic[];
  cause?: Failure;
  recoverable: boolean;
  suggestions?: string[];
}

export function failure(
  reason: FailureReason,
  message: string,
  opts?: Partial<Omit<Failure, "reason" | "message">>
): Failure {
  return {
    reason,
    message,
    diagnostics: opts?.diagnostics ?? [],
    recoverable: opts?.recoverable ?? false,
    context: opts?.context,
    cause: opts?.cause,
    suggestions: opts?.suggestions,
  };
}

export function wrapFailure(
  inner: Failure,
  message: string,
  context?: Record<string, unknown>
): Failure {
  return {
    ...inner,
    message,
    context: { ...inner.context, ...context },
    cause: inner,
  };
}

export function isFailureReason(f: Failure, reason: FailureReason): boolean {
  return f.reason === reason;
}

export function allDiagnostics(f: Failure, seen = new Set<Diagnostic>()): Diagnostic[] {
  const collected: Diagnostic[] = [];
  for (const diag of f.diagnostics ?? []) {
    if (!seen.has(diag)) {
      seen.add(diag);
      collected.push(diag);
    }
  }
  if (f.cause) {
    collected.push(...allDiagnostics(f.cause, seen));
  }
  return collected;
}
