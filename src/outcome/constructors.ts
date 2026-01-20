import type { Done, Fail, Pause, OutcomeMeta, Suspended } from "./outcome";
import type { Failure, FailureReason } from "./failure";
import { failure } from "./failure";
import { makeDiagnostic } from "./codes";
import { warnDiag, type Diagnostic } from "./diagnostic";

export function done<A>(value: A, meta: OutcomeMeta = {}): Done<A> {
  return { tag: "Done", value, meta };
}

export const ok = done;

export function fail(f: Failure, meta: OutcomeMeta = {}): Fail {
  return { tag: "Fail", failure: f, meta };
}

export function err(
  failureOrReason: Failure | FailureReason,
  messageOrMeta?: string | OutcomeMeta,
  opts?: Partial<Omit<Failure, "reason" | "message">>,
  meta: OutcomeMeta = {}
): Fail {
  if (typeof failureOrReason === "string") {
    const message = typeof messageOrMeta === "string" ? messageOrMeta : "";
    const finalMeta =
      typeof messageOrMeta === "object" && messageOrMeta !== null && !Array.isArray(messageOrMeta)
        ? (messageOrMeta as OutcomeMeta)
        : meta;
    return fail(failure(failureOrReason, message, opts), finalMeta ?? {});
  }

  const finalMeta =
    typeof messageOrMeta === "object" && messageOrMeta !== null && !Array.isArray(messageOrMeta)
      ? (messageOrMeta as OutcomeMeta)
      : meta;
  return fail(failureOrReason, finalMeta ?? {});
}

export function pause<A>(suspended: Suspended<A>, meta: OutcomeMeta = {}): Pause<A> {
  return { tag: "Pause", suspended, meta };
}

export function budgetExceeded(resource: string, meta: OutcomeMeta = {}): Fail {
  return fail(
    failure("budget-exceeded", `Budget exceeded: ${resource}`, {
      diagnostics: [makeDiagnostic("E0301", { resource })],
      recoverable: false,
    }),
    meta
  );
}

export function timeout(ms: number, meta: OutcomeMeta = {}): Fail {
  return fail(
    failure("timeout", `Timeout after ${ms}ms`, {
      diagnostics: [makeDiagnostic("E0300", { ms })],
      recoverable: true,
    }),
    meta
  );
}

export function validationFailed(
  message: string,
  context?: Record<string, unknown>,
  meta: OutcomeMeta = {}
): Fail {
  return fail(
    failure("validation-failed", message, {
      diagnostics: [makeDiagnostic("E0400")],
      context,
      recoverable: true,
    }),
    meta
  );
}

export function toolError(tool: string, error: string, meta: OutcomeMeta = {}): Fail {
  return fail(
    failure("tool-error", `Tool ${tool} failed: ${error}`, {
      diagnostics: [makeDiagnostic("E0303", { tool })],
      recoverable: true,
    }),
    meta
  );
}

export function capabilityDenied(cap: string, meta: OutcomeMeta = {}): Fail {
  return fail(
    failure("precondition-failed", `Missing capability: ${cap}`, {
      diagnostics: [makeDiagnostic("E0500", { cap })],
      recoverable: false,
    }),
    meta
  );
}

export function warn(code: string, message: string, opts?: Partial<Omit<Diagnostic, "code" | "message" | "severity">>) {
  return warnDiag(code, message, opts);
}
