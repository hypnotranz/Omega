// Structured Outcome/Failure/Diagnostic algebraic data types
// Provides simple Result-like helpers with diagnostics attached to failures.

export const FAILURE_CODES = [
  "invalid_input",
  "not_found",
  "conflict",
  "unauthorized",
  "forbidden",
  "timeout",
  "unavailable",
  "internal_error",
] as const;

export type FailureCode = (typeof FAILURE_CODES)[number];

export type DiagnosticSeverity = "error" | "warning" | "info";

export type DiagnosticPosition = {
  line: number;
  column: number;
  offset?: number;
};

export type DiagnosticSpan = {
  file?: string;
  start: DiagnosticPosition;
  end?: DiagnosticPosition;
};

export type Diagnostic = {
  message: string;
  severity: DiagnosticSeverity;
  span?: DiagnosticSpan;
  code?: string;
  source?: string;
  data?: Record<string, unknown>;
};

export type Failure = {
  code: FailureCode;
  message: string;
  diagnostics: Diagnostic[];
  cause?: unknown;
  data?: Record<string, unknown>;
};

export type Ok<T> = {
  tag: "Ok";
  value: T;
  diagnostics: Diagnostic[];
};

export type Err<F extends Failure = Failure> = {
  tag: "Err";
  failure: F;
};

export type Outcome<T> = Ok<T> | Err<Failure>;

type FailureOptions = {
  diagnostics?: Diagnostic[];
  cause?: unknown;
  data?: Record<string, unknown>;
};

type DiagnosticExtras = {
  code?: string;
  source?: string;
  data?: Record<string, unknown>;
};

const FAILURE_CODE_SET = new Set<string>(FAILURE_CODES as readonly string[]);

function assertFailureCode(code: FailureCode | string): asserts code is FailureCode {
  if (!FAILURE_CODE_SET.has(code)) {
    throw new Error(`Unknown failure code: ${code}`);
  }
}

export function failure(code: FailureCode, message: string, options: FailureOptions = {}): Failure {
  assertFailureCode(code);
  return {
    code,
    message,
    diagnostics: options.diagnostics ?? [],
    cause: options.cause,
    data: options.data,
  };
}

export function ok<T>(value: T, diagnostics: Diagnostic[] = []): Ok<T> {
  return { tag: "Ok", value, diagnostics };
}

export function err(codeOrFailure: Failure | FailureCode, message?: string, options: FailureOptions = {}): Err {
  if (typeof codeOrFailure === "string") {
    const failureMessage = message ?? codeOrFailure;
    return { tag: "Err", failure: failure(codeOrFailure, failureMessage, options) };
  }

  assertFailureCode(codeOrFailure.code);
  return { tag: "Err", failure: codeOrFailure };
}

export function isOk<T>(outcome: Outcome<T>): outcome is Ok<T> {
  return outcome.tag === "Ok";
}

export function isErr<T>(outcome: Outcome<T>): outcome is Err {
  return outcome.tag === "Err";
}

function diagnostic(
  severity: DiagnosticSeverity,
  message: string,
  span?: DiagnosticSpan,
  extras: DiagnosticExtras = {},
): Diagnostic {
  return {
    severity,
    message,
    span,
    ...extras,
  };
}

export function warn(message: string, span?: DiagnosticSpan, extras?: DiagnosticExtras): Diagnostic {
  return diagnostic("warning", message, span, extras);
}

export function info(message: string, span?: DiagnosticSpan, extras?: DiagnosticExtras): Diagnostic {
  return diagnostic("info", message, span, extras);
}

export function error(message: string, span?: DiagnosticSpan, extras?: DiagnosticExtras): Diagnostic {
  return diagnostic("error", message, span, extras);
}
