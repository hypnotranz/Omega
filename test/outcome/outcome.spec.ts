import { describe, expect, it } from "vitest";

import {
  err,
  failure,
  info,
  isErr,
  isOk,
  ok,
  warn,
  error as errorDiag,
} from "../../src/core/outcome";
import type { DiagnosticSpan } from "../../src/core/outcome";

describe("Outcome helpers", () => {
  it("creates Ok outcome with value and diagnostics", () => {
    const diag = info("Parsed expression", {
      file: "program.lisp",
      start: { line: 1, column: 1 },
    });
    const outcome = ok(42, [diag]);

    expect(outcome).toMatchObject({
      tag: "Ok",
      value: 42,
      diagnostics: [diag],
    });
    expect(isOk(outcome)).toBe(true);
    expect(isErr(outcome)).toBe(false);
  });

  it("preserves undefined values in Ok outcome", () => {
    const outcome = ok<void>(undefined);
    expect(outcome.value).toBeUndefined();
    expect(outcome.diagnostics).toEqual([]);
  });

  it("creates Err outcome from code and message", () => {
    const diag = warn("Type mismatch", {
      file: "program.lisp",
      start: { line: 3, column: 2 },
      end: { line: 3, column: 10 },
    });
    const outcome = err("invalid_input", "Parameter `x` must be positive", {
      diagnostics: [diag],
    });

    expect(outcome.tag).toBe("Err");
    expect(outcome.failure.code).toBe("invalid_input");
    expect(outcome.failure.message).toContain("positive");
    expect(outcome.failure.diagnostics).toEqual([diag]);
  });

  it("wraps existing failure untouched", () => {
    const existing = failure("internal_error", "boom", {
      data: { requestId: "abc123" },
    });
    const outcome = err(existing);

    expect(outcome.failure).toBe(existing);
    expect(outcome.failure.data?.requestId).toBe("abc123");
  });

  it("rejects unknown failure codes", () => {
    expect(() => failure("bogus_code" as any, "oops")).toThrow(/failure code/i);
  });
});

describe("Diagnostic helpers", () => {
  it("creates warning diagnostic with span", () => {
    const span: DiagnosticSpan = {
      file: "program.lisp",
      start: { line: 5, column: 1, offset: 12 },
      end: { line: 5, column: 10, offset: 21 },
    };
    const diag = warn("Shadowed binding", span);

    expect(diag).toMatchObject({
      severity: "warning",
      message: "Shadowed binding",
      span,
    });
  });

  it("creates error diagnostic without span safely", () => {
    const diag = errorDiag("Catastrophic failure");
    expect(diag.span).toBeUndefined();
    expect(diag.severity).toBe("error");
    expect(diag.message).toBe("Catastrophic failure");
  });

  it("info helper defaults severity", () => {
    const diag = info("Auxiliary note");
    expect(diag.severity).toBe("info");
  });
});
