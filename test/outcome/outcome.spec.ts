import { describe, it, expect } from "vitest";
import type { Span } from "../../src/frameir/meta";
import { isDone, isFail, isPause } from "../../src/outcome/outcome";
import {
  allDiagnostics,
  failure,
  wrapFailure,
} from "../../src/outcome/failure";
import { errorDiag, warnDiag } from "../../src/outcome/diagnostic";
import { DIAGNOSTIC_CODES, makeDiagnostic } from "../../src/outcome/codes";
import {
  budgetExceeded,
  capabilityDenied,
  done,
  err,
  fail,
  ok,
  pause,
  timeout,
  toolError,
  validationFailed,
} from "../../src/outcome/constructors";
import {
  flatMapOutcome,
  mapOutcome,
  match,
  unwrap,
  unwrapOr,
} from "../../src/outcome/matchers";

const sampleSpan: Span = {
  file: "test.omega",
  startLine: 1,
  startCol: 0,
  endLine: 1,
  endCol: 10,
};

describe("Outcome ADT", () => {

  it("constructs Done outcomes with metadata", () => {
    const meta = { span: sampleSpan, durationMs: 12, evidenceIds: ["x"] };
    const outcome = done("value", meta);
    expect(outcome.tag).toBe("Done");
    expect(outcome.value).toBe("value");
    expect(outcome.meta).toEqual(meta);
  });

  it("constructs Fail outcomes with failures", () => {
    const diag = errorDiag("E0001", "err");
    const failureObj = failure("timeout", "took too long", {
      diagnostics: [diag],
      recoverable: true,
    });
    const outcome = fail(failureObj, { durationMs: 5 });
    expect(outcome.tag).toBe("Fail");
    expect(outcome.failure).toBe(failureObj);
    expect(outcome.failure.diagnostics).toEqual([diag]);
    expect(outcome.meta.durationMs).toBe(5);
  });

  it("constructs Pause outcomes that can resume", async () => {
    const suspended = {
      reason: { tag: "AwaitingHumanInput" as const, prompt: "Proceed?" },
      resume: async (input: unknown) => done(String(input)),
      state: { pending: true },
    };
    const outcome = pause<string>(suspended, { budgetUsed: { tokens: 10 } });
    expect(outcome.tag).toBe("Pause");
    expect(outcome.suspended.reason.tag).toBe("AwaitingHumanInput");
    const resumed = await outcome.suspended.resume(42);
    expect(isDone(resumed)).toBe(true);
    if (isDone(resumed)) {
      expect(resumed.value).toBe("42");
    }
  });

  it("type guards discriminate outcome variants", () => {
    const doneOutcome = done(1);
    const failOutcome = fail(failure("internal-error", "boom"));
    const pauseOutcome = pause({
      reason: { tag: "Custom", reason: "wait" },
      resume: async () => done("ok"),
    });

    expect(isDone(doneOutcome)).toBe(true);
    expect(isFail(doneOutcome)).toBe(false);
    expect(isFail(failOutcome)).toBe(true);
    expect(isPause(failOutcome)).toBe(false);
    expect(isPause(pauseOutcome)).toBe(true);
  });

  it("pattern matches outcomes exhaustively", () => {
    const doneOutcome = done("ok");
    const failOutcome = fail(failure("validation-failed", "bad input"));
    const pauseOutcome = pause({
      reason: { tag: "AwaitingApproval", action: "run tool" },
      resume: async () => done("resumed"),
    });

    const doneResult = match(doneOutcome, {
      done: (d) => `done:${d.value}`,
      fail: (f) => `fail:${f.failure.message}`,
      pause: (p) => `pause:${p.suspended.reason.tag}`,
    });
    expect(doneResult).toBe("done:ok");

    const failResult = match(failOutcome, {
      done: () => "nope",
      fail: (f) => f.failure.reason,
      pause: () => "pause",
    });
    expect(failResult).toBe("validation-failed");

    const pauseResult = match(pauseOutcome, {
      done: () => "nope",
      fail: () => "fail",
      pause: (p) => p.suspended.reason.tag,
    });
    expect(pauseResult).toBe("AwaitingApproval");
  });

  it("maps and flatMaps over successful outcomes only", async () => {
    const doneOutcome = done(2);
    const mapped = mapOutcome(doneOutcome, (n) => n * 2);
    expect(isDone(mapped)).toBe(true);
    if (isDone(mapped)) {
      expect(mapped.value).toBe(4);
    }

    const failOutcome = fail(failure("internal-error", "bad"));
    const untouched = mapOutcome(failOutcome, () => "ignored");
    expect(untouched).toBe(failOutcome);

    const flatMapped = await flatMapOutcome(doneOutcome, async (n) =>
      done(n + 1)
    );
    expect(isDone(flatMapped)).toBe(true);
    if (isDone(flatMapped)) {
      expect(flatMapped.value).toBe(3);
    }

    const passthrough = await flatMapOutcome(failOutcome, async () =>
      done("never")
    );
    expect(passthrough).toBe(failOutcome);
  });

  it("unwraps successes and throws on failures or pauses", () => {
    const success = done("value");
    expect(unwrap(success)).toBe("value");

    const failing = fail(failure("internal-error", "boom"));
    expect(() => unwrap(failing)).toThrow("boom");

    const paused = pause({
      reason: { tag: "Custom", reason: "stop" },
      resume: async () => done("later"),
    });
    expect(() => unwrap(paused)).toThrow(/paused/i);
    expect(unwrapOr(failing, "fallback")).toBe("fallback");
  });
});

describe("Failure", () => {
  it("defaults diagnostics and recoverable flags", () => {
    const f = failure("validation-failed", "bad input");
    expect(f.diagnostics).toEqual([]);
    expect(f.recoverable).toBe(false);
  });

  it("wraps failures and merges context while preserving causes", () => {
    const diag = errorDiag("E0400", "schema failed");
    const inner = failure("schema-mismatch", "inner", {
      diagnostics: [diag],
      context: { field: "name" },
      recoverable: true,
    });
    const outer = wrapFailure(inner, "outer", { attempt: 1 });

    expect(outer.cause).toBe(inner);
    expect(outer.context).toMatchObject({ field: "name", attempt: 1 });
    expect(outer.recoverable).toBe(true);

    const diags = allDiagnostics(outer);
    expect(diags).toContain(diag);
    expect(diags.length).toBe(1);
  });
});

describe("Diagnostics and codes", () => {
  it("creates diagnostics with interpolation and severity", () => {
    const diag = makeDiagnostic("E0301", { resource: "tokens" }, sampleSpan);
    expect(diag.code).toBe("E0301");
    expect(diag.severity).toBe("error");
    expect(diag.message).toContain("tokens");
    expect(diag.span).toEqual(sampleSpan);
  });

  it("creates warning diagnostics", () => {
    const diag = warnDiag("W0001", "careful", { data: { depth: 3 } });
    expect(diag.severity).toBe("warning");
    expect(diag.data).toMatchObject({ depth: 3 });
  });

  it("exposes all diagnostic codes", () => {
    expect(Object.keys(DIAGNOSTIC_CODES)).toContain("E0001");
    expect(Object.keys(DIAGNOSTIC_CODES)).toContain("W0005");
  });
});

describe("Outcome constructor helpers", () => {
  it("creates budget and timeout failures with diagnostics", () => {
    const budget = budgetExceeded("tokens");
    expect(budget.failure.reason).toBe("budget-exceeded");
    expect(budget.failure.diagnostics[0]?.code).toBe("E0301");
    expect(budget.failure.recoverable).toBe(false);

    const timedOut = timeout(50);
    expect(timedOut.failure.reason).toBe("timeout");
    expect(timedOut.failure.diagnostics[0]?.code).toBe("E0300");
    expect(timedOut.failure.recoverable).toBe(true);
  });

  it("creates validation, tool, and capability failures", () => {
    const validation = validationFailed("invalid", { field: "name" });
    expect(validation.failure.reason).toBe("validation-failed");
    expect(validation.failure.diagnostics[0]?.code).toBe("E0400");
    expect(validation.failure.context).toMatchObject({ field: "name" });

    const tool = toolError("search", "down");
    expect(tool.failure.reason).toBe("tool-error");
    expect(tool.failure.diagnostics[0]?.code).toBe("E0303");

    const cap = capabilityDenied("inference");
    expect(cap.failure.reason).toBe("precondition-failed");
    expect(cap.failure.diagnostics[0]?.code).toBe("E0500");
  });

  it("aliases ok/err for success/failure outcomes", () => {
    const success = ok(9);
    expect(success.tag).toBe("Done");

    const failureOutcome = err("internal-error", "boom");
    expect(failureOutcome.tag).toBe("Fail");
    expect(failureOutcome.failure.reason).toBe("internal-error");
    expect(failureOutcome.failure.message).toBe("boom");
  });
});
