// test/core/opr/validate.spec.ts
// Tests for validation pipeline (ΩPR validation with structured violations)

import { describe, it, expect } from "vitest";
import {
  validateKernelOutput,
  checkProgressInvariants,
  validateFieldType,
  validateEffectsArray,
} from "../../../src/core/opr/validate";
import type {
  ValidationViolation,
  KernelState,
  ProgressInvariants,
} from "../../../src/core/opr/types";

describe("validateKernelOutput", () => {
  describe("V1: accepts valid kernel output", () => {
    it("returns ok: true for valid JSON with all required fields", () => {
      const validOutput = JSON.stringify({
        kernel: "kernel_123",
        op: "step",
        ok: true,
        result: { value: 42 },
        next_state: { iteration: 1, facts: ["fact1"] },
        effects: [],
        diagnostics: { notes: ["All good"] },
      });

      const result = validateKernelOutput(validOutput, {
        kernelId: "kernel_123",
        op: "step",
      });

      expect(result.ok).toBe(true);
      expect(result.violations).toHaveLength(0);
    });

    it("handles effects with proper structure", () => {
      const validOutput = JSON.stringify({
        kernel: "kernel_456",
        op: "execute",
        ok: true,
        result: null,
        next_state: null,
        effects: [
          {
            type: "callback.eval_lisp",
            idempotency_key: "key_1",
            correlation_id: "corr_1",
            payload: { code: "(+ 1 2)" },
          },
        ],
        diagnostics: {},
      });

      const result = validateKernelOutput(validOutput, {
        kernelId: "kernel_456",
        op: "execute",
      });

      expect(result.ok).toBe(true);
      expect(result.violations).toHaveLength(0);
    });

    it("allows next_state to be null", () => {
      const output = JSON.stringify({
        kernel: "k1",
        op: "check",
        ok: false,
        result: null,
        next_state: null,
        effects: [],
        diagnostics: { errors: ["Check failed"] },
      });

      const result = validateKernelOutput(output, {
        kernelId: "k1",
        op: "check",
      });

      expect(result.ok).toBe(true);
      expect(result.violations).toHaveLength(0);
    });
  });

  describe("V2: rejects non-JSON with NOT_JSON code", () => {
    it("returns NOT_JSON violation for invalid JSON", () => {
      const invalidJson = "{ invalid json }";

      const result = validateKernelOutput(invalidJson, {
        kernelId: "k1",
        op: "step",
      });

      expect(result.ok).toBe(false);
      expect(result.violations).toHaveLength(1);
      expect(result.violations[0]).toMatchObject({
        path: "$",
        code: "NOT_JSON",
      });
      expect(result.violations[0].message).toContain("Invalid JSON");
    });

    it("includes error message in violation", () => {
      const result = validateKernelOutput('{"incomplete"', {
        kernelId: "k1",
        op: "step",
      });

      expect(result.ok).toBe(false);
      expect(result.violations[0].code).toBe("NOT_JSON");
      expect(result.violations[0].message).toBeTruthy();
    });
  });

  describe("V3: rejects missing required fields", () => {
    it("detects missing 'kernel' field", () => {
      const missingKernel = JSON.stringify({
        op: "step",
        ok: true,
        result: null,
        next_state: null,
        effects: [],
        diagnostics: {},
      });

      const result = validateKernelOutput(missingKernel, {
        kernelId: "k1",
        op: "step",
      });

      expect(result.ok).toBe(false);
      const violation = result.violations.find((v) => v.code === "MISSING_FIELD");
      expect(violation).toBeDefined();
      expect(violation?.path).toBe("$.kernel");
    });

    it("detects missing multiple fields", () => {
      const minimal = JSON.stringify({
        kernel: "k1",
      });

      const result = validateKernelOutput(minimal, {
        kernelId: "k1",
        op: "step",
      });

      expect(result.ok).toBe(false);
      const missingFields = result.violations.filter(
        (v) => v.code === "MISSING_FIELD"
      );
      expect(missingFields.length).toBeGreaterThan(0);
    });

    it("stops validation and returns early with missing required fields", () => {
      const result = validateKernelOutput(JSON.stringify({ kernel: "k1" }), {
        kernelId: "k1",
        op: "step",
      });

      expect(result.ok).toBe(false);
      // Should have violations but not proceed to later checks
      expect(result.violations.length).toBeGreaterThan(0);
      const mismatchViolations = result.violations.filter(
        (v) => v.code === "OP_MISMATCH"
      );
      expect(mismatchViolations).toHaveLength(0);
    });
  });

  describe("V4: rejects kernel/op mismatch", () => {
    it("detects kernel ID mismatch", () => {
      const output = JSON.stringify({
        kernel: "wrong_kernel",
        op: "step",
        ok: true,
        result: null,
        next_state: null,
        effects: [],
        diagnostics: {},
      });

      const result = validateKernelOutput(output, {
        kernelId: "expected_kernel",
        op: "step",
      });

      expect(result.ok).toBe(false);
      const violation = result.violations.find(
        (v) => v.code === "KERNEL_MISMATCH"
      );
      expect(violation).toBeDefined();
      expect(violation?.path).toBe("$.kernel");
      expect(violation?.expected).toBe("expected_kernel");
      expect(violation?.actual).toBe("wrong_kernel");
    });

    it("detects operation mismatch", () => {
      const output = JSON.stringify({
        kernel: "k1",
        op: "wrong_op",
        ok: true,
        result: null,
        next_state: null,
        effects: [],
        diagnostics: {},
      });

      const result = validateKernelOutput(output, {
        kernelId: "k1",
        op: "correct_op",
      });

      expect(result.ok).toBe(false);
      const violation = result.violations.find((v) => v.code === "OP_MISMATCH");
      expect(violation).toBeDefined();
      expect(violation?.expected).toBe("correct_op");
      expect(violation?.actual).toBe("wrong_op");
    });
  });

  describe("V5: provides structured violations for repair prompt", () => {
    it("includes parsed object in result even on validation failure", () => {
      const output = JSON.stringify({
        kernel: "k1",
        op: "step",
        ok: true,
        result: { data: "test" },
        next_state: null,
        effects: [],
        diagnostics: {},
      });

      const result = validateKernelOutput(output, {
        kernelId: "wrong_k1",
        op: "step",
      });

      expect(result.ok).toBe(false);
      expect(result.parsed).toBeDefined();
      expect(result.parsed).toEqual(expect.objectContaining({ kernel: "k1" }));
    });

    it("includes violation path for targeted repairs", () => {
      const output = JSON.stringify({
        kernel: "k1",
        op: "step",
        ok: "not_boolean",
        result: null,
        next_state: null,
        effects: [],
        diagnostics: {},
      });

      const result = validateKernelOutput(output, {
        kernelId: "k1",
        op: "step",
      });

      expect(result.ok).toBe(false);
      const typeViolation = result.violations.find((v) => v.code === "WRONG_TYPE");
      expect(typeViolation?.path).toBe("$.ok");
      expect(typeViolation?.expected).toBe("boolean");
      expect(typeViolation?.actual).toBe("string");
    });

    it("collects all violations in single pass", () => {
      const output = JSON.stringify({
        kernel: "wrong",
        op: "wrong",
        ok: "not_bool",
        result: null,
        next_state: null,
        effects: "not_array",
        diagnostics: {},
      });

      const result = validateKernelOutput(output, {
        kernelId: "correct",
        op: "correct",
      });

      expect(result.ok).toBe(false);
      expect(result.violations.length).toBeGreaterThan(1);
      const codes = result.violations.map((v) => v.code);
      expect(codes).toContain("KERNEL_MISMATCH");
      expect(codes).toContain("OP_MISMATCH");
      expect(codes).toContain("WRONG_TYPE");
    });
  });

  describe("V6: validates effects array elements", () => {
    it("accepts effects with required fields", () => {
      const output = JSON.stringify({
        kernel: "k1",
        op: "step",
        ok: true,
        result: null,
        next_state: null,
        effects: [
          {
            type: "callback.artifact.get",
            idempotency_key: "idem_1",
            payload: { artifact_id: "art_123" },
          },
        ],
        diagnostics: {},
      });

      const result = validateKernelOutput(output, {
        kernelId: "k1",
        op: "step",
      });

      expect(result.ok).toBe(true);
      expect(result.violations).toHaveLength(0);
    });

    it("rejects non-array effects", () => {
      const output = JSON.stringify({
        kernel: "k1",
        op: "step",
        ok: true,
        result: null,
        next_state: null,
        effects: { type: "callback.eval_lisp" },
        diagnostics: {},
      });

      const result = validateKernelOutput(output, {
        kernelId: "k1",
        op: "step",
      });

      expect(result.ok).toBe(false);
      const violation = result.violations.find(
        (v) => v.code === "WRONG_TYPE" && v.path === "$.effects"
      );
      expect(violation).toBeDefined();
      expect(violation?.expected).toBe("array");
      expect(violation?.actual).toBe("object");
    });

    it("detects missing 'type' in effect element", () => {
      const output = JSON.stringify({
        kernel: "k1",
        op: "step",
        ok: true,
        result: null,
        next_state: null,
        effects: [
          {
            idempotency_key: "idem_1",
            payload: {},
          },
        ],
        diagnostics: {},
      });

      const result = validateKernelOutput(output, {
        kernelId: "k1",
        op: "step",
      });

      expect(result.ok).toBe(false);
      const violation = result.violations.find(
        (v) => v.path === "$.effects[0].type"
      );
      expect(violation).toBeDefined();
      expect(violation?.code).toBe("MISSING_FIELD");
    });

    it("detects missing 'idempotency_key' in effect element", () => {
      const output = JSON.stringify({
        kernel: "k1",
        op: "step",
        ok: true,
        result: null,
        next_state: null,
        effects: [
          {
            type: "callback.eval_lisp",
            payload: { code: "()" },
          },
        ],
        diagnostics: {},
      });

      const result = validateKernelOutput(output, {
        kernelId: "k1",
        op: "step",
      });

      expect(result.ok).toBe(false);
      const violation = result.violations.find(
        (v) => v.path === "$.effects[0].idempotency_key"
      );
      expect(violation).toBeDefined();
      expect(violation?.code).toBe("MISSING_FIELD");
    });

    it("detects non-object effect elements", () => {
      const output = JSON.stringify({
        kernel: "k1",
        op: "step",
        ok: true,
        result: null,
        next_state: null,
        effects: [null, "string", 123],
        diagnostics: {},
      });

      const result = validateKernelOutput(output, {
        kernelId: "k1",
        op: "step",
      });

      expect(result.ok).toBe(false);
      const violations = result.violations.filter((v) =>
        v.path.startsWith("$.effects[")
      );
      expect(violations.length).toBeGreaterThanOrEqual(3);
    });
  });

  describe("V7: enforces iteration monotonicity", () => {
    it("accepts increasing iteration values", () => {
      const prevState: KernelState = { iteration: 5, facts: ["f1"] };
      const nextState: KernelState = { iteration: 6, facts: ["f1", "f2"] };
      const invariants: ProgressInvariants = {
        iterationMonotonic: true,
        derivedMonotonic: false,
        deltaTermination: false,
      };

      const violations = checkProgressInvariants(prevState, nextState, invariants);

      expect(violations).toHaveLength(0);
    });

    it("rejects decreasing iteration values", () => {
      const prevState: KernelState = { iteration: 10 };
      const nextState: KernelState = { iteration: 9 };
      const invariants: ProgressInvariants = {
        iterationMonotonic: true,
        derivedMonotonic: false,
        deltaTermination: false,
      };

      const violations = checkProgressInvariants(prevState, nextState, invariants);

      expect(violations).toHaveLength(1);
      expect(violations[0]).toMatchObject({
        path: "$.next_state.iteration",
        code: "INVALID_VALUE",
      });
      expect(violations[0].message).toContain("must increase");
      expect(violations[0].message).toContain("->");
    });

    it("rejects same iteration value", () => {
      const prevState: KernelState = { iteration: 5 };
      const nextState: KernelState = { iteration: 5 };
      const invariants: ProgressInvariants = {
        iterationMonotonic: true,
        derivedMonotonic: false,
        deltaTermination: false,
      };

      const violations = checkProgressInvariants(prevState, nextState, invariants);

      expect(violations).toHaveLength(1);
      expect(violations[0].code).toBe("INVALID_VALUE");
    });

    it("skips check when iterationMonotonic is false", () => {
      const prevState: KernelState = { iteration: 10 };
      const nextState: KernelState = { iteration: 5 };
      const invariants: ProgressInvariants = {
        iterationMonotonic: false,
        derivedMonotonic: false,
        deltaTermination: false,
      };

      const violations = checkProgressInvariants(prevState, nextState, invariants);

      expect(violations).toHaveLength(0);
    });

    it("handles missing iteration field (treats as 0)", () => {
      const prevState: KernelState = {};
      const nextState: KernelState = { iteration: 1 };
      const invariants: ProgressInvariants = {
        iterationMonotonic: true,
        derivedMonotonic: false,
        deltaTermination: false,
      };

      const violations = checkProgressInvariants(prevState, nextState, invariants);

      expect(violations).toHaveLength(0);
    });
  });

  describe("V8: enforces derived facts monotonicity", () => {
    it("accepts growing derived facts array", () => {
      const prevState: KernelState = { derived: ["d1", "d2"] };
      const nextState: KernelState = { derived: ["d1", "d2", "d3"] };
      const invariants: ProgressInvariants = {
        iterationMonotonic: false,
        derivedMonotonic: true,
        deltaTermination: false,
      };

      const violations = checkProgressInvariants(prevState, nextState, invariants);

      expect(violations).toHaveLength(0);
    });

    it("rejects shrinking derived facts array", () => {
      const prevState: KernelState = { derived: ["d1", "d2", "d3"] };
      const nextState: KernelState = { derived: ["d1"] };
      const invariants: ProgressInvariants = {
        iterationMonotonic: false,
        derivedMonotonic: true,
        deltaTermination: false,
      };

      const violations = checkProgressInvariants(prevState, nextState, invariants);

      expect(violations).toHaveLength(1);
      expect(violations[0]).toMatchObject({
        path: "$.next_state.derived",
        code: "INVALID_VALUE",
      });
      expect(violations[0].message).toContain("monotonically");
      expect(violations[0].message).toContain("->");
    });

    it("accepts same derived facts size", () => {
      const prevState: KernelState = { derived: ["d1", "d2"] };
      const nextState: KernelState = { derived: ["x1", "x2"] };
      const invariants: ProgressInvariants = {
        iterationMonotonic: false,
        derivedMonotonic: true,
        deltaTermination: false,
      };

      const violations = checkProgressInvariants(prevState, nextState, invariants);

      expect(violations).toHaveLength(0);
    });

    it("skips check when derivedMonotonic is false", () => {
      const prevState: KernelState = { derived: ["d1", "d2"] };
      const nextState: KernelState = { derived: ["d1"] };
      const invariants: ProgressInvariants = {
        iterationMonotonic: false,
        derivedMonotonic: false,
        deltaTermination: false,
      };

      const violations = checkProgressInvariants(prevState, nextState, invariants);

      expect(violations).toHaveLength(0);
    });

    it("handles missing derived field (treats as empty)", () => {
      const prevState: KernelState = {};
      const nextState: KernelState = { derived: ["d1"] };
      const invariants: ProgressInvariants = {
        iterationMonotonic: false,
        derivedMonotonic: true,
        deltaTermination: false,
      };

      const violations = checkProgressInvariants(prevState, nextState, invariants);

      expect(violations).toHaveLength(0);
    });
  });

  describe("helper functions", () => {
    describe("validateFieldType", () => {
      it("adds violation when field type is wrong", () => {
        const violations: ValidationViolation[] = [];
        const obj = { name: 123 };

        validateFieldType(obj, "name", "string", violations);

        expect(violations).toHaveLength(1);
        expect(violations[0]).toMatchObject({
          path: "$.name",
          code: "WRONG_TYPE",
          expected: "string",
          actual: "number",
        });
      });

      it("does not add violation when field type is correct", () => {
        const violations: ValidationViolation[] = [];
        const obj = { name: "test" };

        validateFieldType(obj, "name", "string", violations);

        expect(violations).toHaveLength(0);
      });

      it("does not add violation when field is missing", () => {
        const violations: ValidationViolation[] = [];
        const obj = {};

        validateFieldType(obj, "name", "string", violations);

        expect(violations).toHaveLength(0);
      });
    });

    describe("validateEffectsArray", () => {
      it("validates array of valid effects", () => {
        const violations: ValidationViolation[] = [];
        const effects = [
          {
            type: "callback.eval_lisp",
            idempotency_key: "key1",
            payload: {},
          },
          {
            type: "callback.artifact.get",
            idempotency_key: "key2",
            payload: {},
          },
        ];

        validateEffectsArray(effects, violations);

        expect(violations).toHaveLength(0);
      });

      it("detects non-object array elements", () => {
        const violations: ValidationViolation[] = [];
        const effects: unknown[] = [null, "not_object"];

        validateEffectsArray(effects, violations);

        expect(violations.length).toBeGreaterThan(0);
        expect(violations[0].code).toBe("WRONG_TYPE");
        expect(violations[0].path).toBe("$.effects[0]");
      });

      it("detects missing type field", () => {
        const violations: ValidationViolation[] = [];
        const effects = [{ idempotency_key: "key1", payload: {} }];

        validateEffectsArray(effects, violations);

        expect(violations).toHaveLength(1);
        expect(violations[0].code).toBe("MISSING_FIELD");
        expect(violations[0].path).toBe("$.effects[0].type");
      });

      it("detects missing idempotency_key field", () => {
        const violations: ValidationViolation[] = [];
        const effects = [{ type: "callback.eval_lisp", payload: {} }];

        validateEffectsArray(effects, violations);

        expect(violations).toHaveLength(1);
        expect(violations[0].code).toBe("MISSING_FIELD");
        expect(violations[0].path).toBe("$.effects[0].idempotency_key");
      });

      it("allows optional fields", () => {
        const violations: ValidationViolation[] = [];
        const effects = [
          {
            type: "callback.eval_lisp",
            idempotency_key: "key1",
          },
        ];

        validateEffectsArray(effects, violations);

        expect(violations).toHaveLength(0);
      });
    });
  });

  describe("edge cases", () => {
    it("rejects non-object response", () => {
      const output = JSON.stringify([1, 2, 3]);

      const result = validateKernelOutput(output, {
        kernelId: "k1",
        op: "step",
      });

      expect(result.ok).toBe(false);
      expect(result.violations[0].code).toBe("NOT_OBJECT");
      expect(result.violations[0].actual).toBe("array");
    });

    it("rejects null response", () => {
      const output = JSON.stringify(null);

      const result = validateKernelOutput(output, {
        kernelId: "k1",
        op: "step",
      });

      expect(result.ok).toBe(false);
      expect(result.violations[0].code).toBe("NOT_OBJECT");
    });

    it("rejects primitive response", () => {
      const output = JSON.stringify("string");

      const result = validateKernelOutput(output, {
        kernelId: "k1",
        op: "step",
      });

      expect(result.ok).toBe(false);
      expect(result.violations[0].code).toBe("NOT_OBJECT");
    });

    it("parses JSON only once and reuses result", () => {
      const output = JSON.stringify({
        kernel: "k1",
        op: "step",
        ok: true,
        result: null,
        next_state: null,
        effects: [],
        diagnostics: {},
      });

      const result = validateKernelOutput(output, {
        kernelId: "k1",
        op: "step",
      });

      // Verify parsed result is available
      expect(result.parsed).toBeDefined();
      expect(result.parsed).toEqual(expect.objectContaining({ kernel: "k1" }));
    });

    it("handles checkProgressInvariants with null prevState", () => {
      const nextState: KernelState = { iteration: 1 };
      const invariants: ProgressInvariants = {
        iterationMonotonic: true,
        derivedMonotonic: true,
        deltaTermination: false,
      };

      const violations = checkProgressInvariants(null, nextState, invariants);

      expect(violations).toHaveLength(0);
    });
  });
});
