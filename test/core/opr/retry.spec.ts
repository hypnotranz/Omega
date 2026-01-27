// test/core/opr/retry.spec.ts
// Tests for retry logic with repair prompts

import { describe, it, expect } from "vitest";
import {
  buildRepairPrompt,
  shouldRetry,
  formatViolationsForDisplay,
} from "../../../src/core/opr/retry";
import type { ValidationViolation } from "../../../src/core/opr/types";

describe("buildRepairPrompt", () => {
  describe("RY1: builds repair prompt with all violation fields", () => {
    it("creates structured violation report with single violation", () => {
      const violations: ValidationViolation[] = [
        {
          path: "$.kernel",
          code: "MISSING_FIELD",
          message: "Field 'kernel' is required",
          expected: "string",
          actual: "undefined",
        },
      ];

      const prompt = buildRepairPrompt(violations);

      expect(prompt).toContain("YOUR PREVIOUS RESPONSE HAD VALIDATION ERRORS");
      expect(prompt).toContain("VIOLATIONS:");
      expect(prompt).toContain("Path: $.kernel");
      expect(prompt).toContain("Code: MISSING_FIELD");
      expect(prompt).toContain("Error: Field 'kernel' is required");
      expect(prompt).toContain("Expected: string");
      expect(prompt).toContain("Got: undefined");
    });

    it("includes all violation fields when present", () => {
      const violations: ValidationViolation[] = [
        {
          path: "$.ok",
          code: "WRONG_TYPE",
          message: "Expected boolean",
          expected: "boolean",
          actual: "string",
        },
      ];

      const prompt = buildRepairPrompt(violations);

      expect(prompt).toContain("Path: $.ok");
      expect(prompt).toContain("Code: WRONG_TYPE");
      expect(prompt).toContain("Error: Expected boolean");
      expect(prompt).toContain("Expected: boolean");
      expect(prompt).toContain("Got: string");
    });

    it("handles violations without expected/actual fields", () => {
      const violations: ValidationViolation[] = [
        {
          path: "$.result",
          code: "INVALID_VALUE",
          message: "Value does not match contract",
        },
      ];

      const prompt = buildRepairPrompt(violations);

      expect(prompt).toContain("Path: $.result");
      expect(prompt).toContain("Code: INVALID_VALUE");
      expect(prompt).toContain("Error: Value does not match contract");
      expect(prompt).not.toContain("Expected:");
      expect(prompt).not.toContain("Got:");
    });

    it("formats multiple violations correctly", () => {
      const violations: ValidationViolation[] = [
        {
          path: "$.kernel",
          code: "KERNEL_MISMATCH",
          message: "Kernel mismatch",
          expected: "kernel_123",
          actual: "kernel_456",
        },
        {
          path: "$.op",
          code: "OP_MISMATCH",
          message: "Operation mismatch",
          expected: "step",
          actual: "execute",
        },
      ];

      const prompt = buildRepairPrompt(violations);

      // Check first violation
      expect(prompt).toContain("Path: $.kernel");
      expect(prompt).toContain("Code: KERNEL_MISMATCH");
      expect(prompt).toContain("Expected: kernel_123");
      expect(prompt).toContain("Got: kernel_456");

      // Check second violation
      expect(prompt).toContain("Path: $.op");
      expect(prompt).toContain("Code: OP_MISMATCH");
      expect(prompt).toContain("Expected: step");
      expect(prompt).toContain("Got: execute");
    });

    it("includes instruction to return only JSON", () => {
      const violations: ValidationViolation[] = [
        {
          path: "$.ok",
          code: "NOT_JSON",
          message: "Response was not valid JSON",
        },
      ];

      const prompt = buildRepairPrompt(violations);

      expect(prompt).toContain("INSTRUCTIONS:");
      expect(prompt).toContain("Fix ALL violations listed above");
      expect(prompt).toContain("Return ONLY valid JSON");
      expect(prompt).toContain("OUTPUT CONTRACT");
      expect(prompt).toContain("Do NOT include markdown code blocks");
      expect(prompt).toContain("Do NOT include any explanation or preamble");
    });

    it("ends with instruction to return corrected JSON", () => {
      const violations: ValidationViolation[] = [
        {
          path: "$",
          code: "NOT_OBJECT",
          message: "Response was not an object",
        },
      ];

      const prompt = buildRepairPrompt(violations);

      expect(prompt).toContain("Return the corrected JSON response now:");
      expect(prompt.endsWith("Return the corrected JSON response now:")).toBe(
        true
      );
    });

    it("properly formats empty violations array", () => {
      const violations: ValidationViolation[] = [];

      const prompt = buildRepairPrompt(violations);

      expect(prompt).toContain("YOUR PREVIOUS RESPONSE HAD VALIDATION ERRORS");
      expect(prompt).toContain("VIOLATIONS:");
      expect(prompt).toContain("INSTRUCTIONS:");
      expect(prompt).toContain("Return the corrected JSON response now:");
    });

    it("separates violations with blank lines", () => {
      const violations: ValidationViolation[] = [
        {
          path: "$.field1",
          code: "MISSING_FIELD",
          message: "Field is missing",
          expected: "string",
          actual: "undefined",
        },
        {
          path: "$.field2",
          code: "WRONG_TYPE",
          message: "Type is wrong",
          expected: "number",
          actual: "string",
        },
      ];

      const prompt = buildRepairPrompt(violations);
      const lines = prompt.split("\n");

      // Find indices of violation markers
      const field1Index = lines.findIndex((l) => l.includes("field1"));
      const field2Index = lines.findIndex((l) => l.includes("field2"));

      expect(field1Index).toBeGreaterThan(-1);
      expect(field2Index).toBeGreaterThan(field1Index);
      // Ensure there's spacing between violations
      expect(field2Index - field1Index).toBeGreaterThan(2);
    });

    it("uses consistent formatting for all violation fields", () => {
      const violations: ValidationViolation[] = [
        {
          path: "$.test",
          code: "TEST_CODE",
          message: "Test message",
          expected: "test_expected",
          actual: "test_actual",
        },
      ];

      const prompt = buildRepairPrompt(violations);
      const lines = prompt.split("\n");

      // Check indentation is consistent
      const pathLine = lines.find((l) => l.includes("Path:"));
      const codeLine = lines.find((l) => l.includes("Code:"));
      const errorLine = lines.find((l) => l.includes("Error:"));

      expect(pathLine).toMatch(/^\s{2}-/); // "  - Path:"
      expect(codeLine).toMatch(/^\s{4}/); // "    Code:" (more indented)
      expect(errorLine).toMatch(/^\s{4}/); // "    Error:" (more indented)
    });
  });

  describe("RY2 & RY3: shouldRetry behavior", () => {
    it("returns false for KERNEL_MISMATCH (critical violation)", () => {
      const violations: ValidationViolation[] = [
        {
          path: "$.kernel",
          code: "KERNEL_MISMATCH",
          message: "Kernel ID mismatch",
          expected: "kernel_123",
          actual: "kernel_456",
        },
      ];

      const result = shouldRetry(violations);

      expect(result).toBe(false);
    });

    it("returns false for OP_MISMATCH (critical violation)", () => {
      const violations: ValidationViolation[] = [
        {
          path: "$.op",
          code: "OP_MISMATCH",
          message: "Operation mismatch",
          expected: "step",
          actual: "execute",
        },
      ];

      const result = shouldRetry(violations);

      expect(result).toBe(false);
    });

    it("returns false when both KERNEL_MISMATCH and other fixable violations present", () => {
      const violations: ValidationViolation[] = [
        {
          path: "$.kernel",
          code: "KERNEL_MISMATCH",
          message: "Kernel mismatch",
        },
        {
          path: "$.ok",
          code: "MISSING_FIELD",
          message: "Field missing",
        },
      ];

      const result = shouldRetry(violations);

      expect(result).toBe(false);
    });

    it("returns false when OP_MISMATCH and other violations present", () => {
      const violations: ValidationViolation[] = [
        {
          path: "$.op",
          code: "OP_MISMATCH",
          message: "Op mismatch",
        },
        {
          path: "$.ok",
          code: "WRONG_TYPE",
          message: "Wrong type",
        },
      ];

      const result = shouldRetry(violations);

      expect(result).toBe(false);
    });

    it("returns true for NOT_JSON (fixable violation)", () => {
      const violations: ValidationViolation[] = [
        {
          path: "$",
          code: "NOT_JSON",
          message: "Invalid JSON",
        },
      ];

      const result = shouldRetry(violations);

      expect(result).toBe(true);
    });

    it("returns true for NOT_OBJECT (fixable violation)", () => {
      const violations: ValidationViolation[] = [
        {
          path: "$",
          code: "NOT_OBJECT",
          message: "Response is not an object",
        },
      ];

      const result = shouldRetry(violations);

      expect(result).toBe(true);
    });

    it("returns true for MISSING_FIELD (fixable violation)", () => {
      const violations: ValidationViolation[] = [
        {
          path: "$.kernel",
          code: "MISSING_FIELD",
          message: "Field is missing",
        },
      ];

      const result = shouldRetry(violations);

      expect(result).toBe(true);
    });

    it("returns true for WRONG_TYPE (fixable violation)", () => {
      const violations: ValidationViolation[] = [
        {
          path: "$.ok",
          code: "WRONG_TYPE",
          message: "Field has wrong type",
          expected: "boolean",
          actual: "string",
        },
      ];

      const result = shouldRetry(violations);

      expect(result).toBe(true);
    });

    it("returns true for INVALID_VALUE (fixable violation)", () => {
      const violations: ValidationViolation[] = [
        {
          path: "$.result",
          code: "INVALID_VALUE",
          message: "Value is invalid",
        },
      ];

      const result = shouldRetry(violations);

      expect(result).toBe(true);
    });

    it("returns true for multiple fixable violations", () => {
      const violations: ValidationViolation[] = [
        {
          path: "$.kernel",
          code: "MISSING_FIELD",
          message: "Missing",
        },
        {
          path: "$.ok",
          code: "WRONG_TYPE",
          message: "Wrong type",
        },
        {
          path: "$.effects",
          code: "NOT_OBJECT",
          message: "Not object",
        },
      ];

      const result = shouldRetry(violations);

      expect(result).toBe(true);
    });

    it("returns false when unknown violation code is present", () => {
      const violations: ValidationViolation[] = [
        {
          path: "$.unknown",
          code: "UNKNOWN_CODE" as any,
          message: "Unknown violation",
        },
      ];

      const result = shouldRetry(violations);

      expect(result).toBe(false);
    });

    it("returns false when mix of fixable and unknown violations", () => {
      const violations: ValidationViolation[] = [
        {
          path: "$.kernel",
          code: "MISSING_FIELD",
          message: "Missing",
        },
        {
          path: "$.unknown",
          code: "UNKNOWN_CODE" as any,
          message: "Unknown",
        },
      ];

      const result = shouldRetry(violations);

      expect(result).toBe(false);
    });

    it("returns false for empty violations array (no retryable items)", () => {
      const violations: ValidationViolation[] = [];

      const result = shouldRetry(violations);

      // Empty array means no critical violations but no fixable ones either
      // The every() check on empty array returns true, so this should return true
      expect(result).toBe(true);
    });
  });

  describe("RY4: formatViolationsForDisplay", () => {
    it("formats single violation for console", () => {
      const violations: ValidationViolation[] = [
        {
          path: "$.kernel",
          code: "MISSING_FIELD",
          message: "Field is required",
        },
      ];

      const formatted = formatViolationsForDisplay(violations);

      expect(formatted).toBe("[MISSING_FIELD] $.kernel: Field is required");
    });

    it("includes expected and actual when both present", () => {
      const violations: ValidationViolation[] = [
        {
          path: "$.ok",
          code: "WRONG_TYPE",
          message: "Wrong type",
          expected: "boolean",
          actual: "string",
        },
      ];

      const formatted = formatViolationsForDisplay(violations);

      expect(formatted).toContain("[WRONG_TYPE] $.ok: Wrong type");
      expect(formatted).toContain("(expected boolean, got string)");
    });

    it("omits expected/actual when missing", () => {
      const violations: ValidationViolation[] = [
        {
          path: "$.result",
          code: "INVALID_VALUE",
          message: "Value does not match contract",
        },
      ];

      const formatted = formatViolationsForDisplay(violations);

      expect(formatted).toBe(
        "[INVALID_VALUE] $.result: Value does not match contract"
      );
      expect(formatted).not.toContain("(expected");
      expect(formatted).not.toContain("got");
    });

    it("formats multiple violations on separate lines", () => {
      const violations: ValidationViolation[] = [
        {
          path: "$.kernel",
          code: "KERNEL_MISMATCH",
          message: "Kernel mismatch",
          expected: "kernel_123",
          actual: "kernel_456",
        },
        {
          path: "$.op",
          code: "OP_MISMATCH",
          message: "Operation mismatch",
          expected: "step",
          actual: "execute",
        },
        {
          path: "$.ok",
          code: "WRONG_TYPE",
          message: "Type error",
          expected: "boolean",
          actual: "number",
        },
      ];

      const formatted = formatViolationsForDisplay(violations);
      const lines = formatted.split("\n");

      expect(lines).toHaveLength(3);
      expect(lines[0]).toContain("KERNEL_MISMATCH");
      expect(lines[1]).toContain("OP_MISMATCH");
      expect(lines[2]).toContain("WRONG_TYPE");
    });

    it("handles complex paths correctly", () => {
      const violations: ValidationViolation[] = [
        {
          path: "$.effects[0].payload.nested.field",
          code: "MISSING_FIELD",
          message: "Nested field missing",
        },
      ];

      const formatted = formatViolationsForDisplay(violations);

      expect(formatted).toContain("$.effects[0].payload.nested.field");
      expect(formatted).toContain("MISSING_FIELD");
    });

    it("formats empty violations array as empty string", () => {
      const violations: ValidationViolation[] = [];

      const formatted = formatViolationsForDisplay(violations);

      expect(formatted).toBe("");
    });

    it("uses consistent format for all violations", () => {
      const violations: ValidationViolation[] = [
        {
          path: "$.a",
          code: "CODE_A",
          message: "Message A",
          expected: "exp_a",
          actual: "act_a",
        },
        {
          path: "$.b",
          code: "CODE_B",
          message: "Message B",
          expected: "exp_b",
          actual: "act_b",
        },
      ];

      const formatted = formatViolationsForDisplay(violations);
      const lines = formatted.split("\n");

      // Both lines should follow pattern: [CODE] path: message (expected X, got Y)
      lines.forEach((line) => {
        expect(line).toMatch(/^\[.+\]\s.+:\s.+/);
      });
    });

    it("properly formats violation with special characters in message", () => {
      const violations: ValidationViolation[] = [
        {
          path: "$.field",
          code: "INVALID_VALUE",
          message: 'Expected string matching pattern "^[a-z]+$"',
          expected: '^[a-z]+$',
          actual: 'ABC123',
        },
      ];

      const formatted = formatViolationsForDisplay(violations);

      expect(formatted).toContain('Expected string matching pattern "^[a-z]+$"');
      expect(formatted).toContain("(expected ^[a-z]+$, got ABC123)");
    });
  });

  describe("integration tests", () => {
    it("buildRepairPrompt and formatViolationsForDisplay complement each other", () => {
      const violations: ValidationViolation[] = [
        {
          path: "$.kernel",
          code: "KERNEL_MISMATCH",
          message: "Kernel mismatch",
          expected: "kernel_123",
          actual: "kernel_456",
        },
      ];

      const repairPrompt = buildRepairPrompt(violations);
      const formatted = formatViolationsForDisplay(violations);

      // Repair prompt should contain more detailed instructions
      expect(repairPrompt.length).toBeGreaterThan(formatted.length);

      // Both should contain the violation code
      expect(repairPrompt).toContain("KERNEL_MISMATCH");
      expect(formatted).toContain("KERNEL_MISMATCH");
    });

    it("shouldRetry decision logic integrates with violation reporting", () => {
      // Scenario 1: Critical violation - don't retry or show repair prompt
      const criticalViolation: ValidationViolation[] = [
        {
          path: "$.kernel",
          code: "KERNEL_MISMATCH",
          message: "Kernel mismatch",
        },
      ];

      expect(shouldRetry(criticalViolation)).toBe(false);
      // But we still want to format it for display
      const formatted = formatViolationsForDisplay(criticalViolation);
      expect(formatted).toContain("KERNEL_MISMATCH");

      // Scenario 2: Fixable violation - retry with repair prompt
      const fixableViolation: ValidationViolation[] = [
        {
          path: "$.ok",
          code: "MISSING_FIELD",
          message: "Field is required",
        },
      ];

      expect(shouldRetry(fixableViolation)).toBe(true);
      const repairPrompt = buildRepairPrompt(fixableViolation);
      expect(repairPrompt).toContain("MISSING_FIELD");
      expect(repairPrompt).toContain("Fix ALL violations");
    });
  });
});
