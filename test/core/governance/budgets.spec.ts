// test/core/governance/budgets.spec.ts
// Tests for budget enforcement (circuit breaker semantics)

import { describe, it, expect } from "vitest";
import {
  budgetDefault,
  budgetConsumeOracleTurn,
  budgetConsumeEvalStep,
  budgetConsumeToolCall,
  budgetRemaining,
  BudgetTracker,
} from "../../../src/core/governance/budgets";

describe("budget system", () => {
  describe("budgetDefault", () => {
    it("creates budget with default limits", () => {
      const b = budgetDefault();
      expect(b.limits.maxOracleTurns).toBe(10_000);
      expect(b.limits.maxEvalSteps).toBe(500_000);
      expect(b.limits.maxToolCalls).toBe(9999);
      expect(b.consumed.oracleTurns).toBe(0);
      expect(b.consumed.evalSteps).toBe(0);
      expect(b.consumed.toolCalls).toBe(0);
    });

    it("accepts partial overrides", () => {
      const b = budgetDefault({ maxOracleTurns: 100 });
      expect(b.limits.maxOracleTurns).toBe(100);
      expect(b.limits.maxEvalSteps).toBe(500_000); // default
    });
  });

  describe("budgetConsumeOracleTurn", () => {
    it("increments oracle turns", () => {
      const b0 = budgetDefault({ maxOracleTurns: 10 });
      const b1 = budgetConsumeOracleTurn(b0);
      expect(b1.consumed.oracleTurns).toBe(1);
      expect(b0.consumed.oracleTurns).toBe(0); // immutable
    });

    it("throws when budget exhausted", () => {
      const b0 = budgetDefault({ maxOracleTurns: 2 });
      const b1 = budgetConsumeOracleTurn(b0);
      const b2 = budgetConsumeOracleTurn(b1);
      expect(() => budgetConsumeOracleTurn(b2)).toThrow("budget exhausted: oracleTurns");
    });
  });

  describe("budgetConsumeEvalStep", () => {
    it("increments eval steps by 1 by default", () => {
      const b0 = budgetDefault({ maxEvalSteps: 100 });
      const b1 = budgetConsumeEvalStep(b0);
      expect(b1.consumed.evalSteps).toBe(1);
    });

    it("increments eval steps by specified amount", () => {
      const b0 = budgetDefault({ maxEvalSteps: 100 });
      const b1 = budgetConsumeEvalStep(b0, 50);
      expect(b1.consumed.evalSteps).toBe(50);
    });

    it("throws when budget exhausted", () => {
      const b0 = budgetDefault({ maxEvalSteps: 10 });
      const b1 = budgetConsumeEvalStep(b0, 8);
      expect(() => budgetConsumeEvalStep(b1, 5)).toThrow("budget exhausted: evalSteps");
    });
  });

  describe("budgetConsumeToolCall", () => {
    it("increments tool calls", () => {
      const b0 = budgetDefault({ maxToolCalls: 10 });
      const b1 = budgetConsumeToolCall(b0);
      expect(b1.consumed.toolCalls).toBe(1);
    });

    it("throws when budget exhausted", () => {
      const b0 = budgetDefault({ maxToolCalls: 1 });
      const b1 = budgetConsumeToolCall(b0);
      expect(() => budgetConsumeToolCall(b1)).toThrow("budget exhausted: toolCalls");
    });
  });

  describe("budgetRemaining", () => {
    it("calculates remaining budget", () => {
      const b0 = budgetDefault({ maxOracleTurns: 100, maxEvalSteps: 1000, maxToolCalls: 50 });
      const b1 = budgetConsumeOracleTurn(b0);
      const b2 = budgetConsumeEvalStep(b1, 100);
      const b3 = budgetConsumeToolCall(b2);

      const remaining = budgetRemaining(b3);
      expect(remaining.maxOracleTurns).toBe(99);
      expect(remaining.maxEvalSteps).toBe(900);
      expect(remaining.maxToolCalls).toBe(49);
    });
  });

  describe("BudgetTracker (mutable wrapper)", () => {
    it("creates tracker with default limits", () => {
      const tracker = new BudgetTracker();
      const snap = tracker.snapshot();
      expect(snap.limits.maxOracleTurns).toBe(10_000);
      expect(snap.limits.maxEvalSteps).toBe(500_000);
      expect(snap.limits.maxToolCalls).toBe(9999);
      expect(snap.consumed.oracleTurns).toBe(0);
      expect(snap.consumed.evalSteps).toBe(0);
      expect(snap.consumed.toolCalls).toBe(0);
    });

    it("creates tracker with custom limits", () => {
      const tracker = new BudgetTracker({ maxOracleTurns: 5, maxEvalSteps: 100 });
      const snap = tracker.snapshot();
      expect(snap.limits.maxOracleTurns).toBe(5);
      expect(snap.limits.maxEvalSteps).toBe(100);
    });

    it("consumeEvalStep mutates in place", () => {
      const tracker = new BudgetTracker({ maxEvalSteps: 100 });
      tracker.consumeEvalStep();
      expect(tracker.snapshot().consumed.evalSteps).toBe(1);
      tracker.consumeEvalStep(5);
      expect(tracker.snapshot().consumed.evalSteps).toBe(6);
    });

    it("consumeOracleTurn mutates in place", () => {
      const tracker = new BudgetTracker({ maxOracleTurns: 10 });
      tracker.consumeOracleTurn();
      tracker.consumeOracleTurn();
      expect(tracker.snapshot().consumed.oracleTurns).toBe(2);
    });

    it("consumeToolCall mutates in place", () => {
      const tracker = new BudgetTracker({ maxToolCalls: 10 });
      tracker.consumeToolCall();
      expect(tracker.snapshot().consumed.toolCalls).toBe(1);
    });

    it("throws when eval budget exhausted", () => {
      const tracker = new BudgetTracker({ maxEvalSteps: 5 });
      tracker.consumeEvalStep(4);
      tracker.consumeEvalStep(1);
      expect(() => tracker.consumeEvalStep()).toThrow("budget exhausted: evalSteps");
    });

    it("throws when oracle budget exhausted", () => {
      const tracker = new BudgetTracker({ maxOracleTurns: 2 });
      tracker.consumeOracleTurn();
      tracker.consumeOracleTurn();
      expect(() => tracker.consumeOracleTurn()).toThrow("budget exhausted: oracleTurns");
    });

    it("throws when tool budget exhausted", () => {
      const tracker = new BudgetTracker({ maxToolCalls: 1 });
      tracker.consumeToolCall();
      expect(() => tracker.consumeToolCall()).toThrow("budget exhausted: toolCalls");
    });

    it("remaining() returns remaining budget", () => {
      const tracker = new BudgetTracker({ maxOracleTurns: 10, maxEvalSteps: 100, maxToolCalls: 5 });
      tracker.consumeOracleTurn();
      tracker.consumeEvalStep(30);
      tracker.consumeToolCall();

      const remaining = tracker.remaining();
      expect(remaining.maxOracleTurns).toBe(9);
      expect(remaining.maxEvalSteps).toBe(70);
      expect(remaining.maxToolCalls).toBe(4);
    });

    it("isEvalExhausted() returns correct status", () => {
      const tracker = new BudgetTracker({ maxEvalSteps: 5 });
      expect(tracker.isEvalExhausted()).toBe(false);
      tracker.consumeEvalStep(5);
      expect(tracker.isEvalExhausted()).toBe(true);
    });

    it("isOracleExhausted() returns correct status", () => {
      const tracker = new BudgetTracker({ maxOracleTurns: 2 });
      expect(tracker.isOracleExhausted()).toBe(false);
      tracker.consumeOracleTurn();
      tracker.consumeOracleTurn();
      expect(tracker.isOracleExhausted()).toBe(true);
    });

    it("isToolExhausted() returns correct status", () => {
      const tracker = new BudgetTracker({ maxToolCalls: 1 });
      expect(tracker.isToolExhausted()).toBe(false);
      tracker.consumeToolCall();
      expect(tracker.isToolExhausted()).toBe(true);
    });

    it("snapshot() returns independent copy", () => {
      const tracker = new BudgetTracker({ maxEvalSteps: 100 });
      const snap1 = tracker.snapshot();
      tracker.consumeEvalStep(10);
      const snap2 = tracker.snapshot();

      expect(snap1.consumed.evalSteps).toBe(0);
      expect(snap2.consumed.evalSteps).toBe(10);
    });
  });
});
