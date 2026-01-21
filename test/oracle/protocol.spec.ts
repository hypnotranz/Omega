/**
 * ═══════════════════════════════════════════════════════════════════════════
 * LLM Calls as Functions Tests
 * ═══════════════════════════════════════════════════════════════════════════
 *
 * Quick Reference: docs/USER-MANUAL--00--Quick-Reference.md#2-llm-calls-as-functions
 * Full Chapter:    docs/USER-MANUAL--02--Llm-Calls-As-Functions.md
 * Demo:            demo/by-chapter/ch02-llm-calls.ts
 * ═══════════════════════════════════════════════════════════════════════════
 */
// Test that the Oracle Protocol actually works: (effect infer.op ...) triggers
// ScriptedOracleAdapter, which does REPL re-entry via ReqEval, and returns result.
//
// SOURCE: ARCHITECTURE/32-LANGUAGE-OFFICIAL-13-IMPLEMENTATION-14.md

import { describe, it, expect } from "vitest";
import { evalOmega } from "../helpers/omegaHarness";

describe("Oracle Protocol REPL re-entry", () => {
  it("O1: (effect infer.op ...) triggers oracle and returns computed result", async () => {
    // The ScriptedOracleAdapter does:
    // 1. Receives infer.op with payload
    // 2. Issues ReqEval to evaluate (+ 20 22)
    // 3. Returns Meaning with denotation = 42
    //
    // So regardless of what payload we send, we should get back 42.
    const result = await evalOmega(`(effect infer.op "anything")`);

    // ScriptedOracleAdapter evaluates (+ 20 22) = 42 and returns that as the meaning
    expect(result).toBe(42);
  });

  it("O2: infer.op result can be used in further computation", async () => {
    // Oracle returns 42, then we add 8 to it
    const result = await evalOmega(`
      (+ (effect infer.op "get me a number") 8)
    `);
    expect(result).toBe(50); // 42 + 8
  });

  it("O3: multiple infer.op calls work in sequence", async () => {
    // Each call to ScriptedOracleAdapter returns 42
    const result = await evalOmega(`
      (+ (effect infer.op "first")
         (effect infer.op "second"))
    `);
    expect(result).toBe(84); // 42 + 42
  });

  it("O4: infer.op works inside a let binding", async () => {
    const result = await evalOmega(`
      (let ((x (effect infer.op "compute something")))
        (* x 2))
    `);
    expect(result).toBe(84); // 42 * 2
  });

  it("O5: infer.op works inside a lambda", async () => {
    const result = await evalOmega(`
      (let ((get-oracle-value (lambda () (effect infer.op "query"))))
        (get-oracle-value))
    `);
    expect(result).toBe(42);
  });
});
