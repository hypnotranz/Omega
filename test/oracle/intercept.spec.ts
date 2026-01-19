// Test that user-defined handlers can intercept infer.op
// This is the "caching/mocking" capability - user handlers catch effect before runtime

import { describe, it, expect } from "vitest";
import { evalOmega } from "../helpers/omegaHarness";

describe("User handler intercepts infer.op", () => {
  it("I1: user handler can catch and mock infer.op", async () => {
    // Define a handler that intercepts infer.op and returns a fixed value
    // instead of calling the real oracle
    const result = await evalOmega(`
      (handle
        (effect infer.op "what is 2+2?")
        (on infer.op (payload k)
          (k 999)))
    `);

    // The user handler intercepted infer.op and returned 999
    // instead of the ScriptedOracleAdapter's 42
    expect(result).toBe(999);
  });

  it("I2: nested handlers - inner catches first", async () => {
    const result = await evalOmega(`
      (handle
        (handle
          (effect infer.op "query")
          (on infer.op (payload k)
            (k 111)))
        (on infer.op (payload k)
          (k 222)))
    `);

    // Inner handler catches first
    expect(result).toBe(111);
  });

  it("I3: handler can pass through to outer/runtime if it wants", async () => {
    // If the user handler doesn't handle infer.op, it falls through
    // to the runtime's ScriptedOracleAdapter which returns 42
    const result = await evalOmega(`
      (handle
        (effect infer.op "query")
        (on some-other.op (x k)
          (k 999)))
    `);

    // No user handler for infer.op, so ScriptedOracleAdapter handles it
    // ScriptedOracleAdapter evaluates (+ 20 22) = 42
    expect(result).toBe(42);
  });
});
