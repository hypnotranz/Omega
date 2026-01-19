// SOURCE: ARCHITECTURE/32-LANGUAGE-OFFICIAL-13-IMPLEMENTATION-4.md
// AUTO-EXTRACTED - Do not edit directly. Edit the source document.

import { describe, it, expect } from "vitest";

// Assumed APIs from artifacts subsystem:
//   compileStrict(sourceText) -> { expandedHash, coreHash, receipts }
//   replayCompileStrict(sourceText, receipts) -> same hashes without oracle calls
import { compileStrict, replayCompileStrict } from "../helpers/hermeticHarness";

describe("hermetic compilation receipts", () => {
  it("R1/R3: strict compile is reproducible and replayable", async () => {
    const src = `(begin (define x 1) (+ x 2))`;

    const a = await compileStrict(src);
    const b = await compileStrict(src);

    expect(a.expandedHash).toBe(b.expandedHash);
    expect(a.coreHash).toBe(b.coreHash);

    const r = await replayCompileStrict(src, a.receipts);
    expect(r.expandedHash).toBe(a.expandedHash);
    expect(r.coreHash).toBe(a.coreHash);
  });
});