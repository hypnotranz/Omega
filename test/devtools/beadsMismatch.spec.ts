// test/devtools/beadsMismatch.spec.ts
// Tests for parsing and recommending fixes for beads repo mismatch warnings.

import { describe, it, expect } from "vitest";
import { parseBeadsRepoMismatch, getBeadsRepoMismatchFixes } from "../../src/devtools/beadsMismatch";

const SAMPLE_WARNING = [
  "DATABASE MISMATCH DETECTED!",
  "",
  "This database belongs to a different repository:",
  "  Database repo ID:  3d96f2ba",
  "  Current repo ID:   c9726b51",
  "",
  "This usually means:",
  "  1. You copied a .beads directory from another repo (don't do this!)",
].join("\n");

describe("parseBeadsRepoMismatch", () => {
  it("returns null when the marker is missing", () => {
    expect(parseBeadsRepoMismatch("all clear")).toBeNull();
  });

  it("parses repo ids from a warning message", () => {
    expect(parseBeadsRepoMismatch(SAMPLE_WARNING)).toEqual({
      databaseRepoId: "3d96f2ba",
      currentRepoId: "c9726b51",
    });
  });

  it("throws on non-string input", () => {
    expect(() => parseBeadsRepoMismatch(null)).toThrow("string");
  });

  it("throws when ids are missing from a mismatch message", () => {
    const incomplete = "DATABASE MISMATCH DETECTED!\nDatabase repo ID:";
    expect(() => parseBeadsRepoMismatch(incomplete)).toThrow("repository IDs");
  });
});

describe("getBeadsRepoMismatchFixes", () => {
  const mismatch = { databaseRepoId: "3d96f2ba", currentRepoId: "c9726b51" };

  it("returns the migrate command as the first recommendation", () => {
    const fixes = getBeadsRepoMismatchFixes(mismatch);
    expect(fixes[0]?.action).toBe("bd migrate --update-repo-id");
  });

  it("returns a defensive copy of recommendations", () => {
    const fixes = getBeadsRepoMismatchFixes(mismatch);
    fixes[0]!.action = "mutated";
    expect(getBeadsRepoMismatchFixes(mismatch)[0]?.action).toBe("bd migrate --update-repo-id");
  });

  it("throws when mismatch values are missing", () => {
    expect(() => getBeadsRepoMismatchFixes({ databaseRepoId: "", currentRepoId: "" })).toThrow("repo id");
  });
});
