// SOURCE: ARCHITECTURE/32-LANGUAGE-OFFICIAL-13-IMPLEMENTATION-11.md
// AUTO-EXTRACTED - Do not edit directly. Edit the source document.

import { createHash } from "node:crypto";

export type Hash = string;

/** Deterministic SHA-256 digest for text. */
export function sha256Text(s: string): Hash {
  return createHash("sha256").update(s, "utf8").digest("hex");
}

/** JSON replacer that handles BigInt. */
function jsonReplacer(_key: string, value: unknown): unknown {
  if (typeof value === "bigint") {
    return { __bigint__: value.toString() };
  }
  return value;
}

/** Deterministic SHA-256 digest for JSON (stable if JSON.stringify stable). */
export function sha256JSON(x: unknown): Hash {
  return sha256Text(JSON.stringify(x, jsonReplacer));
}