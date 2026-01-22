// src/devtools/beadsMismatch.ts
// Helpers for parsing and resolving beads repository mismatch warnings.

export const BEADS_REPO_MISMATCH_MARKER = "DATABASE MISMATCH DETECTED!";

const DATABASE_REPO_ID_PATTERN = /Database repo ID:\s*([0-9a-fA-F]+)/;
const CURRENT_REPO_ID_PATTERN = /Current repo ID:\s*([0-9a-fA-F]+)/;
const MISSING_IDS_MESSAGE = "Beads mismatch message missing repository IDs.";

export type BeadsRepoMismatch = {
  databaseRepoId: string;
  currentRepoId: string;
};

export type BeadsRepoMismatchFix = {
  action: string;
  reason: string;
  risk?: "low" | "medium" | "high";
};

const RECOMMENDED_FIXES: BeadsRepoMismatchFix[] = [
  {
    action: "bd migrate --update-repo-id",
    reason: "Update the database repo fingerprint to match the current repository.",
    risk: "low",
  },
  {
    action: "rm -rf .beads && bd init",
    reason: "Recreate the local beads database if it belongs to a different repository.",
    risk: "medium",
  },
  {
    action: "BEADS_IGNORE_REPO_MISMATCH=1 bd daemon",
    reason: "Ignore the mismatch warning to access data (use only if you trust the database).",
    risk: "high",
  },
];

const STRING_TYPE_ERROR = "Beads mismatch message must be a string.";

function requireNonEmptyRepoId(value: unknown, label: string): string {
  if (typeof value !== "string" || value.trim().length === 0) {
    throw new Error(`${label} repo id is required.`);
  }
  return value.trim();
}

export function parseBeadsRepoMismatch(message: unknown): BeadsRepoMismatch | null {
  if (typeof message !== "string") {
    throw new TypeError(STRING_TYPE_ERROR);
  }

  if (!message.includes(BEADS_REPO_MISMATCH_MARKER)) {
    return null;
  }

  const databaseMatch = message.match(DATABASE_REPO_ID_PATTERN);
  const currentMatch = message.match(CURRENT_REPO_ID_PATTERN);

  if (!databaseMatch || !currentMatch) {
    throw new Error(MISSING_IDS_MESSAGE);
  }

  return {
    databaseRepoId: requireNonEmptyRepoId(databaseMatch[1], "Database"),
    currentRepoId: requireNonEmptyRepoId(currentMatch[1], "Current"),
  };
}

export function getBeadsRepoMismatchFixes(mismatch: BeadsRepoMismatch): BeadsRepoMismatchFix[] {
  if (!mismatch || typeof mismatch !== "object") {
    throw new TypeError("Beads mismatch details are required.");
  }

  requireNonEmptyRepoId(mismatch.databaseRepoId, "Database");
  requireNonEmptyRepoId(mismatch.currentRepoId, "Current");

  return RECOMMENDED_FIXES.map((fix) => ({ ...fix }));
}
