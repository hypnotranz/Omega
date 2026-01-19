// src/core/governance/caps.ts
// SOURCE: ARCHITECTURE/32-LANGUAGE-OFFICIAL-13-IMPLEMENTATION-16.md
// Patch Set D1: Capability enforcement

export type Cap = string;

export type CapSet = Cap[];

export function capRequire(caps: CapSet, required: Cap, context: string): void {
  if (caps.includes("*")) return; // wildcard grants all
  if (caps.includes(required)) return;

  // Check for wildcard in capability domain (e.g., "tool.*" grants "tool.read")
  const domain = required.split(".")[0];
  if (caps.includes(`${domain}.*`)) return;

  throw new Error(`capability denied: ${required} (context: ${context})`);
}

export function capHas(caps: CapSet, cap: Cap): boolean {
  if (caps.includes("*")) return true;
  if (caps.includes(cap)) return true;
  const domain = cap.split(".")[0];
  if (caps.includes(`${domain}.*`)) return true;
  return false;
}

export const DEFAULT_CAPS: CapSet = ["eval", "apply", "observe"];

export const FULL_CAPS: CapSet = ["*"];
