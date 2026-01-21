import { PrimitiveRegistry } from "./registry";
import type { PrimitiveDescriptor } from "./types";

export interface ValidationResult {
  valid: boolean;
  errors: string[];
}

export function validateDescriptor(descriptor: PrimitiveDescriptor): ValidationResult {
  const errors: string[] = [];

  if (!descriptor.id) {
    errors.push("missing id");
  }
  if (!descriptor.signature) {
    errors.push(`${descriptor.id || "<unknown>"}: missing signature`);
  }
  if (!descriptor.doc?.summary) {
    errors.push(`${descriptor.id || "<unknown>"}: missing doc.summary`);
  }
  if (!descriptor.version) {
    errors.push(`${descriptor.id || "<unknown>"}: missing version`);
  }
  if (!descriptor.effects || descriptor.effects.length === 0) {
    errors.push(`${descriptor.id || "<unknown>"}: missing effects`);
  }
  if (descriptor.effects?.includes("Oracle") && !descriptor.constraints?.mustBeDominatedByBudget) {
    errors.push(`${descriptor.id || "<unknown>"}: Oracle effect should require budget dominance`);
  }
  if (descriptor.effects?.includes("Tool") && !descriptor.constraints?.requiresToolContract) {
    errors.push(`${descriptor.id || "<unknown>"}: Tool effect should require tool contract`);
  }

  return { valid: errors.length === 0, errors };
}

export function validateRegistry(registry: PrimitiveRegistry): ValidationResult {
  return registry.validate();
}
