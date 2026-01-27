/**
 * OPR Kernel Registry
 *
 * Kernels are prompts that define how an LLM processes structured input.
 * Each kernel specifies:
 *   - Input format (what the "program" looks like)
 *   - Processing rules
 *   - Output schema (the JSON contract)
 */

import type { KernelPromptConfig } from '../runtime';

// Re-export all kernels
export { LOGIC_KERNEL } from './logic';
export { ANALYZE_KERNEL } from './analyze';
export { SEMANTIC_KERNEL } from './semantic';
export { CODE_REVIEW_KERNEL } from './codeReview';
export { TRANSFORM_KERNEL } from './transform';
export { EXTRACT_KERNEL } from './extract';
export { CLASSIFY_KERNEL } from './classify';
export { SYNTHESIZE_KERNEL } from './synthesize';
export { VALIDATE_KERNEL } from './validate';
export { PLAN_KERNEL } from './plan';

// Kernel registry
const kernels = new Map<string, KernelPromptConfig>();

export function registerKernel(config: KernelPromptConfig): void {
  kernels.set(config.id, config);
}

export function getKernel(id: string): KernelPromptConfig | undefined {
  return kernels.get(id);
}

export function listKernels(): string[] {
  return Array.from(kernels.keys());
}

// Auto-register all kernels on import
import { LOGIC_KERNEL } from './logic';
import { ANALYZE_KERNEL } from './analyze';
import { SEMANTIC_KERNEL } from './semantic';
import { CODE_REVIEW_KERNEL } from './codeReview';
import { TRANSFORM_KERNEL } from './transform';
import { EXTRACT_KERNEL } from './extract';
import { CLASSIFY_KERNEL } from './classify';
import { SYNTHESIZE_KERNEL } from './synthesize';
import { VALIDATE_KERNEL } from './validate';
import { PLAN_KERNEL } from './plan';

[
  LOGIC_KERNEL,
  ANALYZE_KERNEL,
  SEMANTIC_KERNEL,
  CODE_REVIEW_KERNEL,
  TRANSFORM_KERNEL,
  EXTRACT_KERNEL,
  CLASSIFY_KERNEL,
  SYNTHESIZE_KERNEL,
  VALIDATE_KERNEL,
  PLAN_KERNEL,
].forEach(registerKernel);
