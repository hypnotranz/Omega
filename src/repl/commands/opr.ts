/**
 * OPR REPL Commands
 *
 * Commands for interactive OPR usage: :opr-kernels, :opr-step, :opr-run, :opr-receipts, :opr-verify
 */

import type { KernelState, OprReceipt } from '../../core/opr/types';
import { verifyReceiptChain, InMemoryReceiptStore, type ReceiptStore } from '../../core/opr/receipts';
import { OprRuntime, type KernelPromptConfig, type SessionBudget } from '../../core/opr/runtime';
import type { OprLLMAdapter } from '../../core/opr/adapters/types';
import { readFile } from 'fs/promises';

/**
 * OPR Session State - tracks receipts and kernel states across commands
 */
export interface OprSessionState {
  receipts: ReceiptStore;
  activeKernels: Map<
    string,
    {
      kernelId: string;
      state: KernelState;
      receipts: OprReceipt[];
    }
  >;
  stats: {
    totalSteps: number;
    totalAttempts: number;
    successfulSteps: number;
    callbacksExecuted: number;
  };
}

/**
 * Kernel registry interface
 */
export interface KernelRegistry {
  list(): string[];
  get(id: string): KernelPromptConfig | undefined;
}

/**
 * Context required for OPR commands
 */
export interface OprCommandContext {
  /** Get or create OPR session state */
  getOprState(): OprSessionState;

  /** Create an LLM adapter for OPR calls */
  createAdapter(): OprLLMAdapter;

  /** Session budget (optional) */
  budget?: SessionBudget;

  /** Kernel registry */
  kernelRegistry: KernelRegistry;

  /** Evaluate a Lisp expression and return the result */
  evalLisp?(expr: string): Promise<unknown>;

  /** Log output */
  log(message: string): void;
}

/**
 * Create a new OPR session state
 */
export function createOprSessionState(): OprSessionState {
  return {
    receipts: new InMemoryReceiptStore(),
    activeKernels: new Map(),
    stats: {
      totalSteps: 0,
      totalAttempts: 0,
      successfulSteps: 0,
      callbacksExecuted: 0,
    },
  };
}

/**
 * Handle :opr-kernels command - list available kernels
 */
export async function handleOprKernels(ctx: OprCommandContext): Promise<void> {
  const kernels = ctx.kernelRegistry.list();

  ctx.log('');
  ctx.log('Available OPR Kernels');
  ctx.log('=====================');

  if (kernels.length === 0) {
    ctx.log('  (no kernels registered)');
  } else {
    for (const id of kernels) {
      ctx.log(`  - ${id}`);
    }
  }

  ctx.log('');
}

/**
 * Handle :opr-step command - execute single kernel step
 */
export async function handleOprStep(ctx: OprCommandContext, args: string): Promise<void> {
  const match = args.match(/^(\S+)\s+(.+)$/);
  if (!match) {
    ctx.log('Usage: :opr-step <kernel-id> <program-expr>');
    ctx.log('Example: :opr-step opr.logic.v1 (hash :rules "..." :facts \'(...))');
    return;
  }

  const [, kernelId, programExpr] = match;

  // Validate kernel exists
  const kernel = ctx.kernelRegistry.get(kernelId);
  if (!kernel) {
    ctx.log(`Unknown kernel: ${kernelId}`);
    ctx.log('Use :opr-kernels to list available kernels.');
    return;
  }

  // Evaluate the program expression
  let program: unknown;
  if (ctx.evalLisp) {
    try {
      program = await ctx.evalLisp(programExpr);
    } catch (e) {
      ctx.log(`Error evaluating program expression: ${(e as Error).message}`);
      return;
    }
  } else {
    // Fallback: try to parse as JSON
    try {
      program = JSON.parse(programExpr);
    } catch {
      ctx.log('Cannot evaluate expression: no Lisp evaluator available and not valid JSON');
      return;
    }
  }

  const oprState = ctx.getOprState();

  // Create runtime
  const runtime = new OprRuntime({
    kernel,
    adapter: ctx.createAdapter(),
    receipts: oprState.receipts,
    budget: { maxAttempts: 3 },
    sessionBudget: ctx.budget,
  });

  ctx.log(`\nExecuting ${kernelId} step...`);

  // Execute step
  const result = await runtime.step({
    program,
    state: oprState.activeKernels.get(kernelId)?.state ?? null,
  });

  // Update session state
  oprState.stats.totalSteps++;
  oprState.stats.totalAttempts += result.attempts;

  if (result.tag === 'ok') {
    oprState.stats.successfulSteps++;

    // Store active kernel state for continuation
    if (result.output.next_state !== null) {
      oprState.activeKernels.set(kernelId, {
        kernelId,
        state: result.output.next_state,
        receipts: result.receipts,
      });
    } else {
      oprState.activeKernels.delete(kernelId);
    }

    ctx.log('');
    ctx.log('Step Succeeded');
    ctx.log(`  Kernel: ${kernelId}`);
    ctx.log(`  Attempts: ${result.attempts}`);
    ctx.log(`  Result: ${JSON.stringify(result.output.result, null, 2)}`);
    if (result.output.next_state) {
      ctx.log(`  Next State: ${JSON.stringify(result.output.next_state, null, 2)}`);
    }
    if (result.output.effects.length > 0) {
      ctx.log(`  Effects: ${result.output.effects.length} callback(s) pending`);
    }
  } else {
    ctx.log('');
    ctx.log(`Step Failed: ${result.tag}`);
    ctx.log(`  Attempts: ${result.attempts}`);

    if (result.tag === 'validation-failed') {
      ctx.log('  Violations:');
      for (const v of result.violations) {
        ctx.log(`    ${v.path}: ${v.message}`);
      }
    }
  }

  ctx.log('');
}

/**
 * Handle :opr-run command - run kernel to fixpoint
 */
export async function handleOprRun(ctx: OprCommandContext, args: string): Promise<void> {
  const match = args.match(/^(\S+)\s+(.+)$/);
  if (!match) {
    ctx.log('Usage: :opr-run <kernel-id> <program-expr>');
    return;
  }

  const [, kernelId, programExpr] = match;

  const kernel = ctx.kernelRegistry.get(kernelId);
  if (!kernel) {
    ctx.log(`Unknown kernel: ${kernelId}. Use :opr-kernels to list.`);
    return;
  }

  let program: unknown;
  if (ctx.evalLisp) {
    try {
      program = await ctx.evalLisp(programExpr);
    } catch (e) {
      ctx.log(`Error evaluating program expression: ${(e as Error).message}`);
      return;
    }
  } else {
    try {
      program = JSON.parse(programExpr);
    } catch {
      ctx.log('Cannot evaluate expression');
      return;
    }
  }

  const oprState = ctx.getOprState();

  const runtime = new OprRuntime({
    kernel,
    adapter: ctx.createAdapter(),
    receipts: oprState.receipts,
    budget: { maxAttempts: 3 },
    sessionBudget: ctx.budget,
    invariants: {
      iterationMonotonic: true,
      derivedMonotonic: true,
      deltaTermination: false,
    },
  });

  ctx.log(`\nRunning ${kernelId} to fixpoint...`);

  const result = await runtime.runToFixpoint({
    program,
    state: null,
  });

  if (result.tag === 'ok') {
    ctx.log('');
    ctx.log('Fixpoint Reached');
    ctx.log(`  Iterations: ${result.iterations}`);
    ctx.log(`  Total receipts: ${result.receipts.length}`);
    ctx.log(`  Final State: ${JSON.stringify(result.finalState, null, 2)}`);
  } else if (result.tag === 'max-iterations') {
    ctx.log(`\nMax iterations (${result.iterations}) reached without fixpoint`);
  } else {
    ctx.log(`\nError after ${result.iterations} iterations: ${result.error.tag}`);
  }

  ctx.log('');
}

/**
 * Handle :opr-receipts command - show receipt chain
 */
export async function handleOprReceipts(ctx: OprCommandContext): Promise<void> {
  const oprState = ctx.getOprState();
  const receipts = oprState.receipts.getAll();

  if (receipts.length === 0) {
    ctx.log('\nNo OPR receipts in current session.\n');
    return;
  }

  ctx.log('');
  ctx.log('OPR Receipt Chain');
  ctx.log('==================');

  for (let i = 0; i < receipts.length; i++) {
    const r = receipts[i];
    const statusIcon = r.status === 'OK' ? '[OK]' : '[X]';
    ctx.log(`${i + 1}. ${statusIcon} ${r.status} - ${r.kernel_id}:${r.op}`);
    ctx.log(`     Attempt: ${r.attempt}  Created: ${r.created_at}`);
    if (r.errors.length > 0) {
      ctx.log(`     Errors: ${r.errors[0]}`);
    }
  }

  const validity = verifyReceiptChain(receipts);
  ctx.log('');
  ctx.log(`Chain Integrity: ${validity.valid ? 'VALID' : `BROKEN at ${validity.brokenAt}`}`);
  ctx.log('');
}

/**
 * Handle :opr-verify command - verify receipt chain integrity
 */
export async function handleOprVerify(ctx: OprCommandContext, args: string): Promise<void> {
  let receipts: OprReceipt[];

  if (args.trim()) {
    // Load from file
    try {
      const content = await readFile(args.trim(), 'utf-8');
      receipts = JSON.parse(content);
    } catch (e) {
      ctx.log(`Error loading receipt file: ${(e as Error).message}`);
      return;
    }
  } else {
    // Use session receipts
    const oprState = ctx.getOprState();
    receipts = oprState.receipts.getAll();
    if (receipts.length === 0) {
      ctx.log('No OPR receipts in session. Provide a file path to verify.');
      return;
    }
  }

  const result = verifyReceiptChain(receipts);

  if (result.valid) {
    ctx.log('');
    ctx.log('Receipt chain is VALID');
    ctx.log(`  ${receipts.length} receipts verified`);
    if (receipts.length > 0) {
      ctx.log(`  Chain hash: ${receipts[receipts.length - 1].receipt_hash}`);
    }
  } else {
    ctx.log('');
    ctx.log('Receipt chain is BROKEN');
    ctx.log(`  Broken at index: ${result.brokenAt}`);
    ctx.log(`  Error: ${result.error}`);
  }

  ctx.log('');
}

/**
 * Get all OPR command definitions for registration
 */
export function getOprCommands(): Record<
  string,
  {
    description: string;
    usage: string;
    handler: (ctx: OprCommandContext, args: string) => Promise<void>;
  }
> {
  return {
    ':opr-kernels': {
      description: 'List available OPR kernels',
      usage: ':opr-kernels',
      handler: async (ctx) => handleOprKernels(ctx),
    },
    ':opr-step': {
      description: 'Execute single OPR kernel step',
      usage: ':opr-step <kernel-id> <program-expr>',
      handler: handleOprStep,
    },
    ':opr-run': {
      description: 'Run OPR kernel to fixpoint',
      usage: ':opr-run <kernel-id> <program-expr>',
      handler: handleOprRun,
    },
    ':opr-receipts': {
      description: 'Show OPR receipt chain for current session',
      usage: ':opr-receipts',
      handler: async (ctx) => handleOprReceipts(ctx),
    },
    ':opr-verify': {
      description: 'Verify OPR receipt chain integrity',
      usage: ':opr-verify [receipt-file]',
      handler: handleOprVerify,
    },
  };
}
