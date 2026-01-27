# 021f1-repl-commands: REPL Commands

> **Output**: `src/repl/commands/opr.ts`

> **Scope**: Implement REPL commands for ΩPR: :opr-kernels, :opr-step, :opr-run, :opr-receipts, :opr-verify
> **Architecture Reference**: [021-OPR-RUNTIME.md](021-OPR-RUNTIME.md#repl-integration)
> **Depends on**: job-021e2-effect-handlers, job-021b5-prompt-registry (Layer 2)

## Overview

Add REPL commands for interactive ΩPR usage. Users can list kernels, execute steps, run to fixpoint, and inspect receipts.

## File to Create

`src/repl/commands/opr.ts`

## Imports Contract

What this task needs from its dependencies:

### From 021-types (../types):
- ReplState
- ReplCommand

### From 021-registry (../../core/opr/prompts/registry):
- kernelRegistry

### From 021-receipts (../../core/opr/receipts):
- verifyReceiptChain

### From 021-runtime (../../core/opr/runtime):
- OprRuntime

### From 021-promises (fs/promises):
- readFile

### From 021-opr (./commands/opr):
- oprCommands

## Implementation

### Command Registration

```typescript
import type { ReplState, ReplCommand } from '../types';
import { kernelRegistry } from '../../core/opr/prompts/registry';
import { verifyReceiptChain } from '../../core/opr/receipts';
import { OprRuntime } from '../../core/opr/runtime';
import { readFile } from 'fs/promises';

export const oprCommands: Record<string, ReplCommand> = {
  ':opr-kernels': {
    description: 'List available ΩPR kernels',
    usage: ':opr-kernels',
    handler: handleOprKernels,
  },

  ':opr-step': {
    description: 'Execute single ΩPR kernel step',
    usage: ':opr-step <kernel-id> <program-expr>',
    handler: handleOprStep,
  },

  ':opr-run': {
    description: 'Run ΩPR kernel to fixpoint',
    usage: ':opr-run <kernel-id> <program-expr>',
    handler: handleOprRun,
  },

  ':opr-receipts': {
    description: 'Show ΩPR receipt chain for current session',
    usage: ':opr-receipts',
    handler: handleOprReceipts,
  },

  ':opr-verify': {
    description: 'Verify ΩPR receipt chain integrity',
    usage: ':opr-verify [receipt-file]',
    handler: handleOprVerify,
  },
};
```

### :opr-kernels Handler

```typescript
async function handleOprKernels(repl: ReplState): Promise<void> {
  const kernels = kernelRegistry.list();

  console.log('\n╔══════════════════════════════════════════════════════════════╗');
  console.log('║                   Available ΩPR Kernels                       ║');
  console.log('╠══════════════════════════════════════════════════════════════╣');

  for (const id of kernels) {
    const config = kernelRegistry.get(id)!;
    console.log(`║  ${id.padEnd(20)} - ${config.description.slice(0, 35).padEnd(35)} ║`);
  }

  console.log('╚══════════════════════════════════════════════════════════════╝\n');
}
```

### :opr-step Handler

```typescript
async function handleOprStep(repl: ReplState, args: string): Promise<void> {
  const match = args.match(/^(\S+)\s+(.+)$/);
  if (!match) {
    console.log('Usage: :opr-step <kernel-id> <program-expr>');
    console.log('Example: :opr-step opr.logic.v1 (hash :rules "..." :facts \'(...))');
    return;
  }

  const [, kernelId, programExpr] = match;

  // Validate kernel exists
  const kernel = kernelRegistry.get(kernelId);
  if (!kernel) {
    console.log(`Unknown kernel: ${kernelId}`);
    console.log('Use :opr-kernels to list available kernels.');
    return;
  }

  // Evaluate the program expression in Lisp
  let program: unknown;
  try {
    program = await repl.evalLispExpr(programExpr);
  } catch (e) {
    console.log(`Error evaluating program expression: ${(e as Error).message}`);
    return;
  }

  // Get or create OPR state for this session
  const oprState = ensureOprState(repl);

  // Create runtime
  const runtime = new OprRuntime({
    kernel,
    adapter: repl.createOprAdapter(),
    receipts: oprState.receipts,
    budget: { maxAttempts: 3, sessionBudget: repl.budget },
  });

  console.log(`\nExecuting ${kernelId} step...`);

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

    // Print result
    console.log('\n┌─────────────────────────────────────────────────────────────┐');
    console.log('│ ✓ Step Succeeded                                             │');
    console.log('├─────────────────────────────────────────────────────────────┤');
    console.log(`│ Kernel: ${kernelId.padEnd(50)} │`);
    console.log(`│ Attempts: ${result.attempts}                                              │`);
    console.log('├─────────────────────────────────────────────────────────────┤');
    console.log('│ Result:                                                      │');
    console.log(formatJson(result.output.result, '│   '));
    if (result.output.next_state) {
      console.log('├─────────────────────────────────────────────────────────────┤');
      console.log('│ Next State:                                                  │');
      console.log(formatJson(result.output.next_state, '│   '));
    }
    if (result.output.effects.length > 0) {
      console.log('├─────────────────────────────────────────────────────────────┤');
      console.log(`│ Effects: ${result.output.effects.length} callback(s) pending                        │`);
    }
    console.log('└─────────────────────────────────────────────────────────────┘\n');
  } else {
    // Print error
    console.log('\n┌─────────────────────────────────────────────────────────────┐');
    console.log(`│ ✗ Step Failed: ${result.tag.padEnd(42)} │`);
    console.log('├─────────────────────────────────────────────────────────────┤');
    console.log(`│ Attempts: ${result.attempts}                                              │`);

    if (result.tag === 'validation-failed') {
      console.log('├─────────────────────────────────────────────────────────────┤');
      console.log('│ Violations:                                                  │');
      for (const v of result.violations) {
        console.log(`│   ${v.path}: ${v.message.slice(0, 45).padEnd(45)} │`);
      }
    }

    console.log('└─────────────────────────────────────────────────────────────┘\n');
  }
}
```

### :opr-run Handler

```typescript
async function handleOprRun(repl: ReplState, args: string): Promise<void> {
  const match = args.match(/^(\S+)\s+(.+)$/);
  if (!match) {
    console.log('Usage: :opr-run <kernel-id> <program-expr>');
    return;
  }

  const [, kernelId, programExpr] = match;

  const kernel = kernelRegistry.get(kernelId);
  if (!kernel) {
    console.log(`Unknown kernel: ${kernelId}. Use :opr-kernels to list.`);
    return;
  }

  let program: unknown;
  try {
    program = await repl.evalLispExpr(programExpr);
  } catch (e) {
    console.log(`Error evaluating program expression: ${(e as Error).message}`);
    return;
  }

  const oprState = ensureOprState(repl);

  const runtime = new OprRuntime({
    kernel,
    adapter: repl.createOprAdapter(),
    receipts: oprState.receipts,
    budget: { maxAttempts: 3, sessionBudget: repl.budget },
    invariants: { iterationMonotonic: true, derivedMonotonic: true, deltaTermination: false },
  });

  console.log(`\nRunning ${kernelId} to fixpoint...`);

  const result = await runtime.runToFixpoint({
    program,
    state: null,
  });

  // Print result
  if (result.tag === 'ok') {
    console.log('\n┌─────────────────────────────────────────────────────────────┐');
    console.log('│ ✓ Fixpoint Reached                                           │');
    console.log('├─────────────────────────────────────────────────────────────┤');
    console.log(`│ Iterations: ${result.iterations}                                           │`);
    console.log(`│ Total receipts: ${result.receipts.length}                                      │`);
    console.log('├─────────────────────────────────────────────────────────────┤');
    console.log('│ Final State:                                                 │');
    console.log(formatJson(result.finalState, '│   '));
    console.log('└─────────────────────────────────────────────────────────────┘\n');
  } else if (result.tag === 'max-iterations') {
    console.log(`\n⚠ Max iterations (${result.iterations}) reached without fixpoint\n`);
  } else {
    console.log(`\n✗ Error after ${result.iterations} iterations: ${result.error.tag}\n`);
  }
}
```

### :opr-receipts Handler

```typescript
async function handleOprReceipts(repl: ReplState): Promise<void> {
  const oprState = repl.session?.oprState;
  if (!oprState || oprState.receipts.count() === 0) {
    console.log('\nNo ΩPR receipts in current session.\n');
    return;
  }

  const receipts = oprState.receipts.getAll();

  console.log('\n╔══════════════════════════════════════════════════════════════╗');
  console.log('║                    ΩPR Receipt Chain                          ║');
  console.log('╠══════════════════════════════════════════════════════════════╣');

  for (let i = 0; i < receipts.length; i++) {
    const r = receipts[i];
    const statusIcon = r.status === 'OK' ? '✓' : '✗';
    console.log(`║ [${i + 1}] ${statusIcon} ${r.status.padEnd(8)} ${r.kernel_id}:${r.op.padEnd(20)} ║`);
    console.log(`║     Attempt: ${r.attempt}  Created: ${r.created_at.slice(11, 19)}              ║`);
    if (r.errors.length > 0) {
      console.log(`║     Errors: ${r.errors[0].slice(0, 45)}...                ║`);
    }
    if (i < receipts.length - 1) {
      console.log('╟──────────────────────────────────────────────────────────────╢');
    }
  }

  console.log('╠══════════════════════════════════════════════════════════════╣');

  const validity = verifyReceiptChain(receipts);
  const validityStr = validity.valid ? '✓ VALID' : `✗ BROKEN at ${validity.brokenAt}`;
  console.log(`║ Chain Integrity: ${validityStr.padEnd(42)} ║`);
  console.log('╚══════════════════════════════════════════════════════════════╝\n');
}
```

### :opr-verify Handler

```typescript
async function handleOprVerify(repl: ReplState, args: string): Promise<void> {
  let receipts: OprReceipt[];

  if (args.trim()) {
    // Load from file
    try {
      const content = await readFile(args.trim(), 'utf-8');
      receipts = JSON.parse(content);
    } catch (e) {
      console.log(`Error loading receipt file: ${(e as Error).message}`);
      return;
    }
  } else {
    // Use session receipts
    const oprState = repl.session?.oprState;
    if (!oprState) {
      console.log('No ΩPR receipts in session. Provide a file path to verify.');
      return;
    }
    receipts = oprState.receipts.getAll();
  }

  const result = verifyReceiptChain(receipts);

  if (result.valid) {
    console.log('\n┌─────────────────────────────────────────────────────────────┐');
    console.log('│ ✓ Receipt chain is VALID                                     │');
    console.log('├─────────────────────────────────────────────────────────────┤');
    console.log(`│   ${receipts.length} receipts verified                                     │`);
    console.log(`│   Chain hash: ${receipts.length > 0 ? receipts[receipts.length-1].receipt_hash.slice(0, 20) : 'N/A'}... │`);
    console.log('└─────────────────────────────────────────────────────────────┘\n');
  } else {
    console.log('\n┌─────────────────────────────────────────────────────────────┐');
    console.log('│ ✗ Receipt chain is BROKEN                                    │');
    console.log('├─────────────────────────────────────────────────────────────┤');
    console.log(`│   Broken at index: ${result.brokenAt}                                       │`);
    console.log(`│   Error: ${result.error?.slice(0, 48).padEnd(48)} │`);
    console.log('└─────────────────────────────────────────────────────────────┘\n');
  }
}
```

### Helper Functions

```typescript
function ensureOprState(repl: ReplState): OprSessionState {
  if (!repl.session) {
    throw new Error('No active session');
  }
  if (!repl.session.oprState) {
    repl.session.oprState = {
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
  return repl.session.oprState;
}

function formatJson(obj: unknown, prefix: string): string {
  const lines = JSON.stringify(obj, null, 2).split('\n');
  return lines.map(line => `${prefix}${line}`).join('\n');
}
```

## Integration with REPL

```typescript
// In bin/omega-repl.ts or src/repl/index.ts

import { oprCommands } from './commands/opr';

// Register OPR commands
for (const [name, command] of Object.entries(oprCommands)) {
  registerCommand(name, command);
}

// Update :help to include OPR commands
```

## Exports Contract
```typescript
export { oprCommands };
```

## Acceptance Criteria

1. [ ] `:opr-kernels` lists all registered kernels with descriptions
2. [ ] `:opr-step <kernel> <expr>` executes single step and displays result
3. [ ] `:opr-run <kernel> <expr>` runs to fixpoint and displays final state
4. [ ] `:opr-receipts` displays receipt chain with status icons
5. [ ] `:opr-verify [file]` verifies chain integrity
6. [ ] Commands update session oprState with receipts and stats
7. [ ] Commands integrate with session budget
8. [ ] `:help` includes all OPR commands

## Test Cases (manual verification)

- REPL1: `:opr-kernels` shows opr.logic.v1, opr.analyze.v1, opr.semantic.v1
- REPL2: `:opr-step opr.logic.v1 (hash :rules "..." :facts '(...))` executes
- REPL3: `:opr-receipts` shows receipts after step execution
- REPL4: `:opr-verify` reports valid chain after successful steps

## Verification

```bash
npx tsc --noEmit src/repl/commands/opr.ts
```
