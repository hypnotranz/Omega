# 1100: REPL Refactor

## Status: COMPLETE ✅
- **Completed**: 2026-01-30
- **Location**: OmegaLLM-3/bin/omega-repl.ts (580 lines)
- **Reduction**: 3163 → 580 lines (82% reduction)

## Purpose
Refactor omega-repl.ts (~3163 lines) to use OmegaRuntime, drastically reducing code and improving maintainability.

## Dependencies
- 1000-runtime-assembly.md ✅

## Source References
- bin/omega-repl.ts (current implementation)
- Expected reduction: ~3163 lines → ~500 lines
- Actual reduction: ~3163 lines → 580 lines

---

## Current State

The current `omega-repl.ts` contains:
- CESK machine implementation (duplicated)
- Macro system (duplicated)
- History management (duplicated)
- Snapshot management (duplicated)
- LLM integration (duplicated)
- AMB implementation (duplicated)
- Streams implementation (duplicated)
- And much more...

All of this is duplicated in other files and should be unified.

---

## Target Architecture

```typescript
// bin/omega-repl.ts (target: ~500 lines)

import { createRuntime, OmegaRuntime } from '../src/runtime';
import * as readline from 'readline';

class OmegaREPL {
  private runtime: OmegaRuntime;
  private rl: readline.Interface;
  private prompt = 'Ω> ';

  async start(): Promise<void> {
    // Create runtime with REPL preset
    this.runtime = await createRuntime({ preset: 'repl' });

    // Setup readline
    this.rl = readline.createInterface({
      input: process.stdin,
      output: process.stdout,
      completer: this.completer.bind(this)
    });

    // Wire events to console
    this.setupEventHandlers();

    // REPL loop
    this.repl();
  }

  private setupEventHandlers(): void {
    this.runtime.on('output', (event) => {
      process.stdout.write(event.text);
    });

    this.runtime.on('error', (event) => {
      console.error(`Error: ${event.message}`);
    });

    this.runtime.on('eval-complete', (event) => {
      console.log(`=> ${formatValue(event.result)}`);
    });
  }

  private async repl(): Promise<void> {
    while (true) {
      const line = await this.readline(this.prompt);

      if (line === null) {
        // EOF
        await this.cleanup();
        break;
      }

      if (this.handleCommand(line)) continue;

      try {
        const result = await this.runtime.eval(line);
        // Result already printed via event handler
      } catch (e) {
        if (e.name === 'AmbExhaustedError') {
          console.log('No more solutions');
        } else {
          console.error(e.message);
        }
      }
    }
  }

  private handleCommand(line: string): boolean {
    const trimmed = line.trim();
    if (!trimmed.startsWith(':')) return false;

    const [cmd, ...args] = trimmed.slice(1).split(' ');

    switch (cmd) {
      case 'quit':
      case 'q':
        process.exit(0);

      case 'help':
      case 'h':
        this.printHelp();
        return true;

      case 'history':
        this.showHistory(parseInt(args[0]) || 10);
        return true;

      case 'back':
        this.timeTravel(-1);
        return true;

      case 'forward':
        this.timeTravel(1);
        return true;

      case 'save':
        this.runtime.snapshots.save(args[0] || 'default');
        console.log(`Saved snapshot: ${args[0] || 'default'}`);
        return true;

      case 'restore':
        this.runtime.snapshots.restore(args[0] || 'default');
        console.log(`Restored snapshot: ${args[0] || 'default'}`);
        return true;

      case 'budget':
        this.showBudget();
        return true;

      case 'amb-reset':
        this.runtime.amb.resetSearch();
        console.log('AMB search reset');
        return true;

      default:
        console.log(`Unknown command: ${cmd}`);
        return true;
    }
  }

  private showHistory(n: number): void {
    const entries = this.runtime.history.getLast(n);
    for (const entry of entries) {
      console.log(`[${entry.index}] ${entry.input}`);
      console.log(`    => ${formatValue(entry.result.value)}`);
    }
  }

  private timeTravel(direction: number): void {
    const state = direction > 0
      ? this.runtime.history.forward()
      : this.runtime.history.back();

    if (state) {
      console.log(`Time traveled to entry ${this.runtime.history.getCurrentPosition()}`);
    } else {
      console.log(direction > 0 ? 'At latest state' : 'At earliest state');
    }
  }

  private showBudget(): void {
    const state = this.runtime.budget.getState();
    console.log('Budget Status:');
    console.log(`  Tokens: ${state.tokens.used}/${state.tokens.remaining + state.tokens.used}`);
    console.log(`  Cost: $${state.cost.used.toFixed(4)}`);
    console.log(`  LLM Calls: ${state.operations.llmCalls}`);
  }

  private completer(line: string): [string[], string] {
    // Use runtime's completion
    const completions = this.runtime.debug.getCompletions(line, line.length);
    const matches = completions.map(c => c.label);
    return [matches, line];
  }

  private printHelp(): void {
    console.log(`
OmegaLLM REPL Commands:
  :quit, :q        Exit the REPL
  :help, :h        Show this help
  :history [n]     Show last n history entries
  :back            Go back in history
  :forward         Go forward in history
  :save [name]     Save snapshot
  :restore [name]  Restore snapshot
  :budget          Show budget status
  :amb-reset       Reset AMB search
    `);
  }

  private readline(prompt: string): Promise<string | null> {
    return new Promise((resolve) => {
      this.rl.question(prompt, (answer) => {
        resolve(answer);
      });
      this.rl.once('close', () => resolve(null));
    });
  }

  private async cleanup(): Promise<void> {
    await this.runtime.dispose();
    this.rl.close();
  }
}

// Main
const repl = new OmegaREPL();
repl.start().catch(console.error);
```

---

## Migration Steps

1. **Identify dependencies**: Map current functions to OmegaRuntime subsystems
2. **Remove duplicated code**: Delete all code that's now in OmegaRuntime
3. **Wire events**: Connect runtime events to console output
4. **Preserve commands**: Keep REPL-specific commands as thin wrappers
5. **Test compatibility**: Ensure all existing functionality works

---

## Code Mapping

| Current Code | Moves To |
|--------------|----------|
| `evalExpr()`, `step()` | `runtime.eval()`, `runtime.step()` |
| History management | `runtime.history.*` |
| Snapshot functions | `runtime.snapshots.*` |
| AMB implementation | `runtime.amb.*` |
| Stream functions | `runtime.streams.*` |
| LLM calls | `runtime.llm.*` |
| Macro expansion | Internal to runtime |
| CESK machine | Internal to runtime |

---

## Test Requirements

### Unit Tests
- [ ] REPL starts successfully
- [ ] Commands parse correctly
- [ ] History commands work
- [ ] Snapshot commands work
- [ ] Budget command shows status
- [ ] Completion works

### Integration Tests
- [ ] Full evaluation through REPL
- [ ] Time travel works
- [ ] AMB reset works
- [ ] All commands functional
- [ ] Exit cleanup happens

---

## Acceptance Criteria
1. Code reduced from ~3163 to ~500 lines
2. All existing REPL functionality preserved
3. Commands work identically
4. Performance is equivalent or better
5. Tests pass
6. Clean separation of concerns
