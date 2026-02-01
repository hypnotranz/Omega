# 1120: Debugger Refactor

## Status: COMPLETE ✅
- **Completed**: 2026-01-30
- **Location**: OmegaLLM-3/bin/omega-debugger.ts (529 lines)
- **Reduction**: 1197 → 529 lines (56% reduction)

## Purpose
Refactor omega-debugger.ts (~1197 lines) to use OmegaRuntime, creating a thin CLI wrapper.

## Dependencies
- 1000-runtime-assembly.md ✅

## Source References
- bin/omega-debugger.ts (current implementation)
- Expected reduction: ~1197 lines → ~300 lines
- Actual reduction: 1197 → 529 lines

---

## Current State

The current `omega-debugger.ts` contains:
- Duplicated CESK machine
- Manual breakpoint handling
- Inline history management
- Custom stepping logic

---

## Target Architecture

```typescript
// bin/omega-debugger.ts (target: ~300 lines)

import { createRuntime, OmegaRuntime } from '../src/runtime';
import * as readline from 'readline';

class OmegaDebugger {
  private runtime: OmegaRuntime;
  private rl: readline.Interface;

  async start(program?: string): Promise<void> {
    // Create runtime with debug preset
    this.runtime = await createRuntime({ preset: 'debug' });

    // Setup readline
    this.rl = readline.createInterface({
      input: process.stdin,
      output: process.stdout
    });

    // Wire events
    this.setupEventHandlers();

    // Load program if provided
    if (program) {
      await this.loadProgram(program);
    }

    // Debugger loop
    await this.debugLoop();
  }

  private setupEventHandlers(): void {
    this.runtime.on('stopped', (event) => {
      console.log(`\nStopped: ${event.reason}`);
      this.showCurrentLocation();
    });

    this.runtime.on('breakpoint', (event) => {
      if (event.type === 'hit') {
        console.log(`Breakpoint ${event.breakpoint.id} hit`);
      }
    });

    this.runtime.on('output', (event) => {
      process.stdout.write(event.output);
    });
  }

  private async debugLoop(): Promise<void> {
    this.printHelp();

    while (true) {
      const line = await this.readline('(debug) ');
      if (line === null) break;

      await this.handleCommand(line);
    }
  }

  private async handleCommand(line: string): Promise<void> {
    const [cmd, ...args] = line.trim().split(/\s+/);

    switch (cmd) {
      case 'q':
      case 'quit':
        await this.cleanup();
        process.exit(0);

      case 'h':
      case 'help':
        this.printHelp();
        break;

      case 'l':
      case 'load':
        await this.loadProgram(args[0]);
        break;

      case 'r':
      case 'run':
        await this.runtime.debug.continue();
        break;

      case 'c':
      case 'continue':
        await this.runtime.debug.continue();
        break;

      case 'n':
      case 'next':
        await this.runtime.debug.stepOver();
        break;

      case 's':
      case 'step':
        await this.runtime.debug.stepInto();
        break;

      case 'o':
      case 'out':
        await this.runtime.debug.stepOut();
        break;

      case 'b':
      case 'break':
        this.setBreakpoint(args);
        break;

      case 'd':
      case 'delete':
        this.deleteBreakpoint(args[0]);
        break;

      case 'bl':
      case 'breakpoints':
        this.listBreakpoints();
        break;

      case 'bt':
      case 'backtrace':
        this.showBacktrace();
        break;

      case 'p':
      case 'print':
        await this.printExpression(args.join(' '));
        break;

      case 'v':
      case 'vars':
        this.showVariables();
        break;

      case 'cesk':
        this.showCESKState();
        break;

      case 'hist':
      case 'history':
        this.showHistory(parseInt(args[0]) || 10);
        break;

      case 'back':
        this.timeTravel(-1);
        break;

      case 'forward':
        this.timeTravel(1);
        break;

      case 'amb':
        this.showAmbState();
        break;

      default:
        if (cmd) {
          // Try to evaluate as expression
          await this.evaluate(line);
        }
    }
  }

  private setBreakpoint(args: string[]): void {
    if (args.length === 0) {
      console.log('Usage: break <line> [condition]');
      return;
    }

    const line = parseInt(args[0]);
    const condition = args.slice(1).join(' ') || undefined;

    const bp = this.runtime.breakpoints.addLineBreakpoint(
      this.currentSource,
      line,
      { condition }
    );

    console.log(`Breakpoint ${bp.id} set at line ${line}`);
  }

  private deleteBreakpoint(id: string): void {
    if (this.runtime.breakpoints.removeBreakpoint(id)) {
      console.log(`Breakpoint ${id} deleted`);
    } else {
      console.log(`Breakpoint ${id} not found`);
    }
  }

  private listBreakpoints(): void {
    const breakpoints = this.runtime.breakpoints.getBreakpoints();
    if (breakpoints.length === 0) {
      console.log('No breakpoints set');
      return;
    }

    for (const bp of breakpoints) {
      const status = bp.enabled ? '' : ' (disabled)';
      const condition = bp.condition ? ` if ${bp.condition}` : '';
      console.log(`  ${bp.id}: line ${bp.line}${condition}${status}`);
    }
  }

  private showBacktrace(): void {
    const frames = this.runtime.debug.getStackTrace();

    console.log('Stack trace:');
    for (let i = 0; i < frames.length; i++) {
      const frame = frames[i];
      const marker = i === 0 ? '>' : ' ';
      console.log(`${marker} #${i} ${frame.name} at ${frame.source}:${frame.line}`);
    }
  }

  private showVariables(): void {
    const scopes = this.runtime.debug.getScopes(0);

    for (const scope of scopes) {
      console.log(`\n${scope.name}:`);
      const vars = this.runtime.debug.getVariables(scope.variablesReference);
      for (const v of vars) {
        console.log(`  ${v.name} = ${v.value}`);
      }
    }
  }

  private showCESKState(): void {
    const info = this.runtime.inspector.getCESKInfo(
      this.runtime.debug.getExecutionEngine().getState()
    );

    console.log('\nCESK State:');
    console.log(`  Control: ${info.control}`);
    console.log(`  Continuation: ${info.kontinuation}`);
    console.log(`  Environment: ${info.environment.length} bindings`);
    console.log(`  Store: ${info.store.length} cells`);
  }

  private showAmbState(): void {
    const state = this.runtime.getAmbState();

    if (!state) {
      console.log('Not in AMB search');
      return;
    }

    console.log('\nAMB State:');
    console.log(`  Depth: ${state.depth}`);
    console.log(`  Path: ${state.currentPath.join(' -> ')}`);
    console.log(`  Remaining: ${state.remainingAlternatives}`);
    console.log(`  Explored: ${state.exploredPaths}`);
  }

  private async printExpression(expr: string): Promise<void> {
    try {
      const result = this.runtime.debug.evaluate(expr, 0, 'repl');
      console.log(`=> ${result.result}`);
    } catch (e) {
      console.error(`Error: ${e.message}`);
    }
  }

  private async evaluate(expr: string): Promise<void> {
    try {
      const result = await this.runtime.eval(expr);
      console.log(`=> ${formatValue(result.value)}`);
    } catch (e) {
      console.error(`Error: ${e.message}`);
    }
  }

  private showCurrentLocation(): void {
    const frames = this.runtime.debug.getStackTrace({ levels: 1 });
    if (frames.length > 0) {
      const frame = frames[0];
      console.log(`  at ${frame.name} (${frame.source}:${frame.line})`);
    }
  }

  private printHelp(): void {
    console.log(`
OmegaLLM Debugger Commands:
  Execution:
    run, r              Run/continue program
    continue, c         Continue execution
    next, n             Step over
    step, s             Step into
    out, o              Step out
    back/forward        Time travel

  Breakpoints:
    break, b <line>     Set breakpoint
    delete, d <id>      Delete breakpoint
    breakpoints, bl     List breakpoints

  Inspection:
    print, p <expr>     Evaluate and print expression
    vars, v             Show variables
    backtrace, bt       Show call stack
    cesk                Show CESK state
    amb                 Show AMB state
    history             Show evaluation history

  General:
    load, l <file>      Load program
    help, h             Show this help
    quit, q             Exit debugger
    `);
  }

  private readline(prompt: string): Promise<string | null> {
    return new Promise((resolve) => {
      this.rl.question(prompt, resolve);
      this.rl.once('close', () => resolve(null));
    });
  }

  private async cleanup(): Promise<void> {
    await this.runtime.dispose();
    this.rl.close();
  }
}

// Main
const debugger_ = new OmegaDebugger();
debugger_.start(process.argv[2]).catch(console.error);
```

---

## Test Requirements

### Unit Tests
- [ ] Commands parse correctly
- [ ] Breakpoint commands work
- [ ] Stepping commands work
- [ ] Inspection commands work
- [ ] Time travel works

### Integration Tests
- [ ] Full debug session
- [ ] Breakpoints stop execution
- [ ] Expression evaluation works
- [ ] AMB state visible

---

## Acceptance Criteria
1. Code reduced from ~1197 to ~300 lines
2. All debugger functionality preserved
3. CESK state visible
4. AMB/transaction state inspectable
5. Time travel works via history
