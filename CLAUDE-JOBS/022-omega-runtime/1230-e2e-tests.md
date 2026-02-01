# 1230: E2E CLI Tests

## Status: COMPLETE ✅

## Purpose
End-to-end tests for the refactored CLI tools (REPL, debugger, server).

## Dependencies
- 1100-repl-refactor.md ✅
- 1110-server-refactor.md ✅
- 1120-debugger-refactor.md ✅

## Implementation Notes
Implemented in:
- `test/e2e/cli.test.ts` (31 tests) - REPL, Debugger, Server E2E tests
- `test/e2e/helpers/ProcessRunner.ts` - Process spawning utility
- `test/e2e/fixtures/` - Test programs (debug-program.lisp, amb-program.lisp, simple-program.lisp)

## Source References
- CLI testing patterns
- Process spawning and IPC

---

## Test Structure

```
tests/e2e/
├── repl.test.ts             # REPL E2E tests
├── debugger.test.ts         # Debugger E2E tests
├── server.test.ts           # Server E2E tests
├── helpers/
│   ├── ProcessRunner.ts     # Spawn and control processes
│   ├── REPLClient.ts        # REPL interaction helper
│   └── ProtocolClient.ts    # nREPL protocol client
└── fixtures/
    ├── simple-program.lisp
    ├── amb-program.lisp
    └── debug-program.lisp
```

---

## Process Runner

```typescript
// tests/e2e/helpers/ProcessRunner.ts

import { spawn, ChildProcess } from 'child_process';

export class ProcessRunner {
  private process: ChildProcess | null = null;
  private output: string[] = [];
  private exitCode: number | null = null;

  async start(command: string, args: string[]): Promise<void> {
    return new Promise((resolve, reject) => {
      this.process = spawn(command, args, {
        stdio: ['pipe', 'pipe', 'pipe'],
        env: { ...process.env, FORCE_COLOR: '0' }
      });

      this.process.stdout?.on('data', (data) => {
        this.output.push(data.toString());
      });

      this.process.stderr?.on('data', (data) => {
        this.output.push(data.toString());
      });

      this.process.on('exit', (code) => {
        this.exitCode = code;
      });

      // Wait for startup prompt
      setTimeout(resolve, 500);
    });
  }

  async send(input: string): Promise<string> {
    if (!this.process?.stdin) throw new Error('Process not started');

    const outputBefore = this.output.length;
    this.process.stdin.write(input + '\n');

    // Wait for response
    await new Promise(r => setTimeout(r, 100));

    return this.output.slice(outputBefore).join('');
  }

  async sendAndWait(input: string, pattern: RegExp, timeout = 5000): Promise<string> {
    if (!this.process?.stdin) throw new Error('Process not started');

    return new Promise((resolve, reject) => {
      const timer = setTimeout(() => reject(new Error('Timeout')), timeout);

      const check = () => {
        const fullOutput = this.output.join('');
        if (pattern.test(fullOutput)) {
          clearTimeout(timer);
          resolve(fullOutput);
        } else {
          setTimeout(check, 50);
        }
      };

      this.process!.stdin!.write(input + '\n');
      check();
    });
  }

  async stop(): Promise<number | null> {
    if (this.process) {
      this.process.kill();
      await new Promise(r => setTimeout(r, 100));
    }
    return this.exitCode;
  }

  getOutput(): string {
    return this.output.join('');
  }
}
```

---

## REPL E2E Tests

```typescript
// tests/e2e/repl.test.ts

import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import { ProcessRunner } from './helpers/ProcessRunner';

describe('REPL E2E', () => {
  let repl: ProcessRunner;

  beforeEach(async () => {
    repl = new ProcessRunner();
    await repl.start('npx', ['ts-node', 'bin/omega-repl.ts']);
  });

  afterEach(async () => {
    await repl.stop();
  });

  it('evaluates simple expression', async () => {
    const output = await repl.sendAndWait('(+ 1 2)', /=> 3/);
    expect(output).toContain('=> 3');
  });

  it('handles multi-line input', async () => {
    await repl.send('(define (square x)');
    const output = await repl.sendAndWait('  (* x x))', /=>/);
    expect(output).toContain('=>');

    const result = await repl.sendAndWait('(square 5)', /=> 25/);
    expect(result).toContain('=> 25');
  });

  it('shows history', async () => {
    await repl.sendAndWait('(+ 1 2)', /=> 3/);
    await repl.sendAndWait('(* 3 4)', /=> 12/);

    const history = await repl.sendAndWait(':history', /\[0\]/);
    expect(history).toContain('[0]');
    expect(history).toContain('(+ 1 2)');
  });

  it('saves and restores snapshots', async () => {
    await repl.sendAndWait('(define x 10)', /=>/);
    await repl.sendAndWait(':save checkpoint', /Saved/);

    await repl.sendAndWait('(set! x 20)', /=>/);
    const before = await repl.sendAndWait('x', /=> 20/);
    expect(before).toContain('=> 20');

    await repl.sendAndWait(':restore checkpoint', /Restored/);
    const after = await repl.sendAndWait('x', /=> 10/);
    expect(after).toContain('=> 10');
  });

  it('shows budget status', async () => {
    const output = await repl.sendAndWait(':budget', /Tokens:/);
    expect(output).toContain('Budget Status:');
    expect(output).toContain('Tokens:');
  });

  it('handles AMB search', async () => {
    const output = await repl.sendAndWait(
      '(let ((x (amb \'(1 2 3)))) (require (even? x)) x)',
      /=> 2/
    );
    expect(output).toContain('=> 2');
  });

  it('exits cleanly', async () => {
    await repl.send(':quit');
    const code = await repl.stop();
    expect(code).toBe(0);
  });
});
```

---

## Debugger E2E Tests

```typescript
// tests/e2e/debugger.test.ts

describe('Debugger E2E', () => {
  let debugger_: ProcessRunner;

  beforeEach(async () => {
    debugger_ = new ProcessRunner();
    await debugger_.start('npx', ['ts-node', 'bin/omega-debugger.ts']);
  });

  afterEach(async () => {
    await debugger_.stop();
  });

  it('shows help', async () => {
    const output = await debugger_.sendAndWait('help', /Commands:/);
    expect(output).toContain('Commands:');
    expect(output).toContain('break');
    expect(output).toContain('continue');
  });

  it('sets and lists breakpoints', async () => {
    await debugger_.sendAndWait('load tests/fixtures/debug-program.lisp', /Loaded/);
    await debugger_.sendAndWait('break 3', /Breakpoint.*set/);

    const list = await debugger_.sendAndWait('breakpoints', /line 3/);
    expect(list).toContain('line 3');
  });

  it('stops at breakpoint', async () => {
    await debugger_.sendAndWait('load tests/fixtures/debug-program.lisp', /Loaded/);
    await debugger_.sendAndWait('break 3', /Breakpoint.*set/);

    const stopped = await debugger_.sendAndWait('run', /Stopped.*breakpoint/);
    expect(stopped).toContain('Stopped');
  });

  it('shows backtrace', async () => {
    await debugger_.sendAndWait('load tests/fixtures/debug-program.lisp', /Loaded/);
    await debugger_.sendAndWait('break 3', /set/);
    await debugger_.sendAndWait('run', /Stopped/);

    const bt = await debugger_.sendAndWait('backtrace', /#0/);
    expect(bt).toContain('#0');
  });

  it('evaluates expressions at breakpoint', async () => {
    await debugger_.sendAndWait('load tests/fixtures/debug-program.lisp', /Loaded/);
    await debugger_.sendAndWait('break 3', /set/);
    await debugger_.sendAndWait('run', /Stopped/);

    const result = await debugger_.sendAndWait('print (+ 1 2)', /=> 3/);
    expect(result).toContain('=> 3');
  });

  it('steps through code', async () => {
    await debugger_.sendAndWait('load tests/fixtures/debug-program.lisp', /Loaded/);
    await debugger_.sendAndWait('break 1', /set/);
    await debugger_.sendAndWait('run', /Stopped/);

    await debugger_.sendAndWait('step', /Stopped.*step/);
    await debugger_.sendAndWait('next', /Stopped.*step/);
  });
});
```

---

## Server E2E Tests

```typescript
// tests/e2e/server.test.ts

describe('Protocol Server E2E', () => {
  let client: ProtocolClient;

  beforeEach(async () => {
    // Start server in background
    const server = new ProcessRunner();
    await server.start('npx', ['ts-node', 'bin/omega-server.ts', '--port', '7889']);

    // Connect client
    client = new ProtocolClient();
    await client.connect('localhost', 7889);
  });

  afterEach(async () => {
    await client.disconnect();
  });

  it('creates session', async () => {
    const response = await client.send({ op: 'clone' });
    expect(response['new-session']).toBeDefined();
  });

  it('evaluates code', async () => {
    const sessionResp = await client.send({ op: 'clone' });
    const session = sessionResp['new-session'];

    const evalResp = await client.send({
      op: 'eval',
      session,
      code: '(+ 1 2)'
    });

    expect(evalResp.value).toBe('3');
    expect(evalResp.status).toContain('done');
  });

  it('describes capabilities', async () => {
    const response = await client.send({ op: 'describe' });
    expect(response.ops).toBeDefined();
    expect(response.versions).toBeDefined();
  });
});
```

---

## Test Fixtures

```lisp
;; tests/e2e/fixtures/debug-program.lisp
(define x 10)
(define y 20)
(define z (+ x y))
(display z)
```

```lisp
;; tests/e2e/fixtures/amb-program.lisp
(define (pythagorean-triples n)
  (let ((a (amb (range 1 n)))
        (b (amb (range a n)))
        (c (amb (range b n))))
    (require (= (+ (* a a) (* b b)) (* c c)))
    (list a b c)))

(pythagorean-triples 20)
```

---

## Test Commands

```bash
# Run all E2E tests
npm run test:e2e

# Run specific CLI
npm run test:e2e -- --grep "REPL"

# With timeout extension
npm run test:e2e -- --timeout 30000
```

---

## Acceptance Criteria
1. All CLI tools start and stop cleanly
2. REPL commands work correctly
3. Debugger commands work correctly
4. Protocol server responds correctly
5. Tests complete in <120s
