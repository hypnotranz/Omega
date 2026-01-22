# JOB-019b3-process-manager: Server Process Lifecycle

## Context

This job implements the `ProcessManager` class that handles starting, stopping, and monitoring the server process.

## Goal

Create a `ProcessManager` class that:
1. Starts the server as a detached background process
2. Stops the server gracefully
3. Monitors server health
4. Manages PID file for process tracking
5. Implements `IProcessManager` interface

## Dependencies

- `019-types` (for interfaces)

## Blockers

- None (can run in parallel with other Layer 2 jobs)

## Files to Create

1. `server/process-manager.ts` - ProcessManager class

## Implementation

```typescript
// server/process-manager.ts
import { spawn } from 'child_process';
import * as fs from 'fs';
import * as path from 'path';
import type { IProcessManager } from './types';

export interface ProcessManagerOptions {
  pidFile?: string;
  serverScript?: string;
  port?: number;
  healthCheckUrl?: string;
  healthCheckTimeout?: number;
}

export class ProcessManager implements IProcessManager {
  private pidFile: string;
  private serverScript: string;
  private port: number;
  private healthCheckUrl: string;
  private healthCheckTimeout: number;

  constructor(options: ProcessManagerOptions = {}) {
    this.pidFile = options.pidFile || 
      path.join(process.env.HOME || process.env.USERPROFILE || '.', '.omega-session-server.pid');
    this.serverScript = options.serverScript || 
      path.resolve(__dirname, 'session-server.js');
    this.port = options.port || 3000;
    this.healthCheckUrl = options.healthCheckUrl || 
      `http://localhost:${this.port}/api/health`;
    this.healthCheckTimeout = options.healthCheckTimeout || 500;
  }

  async startServer(options: Record<string, unknown> = {}): Promise<number | undefined> {
    // Check if already running
    if (await this.isServerRunning()) {
      console.log('Server is already running');
      return this.getServerPid() ?? undefined;
    }

    // Determine script path and executor
    const scriptPath = this.resolveServerScript();
    const executor = scriptPath.endsWith('.ts') ? 'tsx' : 'node';

    // Start server as detached process
    const child = spawn(executor, [scriptPath], {
      detached: true,
      stdio: 'ignore',
      env: {
        ...process.env,
        SESSION_SERVER_PORT: String(this.port),
        ...options,
      },
    });

    child.unref();

    if (child.pid) {
      this.writePidFile(child.pid);
      return child.pid;
    }

    return undefined;
  }

  async stopServer(): Promise<void> {
    const pid = await this.getServerPid();

    if (!pid) {
      console.log('Server is not running');
      return;
    }

    try {
      // Send SIGTERM for graceful shutdown
      process.kill(pid, 'SIGTERM');

      // Wait for process to exit
      await this.waitForExit(pid, 5000);

      this.cleanupPidFile();
    } catch (error: any) {
      if (error.code === 'ESRCH') {
        // Process doesn't exist, clean up stale PID
        this.cleanupPidFile();
      } else {
        throw error;
      }
    }
  }

  async isServerRunning(): Promise<boolean> {
    // First try health check
    if (await this.healthCheck()) {
      return true;
    }

    // Fall back to PID check
    const pid = await this.getServerPid();
    if (!pid) return false;

    try {
      process.kill(pid, 0);
      return true;
    } catch {
      // Process not running, clean up stale PID
      this.cleanupPidFile();
      return false;
    }
  }

  async getServerPid(): Promise<number | null> {
    if (!fs.existsSync(this.pidFile)) {
      return null;
    }

    try {
      const content = fs.readFileSync(this.pidFile, 'utf8').trim();
      const pid = parseInt(content, 10);
      return isNaN(pid) ? null : pid;
    } catch {
      return null;
    }
  }

  private resolveServerScript(): string {
    // Check for compiled JS first
    if (fs.existsSync(this.serverScript)) {
      return this.serverScript;
    }

    // Try TypeScript version
    const tsPath = this.serverScript.replace('.js', '.ts');
    if (fs.existsSync(tsPath)) {
      return tsPath;
    }

    return this.serverScript;
  }

  private writePidFile(pid: number): void {
    const dir = path.dirname(this.pidFile);
    if (!fs.existsSync(dir)) {
      fs.mkdirSync(dir, { recursive: true });
    }
    fs.writeFileSync(this.pidFile, String(pid));
  }

  private cleanupPidFile(): void {
    if (fs.existsSync(this.pidFile)) {
      try {
        fs.unlinkSync(this.pidFile);
      } catch (error) {
        console.error('Failed to clean up PID file:', error);
      }
    }
  }

  private async healthCheck(): Promise<boolean> {
    try {
      const controller = new AbortController();
      const timeout = setTimeout(() => controller.abort(), this.healthCheckTimeout);

      const response = await fetch(this.healthCheckUrl, {
        signal: controller.signal,
      });

      clearTimeout(timeout);
      return response.ok;
    } catch {
      return false;
    }
  }

  private async waitForExit(pid: number, timeoutMs: number): Promise<void> {
    const start = Date.now();

    while (Date.now() - start < timeoutMs) {
      try {
        process.kill(pid, 0);
        await new Promise(r => setTimeout(r, 100));
      } catch {
        // Process exited
        return;
      }
    }

    // Force kill if still running
    try {
      process.kill(pid, 'SIGKILL');
    } catch {
      // Already dead
    }
  }
}
```

## Testing

```typescript
// server/process-manager.spec.ts
import * as fs from 'fs';
import * as path from 'path';
import * as os from 'os';
import { ProcessManager } from './process-manager';

describe('ProcessManager', () => {
  let tempDir: string;
  let pidFile: string;

  beforeEach(() => {
    tempDir = fs.mkdtempSync(path.join(os.tmpdir(), 'process-mgr-test-'));
    pidFile = path.join(tempDir, 'test.pid');
  });

  afterEach(() => {
    fs.rmSync(tempDir, { recursive: true, force: true });
  });

  describe('getServerPid', () => {
    it('returns null if PID file does not exist', async () => {
      const pm = new ProcessManager({ pidFile });

      const pid = await pm.getServerPid();

      expect(pid).toBeNull();
    });

    it('returns PID from file', async () => {
      fs.writeFileSync(pidFile, '12345');
      const pm = new ProcessManager({ pidFile });

      const pid = await pm.getServerPid();

      expect(pid).toBe(12345);
    });

    it('returns null for invalid PID file content', async () => {
      fs.writeFileSync(pidFile, 'not-a-number');
      const pm = new ProcessManager({ pidFile });

      const pid = await pm.getServerPid();

      expect(pid).toBeNull();
    });
  });

  describe('isServerRunning', () => {
    it('returns false if no PID file', async () => {
      const pm = new ProcessManager({ pidFile });

      const running = await pm.isServerRunning();

      expect(running).toBe(false);
    });

    it('returns false and cleans up stale PID', async () => {
      // Write a PID that doesn't exist
      fs.writeFileSync(pidFile, '999999999');
      const pm = new ProcessManager({ pidFile });

      const running = await pm.isServerRunning();

      expect(running).toBe(false);
      expect(fs.existsSync(pidFile)).toBe(false);
    });
  });

  // Note: Full start/stop tests require actual server implementation
  // These would be integration tests
});
```

## Success Criteria

1. Server starts as detached process
2. PID file is correctly managed
3. Health check works
4. Graceful shutdown works
5. All tests pass

## Estimated Effort

- Implementation: 1 hour
- Testing: 1 hour
