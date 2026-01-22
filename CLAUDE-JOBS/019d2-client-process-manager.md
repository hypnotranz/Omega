# JOB-019d2-client-process-manager: Auto-start Server from Client

## Context

This job implements the client-side process manager that can auto-start the Session Server when needed.

## Goal

Create a `ClientProcessManager` class that:
1. Detects if server is running
2. Auto-starts server if needed
3. Waits for server to be ready
4. Provides server control from client side

## Dependencies

- `019-types` (for interfaces)
- `019b3-process-manager` (shares similar logic)

## Blockers

- None (can run in parallel with other Layer 4 jobs)

## Files to Create

1. `server/client-process-manager.ts` - Client-side process management

## Implementation

```typescript
// server/client-process-manager.ts
import { spawn } from 'child_process';
import * as fs from 'fs';
import * as path from 'path';

export interface ClientProcessManagerOptions {
  pidFile?: string;
  serverScript?: string;
  port?: number;
  startupTimeout?: number;
  healthCheckInterval?: number;
}

export class ClientProcessManager {
  private pidFile: string;
  private serverScript: string;
  private port: number;
  private startupTimeout: number;
  private healthCheckInterval: number;

  constructor(options: ClientProcessManagerOptions = {}) {
    const homeDir = process.env.HOME || process.env.USERPROFILE || '.';
    
    this.pidFile = options.pidFile || 
      path.join(homeDir, '.omega-session-server.pid');
    this.serverScript = options.serverScript || 
      path.resolve(__dirname, 'session-server.js');
    this.port = options.port || 3000;
    this.startupTimeout = options.startupTimeout || 10000;
    this.healthCheckInterval = options.healthCheckInterval || 200;
  }

  async ensureServerRunning(): Promise<void> {
    if (await this.isServerRunning()) {
      return;
    }

    console.log('Starting session server...');
    await this.startServer();
    await this.waitForServer();
    console.log('Session server started');
  }

  async isServerRunning(): Promise<boolean> {
    // Try health check first
    try {
      const response = await fetch(
        `http://localhost:${this.port}/api/health`,
        { signal: AbortSignal.timeout(500) }
      );
      return response.ok;
    } catch {
      // Server not responding
    }

    // Check PID file
    const pid = this.getServerPid();
    if (!pid) return false;

    try {
      process.kill(pid, 0);
      return true;
    } catch {
      this.cleanupStalePid();
      return false;
    }
  }

  async startServer(): Promise<number | undefined> {
    const scriptPath = this.resolveServerScript();
    const executor = scriptPath.endsWith('.ts') ? 'tsx' : 'node';

    const child = spawn(executor, [scriptPath], {
      detached: true,
      stdio: 'ignore',
      env: {
        ...process.env,
        SESSION_SERVER_PORT: String(this.port),
        NODE_ENV: 'production',
      },
    });

    child.unref();

    if (child.pid) {
      this.writePidFile(child.pid);
      return child.pid;
    }

    return undefined;
  }

  async stopServer(): Promise<boolean> {
    const pid = this.getServerPid();
    if (!pid) return false;

    try {
      process.kill(pid, 'SIGTERM');
      this.cleanupStalePid();
      return true;
    } catch {
      return false;
    }
  }

  private async waitForServer(): Promise<void> {
    const startTime = Date.now();

    while (Date.now() - startTime < this.startupTimeout) {
      try {
        const response = await fetch(
          `http://localhost:${this.port}/api/health`,
          { signal: AbortSignal.timeout(500) }
        );
        if (response.ok) return;
      } catch {
        // Not ready yet
      }

      await new Promise(r => setTimeout(r, this.healthCheckInterval));
    }

    throw new Error(`Server failed to start within ${this.startupTimeout}ms`);
  }

  private resolveServerScript(): string {
    if (fs.existsSync(this.serverScript)) {
      return this.serverScript;
    }

    const tsPath = this.serverScript.replace('.js', '.ts');
    if (fs.existsSync(tsPath)) {
      return tsPath;
    }

    return this.serverScript;
  }

  private getServerPid(): number | null {
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

  private writePidFile(pid: number): void {
    const dir = path.dirname(this.pidFile);
    if (!fs.existsSync(dir)) {
      fs.mkdirSync(dir, { recursive: true });
    }
    fs.writeFileSync(this.pidFile, String(pid));
  }

  private cleanupStalePid(): void {
    if (fs.existsSync(this.pidFile)) {
      try {
        fs.unlinkSync(this.pidFile);
      } catch {}
    }
  }
}
```

## Testing

```typescript
// server/client-process-manager.spec.ts
import * as fs from 'fs';
import * as path from 'path';
import * as os from 'os';
import { ClientProcessManager } from './client-process-manager';

// Mock fetch
const mockFetch = jest.fn();
global.fetch = mockFetch;

describe('ClientProcessManager', () => {
  let tempDir: string;
  let pidFile: string;

  beforeEach(() => {
    tempDir = fs.mkdtempSync(path.join(os.tmpdir(), 'client-pm-test-'));
    pidFile = path.join(tempDir, 'test.pid');
    mockFetch.mockReset();
  });

  afterEach(() => {
    fs.rmSync(tempDir, { recursive: true, force: true });
  });

  describe('isServerRunning', () => {
    it('returns true when health check succeeds', async () => {
      mockFetch.mockResolvedValueOnce({ ok: true });

      const pm = new ClientProcessManager({ pidFile });
      const running = await pm.isServerRunning();

      expect(running).toBe(true);
    });

    it('returns false when health check fails and no PID', async () => {
      mockFetch.mockRejectedValueOnce(new Error('Connection refused'));

      const pm = new ClientProcessManager({ pidFile });
      const running = await pm.isServerRunning();

      expect(running).toBe(false);
    });

    it('cleans up stale PID file', async () => {
      mockFetch.mockRejectedValueOnce(new Error('Connection refused'));
      fs.writeFileSync(pidFile, '999999999'); // Non-existent PID

      const pm = new ClientProcessManager({ pidFile });
      const running = await pm.isServerRunning();

      expect(running).toBe(false);
      expect(fs.existsSync(pidFile)).toBe(false);
    });
  });

  describe('ensureServerRunning', () => {
    it('does nothing if server already running', async () => {
      mockFetch.mockResolvedValue({ ok: true });

      const pm = new ClientProcessManager({ pidFile });
      await pm.ensureServerRunning();

      // Should only call health check once
      expect(mockFetch).toHaveBeenCalledTimes(1);
    });
  });
});
```

## Success Criteria

1. Server auto-start works reliably
2. Health check correctly detects server state
3. Stale PID files are cleaned up
4. All tests pass

## Estimated Effort

- Implementation: 45 minutes
- Testing: 45 minutes
