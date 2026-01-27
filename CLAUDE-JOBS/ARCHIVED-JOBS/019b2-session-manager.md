# JOB-019b2-session-manager: Session Lifecycle Management

## Context

This job implements the `SessionManager` class that coordinates session lifecycle, integrating the pool and persistence components.

## Goal

Create a `SessionManager` class that:
1. Manages session lifecycle (create, load, unload)
2. Coordinates between SessionPool and PersistenceManager
3. Routes command execution to appropriate sessions
4. Implements `ISessionManager` interface

## Dependencies

- `019-types` (for interfaces)
- `019a2-session-instance` (for SessionInstance)
- `019a3-session-pool` (for SessionPool)
- `019b1-persistence-manager` (for PersistenceManager)

## Blockers

- `019a2-session-instance` must be complete
- `019a3-session-pool` must be complete
- `019b1-persistence-manager` must be complete

## Files to Create

1. `server/session-manager.ts` - SessionManager class

## Implementation

```typescript
// server/session-manager.ts
import * as fs from 'fs';
import * as path from 'path';
import type { 
  ISessionManager, 
  ISessionInstance, 
  SessionMetadata, 
  ExecuteResult,
  ServerConfig 
} from './types';
import { SessionPool } from './session-pool';
import { PersistenceManager } from './persistence-manager';
import { createSessionInstance, CommandProcessor } from './session-instance';

// Factory for creating ReplState - injected to avoid circular deps
export type ReplStateFactory = () => Promise<unknown>;

export class SessionManager implements ISessionManager {
  private pool: SessionPool;
  private persistence: PersistenceManager;
  private commandProcessor: CommandProcessor;
  private replStateFactory: ReplStateFactory;

  constructor(
    config: ServerConfig,
    commandProcessor: CommandProcessor,
    replStateFactory: ReplStateFactory
  ) {
    this.pool = new SessionPool(config.maxCachedSessions);
    this.persistence = new PersistenceManager(
      this.pool,
      config.sessionDir,
      config.saveIntervalMs
    );
    this.commandProcessor = commandProcessor;
    this.replStateFactory = replStateFactory;
  }

  async getSession(name: string): Promise<ISessionInstance> {
    // Check pool first
    let instance = this.pool.get(name);
    if (instance) {
      return instance;
    }

    // Try to load from disk
    instance = await this.loadFromDisk(name);
    
    if (!instance) {
      // Create new session
      instance = await this.createNewSession(name);
    }

    // Add to pool
    this.pool.add(name, instance);

    return instance;
  }

  async executeCommand(sessionName: string, command: string): Promise<ExecuteResult> {
    const session = await this.getSession(sessionName);
    return session.execute(command);
  }

  listSessions(): SessionMetadata[] {
    // Combine pool sessions with disk sessions
    const poolMetadata = this.pool.getAllMetadata();
    const diskSessions = this.listDiskSessions();

    // Merge, preferring pool data for sessions in both
    const poolNames = new Set(poolMetadata.map(m => m.name));
    const diskOnly = diskSessions.filter(m => !poolNames.has(m.name));

    return [...poolMetadata, ...diskOnly];
  }

  async saveDirtySessions(): Promise<void> {
    return this.persistence.saveDirtySessions();
  }

  async shutdown(): Promise<void> {
    await this.persistence.forceSave();
    this.persistence.shutdown();
  }

  private async loadFromDisk(name: string): Promise<ISessionInstance | null> {
    if (!this.persistence.sessionExistsOnDisk(name)) {
      return null;
    }

    try {
      // Read session data from disk
      const eventFile = this.persistence.getEventFilePath(name);
      const indexFile = this.persistence.getIndexFilePath(name);

      // Parse index for metadata
      const indexData = JSON.parse(fs.readFileSync(indexFile, 'utf8'));

      // Create fresh ReplState and replay definitions
      const replState = await this.replStateFactory();

      // Read events to extract definitions
      const events = fs.readFileSync(eventFile, 'utf8')
        .split('\n')
        .filter(line => line.trim())
        .map(line => JSON.parse(line));

      // Extract define statements to replay
      const defines = events
        .filter((e: any) => e.type === 'input' && e.code?.trim().startsWith('(define'))
        .map((e: any) => e.code);

      // Replay defines to restore state
      let currentState = replState as any;
      for (const def of defines) {
        const result = await this.commandProcessor(def, currentState);
        currentState = result.replState;
      }

      // Create metadata from index
      const metadata: SessionMetadata = {
        id: name,
        name,
        created: new Date(events[0]?.created || Date.now()),
        lastAccessed: new Date(),
        eventCount: indexData.eventCount || 0,
        checkpointCount: indexData.checkpoints?.length || 0,
        size: fs.statSync(eventFile).size,
      };

      return createSessionInstance(currentState, name, this.commandProcessor, metadata);
    } catch (error) {
      console.error(`Error loading session ${name}:`, error);
      return null;
    }
  }

  private async createNewSession(name: string): Promise<ISessionInstance> {
    const replState = await this.replStateFactory();

    const metadata: SessionMetadata = {
      id: name,
      name,
      created: new Date(),
      lastAccessed: new Date(),
      eventCount: 0,
      checkpointCount: 0,
      size: 0,
    };

    return createSessionInstance(replState as any, name, this.commandProcessor, metadata);
  }

  private listDiskSessions(): SessionMetadata[] {
    const sessionsPath = path.join(
      this.persistence.getEventFilePath('').replace(/[^/\\]+\.jsonl$/, '')
    );

    if (!fs.existsSync(sessionsPath)) {
      return [];
    }

    return fs.readdirSync(sessionsPath)
      .filter(f => f.endsWith('.jsonl'))
      .map(f => {
        const name = f.replace('.jsonl', '');
        const eventFile = path.join(sessionsPath, f);
        const indexFile = path.join(sessionsPath, `${name}.index.json`);

        let eventCount = 0;
        let checkpointCount = 0;

        if (fs.existsSync(indexFile)) {
          try {
            const index = JSON.parse(fs.readFileSync(indexFile, 'utf8'));
            eventCount = index.eventCount || 0;
            checkpointCount = index.checkpoints?.length || 0;
          } catch {}
        }

        return {
          id: name,
          name,
          created: new Date(), // Would need to read from file
          lastAccessed: new Date(),
          eventCount,
          checkpointCount,
          size: fs.statSync(eventFile).size,
        };
      });
  }
}
```

## Testing

```typescript
// server/session-manager.spec.ts
import * as fs from 'fs';
import * as path from 'path';
import * as os from 'os';
import { SessionManager } from './session-manager';
import type { ServerConfig } from './types';

describe('SessionManager', () => {
  let tempDir: string;
  let config: ServerConfig;
  let mockCommandProcessor: jest.Mock;
  let mockReplStateFactory: jest.Mock;

  beforeEach(() => {
    tempDir = fs.mkdtempSync(path.join(os.tmpdir(), 'session-mgr-test-'));

    config = {
      port: 3000,
      sessionDir: tempDir,
      maxCachedSessions: 10,
      saveIntervalMs: 10000,
      logLevel: 'info',
    };

    mockCommandProcessor = jest.fn(async (cmd, state) => ({
      replState: state,
      output: `Executed: ${cmd}`,
      shouldExit: false,
    }));

    mockReplStateFactory = jest.fn(async () => ({
      defs: [],
      debugMode: false,
      stepCount: 0,
      trace: [],
    }));
  });

  afterEach(async () => {
    fs.rmSync(tempDir, { recursive: true, force: true });
  });

  describe('getSession', () => {
    it('creates new session if not exists', async () => {
      const manager = new SessionManager(config, mockCommandProcessor, mockReplStateFactory);

      const session = await manager.getSession('new-session');

      expect(session).toBeDefined();
      expect(session.getMetadata().name).toBe('new-session');
      expect(mockReplStateFactory).toHaveBeenCalled();

      await manager.shutdown();
    });

    it('returns cached session on second call', async () => {
      const manager = new SessionManager(config, mockCommandProcessor, mockReplStateFactory);

      const session1 = await manager.getSession('test');
      const session2 = await manager.getSession('test');

      expect(session1).toBe(session2);
      expect(mockReplStateFactory).toHaveBeenCalledTimes(1);

      await manager.shutdown();
    });
  });

  describe('executeCommand', () => {
    it('executes command in session context', async () => {
      const manager = new SessionManager(config, mockCommandProcessor, mockReplStateFactory);

      const result = await manager.executeCommand('test', '(+ 1 2)');

      expect(result.output).toContain('Executed');
      expect(mockCommandProcessor).toHaveBeenCalled();

      await manager.shutdown();
    });
  });

  describe('listSessions', () => {
    it('lists sessions from pool and disk', async () => {
      const manager = new SessionManager(config, mockCommandProcessor, mockReplStateFactory);

      // Create a session (goes to pool)
      await manager.getSession('pooled');

      const sessions = manager.listSessions();

      expect(sessions.some(s => s.name === 'pooled')).toBe(true);

      await manager.shutdown();
    });
  });

  describe('shutdown', () => {
    it('saves dirty sessions and stops persistence', async () => {
      const manager = new SessionManager(config, mockCommandProcessor, mockReplStateFactory);

      // Create and modify a session
      await manager.executeCommand('test', '(define x 1)');

      await manager.shutdown();

      // Session should be saved
      expect(fs.existsSync(path.join(tempDir, 'sessions', 'test.jsonl'))).toBe(true);
    });
  });
});
```

## Success Criteria

1. Sessions are created, loaded, and cached correctly
2. Commands execute in correct session context
3. Session listing includes both pool and disk sessions
4. Shutdown saves all pending changes
5. All tests pass

## Estimated Effort

- Implementation: 2 hours
- Testing: 1.5 hours
