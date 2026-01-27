# JOB-019b1-persistence-manager: Background Saving Coordination

## Context

This job implements the `PersistenceManager` class that coordinates background saving of dirty sessions.

## Goal

Create a `PersistenceManager` class that:
1. Periodically saves dirty sessions in the background
2. Coordinates with SessionPool to find dirty sessions
3. Handles graceful shutdown with final save
4. Implements `IPersistenceManager` interface

## Dependencies

- `019-types` (for interfaces)
- `019a3-session-pool` (for ISessionPool)
- `019a4-incremental-writer` (for IIncrementalWriter)

## Blockers

- `019a3-session-pool` must be complete
- `019a4-incremental-writer` must be complete

## Files to Create

1. `server/persistence-manager.ts` - PersistenceManager class

## Implementation

```typescript
// server/persistence-manager.ts
import * as fs from 'fs';
import * as path from 'path';
import type { IPersistenceManager, ISessionPool, ISessionInstance } from './types';
import { IncrementalWriter } from './incremental-writer';

export class PersistenceManager implements IPersistenceManager {
  private writers: Map<string, IncrementalWriter> = new Map();
  private saveInterval: NodeJS.Timeout | null = null;
  private saving = false;
  private shuttingDown = false;

  constructor(
    private pool: ISessionPool,
    private sessionDir: string,
    private saveIntervalMs: number = 5000
  ) {
    this.ensureDirectories();
    this.startPeriodicSave();
  }

  async saveSession(session: ISessionInstance): Promise<void> {
    const name = session.getMetadata().name;
    const writer = this.getOrCreateWriter(name);

    const events = session.getChangesSinceLastSave();
    if (events.length === 0) return;

    await writer.appendEvents(events, session.getState());
    session.updateSavePoint();
  }

  async loadSession(name: string): Promise<ISessionInstance | null> {
    const eventFile = this.getEventFilePath(name);
    const indexFile = this.getIndexFilePath(name);

    if (!fs.existsSync(eventFile) || !fs.existsSync(indexFile)) {
      return null;
    }

    // Loading is handled by SessionManager which has access to command processor
    // This method returns null to signal that file exists but loading
    // requires external dependencies
    return null;
  }

  async saveDirtySessions(): Promise<void> {
    if (this.saving || this.shuttingDown) return;

    try {
      this.saving = true;
      const dirtySessions = this.pool.getDirty();

      await Promise.all(
        dirtySessions.map(session => this.saveSession(session))
      );
    } catch (error) {
      console.error('Error saving dirty sessions:', error);
    } finally {
      this.saving = false;
    }
  }

  async forceSave(): Promise<void> {
    // Wait for any in-progress save to complete
    while (this.saving) {
      await new Promise(r => setTimeout(r, 50));
    }
    return this.saveDirtySessions();
  }

  shutdown(): void {
    this.shuttingDown = true;

    if (this.saveInterval) {
      clearInterval(this.saveInterval);
      this.saveInterval = null;
    }

    // Synchronous final save
    this.forceSaveSync();
  }

  // Check if session files exist on disk
  sessionExistsOnDisk(name: string): boolean {
    return fs.existsSync(this.getEventFilePath(name));
  }

  // Get paths for external use
  getEventFilePath(name: string): string {
    return path.join(this.sessionDir, 'sessions', `${name}.jsonl`);
  }

  getIndexFilePath(name: string): string {
    return path.join(this.sessionDir, 'sessions', `${name}.index.json`);
  }

  private getOrCreateWriter(name: string): IncrementalWriter {
    let writer = this.writers.get(name);
    if (!writer) {
      writer = new IncrementalWriter(
        this.getEventFilePath(name),
        this.getIndexFilePath(name)
      );
      this.writers.set(name, writer);
    }
    return writer;
  }

  private startPeriodicSave(): void {
    if (this.saveInterval) {
      clearInterval(this.saveInterval);
    }

    this.saveInterval = setInterval(() => {
      this.saveDirtySessions().catch(err => {
        console.error('Error in periodic save:', err);
      });
    }, this.saveIntervalMs);
  }

  private forceSaveSync(): void {
    const dirtySessions = this.pool.getDirty();

    for (const session of dirtySessions) {
      try {
        const name = session.getMetadata().name;
        const events = session.getChangesSinceLastSave();

        if (events.length === 0) continue;

        // Synchronous write for shutdown
        const eventFile = this.getEventFilePath(name);
        const lines = events.map(e => JSON.stringify(e)).join('\n') + '\n';
        fs.appendFileSync(eventFile, lines);

        session.updateSavePoint();
      } catch (error) {
        console.error(`Error force-saving session:`, error);
      }
    }
  }

  private ensureDirectories(): void {
    fs.mkdirSync(path.join(this.sessionDir, 'sessions'), { recursive: true });
    fs.mkdirSync(path.join(this.sessionDir, 'receipts'), { recursive: true });
  }
}
```

## Testing

```typescript
// server/persistence-manager.spec.ts
import * as fs from 'fs';
import * as path from 'path';
import * as os from 'os';
import { PersistenceManager } from './persistence-manager';
import type { ISessionPool, ISessionInstance, SessionMetadata, SessionEvent } from './types';

describe('PersistenceManager', () => {
  let tempDir: string;
  let mockPool: ISessionPool;
  let mockSessions: ISessionInstance[];

  function createMockSession(name: string, dirty: boolean, events: SessionEvent[] = []): ISessionInstance {
    return {
      execute: jest.fn(),
      getState: jest.fn(() => ({})),
      getMetadata: () => ({
        id: name,
        name,
        created: new Date(),
        lastAccessed: new Date(),
        eventCount: events.length,
        checkpointCount: 0,
        size: 0,
      }),
      isDirty: () => dirty,
      markClean: jest.fn(),
      getLastAccessTime: () => Date.now(),
      getChangesSinceLastSave: () => events,
      updateSavePoint: jest.fn(),
    };
  }

  beforeEach(() => {
    tempDir = fs.mkdtempSync(path.join(os.tmpdir(), 'persist-test-'));
    mockSessions = [];

    mockPool = {
      get: jest.fn(),
      add: jest.fn(),
      remove: jest.fn(),
      getAll: () => mockSessions,
      getAllMetadata: () => mockSessions.map(s => s.getMetadata()),
      getDirty: () => mockSessions.filter(s => s.isDirty()),
      clear: jest.fn(),
      size: () => mockSessions.length,
    };
  });

  afterEach(() => {
    fs.rmSync(tempDir, { recursive: true, force: true });
  });

  describe('constructor', () => {
    it('creates session directories', () => {
      const pm = new PersistenceManager(mockPool, tempDir, 10000);

      expect(fs.existsSync(path.join(tempDir, 'sessions'))).toBe(true);
      expect(fs.existsSync(path.join(tempDir, 'receipts'))).toBe(true);

      pm.shutdown();
    });
  });

  describe('saveSession', () => {
    it('saves session events to file', async () => {
      const pm = new PersistenceManager(mockPool, tempDir, 10000);

      const events: SessionEvent[] = [
        { type: 'input', seq: 0, code: 'test', timestamp: new Date().toISOString() },
      ];
      const session = createMockSession('test', true, events);

      await pm.saveSession(session);

      const eventFile = path.join(tempDir, 'sessions', 'test.jsonl');
      expect(fs.existsSync(eventFile)).toBe(true);
      expect(session.updateSavePoint).toHaveBeenCalled();

      pm.shutdown();
    });

    it('skips save if no changes', async () => {
      const pm = new PersistenceManager(mockPool, tempDir, 10000);

      const session = createMockSession('test', false, []);

      await pm.saveSession(session);

      const eventFile = path.join(tempDir, 'sessions', 'test.jsonl');
      expect(fs.existsSync(eventFile)).toBe(false);

      pm.shutdown();
    });
  });

  describe('saveDirtySessions', () => {
    it('saves all dirty sessions from pool', async () => {
      const pm = new PersistenceManager(mockPool, tempDir, 10000);

      const events: SessionEvent[] = [
        { type: 'input', seq: 0, code: 'test', timestamp: new Date().toISOString() },
      ];

      mockSessions = [
        createMockSession('clean', false, []),
        createMockSession('dirty1', true, events),
        createMockSession('dirty2', true, events),
      ];

      await pm.saveDirtySessions();

      expect(fs.existsSync(path.join(tempDir, 'sessions', 'dirty1.jsonl'))).toBe(true);
      expect(fs.existsSync(path.join(tempDir, 'sessions', 'dirty2.jsonl'))).toBe(true);
      expect(fs.existsSync(path.join(tempDir, 'sessions', 'clean.jsonl'))).toBe(false);

      pm.shutdown();
    });
  });

  describe('sessionExistsOnDisk', () => {
    it('returns true if session file exists', async () => {
      const pm = new PersistenceManager(mockPool, tempDir, 10000);

      const events: SessionEvent[] = [
        { type: 'input', seq: 0, code: 'test', timestamp: new Date().toISOString() },
      ];
      const session = createMockSession('test', true, events);

      await pm.saveSession(session);

      expect(pm.sessionExistsOnDisk('test')).toBe(true);
      expect(pm.sessionExistsOnDisk('nonexistent')).toBe(false);

      pm.shutdown();
    });
  });

  describe('shutdown', () => {
    it('stops periodic saves and does final save', () => {
      const pm = new PersistenceManager(mockPool, tempDir, 100);

      const events: SessionEvent[] = [
        { type: 'input', seq: 0, code: 'test', timestamp: new Date().toISOString() },
      ];
      mockSessions = [createMockSession('test', true, events)];

      pm.shutdown();

      // Should have saved on shutdown
      expect(fs.existsSync(path.join(tempDir, 'sessions', 'test.jsonl'))).toBe(true);
    });
  });
});
```

## Success Criteria

1. Periodic background saving works
2. Dirty sessions are correctly identified and saved
3. Graceful shutdown saves all pending changes
4. All tests pass

## Estimated Effort

- Implementation: 1.5 hours
- Testing: 1 hour
