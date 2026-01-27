# JOB-019e3-snapshot-manager: Session Snapshots

## Context

This job implements the `SnapshotManager` class that creates and manages lightweight session snapshots for quick restore points.

## Goal

Create a `SnapshotManager` class that:
1. Creates snapshots of session state
2. Stores snapshots with metadata
3. Restores sessions from snapshots
4. Lists and manages snapshots

## Dependencies

- `019-types` (for SnapshotMetadata interface)

## Blockers

- None (can run in parallel with other Layer 5 jobs)

## Files to Create

1. `server/snapshot-manager.ts` - Snapshot management

## Implementation

```typescript
// server/snapshot-manager.ts
import * as fs from 'fs';
import * as path from 'path';
import type { SnapshotMetadata, ISessionInstance } from './types';

export class SnapshotManager {
  private snapshotsDir: string;

  constructor(sessionDir: string) {
    this.snapshotsDir = path.join(sessionDir, 'snapshots');
    fs.mkdirSync(this.snapshotsDir, { recursive: true });
  }

  async createSnapshot(
    session: ISessionInstance,
    description: string = '',
    tags: string[] = []
  ): Promise<SnapshotMetadata> {
    const sessionName = session.getMetadata().name;
    const snapshotId = `${sessionName}-${Date.now()}`;

    const metadata: SnapshotMetadata = {
      id: snapshotId,
      sessionName,
      created: new Date(),
      description,
      tags,
    };

    // Create snapshot directory
    const snapshotDir = path.join(this.snapshotsDir, snapshotId);
    fs.mkdirSync(snapshotDir, { recursive: true });

    // Save metadata
    fs.writeFileSync(
      path.join(snapshotDir, 'metadata.json'),
      JSON.stringify(metadata, null, 2)
    );

    // Save session state (simplified - actual impl would use serializer)
    const state = session.getState();
    fs.writeFileSync(
      path.join(snapshotDir, 'state.json'),
      JSON.stringify(state, null, 2)
    );

    return metadata;
  }

  listSnapshots(sessionName?: string): SnapshotMetadata[] {
    if (!fs.existsSync(this.snapshotsDir)) {
      return [];
    }

    const snapshots: SnapshotMetadata[] = [];
    const dirs = fs.readdirSync(this.snapshotsDir);

    for (const dir of dirs) {
      const metadataPath = path.join(this.snapshotsDir, dir, 'metadata.json');

      if (fs.existsSync(metadataPath)) {
        try {
          const metadata = JSON.parse(fs.readFileSync(metadataPath, 'utf8'));

          if (!sessionName || metadata.sessionName === sessionName) {
            snapshots.push(metadata);
          }
        } catch {
          // Skip invalid snapshots
        }
      }
    }

    // Sort by creation date (newest first)
    return snapshots.sort((a, b) =>
      new Date(b.created).getTime() - new Date(a.created).getTime()
    );
  }

  getSnapshot(snapshotId: string): SnapshotMetadata | null {
    const metadataPath = path.join(this.snapshotsDir, snapshotId, 'metadata.json');

    if (!fs.existsSync(metadataPath)) {
      return null;
    }

    try {
      return JSON.parse(fs.readFileSync(metadataPath, 'utf8'));
    } catch {
      return null;
    }
  }

  getSnapshotState(snapshotId: string): unknown | null {
    const statePath = path.join(this.snapshotsDir, snapshotId, 'state.json');

    if (!fs.existsSync(statePath)) {
      return null;
    }

    try {
      return JSON.parse(fs.readFileSync(statePath, 'utf8'));
    } catch {
      return null;
    }
  }

  deleteSnapshot(snapshotId: string): boolean {
    const snapshotDir = path.join(this.snapshotsDir, snapshotId);

    if (!fs.existsSync(snapshotDir)) {
      return false;
    }

    try {
      fs.rmSync(snapshotDir, { recursive: true, force: true });
      return true;
    } catch {
      return false;
    }
  }

  findSnapshotsByTag(tag: string): SnapshotMetadata[] {
    return this.listSnapshots().filter(s => s.tags.includes(tag));
  }

  getSnapshotCount(sessionName?: string): number {
    return this.listSnapshots(sessionName).length;
  }
}
```

## Testing

```typescript
// server/snapshot-manager.spec.ts
import * as fs from 'fs';
import * as path from 'path';
import * as os from 'os';
import { SnapshotManager } from './snapshot-manager';
import type { ISessionInstance } from './types';

describe('SnapshotManager', () => {
  let tempDir: string;
  let manager: SnapshotManager;
  let mockSession: ISessionInstance;

  beforeEach(() => {
    tempDir = fs.mkdtempSync(path.join(os.tmpdir(), 'snapshot-test-'));
    manager = new SnapshotManager(tempDir);

    mockSession = {
      execute: jest.fn(),
      getState: () => ({ defs: [], debugMode: false }),
      getMetadata: () => ({
        id: 'test',
        name: 'test',
        created: new Date(),
        lastAccessed: new Date(),
        eventCount: 5,
        checkpointCount: 2,
        size: 1024,
      }),
      isDirty: () => false,
      markClean: jest.fn(),
      getLastAccessTime: () => Date.now(),
      getChangesSinceLastSave: () => [],
      updateSavePoint: jest.fn(),
    };
  });

  afterEach(() => {
    fs.rmSync(tempDir, { recursive: true, force: true });
  });

  describe('createSnapshot', () => {
    it('creates snapshot with metadata', async () => {
      const snapshot = await manager.createSnapshot(
        mockSession,
        'Test snapshot',
        ['tag1', 'tag2']
      );

      expect(snapshot.sessionName).toBe('test');
      expect(snapshot.description).toBe('Test snapshot');
      expect(snapshot.tags).toEqual(['tag1', 'tag2']);
    });

    it('saves state to disk', async () => {
      const snapshot = await manager.createSnapshot(mockSession);

      const state = manager.getSnapshotState(snapshot.id);
      expect(state).toBeDefined();
    });
  });

  describe('listSnapshots', () => {
    it('lists all snapshots', async () => {
      await manager.createSnapshot(mockSession);
      await manager.createSnapshot(mockSession);

      const snapshots = manager.listSnapshots();
      expect(snapshots).toHaveLength(2);
    });

    it('filters by session name', async () => {
      await manager.createSnapshot(mockSession);

      const otherSession = {
        ...mockSession,
        getMetadata: () => ({ ...mockSession.getMetadata(), name: 'other' }),
      };
      await manager.createSnapshot(otherSession);

      const snapshots = manager.listSnapshots('test');
      expect(snapshots).toHaveLength(1);
    });

    it('sorts by creation date (newest first)', async () => {
      const snap1 = await manager.createSnapshot(mockSession);
      await new Promise(r => setTimeout(r, 10));
      const snap2 = await manager.createSnapshot(mockSession);

      const snapshots = manager.listSnapshots();
      expect(snapshots[0].id).toBe(snap2.id);
    });
  });

  describe('deleteSnapshot', () => {
    it('deletes snapshot', async () => {
      const snapshot = await manager.createSnapshot(mockSession);

      expect(manager.deleteSnapshot(snapshot.id)).toBe(true);
      expect(manager.getSnapshot(snapshot.id)).toBeNull();
    });

    it('returns false for non-existent snapshot', () => {
      expect(manager.deleteSnapshot('nonexistent')).toBe(false);
    });
  });

  describe('findSnapshotsByTag', () => {
    it('finds snapshots by tag', async () => {
      await manager.createSnapshot(mockSession, '', ['important']);
      await manager.createSnapshot(mockSession, '', ['other']);
      await manager.createSnapshot(mockSession, '', ['important', 'other']);

      const snapshots = manager.findSnapshotsByTag('important');
      expect(snapshots).toHaveLength(2);
    });
  });
});
```

## Success Criteria

1. Snapshots are created correctly
2. Listing and filtering works
3. Deletion works
4. All tests pass

## Estimated Effort

- Implementation: 45 minutes
- Testing: 45 minutes
