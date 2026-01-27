# JOB-019e1-concurrency-manager: Multi-client Access Control

## Context

This job implements the `ConcurrencyManager` class that handles concurrent access to sessions from multiple clients.

## Goal

Create a `ConcurrencyManager` class that:
1. Tracks active clients per session
2. Implements read/write locking
3. Prevents conflicting modifications
4. Supports read-only and read-write access modes

## Dependencies

- `019-types` (for AccessMode enum)

## Blockers

- None (can run in parallel with other Layer 5 jobs)

## Files to Create

1. `server/concurrency-manager.ts` - Concurrency control

## Implementation

```typescript
// server/concurrency-manager.ts
import { AccessMode } from './types';

export interface ClientAccess {
  clientId: string;
  mode: AccessMode;
  acquiredAt: Date;
}

export class ConcurrencyManager {
  private activeSessions: Map<string, Map<string, ClientAccess>> = new Map();
  private writeLocks: Map<string, string> = new Map();

  acquireAccess(sessionName: string, clientId: string, mode: AccessMode): boolean {
    // Initialize session tracking if needed
    if (!this.activeSessions.has(sessionName)) {
      this.activeSessions.set(sessionName, new Map());
    }

    const sessionClients = this.activeSessions.get(sessionName)!;

    // For read-only, always allow
    if (mode === AccessMode.ReadOnly) {
      sessionClients.set(clientId, {
        clientId,
        mode,
        acquiredAt: new Date(),
      });
      return true;
    }

    // For read-write, check if locked by another client
    const currentLock = this.writeLocks.get(sessionName);
    if (currentLock && currentLock !== clientId) {
      return false;
    }

    // Acquire write lock
    this.writeLocks.set(sessionName, clientId);
    sessionClients.set(clientId, {
      clientId,
      mode,
      acquiredAt: new Date(),
    });

    return true;
  }

  releaseAccess(sessionName: string, clientId: string): void {
    const sessionClients = this.activeSessions.get(sessionName);
    if (!sessionClients) return;

    sessionClients.delete(clientId);

    // Clean up empty session tracking
    if (sessionClients.size === 0) {
      this.activeSessions.delete(sessionName);
    }

    // Release write lock if held by this client
    if (this.writeLocks.get(sessionName) === clientId) {
      this.writeLocks.delete(sessionName);
    }
  }

  getActiveClients(sessionName: string): ClientAccess[] {
    const sessionClients = this.activeSessions.get(sessionName);
    if (!sessionClients) return [];
    return Array.from(sessionClients.values());
  }

  getWriteLockHolder(sessionName: string): string | null {
    return this.writeLocks.get(sessionName) || null;
  }

  isSessionLocked(sessionName: string): boolean {
    return this.writeLocks.has(sessionName);
  }

  getActiveSessionCount(): number {
    return this.activeSessions.size;
  }

  // Upgrade from read-only to read-write
  upgradeAccess(sessionName: string, clientId: string): boolean {
    const sessionClients = this.activeSessions.get(sessionName);
    if (!sessionClients || !sessionClients.has(clientId)) {
      return false;
    }

    return this.acquireAccess(sessionName, clientId, AccessMode.ReadWrite);
  }

  // Downgrade from read-write to read-only
  downgradeAccess(sessionName: string, clientId: string): void {
    const sessionClients = this.activeSessions.get(sessionName);
    if (!sessionClients) return;

    const access = sessionClients.get(clientId);
    if (!access) return;

    // Release write lock
    if (this.writeLocks.get(sessionName) === clientId) {
      this.writeLocks.delete(sessionName);
    }

    // Update to read-only
    sessionClients.set(clientId, {
      ...access,
      mode: AccessMode.ReadOnly,
    });
  }
}
```

## Testing

```typescript
// server/concurrency-manager.spec.ts
import { ConcurrencyManager } from './concurrency-manager';
import { AccessMode } from './types';

describe('ConcurrencyManager', () => {
  let manager: ConcurrencyManager;

  beforeEach(() => {
    manager = new ConcurrencyManager();
  });

  describe('acquireAccess', () => {
    it('allows multiple read-only clients', () => {
      expect(manager.acquireAccess('session1', 'client1', AccessMode.ReadOnly)).toBe(true);
      expect(manager.acquireAccess('session1', 'client2', AccessMode.ReadOnly)).toBe(true);
      expect(manager.acquireAccess('session1', 'client3', AccessMode.ReadOnly)).toBe(true);

      expect(manager.getActiveClients('session1')).toHaveLength(3);
    });

    it('allows single write client', () => {
      expect(manager.acquireAccess('session1', 'client1', AccessMode.ReadWrite)).toBe(true);
      expect(manager.getWriteLockHolder('session1')).toBe('client1');
    });

    it('blocks second write client', () => {
      manager.acquireAccess('session1', 'client1', AccessMode.ReadWrite);

      expect(manager.acquireAccess('session1', 'client2', AccessMode.ReadWrite)).toBe(false);
    });

    it('allows same client to re-acquire write lock', () => {
      manager.acquireAccess('session1', 'client1', AccessMode.ReadWrite);

      expect(manager.acquireAccess('session1', 'client1', AccessMode.ReadWrite)).toBe(true);
    });

    it('allows read while write locked', () => {
      manager.acquireAccess('session1', 'client1', AccessMode.ReadWrite);

      expect(manager.acquireAccess('session1', 'client2', AccessMode.ReadOnly)).toBe(true);
    });
  });

  describe('releaseAccess', () => {
    it('removes client from session', () => {
      manager.acquireAccess('session1', 'client1', AccessMode.ReadOnly);
      manager.releaseAccess('session1', 'client1');

      expect(manager.getActiveClients('session1')).toHaveLength(0);
    });

    it('releases write lock', () => {
      manager.acquireAccess('session1', 'client1', AccessMode.ReadWrite);
      manager.releaseAccess('session1', 'client1');

      expect(manager.isSessionLocked('session1')).toBe(false);
    });

    it('allows new write client after release', () => {
      manager.acquireAccess('session1', 'client1', AccessMode.ReadWrite);
      manager.releaseAccess('session1', 'client1');

      expect(manager.acquireAccess('session1', 'client2', AccessMode.ReadWrite)).toBe(true);
    });
  });

  describe('upgradeAccess', () => {
    it('upgrades read-only to read-write', () => {
      manager.acquireAccess('session1', 'client1', AccessMode.ReadOnly);

      expect(manager.upgradeAccess('session1', 'client1')).toBe(true);
      expect(manager.getWriteLockHolder('session1')).toBe('client1');
    });

    it('fails upgrade if another client has write lock', () => {
      manager.acquireAccess('session1', 'client1', AccessMode.ReadWrite);
      manager.acquireAccess('session1', 'client2', AccessMode.ReadOnly);

      expect(manager.upgradeAccess('session1', 'client2')).toBe(false);
    });
  });

  describe('downgradeAccess', () => {
    it('releases write lock but keeps client active', () => {
      manager.acquireAccess('session1', 'client1', AccessMode.ReadWrite);
      manager.downgradeAccess('session1', 'client1');

      expect(manager.isSessionLocked('session1')).toBe(false);
      expect(manager.getActiveClients('session1')).toHaveLength(1);
    });
  });
});
```

## Success Criteria

1. Multiple read-only clients work
2. Write locking prevents conflicts
3. Lock upgrade/downgrade works
4. All tests pass

## Estimated Effort

- Implementation: 45 minutes
- Testing: 45 minutes
