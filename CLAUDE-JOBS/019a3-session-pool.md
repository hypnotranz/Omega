# JOB-019a3-session-pool: LRU Session Cache

## Context

This job implements the `SessionPool` class that provides an LRU (Least Recently Used) cache for session instances.

## Goal

Create a `SessionPool` class that:
1. Maintains an in-memory cache of session instances
2. Implements LRU eviction when cache is full
3. Tracks dirty sessions for persistence
4. Implements `ISessionPool` interface

## Dependencies

- `019-types` (for interfaces)

## Blockers

- None

## Files to Create

1. `server/session-pool.ts` - SessionPool class

## Implementation

```typescript
// server/session-pool.ts
import type { ISessionPool, ISessionInstance, SessionMetadata } from './types';

export class SessionPool implements ISessionPool {
  private sessions: Map<string, ISessionInstance> = new Map();
  private lruOrder: string[] = [];
  private maxSize: number;

  constructor(maxSize: number = 10) {
    this.maxSize = maxSize;
  }

  get(name: string): ISessionInstance | undefined {
    const session = this.sessions.get(name);

    if (session) {
      this.touchLRU(name);
    }

    return session;
  }

  add(name: string, instance: ISessionInstance): void {
    // If already exists, just update LRU
    if (this.sessions.has(name)) {
      this.sessions.set(name, instance);
      this.touchLRU(name);
      return;
    }

    // Check if we need to evict
    if (this.sessions.size >= this.maxSize) {
      this.evictLRU();
    }

    // Add to pool
    this.sessions.set(name, instance);
    this.lruOrder.unshift(name);
  }

  remove(name: string): boolean {
    const removed = this.sessions.delete(name);

    if (removed) {
      const index = this.lruOrder.indexOf(name);
      if (index >= 0) {
        this.lruOrder.splice(index, 1);
      }
    }

    return removed;
  }

  getAll(): ISessionInstance[] {
    return Array.from(this.sessions.values());
  }

  getAllMetadata(): SessionMetadata[] {
    return this.getAll().map(s => s.getMetadata());
  }

  getDirty(): ISessionInstance[] {
    return this.getAll().filter(s => s.isDirty());
  }

  clear(): void {
    this.sessions.clear();
    this.lruOrder = [];
  }

  size(): number {
    return this.sessions.size;
  }

  // Get the session that would be evicted next (for testing/monitoring)
  peekLRU(): string | undefined {
    return this.lruOrder[this.lruOrder.length - 1];
  }

  private touchLRU(name: string): void {
    const index = this.lruOrder.indexOf(name);
    if (index >= 0) {
      this.lruOrder.splice(index, 1);
    }
    this.lruOrder.unshift(name);
  }

  private evictLRU(): ISessionInstance | undefined {
    if (this.lruOrder.length === 0) return undefined;

    const lruName = this.lruOrder.pop();
    if (!lruName) return undefined;

    const evicted = this.sessions.get(lruName);
    this.sessions.delete(lruName);

    return evicted;
  }
}
```

## Testing

```typescript
// server/session-pool.spec.ts
import { SessionPool } from './session-pool';
import type { ISessionInstance, SessionMetadata } from './types';

describe('SessionPool', () => {
  // Mock session instance factory
  function createMockSession(name: string, dirty = false): ISessionInstance {
    return {
      execute: jest.fn(),
      getState: jest.fn(),
      getMetadata: () => ({
        id: name,
        name,
        created: new Date(),
        lastAccessed: new Date(),
        eventCount: 0,
        checkpointCount: 0,
        size: 0,
      }),
      isDirty: () => dirty,
      markClean: jest.fn(),
      getLastAccessTime: () => Date.now(),
      getChangesSinceLastSave: () => [],
      updateSavePoint: jest.fn(),
    };
  }

  describe('add and get', () => {
    it('adds and retrieves sessions', () => {
      const pool = new SessionPool(10);
      const session = createMockSession('test');

      pool.add('test', session);

      expect(pool.get('test')).toBe(session);
    });

    it('returns undefined for non-existent sessions', () => {
      const pool = new SessionPool(10);

      expect(pool.get('nonexistent')).toBeUndefined();
    });
  });

  describe('LRU eviction', () => {
    it('evicts least recently used when full', () => {
      const pool = new SessionPool(2);

      pool.add('first', createMockSession('first'));
      pool.add('second', createMockSession('second'));
      pool.add('third', createMockSession('third'));

      expect(pool.get('first')).toBeUndefined();
      expect(pool.get('second')).toBeDefined();
      expect(pool.get('third')).toBeDefined();
    });

    it('updates LRU order on get', () => {
      const pool = new SessionPool(2);

      pool.add('first', createMockSession('first'));
      pool.add('second', createMockSession('second'));

      // Access 'first' to make it most recently used
      pool.get('first');

      // Add third - should evict 'second' now
      pool.add('third', createMockSession('third'));

      expect(pool.get('first')).toBeDefined();
      expect(pool.get('second')).toBeUndefined();
      expect(pool.get('third')).toBeDefined();
    });

    it('peekLRU returns next eviction candidate', () => {
      const pool = new SessionPool(3);

      pool.add('first', createMockSession('first'));
      pool.add('second', createMockSession('second'));

      expect(pool.peekLRU()).toBe('first');

      pool.get('first'); // Touch first

      expect(pool.peekLRU()).toBe('second');
    });
  });

  describe('remove', () => {
    it('removes session from pool', () => {
      const pool = new SessionPool(10);
      pool.add('test', createMockSession('test'));

      expect(pool.remove('test')).toBe(true);
      expect(pool.get('test')).toBeUndefined();
    });

    it('returns false for non-existent session', () => {
      const pool = new SessionPool(10);

      expect(pool.remove('nonexistent')).toBe(false);
    });
  });

  describe('getDirty', () => {
    it('returns only dirty sessions', () => {
      const pool = new SessionPool(10);

      pool.add('clean', createMockSession('clean', false));
      pool.add('dirty1', createMockSession('dirty1', true));
      pool.add('dirty2', createMockSession('dirty2', true));

      const dirty = pool.getDirty();

      expect(dirty.length).toBe(2);
      expect(dirty.map(s => s.getMetadata().name)).toContain('dirty1');
      expect(dirty.map(s => s.getMetadata().name)).toContain('dirty2');
    });
  });

  describe('size and clear', () => {
    it('reports correct size', () => {
      const pool = new SessionPool(10);

      expect(pool.size()).toBe(0);

      pool.add('test1', createMockSession('test1'));
      pool.add('test2', createMockSession('test2'));

      expect(pool.size()).toBe(2);
    });

    it('clear removes all sessions', () => {
      const pool = new SessionPool(10);

      pool.add('test1', createMockSession('test1'));
      pool.add('test2', createMockSession('test2'));

      pool.clear();

      expect(pool.size()).toBe(0);
      expect(pool.get('test1')).toBeUndefined();
    });
  });
});
```

## Success Criteria

1. LRU eviction works correctly
2. Dirty session tracking works
3. All CRUD operations work
4. All tests pass

## Estimated Effort

- Implementation: 45 minutes
- Testing: 45 minutes
