# JOB-019e2-session-monitor: Metrics and Statistics

## Context

This job implements the `SessionMonitor` class that tracks performance metrics and statistics for sessions.

## Goal

Create a `SessionMonitor` class that:
1. Tracks command execution times
2. Monitors memory usage
3. Records session access patterns
4. Provides metrics for performance analysis

## Dependencies

- `019-types` (for SessionMetrics interface)

## Blockers

- None (can run in parallel with other Layer 5 jobs)

## Files to Create

1. `server/session-monitor.ts` - Metrics tracking

## Implementation

```typescript
// server/session-monitor.ts
import type { SessionMetrics } from './types';

export class SessionMonitor {
  private metrics: Map<string, SessionMetrics> = new Map();
  private commandTimings: Map<string, number[]> = new Map();
  private maxTimingsHistory = 100;

  recordCommandExecution(sessionName: string, executionTimeMs: number): void {
    this.ensureMetrics(sessionName);

    const sessionMetrics = this.metrics.get(sessionName)!;
    const timings = this.commandTimings.get(sessionName)!;

    // Update command count
    sessionMetrics.commandCount++;
    sessionMetrics.lastAccessed = new Date();

    // Track timing
    timings.push(executionTimeMs);
    if (timings.length > this.maxTimingsHistory) {
      timings.shift();
    }

    // Recompute average
    sessionMetrics.averageExecutionTime = 
      timings.reduce((sum, t) => sum + t, 0) / timings.length;
  }

  updateMemoryUsage(sessionName: string, memoryUsageBytes: number): void {
    this.ensureMetrics(sessionName);

    const sessionMetrics = this.metrics.get(sessionName)!;
    sessionMetrics.peakMemoryUsage = Math.max(
      sessionMetrics.peakMemoryUsage,
      memoryUsageBytes
    );
  }

  updateSessionSize(sessionName: string, sizeBytes: number): void {
    this.ensureMetrics(sessionName);

    const sessionMetrics = this.metrics.get(sessionName)!;
    sessionMetrics.sessionSize = sizeBytes;
  }

  getMetrics(sessionName: string): SessionMetrics | null {
    return this.metrics.get(sessionName) || null;
  }

  getAllMetrics(): Map<string, SessionMetrics> {
    return new Map(this.metrics);
  }

  getAggregateMetrics(): {
    totalSessions: number;
    totalCommands: number;
    averageExecutionTime: number;
    totalMemoryUsage: number;
  } {
    let totalCommands = 0;
    let totalTime = 0;
    let totalMemory = 0;

    for (const metrics of this.metrics.values()) {
      totalCommands += metrics.commandCount;
      totalTime += metrics.averageExecutionTime * metrics.commandCount;
      totalMemory += metrics.peakMemoryUsage;
    }

    return {
      totalSessions: this.metrics.size,
      totalCommands,
      averageExecutionTime: totalCommands > 0 ? totalTime / totalCommands : 0,
      totalMemoryUsage: totalMemory,
    };
  }

  clearMetrics(sessionName: string): void {
    this.metrics.delete(sessionName);
    this.commandTimings.delete(sessionName);
  }

  clearAllMetrics(): void {
    this.metrics.clear();
    this.commandTimings.clear();
  }

  private ensureMetrics(sessionName: string): void {
    if (!this.metrics.has(sessionName)) {
      this.metrics.set(sessionName, {
        commandCount: 0,
        averageExecutionTime: 0,
        peakMemoryUsage: 0,
        lastAccessed: new Date(),
        sessionSize: 0,
      });
    }

    if (!this.commandTimings.has(sessionName)) {
      this.commandTimings.set(sessionName, []);
    }
  }
}
```

## Testing

```typescript
// server/session-monitor.spec.ts
import { SessionMonitor } from './session-monitor';

describe('SessionMonitor', () => {
  let monitor: SessionMonitor;

  beforeEach(() => {
    monitor = new SessionMonitor();
  });

  describe('recordCommandExecution', () => {
    it('tracks command count', () => {
      monitor.recordCommandExecution('session1', 100);
      monitor.recordCommandExecution('session1', 200);

      const metrics = monitor.getMetrics('session1');
      expect(metrics?.commandCount).toBe(2);
    });

    it('computes average execution time', () => {
      monitor.recordCommandExecution('session1', 100);
      monitor.recordCommandExecution('session1', 200);
      monitor.recordCommandExecution('session1', 300);

      const metrics = monitor.getMetrics('session1');
      expect(metrics?.averageExecutionTime).toBe(200);
    });

    it('updates last accessed time', () => {
      const before = new Date();
      monitor.recordCommandExecution('session1', 100);
      const after = new Date();

      const metrics = monitor.getMetrics('session1');
      expect(metrics?.lastAccessed.getTime()).toBeGreaterThanOrEqual(before.getTime());
      expect(metrics?.lastAccessed.getTime()).toBeLessThanOrEqual(after.getTime());
    });
  });

  describe('updateMemoryUsage', () => {
    it('tracks peak memory usage', () => {
      monitor.updateMemoryUsage('session1', 1000);
      monitor.updateMemoryUsage('session1', 500);
      monitor.updateMemoryUsage('session1', 2000);
      monitor.updateMemoryUsage('session1', 1500);

      const metrics = monitor.getMetrics('session1');
      expect(metrics?.peakMemoryUsage).toBe(2000);
    });
  });

  describe('updateSessionSize', () => {
    it('tracks session size', () => {
      monitor.updateSessionSize('session1', 5000);

      const metrics = monitor.getMetrics('session1');
      expect(metrics?.sessionSize).toBe(5000);
    });
  });

  describe('getAggregateMetrics', () => {
    it('aggregates across all sessions', () => {
      monitor.recordCommandExecution('session1', 100);
      monitor.recordCommandExecution('session1', 200);
      monitor.recordCommandExecution('session2', 300);
      monitor.updateMemoryUsage('session1', 1000);
      monitor.updateMemoryUsage('session2', 2000);

      const aggregate = monitor.getAggregateMetrics();

      expect(aggregate.totalSessions).toBe(2);
      expect(aggregate.totalCommands).toBe(3);
      expect(aggregate.totalMemoryUsage).toBe(3000);
    });
  });

  describe('clearMetrics', () => {
    it('clears metrics for specific session', () => {
      monitor.recordCommandExecution('session1', 100);
      monitor.recordCommandExecution('session2', 200);

      monitor.clearMetrics('session1');

      expect(monitor.getMetrics('session1')).toBeNull();
      expect(monitor.getMetrics('session2')).not.toBeNull();
    });
  });
});
```

## Success Criteria

1. Command timing tracking works
2. Memory usage tracking works
3. Aggregate metrics are accurate
4. All tests pass

## Estimated Effort

- Implementation: 30 minutes
- Testing: 30 minutes
