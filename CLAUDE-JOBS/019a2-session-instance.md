# JOB-019a2-session-instance: SessionInstance Wrapper

## Context

This job implements the `SessionInstance` class that wraps a `ReplState` and provides the interface for executing commands and tracking changes.

## Goal

Create a `SessionInstance` class that:
1. Wraps `ReplState` with metadata tracking
2. Executes commands via the existing `processReplCommand` function
3. Tracks dirty state and changes since last save
4. Implements `ISessionInstance` interface

## Dependencies

- `019-types` (for interfaces)

## Blockers

- None (uses existing `processReplCommand` from omega-repl.ts)

## Files to Create

1. `server/session-instance.ts` - SessionInstance class

## Implementation

```typescript
// server/session-instance.ts
import type { ISessionInstance, SessionMetadata, ExecuteResult, SessionEvent } from './types';
import type { ReplState } from '../omega-repl';

// Import the command processor from existing code
// This will be injected to avoid circular dependencies
export type CommandProcessor = (
  command: string,
  state: ReplState
) => Promise<{ replState: ReplState; output: string; shouldExit: boolean }>;

export class SessionInstance implements ISessionInstance {
  private replState: ReplState;
  private metadata: SessionMetadata;
  private dirty: boolean = false;
  private lastAccessTime: number = Date.now();
  private lastSaveSeq: number = 0;
  private pendingEvents: SessionEvent[] = [];
  private commandProcessor: CommandProcessor;

  constructor(
    replState: ReplState,
    metadata: SessionMetadata,
    commandProcessor: CommandProcessor
  ) {
    this.replState = replState;
    this.metadata = metadata;
    this.commandProcessor = commandProcessor;
  }

  async execute(command: string): Promise<ExecuteResult> {
    this.lastAccessTime = Date.now();

    // Record input event
    const inputEvent: SessionEvent = {
      type: 'input',
      seq: this.getNextSeq(),
      code: command,
      timestamp: new Date().toISOString(),
    };
    this.pendingEvents.push(inputEvent);

    // Execute command
    const { replState: newState, output, shouldExit } = 
      await this.commandProcessor(command.trim(), this.replState);

    this.replState = newState;
    this.dirty = true;

    // Record output event
    const outputEvent: SessionEvent = {
      type: 'output',
      seq: this.getNextSeq(),
      value: output,
      timestamp: new Date().toISOString(),
    };
    this.pendingEvents.push(outputEvent);

    // Update metadata
    this.updateMetadata();

    return { output, shouldExit };
  }

  getState(): ReplState {
    return this.replState;
  }

  getMetadata(): SessionMetadata {
    return { ...this.metadata };
  }

  isDirty(): boolean {
    return this.dirty;
  }

  markClean(): void {
    this.dirty = false;
  }

  getLastAccessTime(): number {
    return this.lastAccessTime;
  }

  getChangesSinceLastSave(): SessionEvent[] {
    return [...this.pendingEvents];
  }

  updateSavePoint(): void {
    this.lastSaveSeq = this.metadata.eventCount;
    this.pendingEvents = [];
    this.dirty = false;
  }

  private getNextSeq(): number {
    return this.metadata.eventCount++;
  }

  private updateMetadata(): void {
    this.metadata.lastAccessed = new Date();
  }
}

// Factory function for creating instances
export function createSessionInstance(
  replState: ReplState,
  name: string,
  commandProcessor: CommandProcessor,
  existingMetadata?: Partial<SessionMetadata>
): SessionInstance {
  const metadata: SessionMetadata = {
    id: existingMetadata?.id || name,
    name: existingMetadata?.name || name,
    created: existingMetadata?.created || new Date(),
    lastAccessed: new Date(),
    eventCount: existingMetadata?.eventCount || 0,
    checkpointCount: existingMetadata?.checkpointCount || 0,
    size: existingMetadata?.size || 0,
  };

  return new SessionInstance(replState, metadata, commandProcessor);
}
```

## Testing

```typescript
// server/session-instance.spec.ts
import { SessionInstance, createSessionInstance, CommandProcessor } from './session-instance';
import type { ReplState } from '../omega-repl';

describe('SessionInstance', () => {
  // Mock command processor
  const mockProcessor: CommandProcessor = jest.fn(async (cmd, state) => ({
    replState: state,
    output: `Executed: ${cmd}`,
    shouldExit: false,
  }));

  // Mock ReplState
  const mockReplState: ReplState = {
    defs: [],
    debugMode: false,
    stepCount: 0,
    trace: [],
    breakpoints: [],
    nextBreakpointId: 1,
    recordingEnabled: true,
    history: [],
    maxHistory: 100,
    snapshots: new Map(),
    sessionDir: '.omega-session',
  } as any;

  beforeEach(() => {
    jest.clearAllMocks();
  });

  describe('execute', () => {
    it('executes command and returns result', async () => {
      const instance = createSessionInstance(mockReplState, 'test', mockProcessor);
      
      const result = await instance.execute('(+ 1 2)');
      
      expect(result.output).toBe('Executed: (+ 1 2)');
      expect(result.shouldExit).toBe(false);
      expect(mockProcessor).toHaveBeenCalledWith('(+ 1 2)', mockReplState);
    });

    it('marks instance as dirty after execute', async () => {
      const instance = createSessionInstance(mockReplState, 'test', mockProcessor);
      
      expect(instance.isDirty()).toBe(false);
      await instance.execute('(+ 1 2)');
      expect(instance.isDirty()).toBe(true);
    });

    it('tracks events since last save', async () => {
      const instance = createSessionInstance(mockReplState, 'test', mockProcessor);
      
      await instance.execute('(+ 1 2)');
      
      const events = instance.getChangesSinceLastSave();
      expect(events.length).toBe(2); // input + output
      expect(events[0].type).toBe('input');
      expect(events[1].type).toBe('output');
    });

    it('updates last access time', async () => {
      const instance = createSessionInstance(mockReplState, 'test', mockProcessor);
      const before = instance.getLastAccessTime();
      
      await new Promise(r => setTimeout(r, 10));
      await instance.execute('(+ 1 2)');
      
      expect(instance.getLastAccessTime()).toBeGreaterThan(before);
    });
  });

  describe('getMetadata', () => {
    it('returns copy of metadata', () => {
      const instance = createSessionInstance(mockReplState, 'test', mockProcessor);
      
      const meta1 = instance.getMetadata();
      const meta2 = instance.getMetadata();
      
      expect(meta1).not.toBe(meta2);
      expect(meta1).toEqual(meta2);
    });
  });

  describe('markClean', () => {
    it('clears dirty flag', async () => {
      const instance = createSessionInstance(mockReplState, 'test', mockProcessor);
      
      await instance.execute('(+ 1 2)');
      expect(instance.isDirty()).toBe(true);
      
      instance.markClean();
      expect(instance.isDirty()).toBe(false);
    });
  });

  describe('updateSavePoint', () => {
    it('clears pending events', async () => {
      const instance = createSessionInstance(mockReplState, 'test', mockProcessor);
      
      await instance.execute('(+ 1 2)');
      expect(instance.getChangesSinceLastSave().length).toBe(2);
      
      instance.updateSavePoint();
      expect(instance.getChangesSinceLastSave().length).toBe(0);
    });
  });
});
```

## Success Criteria

1. SessionInstance correctly wraps ReplState
2. Commands execute through injected processor
3. Dirty tracking works correctly
4. Event tracking for persistence works
5. All tests pass

## Estimated Effort

- Implementation: 1 hour
- Testing: 1 hour
