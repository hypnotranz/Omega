# JOB-019a4-incremental-writer: Optimized File Writing

## Context

This job implements the `IncrementalWriter` class that efficiently appends events to session files without rewriting the entire file.

## Goal

Create an `IncrementalWriter` class that:
1. Appends events to JSONL files efficiently
2. Maintains index file with checkpoints and state references
3. Tracks byte offsets for fast seeking
4. Implements `IIncrementalWriter` interface

## Dependencies

- `019-types` (for interfaces)

## Blockers

- None

## Files to Create

1. `server/incremental-writer.ts` - IncrementalWriter class

## Implementation

```typescript
// server/incremental-writer.ts
import * as fs from 'fs';
import * as path from 'path';
import { promisify } from 'util';
import type { IIncrementalWriter, SessionEvent } from './types';

const appendFile = promisify(fs.appendFile);
const writeFile = promisify(fs.writeFile);
const readFile = promisify(fs.readFile);

export interface SessionIndex {
  sessionId: string;
  eventCount: number;
  checkpoints: CheckpointEntry[];
  states: Record<string, unknown>;
  receipts: Record<string, unknown>;
}

export interface CheckpointEntry {
  seq: number;
  reason: string;
  stateId: string;
  byteOffset: number;
}

export class IncrementalWriter implements IIncrementalWriter {
  private eventByteOffset = 0;
  private cachedIndex: SessionIndex | null = null;
  private indexDirty = false;

  constructor(
    private eventFile: string,
    private indexFile: string
  ) {
    this.initializeOffsets();
  }

  private initializeOffsets(): void {
    if (fs.existsSync(this.eventFile)) {
      const stats = fs.statSync(this.eventFile);
      this.eventByteOffset = stats.size;
    }
  }

  async appendEvents(events: SessionEvent[], _state: unknown): Promise<void> {
    if (events.length === 0) return;

    // Ensure event file exists with header
    await this.ensureEventFile();

    // Load index if not cached
    await this.loadIndex();

    // Append each event
    for (const event of events) {
      await this.appendEvent(event);
    }

    // Write updated index
    await this.flushIndex();
  }

  private async ensureEventFile(): Promise<void> {
    if (fs.existsSync(this.eventFile)) return;

    const sessionId = path.basename(this.eventFile, '.jsonl');
    const header: SessionEvent = {
      type: 'session',
      version: 1,
      id: sessionId,
      created: new Date().toISOString(),
    };

    const line = `${JSON.stringify(header)}\n`;
    await writeFile(this.eventFile, line);
    this.eventByteOffset = Buffer.byteLength(line, 'utf8');
  }

  private async loadIndex(): Promise<void> {
    if (this.cachedIndex) return;

    if (fs.existsSync(this.indexFile)) {
      const data = await readFile(this.indexFile, 'utf8');
      this.cachedIndex = JSON.parse(data);
    } else {
      const sessionId = path.basename(this.eventFile, '.jsonl');
      this.cachedIndex = {
        sessionId,
        eventCount: 0,
        checkpoints: [],
        states: {},
        receipts: {},
      };
      this.indexDirty = true;
    }
  }

  private async appendEvent(event: SessionEvent): Promise<void> {
    const line = `${JSON.stringify(event)}\n`;
    await appendFile(this.eventFile, line);

    const eventBytes = Buffer.byteLength(line, 'utf8');

    // Track checkpoints
    if (event.type === 'checkpoint') {
      this.cachedIndex!.checkpoints.push({
        seq: event.seq,
        reason: event.reason,
        stateId: event.stateId,
        byteOffset: this.eventByteOffset,
      });
      this.indexDirty = true;
    }

    // Track receipts
    if (event.type === 'llm_resp' && 'receiptKey' in event) {
      // Receipt data would be stored separately
      this.indexDirty = true;
    }

    // Update counters
    if ('seq' in event && typeof event.seq === 'number') {
      this.cachedIndex!.eventCount = Math.max(
        this.cachedIndex!.eventCount,
        event.seq + 1
      );
      this.indexDirty = true;
    }

    this.eventByteOffset += eventBytes;
  }

  private async flushIndex(): Promise<void> {
    if (!this.indexDirty || !this.cachedIndex) return;

    await writeFile(
      this.indexFile,
      JSON.stringify(this.cachedIndex, null, 2)
    );
    this.indexDirty = false;
  }

  // For testing: get current state
  getByteOffset(): number {
    return this.eventByteOffset;
  }

  getCachedIndex(): SessionIndex | null {
    return this.cachedIndex;
  }
}
```

## Testing

```typescript
// server/incremental-writer.spec.ts
import * as fs from 'fs';
import * as path from 'path';
import * as os from 'os';
import { IncrementalWriter } from './incremental-writer';
import type { SessionEvent } from './types';

describe('IncrementalWriter', () => {
  let tempDir: string;
  let eventFile: string;
  let indexFile: string;

  beforeEach(() => {
    tempDir = fs.mkdtempSync(path.join(os.tmpdir(), 'writer-test-'));
    eventFile = path.join(tempDir, 'test.jsonl');
    indexFile = path.join(tempDir, 'test.index.json');
  });

  afterEach(() => {
    fs.rmSync(tempDir, { recursive: true, force: true });
  });

  describe('appendEvents', () => {
    it('creates event file with header if not exists', async () => {
      const writer = new IncrementalWriter(eventFile, indexFile);

      const events: SessionEvent[] = [
        { type: 'input', seq: 0, code: '(+ 1 2)', timestamp: new Date().toISOString() },
      ];

      await writer.appendEvents(events, {});

      expect(fs.existsSync(eventFile)).toBe(true);

      const content = fs.readFileSync(eventFile, 'utf8');
      const lines = content.trim().split('\n');

      expect(lines.length).toBe(2); // header + event
      expect(JSON.parse(lines[0]).type).toBe('session');
      expect(JSON.parse(lines[1]).type).toBe('input');
    });

    it('appends to existing file', async () => {
      const writer = new IncrementalWriter(eventFile, indexFile);

      await writer.appendEvents([
        { type: 'input', seq: 0, code: 'first', timestamp: new Date().toISOString() },
      ], {});

      await writer.appendEvents([
        { type: 'input', seq: 1, code: 'second', timestamp: new Date().toISOString() },
      ], {});

      const content = fs.readFileSync(eventFile, 'utf8');
      const lines = content.trim().split('\n');

      expect(lines.length).toBe(3); // header + 2 events
    });

    it('creates and updates index file', async () => {
      const writer = new IncrementalWriter(eventFile, indexFile);

      await writer.appendEvents([
        { type: 'input', seq: 0, code: 'test', timestamp: new Date().toISOString() },
        { type: 'output', seq: 1, value: 'result', timestamp: new Date().toISOString() },
      ], {});

      expect(fs.existsSync(indexFile)).toBe(true);

      const index = JSON.parse(fs.readFileSync(indexFile, 'utf8'));
      expect(index.eventCount).toBe(2);
    });

    it('tracks checkpoints in index', async () => {
      const writer = new IncrementalWriter(eventFile, indexFile);

      await writer.appendEvents([
        { type: 'checkpoint', seq: 0, reason: 'manual', stateId: 'state-1' },
      ], {});

      const index = JSON.parse(fs.readFileSync(indexFile, 'utf8'));
      expect(index.checkpoints.length).toBe(1);
      expect(index.checkpoints[0].stateId).toBe('state-1');
    });

    it('handles empty events array', async () => {
      const writer = new IncrementalWriter(eventFile, indexFile);

      await writer.appendEvents([], {});

      expect(fs.existsSync(eventFile)).toBe(false);
    });
  });

  describe('byte offset tracking', () => {
    it('tracks byte offset correctly', async () => {
      const writer = new IncrementalWriter(eventFile, indexFile);

      await writer.appendEvents([
        { type: 'input', seq: 0, code: 'test', timestamp: new Date().toISOString() },
      ], {});

      const fileSize = fs.statSync(eventFile).size;
      expect(writer.getByteOffset()).toBe(fileSize);
    });
  });
});
```

## Success Criteria

1. Events append without rewriting file
2. Index file tracks checkpoints and counts
3. Byte offsets are accurate
4. All tests pass

## Estimated Effort

- Implementation: 1 hour
- Testing: 1 hour
