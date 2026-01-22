# JOB-019c: Optimized Persistence

## Context & Motivation

Building on JOB-019a and JOB-019b, this job focuses on implementing an optimized persistence layer for the Session Server. The current performance bottleneck in the REPL is largely due to synchronous file operations for every command execution. This job will implement background saving, incremental updates, and efficient file handling to maintain data integrity without compromising performance.

## Goal

Implement an optimized persistence system that:

1. Handles session persistence asynchronously in the background
2. Uses efficient write strategies to minimize disk operations
3. Maintains compatibility with existing file formats
4. Ensures data integrity even during server crashes
5. Optimizes for both write and read performance

## Technical Approach

### Persistence Strategy

The persistence layer will implement:

1. **Throttled saving** - Only save after significant changes or time intervals
2. **Background writes** - Non-blocking I/O operations
3. **Incremental updates** - Only write changed data when possible
4. **Write batching** - Batch multiple writes together
5. **File format compatibility** - Maintain compatibility with existing files

### Implementation Details

We'll create the following components:

1. `PersistenceManager` - Coordinates persistence operations
2. `SessionPersister` - Handles session-specific persistence
3. `IncrementalWriter` - Optimizes file writes

## Files to Create/Modify

1. `persistence-manager.ts` - Main persistence coordination
2. `session-persister.ts` - Session-specific persistence logic
3. `incremental-writer.ts` - Optimized file writing
4. `session-manager.ts` (modify) - Integrate with persistence

## Code Structure

```typescript
// persistence-manager.ts
import { SessionInstance } from './session-instance';
import { SessionPool } from './session-pool';
import { SessionPersister } from './session-persister';
import { ServerConfig } from './server-config';
import * as fs from 'fs';
import * as path from 'path';

export class PersistenceManager {
  private persisters: Map<string, SessionPersister> = new Map();
  private saveInterval: NodeJS.Timeout | null = null;
  private saving: boolean = false;
  
  constructor(
    private sessionPool: SessionPool,
    private sessionDir: string,
    private saveIntervalMs: number = ServerConfig.saveInterval
  ) {
    this.ensureDirectories();
    this.startPeriodicSave();
  }
  
  async saveDirtySessions(): Promise<void> {
    if (this.saving) return;
    
    try {
      this.saving = true;
      const dirtySessions = this.sessionPool.getDirty();
      
      const savePromises = dirtySessions.map(session => {
        return this.saveSession(session);
      });
      
      await Promise.all(savePromises);
    } catch (error) {
      console.error('Error saving dirty sessions:', error);
    } finally {
      this.saving = false;
    }
  }
  
  async saveSession(session: SessionInstance): Promise<void> {
    const sessionName = session.getMetadata().name;
    let persister = this.persisters.get(sessionName);
    
    if (!persister) {
      persister = new SessionPersister(sessionName, this.sessionDir);
      this.persisters.set(sessionName, persister);
    }
    
    await persister.save(session);
    session.markClean();
  }
  
  async loadSession(name: string): Promise<SessionInstance | null> {
    let persister = this.persisters.get(name);
    
    if (!persister) {
      persister = new SessionPersister(name, this.sessionDir);
      this.persisters.set(name, persister);
    }
    
    return persister.load();
  }
  
  async forceSave(): Promise<void> {
    return this.saveDirtySessions();
  }
  
  shutdown(): void {
    if (this.saveInterval) {
      clearInterval(this.saveInterval);
      this.saveInterval = null;
    }
    
    // Force sync save of any dirty sessions before shutdown
    this.forceSaveSync();
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
    const dirtySessions = this.sessionPool.getDirty();
    
    for (const session of dirtySessions) {
      try {
        const sessionName = session.getMetadata().name;
        const persister = this.persisters.get(sessionName) || 
          new SessionPersister(sessionName, this.sessionDir);
          
        persister.saveSync(session);
        session.markClean();
      } catch (error) {
        console.error(`Error force-saving session ${session.getMetadata().name}:`, error);
      }
    }
  }
  
  private ensureDirectories(): void {
    const sessionsPath = path.join(this.sessionDir, 'sessions');
    const receiptsPath = path.join(this.sessionDir, 'receipts');
    
    fs.mkdirSync(sessionsPath, { recursive: true });
    fs.mkdirSync(receiptsPath, { recursive: true });
  }
}
```

```typescript
// session-persister.ts
import { SessionInstance } from './session-instance';
import { IncrementalWriter } from './incremental-writer';
import { deserializeState, serializeState } from '../core/session/serializer';
import { SessionWriter, SessionReader } from '../core/session';
import * as fs from 'fs';
import * as path from 'path';

export class SessionPersister {
  private writer: IncrementalWriter;
  private eventFile: string;
  private indexFile: string;
  
  constructor(
    private sessionName: string,
    private sessionDir: string
  ) {
    this.eventFile = path.join(this.sessionDir, 'sessions', `${this.sessionName}.jsonl`);
    this.indexFile = path.join(this.sessionDir, 'sessions', `${this.sessionName}.index.json`);
    this.writer = new IncrementalWriter(this.eventFile, this.indexFile);
  }
  
  async save(session: SessionInstance): Promise<void> {
    const replState = session.getState();
    
    // Get changes since last save
    const changes = session.getChangesSinceLastSave();
    
    // Optimize save based on changes
    if (changes.length === 0) {
      // No changes to save
      return;
    }
    
    // Use incremental writer to efficiently update files
    await this.writer.appendEvents(changes, replState);
    
    // Update session metadata after save
    session.updateSavePoint();
  }
  
  async load(): Promise<SessionInstance | null> {
    if (!fs.existsSync(this.eventFile) || !fs.existsSync(this.indexFile)) {
      return null;
    }
    
    try {
      // Reuse existing SessionReader to load session
      const reader = new SessionReader(
        this.eventFile,
        this.indexFile,
        new Map(), // nativeRegistry - will need to be passed in
        new Map()  // solverRegistry - will need to be passed in
      );
      
      await reader.loadAll();
      
      // Create session instance from loaded data
      // This is a placeholder - actual implementation will create a proper SessionInstance
      return null; // To be implemented
    } catch (error) {
      console.error(`Error loading session ${this.sessionName}:`, error);
      return null;
    }
  }
  
  // Synchronous save for shutdown scenarios
  saveSync(session: SessionInstance): void {
    const replState = session.getState();
    
    // Synchronous save implementation
    // This will be slower but ensures data is saved before shutdown
    
    // Save session data
    const sessionData = {
      defs: replState.defs,
      debugMode: replState.debugMode,
      debugCode: replState.debugCode,
      stepCount: replState.stepCount,
      traceLength: replState.trace.length,
      breakpoints: replState.breakpoints,
      nextBreakpointId: replState.nextBreakpointId,
      recordingEnabled: replState.recordingEnabled,
    };
    
    // Save to file synchronously
    fs.writeFileSync(
      path.join(this.sessionDir, 'sessions', `${this.sessionName}.json`),
      JSON.stringify(sessionData, null, 2)
    );
  }
}
```

```typescript
// incremental-writer.ts
import * as fs from 'fs';
import * as path from 'path';
import { promisify } from 'util';
import { ReplState } from '../omega-repl';
import { serializeState } from '../core/session/serializer';

const appendFile = promisify(fs.appendFile);
const writeFile = promisify(fs.writeFile);
const readFile = promisify(fs.readFile);

export class IncrementalWriter {
  private eventByteOffset = 0;
  private cachedIndex: any = null;
  
  constructor(
    private eventFile: string,
    private indexFile: string
  ) {
    // Initialize byte offset if file exists
    if (fs.existsSync(this.eventFile)) {
      const stats = fs.statSync(this.eventFile);
      this.eventByteOffset = stats.size;
    }
  }
  
  async appendEvents(events: any[], replState: ReplState): Promise<void> {
    if (events.length === 0) return;
    
    // Ensure the file exists
    if (!fs.existsSync(this.eventFile)) {
      // Create file with session header
      const sessionHeader = {
        type: 'session',
        version: 1,
        id: path.basename(this.eventFile, '.jsonl'),
        created: new Date().toISOString(),
      };
      
      await writeFile(this.eventFile, `${JSON.stringify(sessionHeader)}\n`);
      this.eventByteOffset = Buffer.byteLength(`${JSON.stringify(sessionHeader)}\n`, 'utf8');
    }
    
    // Load index if not cached
    if (!this.cachedIndex && fs.existsSync(this.indexFile)) {
      const indexData = await readFile(this.indexFile, 'utf8');
      this.cachedIndex = JSON.parse(indexData);
    } else if (!this.cachedIndex) {
      // Initialize new index
      this.cachedIndex = {
        sessionId: path.basename(this.eventFile, '.jsonl'),
        eventCount: 0,
        checkpoints: [],
        states: {},
        receipts: {},
      };
    }
    
    // Track new checkpoints and receipts
    const newCheckpoints = [];
    const newReceipts = {};
    
    // Append each event
    for (const event of events) {
      const eventLine = `${JSON.stringify(event)}\n`;
      await appendFile(this.eventFile, eventLine);
      
      // Update offset tracking
      const eventBytes = Buffer.byteLength(eventLine, 'utf8');
      
      // Track checkpoints
      if (event.type === 'checkpoint') {
        newCheckpoints.push({
          seq: event.seq,
          reason: event.reason || 'auto',
          stateId: event.stateId,
          byteOffset: this.eventByteOffset,
        });
        
        // Serialize state for checkpoint
        if (!this.cachedIndex.states[event.stateId]) {
          // Serialize state and add to index
          // This is a placeholder - actual implementation will use serializeState
          const serializedState = {}; // Will use serializeState(replState.debugState)
          this.cachedIndex.states[event.stateId] = serializedState;
        }
      }
      
      // Track receipts
      if (event.type === 'llm_resp' && event.receiptKey) {
        // Add receipt to index
        // This is a placeholder - actual implementation will copy receipt data
        newReceipts[event.receiptKey] = {}; // Will use actual receipt data
      }
      
      // Update byte offset
      this.eventByteOffset += eventBytes;
    }
    
    // Update index
    if (newCheckpoints.length > 0) {
      this.cachedIndex.checkpoints = [
        ...this.cachedIndex.checkpoints,
        ...newCheckpoints
      ];
    }
    
    // Add new receipts
    Object.assign(this.cachedIndex.receipts, newReceipts);
    
    // Update event count
    const lastEvent = events[events.length - 1];
    if (typeof lastEvent.seq === 'number') {
      this.cachedIndex.eventCount = Math.max(
        this.cachedIndex.eventCount,
        lastEvent.seq + 1
      );
    }
    
    // Write updated index
    await writeFile(
      this.indexFile,
      JSON.stringify(this.cachedIndex, null, 2)
    );
  }
}
```

Update `session-instance.ts` from JOB-019b:

```typescript
// session-instance.ts - Partial update to add change tracking
import { ReplState } from '../omega-repl';
import { SessionMetadata } from './session-metadata';

export class SessionInstance {
  private replState: ReplState;
  private metadata: SessionMetadata;
  private dirty: boolean = false;
  private lastAccessTime: number = Date.now();
  private lastSavePoint: number = 0; // Track last saved event seq
  private changesSinceLastSave: any[] = []; // Track events since last save
  
  // ... existing methods ...
  
  // Add methods for change tracking
  getChangesSinceLastSave(): any[] {
    return this.changesSinceLastSave;
  }
  
  updateSavePoint(): void {
    this.lastSavePoint = this.getLastEventSeq();
    this.changesSinceLastSave = [];
  }
  
  private getLastEventSeq(): number {
    // Get the sequence number of the last event
    return this.replState.trace?.length || 0;
  }
  
  // Method to track changes when executing commands
  private trackChange(event: any): void {
    this.changesSinceLastSave.push(event);
  }
}
```

## Integration with Session Manager

Update `session-manager.ts` from JOB-019b:

```typescript
// session-manager.ts - Partial update to integrate persistence
import { PersistenceManager } from './persistence-manager';

export class SessionManager {
  private pool: SessionPool;
  private sessionDir: string;
  private persistenceManager: PersistenceManager;
  
  constructor() {
    this.pool = new SessionPool(ServerConfig.maxCachedSessions);
    this.sessionDir = ServerConfig.sessionDir;
    
    // Initialize persistence manager
    this.persistenceManager = new PersistenceManager(this.pool, this.sessionDir);
  }
  
  // ... existing methods ...
  
  // Update saveSession method
  private async saveSession(session: SessionInstance): Promise<void> {
    return this.persistenceManager.saveSession(session);
  }
  
  // Update loadSession method
  private async loadSession(name: string): Promise<SessionInstance> {
    const instance = await this.persistenceManager.loadSession(name);
    
    if (instance) {
      return instance;
    }
    
    // Fall back to creating a new session if load fails
    const replState = await initReplState();
    
    const metadata: SessionMetadata = {
      id: name,
      name: name,
      created: new Date(),
      lastAccessed: new Date(),
      eventCount: 0,
      checkpointCount: 0,
      size: 0,
    };
    
    return new SessionInstance(replState, metadata);
  }
  
  // Add method for shutdown
  async shutdown(): Promise<void> {
    await this.persistenceManager.forceSave();
    this.persistenceManager.shutdown();
  }
}
```

## Testing Strategy

1. Benchmark write performance vs current implementation
2. Test data integrity during simulated crashes
3. Verify file format compatibility
4. Measure latency impact during saves

## Success Criteria

1. Command execution remains responsive during saves
2. No data loss during normal operation or crashes
3. Maintains compatibility with existing file formats
4. Background saving works efficiently
5. Files are optimized for minimal disk operations

## Dependencies

1. JOB-019a (Session Server Core)
2. JOB-019b (Session Manager and Pool)
3. Existing serialization code from core/session

## Next Steps

After completing this job, the next steps will be:

1. JOB-019d: REPL Client Adaptation
2. JOB-019e: Advanced Features
