# JOB-019b: Session Manager and Pool

## Context & Motivation

Building on JOB-019a, this job focuses on implementing an efficient session management system for the OmegaLLM Session Server. The Session Manager and Pool components are crucial for maintaining in-memory session state and providing fast access to session data, which is the core performance benefit of this architecture.

## Goal

Implement a robust Session Manager and Pool that:

1. Efficiently maintains session state in memory
2. Implements caching strategies for optimal performance
3. Manages session lifecycle events (creation, loading, unloading)
4. Provides fast access to session data

## Technical Approach

### Session Manager

The Session Manager will be responsible for:

1. Tracking active sessions
2. Handling session creation and initialization
3. Managing session loading and unloading
4. Routing commands to appropriate session instances

### Session Pool

The Session Pool will:

1. Maintain an in-memory cache of session states
2. Implement LRU (Least Recently Used) eviction policy
3. Handle memory limits and optimizations
4. Track session metadata for quick access

### Implementation Details

We'll create the following classes:

1. `SessionManager` - High-level session management
2. `SessionPool` - In-memory session cache with eviction policy
3. `SessionInstance` - Wrapper for individual session state
4. `SessionMetadata` - Lightweight session information

## Files to Create/Modify

1. `session-manager.ts` - Main session management logic
2. `session-pool.ts` - Session caching implementation
3. `session-instance.ts` - Individual session wrapper
4. `session-metadata.ts` - Session metadata structure
5. `session-service.ts` (modify from JOB-019a) - Update with session manager integration

## Code Structure

```typescript
// session-metadata.ts
export interface SessionMetadata {
  id: string;
  name: string;
  created: Date;
  lastAccessed: Date;
  eventCount: number;
  checkpointCount: number;
  size: number; // Estimated memory size
}
```

```typescript
// session-instance.ts
import { ReplState } from '../omega-repl'; // From existing code
import { SessionWriter, SessionReader, JumpController } from '../core/session';
import { SessionMetadata } from './session-metadata';

export class SessionInstance {
  private replState: ReplState;
  private metadata: SessionMetadata;
  private dirty: boolean = false;
  private lastAccessTime: number = Date.now();
  
  constructor(replState: ReplState, metadata: SessionMetadata) {
    this.replState = replState;
    this.metadata = metadata;
  }
  
  async execute(command: string): Promise<{ output: string, shouldExit: boolean }> {
    this.lastAccessTime = Date.now();
    
    // Reuse existing processReplCommand logic from omega-repl.ts
    const { replState: newState, output, shouldExit } = 
      await processReplCommand(command.trim(), this.replState);
      
    this.replState = newState;
    this.dirty = true;
    
    // Update metadata
    this.updateMetadata();
    
    return { output, shouldExit };
  }
  
  getState(): ReplState {
    return this.replState;
  }
  
  getMetadata(): SessionMetadata {
    return this.metadata;
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
  
  private updateMetadata(): void {
    this.metadata.lastAccessed = new Date();
    this.metadata.eventCount = this.replState.trace?.length || 0;
    // Other metadata updates
  }
}
```

```typescript
// session-pool.ts
import { SessionInstance } from './session-instance';
import { SessionMetadata } from './session-metadata';
import { ServerConfig } from './server-config';

export class SessionPool {
  private sessions: Map<string, SessionInstance> = new Map();
  private lruOrder: string[] = [];
  
  constructor(private maxSize: number = ServerConfig.maxCachedSessions) {}
  
  get(sessionName: string): SessionInstance | undefined {
    const session = this.sessions.get(sessionName);
    
    if (session) {
      // Update LRU order
      this.updateLRU(sessionName);
    }
    
    return session;
  }
  
  add(sessionName: string, instance: SessionInstance): void {
    // Check if we need to evict
    if (this.sessions.size >= this.maxSize && !this.sessions.has(sessionName)) {
      this.evictLRU();
    }
    
    // Add to pool
    this.sessions.set(sessionName, instance);
    this.updateLRU(sessionName);
  }
  
  remove(sessionName: string): boolean {
    const removed = this.sessions.delete(sessionName);
    
    if (removed) {
      const index = this.lruOrder.indexOf(sessionName);
      if (index >= 0) {
        this.lruOrder.splice(index, 1);
      }
    }
    
    return removed;
  }
  
  getAll(): SessionInstance[] {
    return Array.from(this.sessions.values());
  }
  
  getAllMetadata(): SessionMetadata[] {
    return Array.from(this.sessions.values()).map(s => s.getMetadata());
  }
  
  getDirty(): SessionInstance[] {
    return Array.from(this.sessions.values()).filter(s => s.isDirty());
  }
  
  clear(): void {
    this.sessions.clear();
    this.lruOrder = [];
  }
  
  size(): number {
    return this.sessions.size;
  }
  
  private updateLRU(sessionName: string): void {
    // Remove from current position
    const index = this.lruOrder.indexOf(sessionName);
    if (index >= 0) {
      this.lruOrder.splice(index, 1);
    }
    
    // Add to front (most recently used)
    this.lruOrder.unshift(sessionName);
  }
  
  private evictLRU(): void {
    if (this.lruOrder.length === 0) return;
    
    // Get least recently used session
    const lruSession = this.lruOrder.pop();
    if (lruSession) {
      const instance = this.sessions.get(lruSession);
      
      // If dirty, we should save before evicting (will be implemented in JOB-019c)
      if (instance?.isDirty()) {
        // Placeholder for save operation
        console.log(`Session ${lruSession} is dirty and should be saved before eviction`);
      }
      
      this.sessions.delete(lruSession);
    }
  }
}
```

```typescript
// session-manager.ts
import { SessionPool } from './session-pool';
import { SessionInstance } from './session-instance';
import { SessionMetadata } from './session-metadata';
import { ServerConfig } from './server-config';
import * as fs from 'fs';
import * as path from 'path';
import { initReplState } from '../omega-repl'; // From existing code

export class SessionManager {
  private pool: SessionPool;
  private sessionDir: string;
  
  constructor() {
    this.pool = new SessionPool(ServerConfig.maxCachedSessions);
    this.sessionDir = ServerConfig.sessionDir;
    
    // Ensure session directory exists
    this.ensureSessionDir();
  }
  
  async getSession(name: string): Promise<SessionInstance> {
    // Check if session is already in pool
    let instance = this.pool.get(name);
    if (instance) {
      return instance;
    }
    
    // Load from disk or create new
    instance = await this.loadSession(name);
    
    // Add to pool
    this.pool.add(name, instance);
    
    return instance;
  }
  
  async executeCommand(sessionName: string, command: string): Promise<{ output: string, shouldExit: boolean }> {
    const session = await this.getSession(sessionName);
    return session.execute(command);
  }
  
  listSessions(): SessionMetadata[] {
    // List sessions from pool and disk
    const poolSessions = this.pool.getAllMetadata();
    const diskSessions = this.listDiskSessions();
    
    // Merge and deduplicate
    const allSessions = [...poolSessions];
    
    for (const diskSession of diskSessions) {
      if (!allSessions.some(s => s.name === diskSession.name)) {
        allSessions.push(diskSession);
      }
    }
    
    return allSessions;
  }
  
  async saveDirtySessions(): Promise<void> {
    const dirtySessions = this.pool.getDirty();
    
    for (const session of dirtySessions) {
      await this.saveSession(session);
    }
  }
  
  private async loadSession(name: string): Promise<SessionInstance> {
    // Check if session exists on disk
    const sessionPath = this.getSessionPath(name);
    
    if (fs.existsSync(sessionPath)) {
      // Reuse existing loadSession from omega-repl.ts
      // This is a placeholder - the actual implementation will use SessionReader
      const replState = await loadSession(name); // From existing code
      
      // Create metadata
      const metadata: SessionMetadata = {
        id: name,
        name: name,
        created: new Date(), // Should get from file
        lastAccessed: new Date(),
        eventCount: replState?.trace?.length || 0,
        checkpointCount: 0, // To be populated
        size: 0, // To be calculated
      };
      
      return new SessionInstance(replState, metadata);
    } else {
      // Create new session
      const replState = await initReplState(); // From existing code
      
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
  }
  
  private async saveSession(session: SessionInstance): Promise<void> {
    // Placeholder for saving session to disk
    // Will be implemented in JOB-019c
    console.log(`Saving session ${session.getMetadata().name}`);
    session.markClean();
  }
  
  private listDiskSessions(): SessionMetadata[] {
    const sessionsPath = path.join(this.sessionDir, "sessions");
    
    if (!fs.existsSync(sessionsPath)) {
      return [];
    }
    
    const files = fs.readdirSync(sessionsPath)
      .filter(f => f.endsWith('.jsonl'))
      .map(f => f.replace('.jsonl', ''));
    
    return files.map(name => {
      // Basic metadata for disk-based sessions
      return {
        id: name,
        name: name,
        created: new Date(), // Should get from file
        lastAccessed: new Date(), // Should get from file
        eventCount: 0, // To be populated
        checkpointCount: 0, // To be populated
        size: 0, // To be calculated
      };
    });
  }
  
  private ensureSessionDir(): void {
    const sessionsPath = path.join(this.sessionDir, "sessions");
    fs.mkdirSync(sessionsPath, { recursive: true });
  }
  
  private getSessionPath(name: string): string {
    return path.join(this.sessionDir, "sessions", `${name}.jsonl`);
  }
}
```

Update `session-service.ts` from JOB-019a:

```typescript
// session-service.ts
import { SessionManager } from './session-manager';

export class SessionService {
  private sessionManager: SessionManager;
  
  constructor() {
    this.sessionManager = new SessionManager();
  }
  
  async executeCommand(sessionName: string, command: string) {
    return this.sessionManager.executeCommand(sessionName, command);
  }
  
  async listSessions() {
    return this.sessionManager.listSessions();
  }
  
  async getSession(name: string) {
    try {
      const session = await this.sessionManager.getSession(name);
      return session.getMetadata();
    } catch (error) {
      console.error(`Error getting session ${name}:`, error);
      return null;
    }
  }
}
```

## Testing Strategy

1. Unit tests for SessionManager and SessionPool
2. Caching behavior tests (LRU eviction)
3. Memory usage monitoring
4. Performance benchmarks for repeated access

## Success Criteria

1. Sessions are efficiently cached in memory
2. LRU eviction works correctly
3. Session metadata is accurately tracked
4. Performance improvement is measurable

## Dependencies

1. JOB-019a (Session Server Core)
2. Existing session management code from omega-repl.ts

## Next Steps

After completing this job, the next steps will be:

1. JOB-019c: Optimized Persistence
2. JOB-019d: REPL Client Adaptation
