# JOB-019e: Advanced Features

## Context & Motivation

Building on the foundational work in JOB-019a through JOB-019d, this job focuses on implementing advanced features that leverage the new server architecture. With the core server functionality, session management, optimized persistence, and client adaptation in place, we can now explore features that wouldn't have been possible or practical with the file-based approach.

## Goal

Implement advanced features that:

1. Enhance the user experience
2. Provide additional functionality
3. Leverage the server architecture for improved performance
4. Enable new workflows

## Technical Approach

### Advanced Features

We'll implement the following advanced features:

1. **Concurrent Session Access**
   - Allow multiple clients to access the same session
   - Support read-only and read-write modes
   - Implement locking mechanism for concurrent modifications

2. **Session Statistics and Monitoring**
   - Track performance metrics
   - Monitor resource usage
   - Provide insights into session operations

3. **Session Snapshots**
   - Create lightweight session snapshots
   - Enable fast restore points
   - Support session versioning

4. **Enhanced Time Travel**
   - Improved checkpoint navigation
   - Visual timeline of session history
   - Checkpoint tagging and annotation

### Implementation Details

We'll create the following components:

1. `ConcurrencyManager` - Handles concurrent access
2. `SessionMonitor` - Tracks statistics and metrics
3. `SnapshotManager` - Manages session snapshots
4. `TimelineNavigator` - Enhanced time travel features

## Files to Create/Modify

1. `concurrency-manager.ts` - Manages concurrent access
2. `session-monitor.ts` - Tracks statistics
3. `snapshot-manager.ts` - Manages snapshots
4. `timeline-navigator.ts` - Enhanced time travel

## Code Structure

```typescript
// concurrency-manager.ts
export enum AccessMode {
  ReadOnly,
  ReadWrite
}

export class ConcurrencyManager {
  private activeSessions: Map<string, Set<string>> = new Map();
  private writeLocks: Map<string, string> = new Map();
  
  constructor() {}
  
  async acquireAccess(sessionName: string, clientId: string, mode: AccessMode): Promise<boolean> {
    // Add client to active sessions
    if (!this.activeSessions.has(sessionName)) {
      this.activeSessions.set(sessionName, new Set());
    }
    this.activeSessions.get(sessionName)!.add(clientId);
    
    // For read-only access, no need to check locks
    if (mode === AccessMode.ReadOnly) {
      return true;
    }
    
    // Check if session is write-locked by another client
    const currentLock = this.writeLocks.get(sessionName);
    if (currentLock && currentLock !== clientId) {
      return false;
    }
    
    // Acquire write lock
    this.writeLocks.set(sessionName, clientId);
    return true;
  }
  
  releaseAccess(sessionName: string, clientId: string): void {
    // Remove client from active sessions
    const clients = this.activeSessions.get(sessionName);
    if (clients) {
      clients.delete(clientId);
      if (clients.size === 0) {
        this.activeSessions.delete(sessionName);
      }
    }
    
    // Release write lock if held by this client
    if (this.writeLocks.get(sessionName) === clientId) {
      this.writeLocks.delete(sessionName);
    }
  }
  
  getActiveClients(sessionName: string): string[] {
    return Array.from(this.activeSessions.get(sessionName) || []);
  }
  
  getWriteLockHolder(sessionName: string): string | null {
    return this.writeLocks.get(sessionName) || null;
  }
  
  isSessionLocked(sessionName: string): boolean {
    return this.writeLocks.has(sessionName);
  }
}
```

```typescript
// session-monitor.ts
export interface SessionMetrics {
  commandCount: number;
  averageExecutionTime: number;
  peakMemoryUsage: number;
  lastAccessed: Date;
  sessionSize: number;
}

export class SessionMonitor {
  private metrics: Map<string, SessionMetrics> = new Map();
  private commandTimings: Map<string, number[]> = new Map();
  
  recordCommandExecution(sessionName: string, executionTimeMs: number): void {
    // Initialize metrics for this session if needed
    if (!this.metrics.has(sessionName)) {
      this.metrics.set(sessionName, {
        commandCount: 0,
        averageExecutionTime: 0,
        peakMemoryUsage: 0,
        lastAccessed: new Date(),
        sessionSize: 0
      });
    }
    
    // Initialize command timings array
    if (!this.commandTimings.has(sessionName)) {
      this.commandTimings.set(sessionName, []);
    }
    
    // Get current metrics and timings
    const sessionMetrics = this.metrics.get(sessionName)!;
    const timings = this.commandTimings.get(sessionName)!;
    
    // Update metrics
    sessionMetrics.commandCount++;
    sessionMetrics.lastAccessed = new Date();
    
    // Add timing and recompute average
    timings.push(executionTimeMs);
    if (timings.length > 100) {
      timings.shift(); // Keep only the last 100 timings
    }
    
    // Compute average
    sessionMetrics.averageExecutionTime = timings.reduce((sum, time) => sum + time, 0) / timings.length;
  }
  
  updateMemoryUsage(sessionName: string, memoryUsageBytes: number): void {
    if (!this.metrics.has(sessionName)) {
      this.recordCommandExecution(sessionName, 0); // Initialize metrics
    }
    
    const sessionMetrics = this.metrics.get(sessionName)!;
    sessionMetrics.peakMemoryUsage = Math.max(sessionMetrics.peakMemoryUsage, memoryUsageBytes);
  }
  
  updateSessionSize(sessionName: string, sizeBytes: number): void {
    if (!this.metrics.has(sessionName)) {
      this.recordCommandExecution(sessionName, 0); // Initialize metrics
    }
    
    const sessionMetrics = this.metrics.get(sessionName)!;
    sessionMetrics.sessionSize = sizeBytes;
  }
  
  getMetrics(sessionName: string): SessionMetrics | null {
    return this.metrics.get(sessionName) || null;
  }
  
  getAllMetrics(): Map<string, SessionMetrics> {
    return new Map(this.metrics);
  }
}
```

```typescript
// snapshot-manager.ts
import * as fs from 'fs';
import * as path from 'path';
import { SessionInstance } from './session-instance';

export interface SnapshotMetadata {
  id: string;
  sessionName: string;
  created: Date;
  description: string;
  tags: string[];
}

export class SnapshotManager {
  private snapshotsDir: string;
  
  constructor(private sessionDir: string) {
    this.snapshotsDir = path.join(sessionDir, 'snapshots');
    fs.mkdirSync(this.snapshotsDir, { recursive: true });
  }
  
  async createSnapshot(
    sessionInstance: SessionInstance, 
    description: string = '',
    tags: string[] = []
  ): Promise<SnapshotMetadata> {
    const sessionName = sessionInstance.getMetadata().name;
    const snapshotId = `${sessionName}-${Date.now()}`;
    
    // Create snapshot metadata
    const metadata: SnapshotMetadata = {
      id: snapshotId,
      sessionName,
      created: new Date(),
      description,
      tags
    };
    
    // Create snapshot directory
    const snapshotDir = path.join(this.snapshotsDir, snapshotId);
    fs.mkdirSync(snapshotDir, { recursive: true });
    
    // Save metadata
    fs.writeFileSync(
      path.join(snapshotDir, 'metadata.json'),
      JSON.stringify(metadata, null, 2)
    );
    
    // Save session state
    // This is a simplified approach - the actual implementation would serialize properly
    const state = sessionInstance.getState();
    fs.writeFileSync(
      path.join(snapshotDir, 'state.json'),
      JSON.stringify(state, null, 2)
    );
    
    return metadata;
  }
  
  async restoreSnapshot(snapshotId: string): Promise<SessionInstance | null> {
    const snapshotDir = path.join(this.snapshotsDir, snapshotId);
    
    if (!fs.existsSync(snapshotDir)) {
      return null;
    }
    
    // Load metadata
    const metadataPath = path.join(snapshotDir, 'metadata.json');
    if (!fs.existsSync(metadataPath)) {
      return null;
    }
    
    const metadata = JSON.parse(fs.readFileSync(metadataPath, 'utf8'));
    
    // Load state
    const statePath = path.join(snapshotDir, 'state.json');
    if (!fs.existsSync(statePath)) {
      return null;
    }
    
    // This is a placeholder - actual implementation would deserialize properly
    const state = JSON.parse(fs.readFileSync(statePath, 'utf8'));
    
    // Create session instance from snapshot
    // This is a placeholder - actual implementation would create a proper instance
    return null; // Would return new SessionInstance(state, metadata)
  }
  
  listSnapshots(sessionName?: string): SnapshotMetadata[] {
    const snapshots: SnapshotMetadata[] = [];
    
    // Read all snapshot directories
    const snapshotDirs = fs.readdirSync(this.snapshotsDir);
    
    for (const dir of snapshotDirs) {
      const metadataPath = path.join(this.snapshotsDir, dir, 'metadata.json');
      
      if (fs.existsSync(metadataPath)) {
        try {
          const metadata = JSON.parse(fs.readFileSync(metadataPath, 'utf8'));
          
          // Filter by session name if provided
          if (!sessionName || metadata.sessionName === sessionName) {
            snapshots.push(metadata);
          }
        } catch (error) {
          console.error(`Error reading snapshot metadata for ${dir}:`, error);
        }
      }
    }
    
    // Sort by creation date (newest first)
    return snapshots.sort((a, b) => 
      new Date(b.created).getTime() - new Date(a.created).getTime()
    );
  }
  
  deleteSnapshot(snapshotId: string): boolean {
    const snapshotDir = path.join(this.snapshotsDir, snapshotId);
    
    if (!fs.existsSync(snapshotDir)) {
      return false;
    }
    
    try {
      // Remove all files in the snapshot directory
      const files = fs.readdirSync(snapshotDir);
      for (const file of files) {
        fs.unlinkSync(path.join(snapshotDir, file));
      }
      
      // Remove the directory
      fs.rmdirSync(snapshotDir);
      return true;
    } catch (error) {
      console.error(`Error deleting snapshot ${snapshotId}:`, error);
      return false;
    }
  }
}
```

```typescript
// timeline-navigator.ts
export interface CheckpointAnnotation {
  checkpointSeq: number;
  label: string;
  description: string;
  tags: string[];
  created: Date;
}

export class TimelineNavigator {
  private annotations: Map<string, Map<number, CheckpointAnnotation>> = new Map();
  
  addAnnotation(
    sessionName: string, 
    checkpointSeq: number, 
    label: string, 
    description: string = '',
    tags: string[] = []
  ): CheckpointAnnotation {
    // Initialize session annotations
    if (!this.annotations.has(sessionName)) {
      this.annotations.set(sessionName, new Map());
    }
    
    // Create annotation
    const annotation: CheckpointAnnotation = {
      checkpointSeq,
      label,
      description,
      tags,
      created: new Date()
    };
    
    // Store annotation
    this.annotations.get(sessionName)!.set(checkpointSeq, annotation);
    
    return annotation;
  }
  
  getAnnotation(sessionName: string, checkpointSeq: number): CheckpointAnnotation | null {
    if (!this.annotations.has(sessionName)) {
      return null;
    }
    
    return this.annotations.get(sessionName)!.get(checkpointSeq) || null;
  }
  
  getSessionAnnotations(sessionName: string): CheckpointAnnotation[] {
    if (!this.annotations.has(sessionName)) {
      return [];
    }
    
    return Array.from(this.annotations.get(sessionName)!.values());
  }
  
  getAnnotationsByTag(sessionName: string, tag: string): CheckpointAnnotation[] {
    const sessionAnnotations = this.getSessionAnnotations(sessionName);
    return sessionAnnotations.filter(a => a.tags.includes(tag));
  }
  
  removeAnnotation(sessionName: string, checkpointSeq: number): boolean {
    if (!this.annotations.has(sessionName)) {
      return false;
    }
    
    return this.annotations.get(sessionName)!.delete(checkpointSeq);
  }
  
  generateTimeline(sessionName: string, checkpoints: any[]): any[] {
    const annotatedCheckpoints = checkpoints.map(cp => {
      const annotation = this.getAnnotation(sessionName, cp.seq);
      
      return {
        ...cp,
        annotation: annotation || null
      };
    });
    
    return annotatedCheckpoints;
  }
}
```

## API Endpoints

Add these new endpoints to the server:

```typescript
// New API endpoints to add to api-routes.ts

// Concurrent Access
router.post('/sessions/:name/access', async (req, res) => {
  const { name } = req.params;
  const { clientId, mode } = req.body;
  
  try {
    const result = await concurrencyManager.acquireAccess(name, clientId, mode);
    res.json({ success: result });
  } catch (error) {
    res.status(500).json({ error: error.message });
  }
});

router.delete('/sessions/:name/access/:clientId', async (req, res) => {
  const { name, clientId } = req.params;
  
  try {
    concurrencyManager.releaseAccess(name, clientId);
    res.json({ success: true });
  } catch (error) {
    res.status(500).json({ error: error.message });
  }
});

// Session Statistics
router.get('/sessions/:name/metrics', async (req, res) => {
  const { name } = req.params;
  
  try {
    const metrics = sessionMonitor.getMetrics(name);
    res.json(metrics || {});
  } catch (error) {
    res.status(500).json({ error: error.message });
  }
});

// Snapshots
router.post('/sessions/:name/snapshots', async (req, res) => {
  const { name } = req.params;
  const { description, tags } = req.body;
  
  try {
    const session = await sessionManager.getSession(name);
    const snapshot = await snapshotManager.createSnapshot(session, description, tags);
    res.json(snapshot);
  } catch (error) {
    res.status(500).json({ error: error.message });
  }
});

router.get('/sessions/:name/snapshots', async (req, res) => {
  const { name } = req.params;
  
  try {
    const snapshots = snapshotManager.listSnapshots(name);
    res.json(snapshots);
  } catch (error) {
    res.status(500).json({ error: error.message });
  }
});

// Timeline
router.post('/sessions/:name/checkpoints/:seq/annotation', async (req, res) => {
  const { name, seq } = req.params;
  const { label, description, tags } = req.body;
  
  try {
    const annotation = timelineNavigator.addAnnotation(
      name,
      parseInt(seq, 10),
      label,
      description,
      tags
    );
    res.json(annotation);
  } catch (error) {
    res.status(500).json({ error: error.message });
  }
});

router.get('/sessions/:name/timeline', async (req, res) => {
  const { name } = req.params;
  
  try {
    const session = await sessionManager.getSession(name);
    const checkpoints = session.getCheckpoints();
    const timeline = timelineNavigator.generateTimeline(name, checkpoints);
    res.json(timeline);
  } catch (error) {
    res.status(500).json({ error: error.message });
  }
});
```

## Client Integration

Add new client commands to support these features:

```typescript
// Additions to omega-repl.ts

// New REPL commands
if (trimmed === ":session snapshot") {
  const description = "Manual snapshot at " + new Date().toLocaleString();
  try {
    await sessionClient.createSnapshot(sessionName, description);
    return {
      replState,
      output: `Created snapshot of session '${sessionName}'`,
      shouldExit: false
    };
  } catch (error) {
    return {
      replState,
      output: `Error creating snapshot: ${error.message}`,
      shouldExit: false
    };
  }
}

if (trimmed.startsWith(":session snapshots")) {
  try {
    const snapshots = await sessionClient.listSnapshots(sessionName);
    const output = formatSnapshots(snapshots);
    return {
      replState,
      output,
      shouldExit: false
    };
  } catch (error) {
    return {
      replState,
      output: `Error listing snapshots: ${error.message}`,
      shouldExit: false
    };
  }
}

if (trimmed.startsWith(":session metrics")) {
  try {
    const metrics = await sessionClient.getSessionMetrics(sessionName);
    const output = formatMetrics(metrics);
    return {
      replState,
      output,
      shouldExit: false
    };
  } catch (error) {
    return {
      replState,
      output: `Error getting metrics: ${error.message}`,
      shouldExit: false
    };
  }
}

if (trimmed.startsWith(":session timeline")) {
  try {
    const timeline = await sessionClient.getTimeline(sessionName);
    const output = formatTimeline(timeline);
    return {
      replState,
      output,
      shouldExit: false
    };
  } catch (error) {
    return {
      replState,
      output: `Error getting timeline: ${error.message}`,
      shouldExit: false
    };
  }
}

// Helper formatting functions
function formatSnapshots(snapshots) {
  if (snapshots.length === 0) {
    return "No snapshots available.";
  }
  
  const lines = ["Snapshots:"];
  
  for (const snapshot of snapshots) {
    const created = new Date(snapshot.created).toLocaleString();
    lines.push(`  [${snapshot.id}] ${snapshot.description || 'No description'} (${created})`);
  }
  
  return lines.join("\n");
}

function formatMetrics(metrics) {
  if (!metrics) {
    return "No metrics available.";
  }
  
  const lines = ["Session Metrics:"];
  lines.push(`  Commands executed: ${metrics.commandCount}`);
  lines.push(`  Average execution time: ${metrics.averageExecutionTime.toFixed(2)}ms`);
  lines.push(`  Peak memory usage: ${formatBytes(metrics.peakMemoryUsage)}`);
  lines.push(`  Session size: ${formatBytes(metrics.sessionSize)}`);
  lines.push(`  Last accessed: ${new Date(metrics.lastAccessed).toLocaleString()}`);
  
  return lines.join("\n");
}

function formatTimeline(timeline) {
  if (timeline.length === 0) {
    return "No checkpoints available.";
  }
  
  const lines = ["Timeline:"];
  
  for (const point of timeline) {
    let line = `  [${point.seq}] ${point.reason || 'Checkpoint'}`;
    
    if (point.annotation) {
      line += ` - ${point.annotation.label}`;
    }
    
    lines.push(line);
  }
  
  return lines.join("\n");
}

function formatBytes(bytes) {
  if (bytes === 0) return '0 Bytes';
  
  const k = 1024;
  const sizes = ['Bytes', 'KB', 'MB', 'GB'];
  const i = Math.floor(Math.log(bytes) / Math.log(k));
  
  return parseFloat((bytes / Math.pow(k, i)).toFixed(2)) + ' ' + sizes[i];
}
```

## Testing Strategy

1. Test concurrent access with multiple clients
2. Verify metrics tracking accuracy
3. Test snapshot creation and restoration
4. Validate timeline functionality

## Success Criteria

1. Multiple clients can access sessions concurrently
2. Session metrics are accurately tracked and reported
3. Snapshots can be created and restored
4. Timeline provides enhanced time travel experience

## Dependencies

1. JOB-019a (Session Server Core)
2. JOB-019b (Session Manager and Pool)
3. JOB-019c (Optimized Persistence)
4. JOB-019d (REPL Client Adaptation)

## Benefits

These advanced features enable new workflows and improve the user experience:

1. Collaborative editing of sessions
2. Performance optimization based on metrics
3. Safer experimentation with snapshot restore points
4. Better understanding of session history through the timeline
