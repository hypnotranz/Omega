# 10: Persistence and Images

## Overview

"Image-based development" means saving the entire runtime state to disk and restoring it later. This is how:
- Common Lisp works (SBCL images)
- Smalltalk works (Squeak images)
- LambdaLLM enables time-travel

---

## What Gets Persisted

| Component | Persisted | Notes |
|-----------|-----------|-------|
| Namespaces | ✅ Yes | All definitions |
| Closures | ✅ Yes | Body AST + env snapshot |
| Continuations | ✅ Yes | Full call stack |
| Facts/Assertions | ✅ Yes | Monotonic knowledge base |
| World state | ⚠️ Partial | Fingerprints, not files |
| FFI state | ❌ No | Reconnect on restore |
| Active connections | ❌ No | Reconnect on restore |

---

## Image Format

### Structure

```typescript
interface Image {
  // Metadata
  version: string;                    // LambdaLLM version
  created: number;                    // Timestamp
  description?: string;               // User description

  // Namespaces
  namespaces: SerializedNamespace[];

  // Global state
  currentNamespace: string;

  // Execution state (optional)
  continuation?: SerializedContinuation;
  environment?: SerializedEnvironment;

  // World state
  worldFingerprints: Record<string, string>;

  // Facts
  facts: string[];
  factEvidence: Record<string, any>;

  // Metrics
  metrics?: Metrics;
}
```

### Serialization

Images are stored as JSON or MessagePack:

```typescript
// Save image
async function saveImage(runtime: Runtime, path: string): Promise<void> {
  const image: Image = {
    version: VERSION,
    created: Date.now(),
    namespaces: runtime.namespaces.map(serializeNamespace),
    currentNamespace: runtime.currentNamespace.name,
    continuation: runtime.continuation ? serializeCont(runtime.continuation) : undefined,
    environment: serializeEnv(runtime.environment),
    worldFingerprints: runtime.world.getFingerprints(),
    facts: [...runtime.facts],
    factEvidence: Object.fromEntries(runtime.factEvidence),
  };

  const data = JSON.stringify(image);
  await fs.writeFile(path, data, 'utf-8');
}

// Load image
async function loadImage(path: string): Promise<Runtime> {
  const data = await fs.readFile(path, 'utf-8');
  const image: Image = JSON.parse(data);

  // Validate version
  if (!isCompatible(image.version, VERSION)) {
    throw new IncompatibleImageError(image.version, VERSION);
  }

  // Restore namespaces
  const namespaces = image.namespaces.map(deserializeNamespace);

  // Restore state
  const runtime = new Runtime();
  for (const ns of namespaces) {
    runtime.registerNamespace(ns);
  }

  if (image.continuation) {
    runtime.continuation = deserializeCont(image.continuation);
  }

  // Restore facts
  for (const fact of image.facts) {
    runtime.assertFact(fact, image.factEvidence[fact]);
  }

  return runtime;
}
```

---

## Snapshot Types

### 1. Full Image

Complete runtime state. Used for:
- Saving work
- Distributing prebuilt environments
- Deployment

```bash
lambdallm image:save myproject.image
lambdallm image:load myproject.image
```

### 2. Context Snapshot

Single execution context. Used for:
- Time-travel debugging
- Forking execution
- Checkpointing

```typescript
interface ContextSnapshot {
  id: string;
  traceId: string;
  parentId?: string;
  step: number;
  continuation: SerializedContinuation;
  environment: SerializedEnvironment;
  worldSnapshot: Record<string, string>;  // path → content
  timestamp: number;
}
```

### 3. Incremental Snapshot

Delta from previous snapshot. Used for:
- Efficient storage of many checkpoints
- Streaming execution traces

```typescript
interface IncrementalSnapshot {
  baseId: string;                         // Previous snapshot
  delta: {
    newBindings: Record<string, SerializedValue>;
    removedBindings: string[];
    newFacts: string[];
    worldChanges: WorldOp[];
    framesPushed: Frame[];
    framesPopped: number;
  };
}
```

---

## Storage Backends

### 1. File System

```typescript
class FileImageStore implements ImageStore {
  constructor(private baseDir: string) {}

  async save(id: string, image: Image): Promise<void> {
    const path = join(this.baseDir, `${id}.image`);
    await fs.writeFile(path, JSON.stringify(image));
  }

  async load(id: string): Promise<Image> {
    const path = join(this.baseDir, `${id}.image`);
    const data = await fs.readFile(path, 'utf-8');
    return JSON.parse(data);
  }
}
```

### 2. SQLite

```typescript
class SQLiteImageStore implements ImageStore {
  constructor(private db: Database) {
    this.db.exec(`
      CREATE TABLE IF NOT EXISTS images (
        id TEXT PRIMARY KEY,
        created INTEGER,
        data BLOB
      );
      CREATE TABLE IF NOT EXISTS snapshots (
        id TEXT PRIMARY KEY,
        trace_id TEXT,
        parent_id TEXT,
        step INTEGER,
        data BLOB,
        FOREIGN KEY (trace_id) REFERENCES traces(id)
      );
    `);
  }
}
```

### 3. Browser (IndexedDB)

```typescript
class IndexedDBImageStore implements ImageStore {
  private db: IDBDatabase;

  async save(id: string, image: Image): Promise<void> {
    const tx = this.db.transaction('images', 'readwrite');
    const store = tx.objectStore('images');
    await store.put({ id, data: image });
  }
}
```

---

## Versioning and Compatibility

### Version Format

```
<major>.<minor>.<patch>
```

### Compatibility Rules

| Image Version | Runtime Version | Compatible? |
|---------------|-----------------|-------------|
| 1.0.0 | 1.0.x | ✅ Yes |
| 1.0.0 | 1.1.x | ✅ Yes (minor) |
| 1.0.0 | 2.0.0 | ❌ No (major) |

### Migration

```typescript
interface ImageMigration {
  fromVersion: string;
  toVersion: string;
  migrate(image: Image): Image;
}

const migrations: ImageMigration[] = [
  {
    fromVersion: '1.0.0',
    toVersion: '1.1.0',
    migrate(image) {
      // Add new field with default
      return { ...image, newField: defaultValue };
    }
  }
];
```

---

## CLI Commands

```bash
# Save current state
lambdallm image:save project.image

# Load saved state
lambdallm image:load project.image

# List saved images
lambdallm image:list

# Show image info
lambdallm image:info project.image

# Export image as JSON
lambdallm image:export project.image > project.json

# Import from JSON
lambdallm image:import project.json project.image
```

---

## REPL Integration

```lisp
;; Save image
(save-image "checkpoint.image")

;; Load image (replaces current state!)
(load-image "checkpoint.image")

;; Save with description
(save-image "before-refactor.image" :description "Working state before refactor")

;; List available images
(list-images)
;; → (("checkpoint.image" :created 1705234567 :size 12345)
;;    ("before-refactor.image" :created 1705234000 :size 15000))
```

---

## Use Cases

### 1. Checkpoint During Development

```lisp
;; Working on something risky
(save-image "checkpoint.image")

;; Try something
(dangerous-refactor!)

;; Oops, broken
(load-image "checkpoint.image")  ; Restore
```

### 2. Share Environment

```bash
# Developer A saves environment with all dependencies loaded
lambdallm image:save dev-env.image

# Developer B loads it instantly (no install/compile)
lambdallm image:load dev-env.image
```

### 3. Time-Travel Debugging

```lisp
;; Automatic checkpointing during execution
(with-checkpoints
  (run-agent task))

;; Later, explore execution history
(list-checkpoints)
;; → ((checkpoint-1 :step 0)
;;    (checkpoint-2 :step 10)
;;    (checkpoint-3 :step 20))

(restore-checkpoint "checkpoint-2")
;; Now at step 10, can inspect/modify/continue
```

### 4. Deployment

```bash
# Build production image with all code preloaded
lambdallm build --output app.image

# Deploy just the image (fast startup)
lambdallm image:run app.image
```
