# 19: Versioning (Compatibility & Evolution)

## Version Scheme

LambdaLLM follows Semantic Versioning with language-specific considerations:

```
MAJOR.MINOR.PATCH[-PRERELEASE]

Examples:
  0.1.0       - Initial development
  1.0.0       - First stable release
  1.1.0       - New features, backward compatible
  1.1.1       - Bug fixes only
  2.0.0       - Breaking changes
  2.0.0-alpha - Pre-release
```

---

## Compatibility Guarantees

### Language Level

| Component | Stability | Notes |
|-----------|-----------|-------|
| Core syntax | Frozen after 1.0 | S-expressions never change |
| Special forms | Stable | `if`, `lambda`, `define`, etc. |
| Primitives | Stable | Core functions stable |
| Stdlib | May evolve | New functions added, deprecation cycle |
| Macros | Implementation detail | Can change between versions |

### Runtime Level

| Component | Stability | Notes |
|-----------|-----------|-------|
| Protocol | Versioned | Major changes bump protocol version |
| Snapshots | Forward compatible | Old snapshots loadable |
| Images | Major-version compatible | v1.x images work with v1.y |
| FFI | Stable | External integrations stable |

---

## Protocol Versioning

```typescript
interface ProtocolVersion {
  major: number;  // Breaking changes
  minor: number;  // New operations
  patch: number;  // Bug fixes
}

// Handshake on connection
interface HelloRequest {
  op: 'hello';
  id: string;
  'client-version': ProtocolVersion;
  'min-server-version'?: ProtocolVersion;
}

interface HelloResponse {
  id: string;
  status: ['done'];
  'server-version': ProtocolVersion;
  'protocol-version': ProtocolVersion;
  ops: string[];  // Available operations
}
```

### Protocol Negotiation

```typescript
function negotiateProtocol(
  clientVersion: ProtocolVersion,
  serverVersion: ProtocolVersion
): ProtocolVersion {
  // Major must match
  if (clientVersion.major !== serverVersion.major) {
    throw new ProtocolMismatchError(
      `Client v${clientVersion.major}.x incompatible with server v${serverVersion.major}.x`
    );
  }

  // Use minimum of minor versions
  return {
    major: clientVersion.major,
    minor: Math.min(clientVersion.minor, serverVersion.minor),
    patch: 0,  // Patch doesn't affect negotiation
  };
}
```

---

## Snapshot Versioning

Snapshots include version metadata for compatibility:

```typescript
interface Snapshot {
  version: {
    runtime: string;      // "1.2.3"
    snapshot: number;     // Schema version
    protocol: string;     // "1.1.0"
  };

  // Actual snapshot data
  continuation: SerializedContinuation;
  environment: SerializedEnvironment;
  world: SerializedWorld;

  // Migration hints
  migrations?: Migration[];
}

interface Migration {
  from: number;           // From schema version
  to: number;             // To schema version
  description: string;
  applied?: boolean;
}
```

### Loading Old Snapshots

```typescript
async function loadSnapshot(data: unknown): Promise<Snapshot> {
  const snapshot = JSON.parse(data as string);

  // Check version
  const snapshotVersion = snapshot.version.snapshot;
  const currentVersion = CURRENT_SNAPSHOT_VERSION;

  if (snapshotVersion > currentVersion) {
    throw new Error(
      `Snapshot from future version ${snapshotVersion}, current is ${currentVersion}`
    );
  }

  // Apply migrations
  if (snapshotVersion < currentVersion) {
    for (let v = snapshotVersion; v < currentVersion; v++) {
      const migration = MIGRATIONS[v];
      if (!migration) {
        throw new Error(`Missing migration for version ${v} → ${v + 1}`);
      }
      snapshot = await migration.apply(snapshot);
    }
  }

  return snapshot;
}

// Example migration
const MIGRATIONS: Record<number, MigrationFn> = {
  1: {
    description: 'Add source locations to frames',
    apply: async (snapshot) => {
      for (const frame of snapshot.continuation.frames) {
        frame.sourceLoc = frame.sourceLoc ?? { file: 'unknown', line: 0 };
      }
      return { ...snapshot, version: { ...snapshot.version, snapshot: 2 } };
    }
  },
  2: {
    description: 'Convert symbols to string representation',
    apply: async (snapshot) => {
      // Migration logic...
      return { ...snapshot, version: { ...snapshot.version, snapshot: 3 } };
    }
  },
};
```

---

## Image Versioning

Images (saved runtime state) have major-version compatibility:

```typescript
interface Image {
  magic: 'LLMI';          // LambdaLLM Image
  version: {
    major: number;        // Must match runtime major version
    minor: number;        // Runtime can be >= image
    format: number;       // Image format version
  };

  // Compressed snapshot data
  data: Uint8Array;

  // Checksums
  checksum: string;

  // Metadata
  metadata: {
    created: string;      // ISO timestamp
    runtime: string;      // Runtime version that created it
    platform: string;     // 'browser' | 'node' | 'bun'
    description?: string;
  };
}
```

### Loading Images

```typescript
async function loadImage(path: string): Promise<Runtime> {
  const data = await readFile(path);
  const image = parseImage(data);

  // Version check
  const runtimeMajor = getRuntimeVersion().major;
  if (image.version.major !== runtimeMajor) {
    throw new ImageVersionError(
      `Image v${image.version.major}.x cannot load in runtime v${runtimeMajor}.x`
    );
  }

  // Decompress and deserialize
  const snapshot = decompress(image.data);

  // Apply any needed migrations
  const migratedSnapshot = await migrateSnapshot(snapshot);

  // Create runtime from snapshot
  return Runtime.fromSnapshot(migratedSnapshot);
}
```

---

## API Versioning

### Embedding API

```typescript
// Major version changes may break embedding code
// Use explicit version imports for stability

// v1.x
import { Runtime } from 'lambdallm/v1';

// Future v2.x
import { Runtime } from 'lambdallm/v2';

// Or use latest (may break on major bump)
import { Runtime } from 'lambdallm';
```

### Deprecation Process

```typescript
// Deprecation annotation
function deprecate(
  fn: Function,
  message: string,
  since: string,
  removal: string
): Function {
  return function(...args: unknown[]) {
    if (process.env.NODE_ENV !== 'production') {
      console.warn(
        `DEPRECATED: ${fn.name} - ${message}\n` +
        `  Deprecated since: v${since}\n` +
        `  Will be removed in: v${removal}`
      );
    }
    return fn.apply(this, args);
  };
}

// Example usage
export const oldFunction = deprecate(
  function oldFunction() { ... },
  'Use newFunction instead',
  '1.5.0',
  '2.0.0'
);
```

---

## Breaking Change Policy

### What Constitutes a Breaking Change

| Change | Breaking? | Notes |
|--------|-----------|-------|
| Remove function | Yes | Must deprecate first |
| Change function signature | Yes | Add new function instead |
| Change special form behavior | Yes | Never do after 1.0 |
| Add new function | No | Backward compatible |
| Add optional parameter | No | Existing code works |
| Change error message text | No | Don't parse error messages |
| Change internal implementation | No | Public API stable |

### Breaking Change Process

```
1. Announce in CHANGELOG
2. Deprecate old API with warning
3. Keep deprecated API for 1 minor version
4. Remove in next major version

Timeline example:
  v1.5.0 - Deprecate oldFn, add newFn
  v1.6.0 - oldFn still works but warns
  v2.0.0 - Remove oldFn
```

---

## Feature Flags

New features can be gated behind flags:

```typescript
interface RuntimeConfig {
  features?: {
    // Experimental features (may change)
    experimentalMacros?: boolean;
    experimentalConcurrency?: boolean;

    // Compatibility modes
    strictMode?: boolean;  // More warnings
    legacyMode?: boolean;  // Old behavior
  };
}

// In code
if (config.features?.experimentalMacros) {
  // New macro system
} else {
  // Old macro system
}
```

```lisp
;; In Lisp
(when (feature? :experimental-macros)
  (use-new-macro-system))
```

---

## Changelog Format

```markdown
# Changelog

## [1.2.0] - 2025-01-15

### Added
- New `partition` function in list module
- Protocol operation `fork` for branching execution

### Changed
- Improved error messages for type mismatches

### Deprecated
- `split-list` - use `partition` instead

### Fixed
- Stack overflow on deeply nested expressions
- Memory leak in long-running sessions

### Security
- Updated dependency X to address CVE-YYYY-NNNN

## [1.1.0] - 2025-01-01
...
```

---

## Version Detection

```typescript
// Runtime version
import { version } from 'lambdallm';
console.log(version);  // "1.2.3"

// Detailed version info
import { versionInfo } from 'lambdallm';
console.log(versionInfo);
// {
//   version: "1.2.3",
//   git: "abc123",
//   built: "2025-01-15T12:00:00Z",
//   node: "20.0.0",
//   bun: "1.0.0"
// }
```

```lisp
;; In Lisp
(version)           ;; → "1.2.3"
(version :detail)   ;; → {:version "1.2.3" :git "abc123" ...}
```

---

## Compatibility Matrix

```
┌─────────────────────────────────────────────────────────────────┐
│                    Compatibility Matrix                         │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  Runtime ↓ / Artifact →    Snapshot    Image    Protocol       │
│  ─────────────────────────────────────────────────────────────  │
│  1.0.x                     1.0+        1.0      1.0             │
│  1.1.x                     1.0+        1.0-1.1  1.0-1.1         │
│  1.2.x                     1.0+        1.0-1.2  1.0-1.2         │
│  2.0.x                     2.0+        2.0      2.0             │
│                                                                 │
│  Legend:                                                        │
│  - Snapshots: Forward compatible within major                   │
│  - Images: Compatible with same major version                   │
│  - Protocol: Major determines compatibility                     │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```
