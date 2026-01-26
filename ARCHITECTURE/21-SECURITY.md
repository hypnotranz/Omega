# ⚠️ COMPLETE REDESIGN REQUIRED

> ## Primitives Defined
>
> **NONE** - This spec defines NO Lisp primitives.
> It's pure TypeScript infrastructure:
> - `Capability` interface
> - `CapabilityManager` class
> - `StepCounter` class
> - `MemoryLimiter` class
> - `SandboxConfig` type
>
> ## Current Implementation
>
> **TypeScript wrapper classes** passed as parameters:
> ```typescript
> // StepCounter passed to evalExpr (line 318):
> function evalExpr(expr, env, cont, ffi, counter: StepCounter): Value {
>   counter.step();  // ← Manual call, can be forgotten
> }
>
> // EnforcedFFI wraps base FFI externally:
> ffi.call('world.read', [path], { capability: readCap });
> ```
>
> ## CEKS Redesign Instructions
>
> ### 1. Add to State type (in step.ts):
> ```typescript
> type State = {
>   // ...existing fields...
>   sec: SecurityContext;  // Already exists, need to ENFORCE
> };
>
> interface SecurityContext {
>   capabilities: Map<string, Capability>;
>   stepCount: number;
>   maxSteps: number;
>   memoryUsed: number;
>   maxMemory: number;
> }
> ```
>
> ### 2. Check at TOP of step() function:
> ```typescript
> function step(s: State): StepOutcome {
>   // MANDATORY: Check before ANY evaluation
>   s.sec.stepCount++;
>   if (s.sec.stepCount > s.sec.maxSteps) {
>     return { tag: 'StepLimitExceeded', steps: s.sec.stepCount };
>   }
>   // ...rest of step
> }
> ```
>
> ### 3. Check capabilities on effect dispatch:
> ```typescript
> case 'Effect': {
>   const requiredCap = effectToCapability(s.control.effect);
>   if (!s.sec.capabilities.has(requiredCap)) {
>     return { tag: 'CapabilityDenied', required: requiredCap };
>   }
>   // ...dispatch effect
> }
> ```
>
> ### 4. NO Lisp primitives needed - this is kernel infrastructure

---

# 21: Security (Sandboxing & Capabilities)

## Threat Model

```
┌─────────────────────────────────────────────────────────────────┐
│                        Threat Model                             │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  Untrusted Code Sources:                                        │
│  ├── User input (REPL)                                         │
│  ├── Loaded files (.lisp)                                      │
│  ├── Network (remote evaluation)                               │
│  └── LLM output (generated code)                               │
│                                                                 │
│  Protected Resources:                                           │
│  ├── File system (read/write)                                  │
│  ├── Network (HTTP, sockets)                                   │
│  ├── Process (exec, spawn)                                     │
│  ├── Memory (DoS via allocation)                               │
│  └── CPU (DoS via infinite loops)                              │
│                                                                 │
│  Attack Vectors:                                                │
│  ├── Arbitrary code execution                                  │
│  ├── Resource exhaustion                                       │
│  ├── Data exfiltration                                         │
│  ├── Privilege escalation                                      │
│  └── Supply chain (malicious modules)                          │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

---

## Capability-Based Security

Instead of checking permissions at every call, we use a capability model:

```typescript
// security/capabilities.ts

// A capability is an unforgeable token granting specific access
interface Capability {
  id: string;
  type: CapabilityType;
  permissions: Permission[];
  constraints?: Constraints;
  revoked?: boolean;
}

type CapabilityType =
  | 'file-read'
  | 'file-write'
  | 'network'
  | 'process'
  | 'llm'
  | 'time';

interface Permission {
  action: string;
  resource: string;  // Glob pattern
}

interface Constraints {
  maxSize?: number;      // Max file size
  rateLimit?: number;    // Calls per minute
  timeout?: number;      // Max execution time
  allowedHosts?: string[];
}
```

### Capability Creation

```typescript
// Only the runtime can create capabilities
class CapabilityManager {
  private capabilities: Map<string, Capability> = new Map();

  // Create a new capability (restricted to trusted code)
  create(type: CapabilityType, permissions: Permission[]): Capability {
    const cap: Capability = {
      id: crypto.randomUUID(),
      type,
      permissions,
      revoked: false,
    };
    this.capabilities.set(cap.id, cap);
    return cap;
  }

  // Revoke a capability
  revoke(id: string): void {
    const cap = this.capabilities.get(id);
    if (cap) {
      cap.revoked = true;
    }
  }

  // Check if capability grants permission
  check(cap: Capability, action: string, resource: string): boolean {
    if (cap.revoked) return false;

    return cap.permissions.some(p =>
      matchAction(p.action, action) &&
      matchGlob(p.resource, resource)
    );
  }
}
```

### Using Capabilities

```typescript
// FFI operations require capabilities

// Without capability - throws SecurityError
ffi.call('world.read', ['/etc/passwd']);  // Error!

// With capability
const readCap = capManager.create('file-read', [
  { action: 'read', resource: '/workspace/**' }
]);

ffi.call('world.read', ['/workspace/config.json'], { capability: readCap });  // OK
ffi.call('world.read', ['/etc/passwd'], { capability: readCap });  // Error! Not in /workspace
```

---

## Sandbox Modes

### Full Sandbox (Default for untrusted code)

```typescript
const sandbox: SandboxConfig = {
  mode: 'full',

  // No file system access
  fileSystem: {
    enabled: false,
  },

  // No network access
  network: {
    enabled: false,
  },

  // No process spawning
  process: {
    enabled: false,
  },

  // Limited memory
  memory: {
    maxHeap: 50 * 1024 * 1024,  // 50MB
  },

  // Limited CPU
  cpu: {
    maxSteps: 1_000_000,
    timeout: 30_000,  // 30 seconds
  },
};
```

### Restricted Sandbox (Semi-trusted code)

```typescript
const restricted: SandboxConfig = {
  mode: 'restricted',

  // Read-only file access to specific paths
  fileSystem: {
    enabled: true,
    readOnly: true,
    allowedPaths: ['/workspace/**', '!**/.env'],
  },

  // Limited network
  network: {
    enabled: true,
    allowedHosts: ['api.openai.com', 'api.anthropic.com'],
    protocols: ['https'],
  },

  // No process spawning
  process: {
    enabled: false,
  },

  memory: {
    maxHeap: 200 * 1024 * 1024,  // 200MB
  },

  cpu: {
    maxSteps: 10_000_000,
    timeout: 120_000,  // 2 minutes
  },
};
```

### Unrestricted (Trusted code only)

```typescript
const unrestricted: SandboxConfig = {
  mode: 'unrestricted',
  // Full access - use with caution
};
```

---

## Resource Limits

### Memory Limits

```typescript
class MemoryLimiter {
  private allocated: number = 0;
  private maxHeap: number;

  constructor(maxHeap: number) {
    this.maxHeap = maxHeap;
  }

  allocate(size: number): void {
    if (this.allocated + size > this.maxHeap) {
      throw new ResourceExhaustedError(
        `Memory limit exceeded: ${this.allocated + size} > ${this.maxHeap}`
      );
    }
    this.allocated += size;
  }

  free(size: number): void {
    this.allocated = Math.max(0, this.allocated - size);
  }

  get usage(): number {
    return this.allocated;
  }
}

// Integration with evaluator
function allocateList(elements: number, limiter: MemoryLimiter): void {
  const estimatedSize = elements * 64;  // ~64 bytes per element
  limiter.allocate(estimatedSize);
}
```

### Step Limits

```typescript
class StepCounter {
  private steps: number = 0;
  private maxSteps: number;

  constructor(maxSteps: number) {
    this.maxSteps = maxSteps;
  }

  step(): void {
    this.steps++;
    if (this.steps > this.maxSteps) {
      throw new ResourceExhaustedError(
        `Step limit exceeded: ${this.steps} > ${this.maxSteps}`
      );
    }
  }

  get count(): number {
    return this.steps;
  }
}

// In evaluator
function evalExpr(expr, env, cont, ffi, counter: StepCounter): Value {
  counter.step();  // Count every eval
  // ... rest of evaluation
}
```

### Timeout Enforcement

```typescript
async function evalWithTimeout(
  code: string,
  runtime: Runtime,
  timeout: number
): Promise<Value> {
  const controller = new AbortController();

  const timeoutId = setTimeout(() => {
    controller.abort();
  }, timeout);

  try {
    return await runtime.eval(code, { signal: controller.signal });
  } finally {
    clearTimeout(timeoutId);
  }
}
```

---

## Input Validation

### Code Validation

```typescript
// Validate code before evaluation
function validateCode(code: string): ValidationResult {
  const errors: string[] = [];

  // Check for obvious issues
  if (code.length > MAX_CODE_LENGTH) {
    errors.push(`Code too long: ${code.length} > ${MAX_CODE_LENGTH}`);
  }

  // Parse to catch syntax errors early
  try {
    readAll(code);
  } catch (e) {
    errors.push(`Syntax error: ${e.message}`);
  }

  // Check for suspicious patterns
  const suspicious = detectSuspiciousPatterns(code);
  if (suspicious.length > 0) {
    errors.push(...suspicious.map(s => `Suspicious pattern: ${s}`));
  }

  return {
    valid: errors.length === 0,
    errors,
  };
}

function detectSuspiciousPatterns(code: string): string[] {
  const patterns: Array<[RegExp, string]> = [
    [/\(eval\s+\(read/, 'Dynamic code execution'],
    [/world\.write.*\/etc/, 'Attempt to write system files'],
    [/process\.exec/, 'Process execution'],
  ];

  return patterns
    .filter(([regex]) => regex.test(code))
    .map(([, desc]) => desc);
}
```

### FFI Argument Validation

```typescript
// Validate FFI arguments
function validateFFICall(
  name: string,
  args: Value[],
  schema: FFISchema
): void {
  const spec = schema.get(name);
  if (!spec) {
    throw new SecurityError(`Unknown FFI function: ${name}`);
  }

  // Check argument count
  if (args.length < spec.minArgs || args.length > spec.maxArgs) {
    throw new SecurityError(
      `${name}: expected ${spec.minArgs}-${spec.maxArgs} args, got ${args.length}`
    );
  }

  // Check argument types
  for (let i = 0; i < args.length; i++) {
    const expected = spec.argTypes[i];
    const actual = typeOf(args[i]);
    if (!expected.includes(actual)) {
      throw new SecurityError(
        `${name}: arg ${i} expected ${expected.join('|')}, got ${actual}`
      );
    }
  }

  // Check for dangerous values
  for (const [i, arg] of args.entries()) {
    if (typeof arg === 'string' && containsPathTraversal(arg)) {
      throw new SecurityError(
        `${name}: arg ${i} contains path traversal`
      );
    }
  }
}

function containsPathTraversal(path: string): boolean {
  return path.includes('..') || path.includes('//');
}
```

---

## LLM Output Safety

LLM-generated code requires extra scrutiny:

```typescript
interface LLMSafetyConfig {
  // Always sandbox LLM output
  forceSandbox: true;

  // Require human approval for certain operations
  requireApproval: string[];

  // Block certain patterns entirely
  blockedPatterns: RegExp[];

  // Maximum complexity
  maxASTDepth: number;
  maxFunctions: number;
}

async function evalLLMOutput(
  code: string,
  config: LLMSafetyConfig
): Promise<Value> {
  // Validate first
  const validation = validateCode(code);
  if (!validation.valid) {
    throw new SecurityError(`LLM output validation failed: ${validation.errors.join(', ')}`);
  }

  // Check for blocked patterns
  for (const pattern of config.blockedPatterns) {
    if (pattern.test(code)) {
      throw new SecurityError(`LLM output contains blocked pattern`);
    }
  }

  // Check complexity
  const ast = readAll(code);
  const depth = maxDepth(ast);
  if (depth > config.maxASTDepth) {
    throw new SecurityError(`LLM output too complex: depth ${depth} > ${config.maxASTDepth}`);
  }

  // Check for operations requiring approval
  const requiringApproval = findOperations(ast, config.requireApproval);
  if (requiringApproval.length > 0) {
    const approved = await requestApproval(requiringApproval);
    if (!approved) {
      throw new SecurityError('User denied approval for LLM operations');
    }
  }

  // Evaluate in sandbox
  return evalInSandbox(code, {
    mode: 'full',
    // Very restrictive for LLM output
    cpu: { maxSteps: 100_000, timeout: 5000 },
    memory: { maxHeap: 10 * 1024 * 1024 },
  });
}
```

---

## Module Security

### Signed Modules

```typescript
interface SignedModule {
  name: string;
  version: string;
  code: string;
  signature: string;
  publicKey: string;
}

async function loadSignedModule(module: SignedModule): Promise<void> {
  // Verify signature
  const valid = await verifySignature(
    module.code,
    module.signature,
    module.publicKey
  );

  if (!valid) {
    throw new SecurityError(`Module signature verification failed: ${module.name}`);
  }

  // Check if public key is trusted
  if (!isTrustedKey(module.publicKey)) {
    throw new SecurityError(`Module signed with untrusted key: ${module.name}`);
  }

  // Load module
  await loadModule(module.code);
}
```

### Permission Declarations

```lisp
;; Modules declare required permissions
(ns my-module
  (:require [lambdallm.core])
  (:permissions
    {:file-read ["/workspace/**"]
     :network ["api.example.com"]})
```

```typescript
// Runtime checks permissions before loading
async function loadModuleWithPermissions(
  module: Module,
  grantedPermissions: Permission[]
): Promise<void> {
  const required = module.requiredPermissions;

  for (const req of required) {
    if (!hasPermission(grantedPermissions, req)) {
      throw new SecurityError(
        `Module ${module.name} requires permission: ${formatPermission(req)}`
      );
    }
  }

  // Create capability with only granted permissions
  const cap = capManager.create(module.name, grantedPermissions);

  // Load with restricted capability
  await loadModuleWithCapability(module, cap);
}
```

---

## Audit Logging

```typescript
interface AuditEvent {
  timestamp: string;
  sessionId: string;
  operation: string;
  args?: unknown[];
  result?: 'success' | 'denied' | 'error';
  reason?: string;
  capability?: string;
}

class AuditLogger {
  private events: AuditEvent[] = [];

  log(event: Omit<AuditEvent, 'timestamp'>): void {
    this.events.push({
      ...event,
      timestamp: new Date().toISOString(),
    });
  }

  // Security-relevant operations to log
  logFFICall(name: string, args: Value[], cap?: Capability): void {
    this.log({
      operation: 'ffi-call',
      args: [name, ...args],
      capability: cap?.id,
      sessionId: getCurrentSession().id,
    });
  }

  logPermissionDenied(operation: string, reason: string): void {
    this.log({
      operation,
      result: 'denied',
      reason,
      sessionId: getCurrentSession().id,
    });
  }

  // Export for analysis
  export(): AuditEvent[] {
    return [...this.events];
  }
}
```

---

## Security Checklist

```
┌─────────────────────────────────────────────────────────────────┐
│                     Security Checklist                          │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  □ All FFI calls go through capability check                   │
│  □ Untrusted code runs in sandbox                              │
│  □ Resource limits enforced (memory, CPU, time)                │
│  □ Input validation on all external data                       │
│  □ LLM output treated as untrusted                             │
│  □ Modules require explicit permission grants                  │
│  □ Security events are logged                                  │
│  □ No eval of arbitrary strings without validation             │
│  □ Path traversal prevented                                    │
│  □ Capability tokens are unforgeable                           │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

---

## Configuration Example

```typescript
// Secure default configuration
const secureConfig: RuntimeConfig = {
  sandbox: {
    mode: 'restricted',
    fileSystem: {
      enabled: true,
      readOnly: true,
      allowedPaths: ['./workspace/**'],
    },
    network: {
      enabled: true,
      allowedHosts: ['api.openai.com'],
    },
  },

  limits: {
    maxSteps: 1_000_000,
    maxMemory: 100 * 1024 * 1024,
    timeout: 60_000,
  },

  llm: {
    forceSandbox: true,
    requireApproval: ['world.write', 'world.delete'],
    blockedPatterns: [/eval\s*\(\s*read/],
  },

  audit: {
    enabled: true,
    logLevel: 'security',
  },
};
```
