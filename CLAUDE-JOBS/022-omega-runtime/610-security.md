# 610: Security Manager

## Status: COMPLETE ✅
- **Completed**: 2026-01-29
- **Location**: OmegaLLM-3/src/runtime/subsystems/SecurityManager.ts (427 lines)

## Purpose
Implements capability-based security - controlling what operations code can perform based on granted capabilities.

## Dependencies
- 100-types.md ✅
- 110-events.md ✅
- 120-providers.md ✅

## Source References
- ARCHITECTURE/21-SECURITY.md
- Object-Capability Model (OCaps)
- Principle of Least Authority (POLA)

---

## Deliverables

```
src/runtime/subsystems/
├── SecurityManager.ts       # Main security manager
└── security/
    ├── Capability.ts        # Capability types
    ├── CapabilityGrant.ts   # Grant/revoke logic
    └── AccessChecker.ts     # Permission checking
```

---

## Key Types

```typescript
export type CapabilityType =
  | 'file-read'
  | 'file-write'
  | 'network'
  | 'llm-call'
  | 'eval'
  | 'system'
  | 'fact-assert'
  | 'fact-retract'
  | 'snapshot'
  | 'history';

export interface Capability {
  type: CapabilityType;
  scope?: string;            // Path pattern, URL pattern, etc.
  constraints?: {
    maxCalls?: number;
    maxBytes?: number;
    allowedMethods?: string[];
    expiresAt?: number;
  };
  grantedAt: number;
  grantedBy?: string;
}

export interface CapabilityRequest {
  type: CapabilityType;
  scope?: string;
  reason?: string;           // Why capability is needed
}

export interface SecurityContext {
  id: string;
  capabilities: Map<CapabilityType, Capability>;
  parent?: SecurityContext;  // For inheritance
  usageCounts: Map<CapabilityType, number>;
}

export interface AccessDeniedError {
  type: CapabilityType;
  scope?: string;
  reason: string;
  context: SecurityContext;
}
```

---

## Key Interface

```typescript
export interface SecurityManager {
  // ─── Context Management ───

  /**
   * Create a new security context.
   */
  createContext(parent?: SecurityContext): SecurityContext;

  /**
   * Get current security context.
   */
  getCurrentContext(): SecurityContext;

  /**
   * Run code in a security context.
   */
  withContext<T>(context: SecurityContext, body: () => T): T;

  // ─── Capability Management ───

  /**
   * Grant a capability to a context.
   */
  grant(context: SecurityContext, capability: Capability): void;

  /**
   * Revoke a capability.
   */
  revoke(context: SecurityContext, type: CapabilityType): void;

  /**
   * Check if capability is granted.
   */
  hasCapability(type: CapabilityType, scope?: string): boolean;

  /**
   * Request a capability (may prompt user).
   */
  requestCapability(request: CapabilityRequest): Promise<boolean>;

  /**
   * Check and consume capability (for limited-use caps).
   */
  useCapability(type: CapabilityType, scope?: string): void;

  // ─── Access Control ───

  /**
   * Check file access.
   */
  checkFileAccess(path: string, mode: 'read' | 'write'): void;

  /**
   * Check network access.
   */
  checkNetworkAccess(url: string, method?: string): void;

  /**
   * Check LLM access.
   */
  checkLLMAccess(): void;

  /**
   * Check eval access.
   */
  checkEvalAccess(): void;

  // ─── Sandbox Mode ───

  /**
   * Create sandboxed context with minimal capabilities.
   */
  createSandbox(allowed?: CapabilityType[]): SecurityContext;

  /**
   * Check if in sandbox.
   */
  isInSandbox(): boolean;
}
```

---

## Capability Checking

```typescript
class SecurityManagerImpl implements SecurityManager {
  private contextStack: SecurityContext[] = [];

  checkFileAccess(path: string, mode: 'read' | 'write'): void {
    const type: CapabilityType = mode === 'read' ? 'file-read' : 'file-write';
    const context = this.getCurrentContext();

    const capability = context.capabilities.get(type);
    if (!capability) {
      throw new AccessDeniedError({
        type,
        scope: path,
        reason: `No ${type} capability`,
        context
      });
    }

    // Check scope (path pattern)
    if (capability.scope && !matchesGlob(path, capability.scope)) {
      throw new AccessDeniedError({
        type,
        scope: path,
        reason: `Path ${path} not in allowed scope ${capability.scope}`,
        context
      });
    }

    // Check constraints
    if (capability.constraints?.maxCalls !== undefined) {
      const used = context.usageCounts.get(type) ?? 0;
      if (used >= capability.constraints.maxCalls) {
        throw new AccessDeniedError({
          type,
          scope: path,
          reason: `Exceeded max calls (${capability.constraints.maxCalls})`,
          context
        });
      }
    }

    // Record usage
    context.usageCounts.set(type, (context.usageCounts.get(type) ?? 0) + 1);

    // Emit event
    this.emitter.emit('capability-used', { type, scope: path, context });
  }
}
```

---

## Sandbox Implementation

```typescript
createSandbox(allowed: CapabilityType[] = []): SecurityContext {
  const sandbox = this.createContext();

  // Grant only specified capabilities with minimal scope
  for (const type of allowed) {
    sandbox.capabilities.set(type, {
      type,
      grantedAt: Date.now(),
      grantedBy: 'sandbox',
      constraints: {
        maxCalls: 100,  // Limit all sandbox capabilities
      }
    });
  }

  // Mark as sandbox
  (sandbox as any).isSandbox = true;

  return sandbox;
}

// Usage
const sandbox = securityManager.createSandbox(['file-read']);
securityManager.withContext(sandbox, () => {
  // Can read files
  const content = readFile('data.txt');  // OK

  // Cannot write files
  writeFile('out.txt', content);  // AccessDeniedError!

  // Cannot call LLM
  llm.complete('hello');  // AccessDeniedError!
});
```

---

## Lisp Interface

```lisp
;; Check capabilities
(security.can? 'file-read "/home/user/data.txt")
; => #t or #f

;; Request capability
(security.request 'network :scope "api.example.com" :reason "fetch data")
; => Prompts user, returns #t or #f

;; Run in sandbox
(security.sandbox
  '(file-read)  ; Only allow file reading
  (lambda ()
    (read-file "data.txt")))

;; Grant capability
(security.grant 'file-write :scope "/tmp/*")

;; Revoke capability
(security.revoke 'file-write)
```

---

## Event Emission

```typescript
// When capability is used
emitter.emit('capability-used', { type, scope, context });

// When capability is granted
emitter.emit('capability-granted', { type, scope, context });

// When access is denied
emitter.emit('access-denied', { type, scope, reason, context });

// When capability is requested
emitter.emit('capability-requested', { request, granted });
```

---

## Test Requirements

### Unit Tests: `tests/runtime/subsystems/SecurityManager.test.ts`
- [ ] createContext() creates new context
- [ ] grant() adds capability
- [ ] revoke() removes capability
- [ ] hasCapability() checks correctly
- [ ] checkFileAccess() enforces scope
- [ ] checkFileAccess() enforces maxCalls
- [ ] checkNetworkAccess() validates URL
- [ ] createSandbox() creates minimal context
- [ ] withContext() scopes correctly
- [ ] Parent context inheritance works

### Integration Tests
- [ ] File operations check capabilities
- [ ] Network operations check capabilities
- [ ] LLM operations check capabilities
- [ ] Sandbox prevents unauthorized access
- [ ] Capability expiration works

---

## Acceptance Criteria
1. All sensitive operations require capabilities
2. Scope restrictions are enforced
3. Usage limits prevent abuse
4. Sandbox mode provides isolation
5. Capability requests prompt user
6. Events enable audit logging
