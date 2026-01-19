Got it. I’m going to treat everything you pasted as **already implemented and working** (SemVer, Provenance, Facts, Fixpoint, Budget, Artifacts, Outcomes, Session, Security, Experts, etc.). What follows is a **strictly incremental change plan**—ordered so you can implement top‑to‑bottom without thrashing architectural seams.

I’ll give each change-set:

* **Intent / risk addressed** (the “enterprise complaint class” it neutralizes)
* **What to add / modify** (files + interfaces)
* **Enforcement semantics** (the invariant you’ll now be able to assert)
* **Tests** (so you don’t regress)
* **Versioning note** (SemVer/compat)

I’m deliberately keeping **Layer 0/1 pure**; all governance/containment/ledger work lives in **Layer 2+** (runtime services + protocol + extensions), using classic Reference Monitor + Policy Enforcement Point patterns.

---

# Change Set 0 — Introduce an explicit ExecutionContext everywhere it matters

## Intent

Most enterprise “trust” failures are *context loss*:

* “Who did what?”
* “Under which policy?”
* “On behalf of which tenant?”
* “What data classification applied?”
* “Was this a replay? a live run? a dry-run?”

Your system already has Sessions/Turns/Audit; this change makes the context **first-class** and **non-optional** so it can be threaded through enforcement, redaction, audit, approvals, and replay.

## Add

### `src/runtime/context.ts`

```ts
export type RiskTier = 'low' | 'moderate' | 'high' | 'regulated';

export interface Principal {
  subject: string;                    // "user:alice" | "svc:billing-job"
  tenantId: string;
  roles: string[];
  attributes: Record<string, string | number | boolean>;
  authContext: {
    method: 'oidc' | 'mTLS' | 'api-key' | 'session-local';
    assuranceLevel?: 'aal1' | 'aal2' | 'aal3';
    issuedAt: string;                 // ISO
  };
}

export interface PolicyRef {
  bundleId: string;                   // "tenantA/policy/v12"
  version: string;                    // semver or monotonic
  hash: string;                       // sha256(canonical bundle)
}

export interface ExecutionContext {
  sessionId: string;
  turnNumber: number;

  principal: Principal;
  policy: PolicyRef;

  riskTier: RiskTier;

  // Whether we are recording nondeterminism or replaying it
  mode: 'live' | 'replay';

  // Correlation + tracing
  trace: {
    requestId: string;
    parentSpanId?: string;
  };

  // Optional “purpose of use” (compliance systems love this)
  purpose?: string;                   // e.g. "clinical-decision-support"
}
```

### Modify `src/runtime/ffi.ts` (Layer 2)

Add a **contextful call surface**, but keep Layer 1 evaluator pure by not forcing it to know about identity.

```ts
export interface CallOptions {
  capability?: unknown;              // will become CapabilityRef later
  purpose?: string;
  dataLabels?: string[];
}

export interface FFI {
  call(name: string, args: unknown[], opts?: CallOptions): unknown;
}

export interface ContextualFFI extends FFI {
  readonly ctx: ExecutionContext;
}
```

### Modify `src/runtime/session.ts` (or wherever Session lives)

* Session owns an `ExecutionContext` template.
* Each `executeTurn` stamps `turnNumber` and `requestId`.

## Enforcement semantics

After this change:

* Every operation can be audited with `(tenantId, subject, policyHash, riskTier, turnNumber)` without “ambient” lookups.
* Future changes (policy, approvals, ledger) become **local refactors**, not cross-cutting pain.

## Tests

* `test/integration/context.test.ts`

  * ensure `ctx.turnNumber` increments
  * ensure `policy.hash` is pinned and stable per session unless explicitly rotated
  * ensure ctx is present in audit payloads (even if audit still array-based today)

## Versioning

* Backward compatible at protocol level unless you add these fields to handshake right now (we will later). Internally safe.

---

# Change Set 1 — Replace “capability objects” with unforgeable CapabilityRef (MAC’d, server-resolved)

## Intent

Your current capability model is conceptually correct (object-capabilities), but in a JS runtime **plain objects are forgeable by shape** unless they never cross the trust boundary.

Enterprises complain: *“How do we know untrusted code can’t mint ‘file-write’ or ‘network’?”*
This change makes capabilities **cryptographically unforgeable** and **bound to session + tenant + constraints**.

## Add

### `src/security/capability-ref.ts`

```ts
export type CapabilityRef = `cap:${string}:${string}`; // cap:<id>:<mac>

export type CapabilityType =
  | 'world-read'
  | 'world-write'
  | 'network'
  | 'process'
  | 'llm'
  | 'time'
  | 'evidence-read'
  | 'evidence-write'
  | 'audit-export';

export interface Permission {
  action: string;                     // "read" | "write" | "connect" | ...
  resource: string;                   // glob/uri template
}

export interface Constraints {
  timeoutMs: number;
  maxCalls: number;
  maxBytes: number;
  allowedHosts: string[];
  purpose?: string;
  expiresAt?: number;
}

interface CapabilityRecord {
  id: string;
  sessionId: string;
  tenantId: string;
  type: CapabilityType;
  permissions: Permission[];
  constraints: Constraints;
  revoked: boolean;
  createdAt: number;
}

export class CapabilityManager {
  private records = new Map<string, CapabilityRecord>();

  constructor(
    private readonly sessionId: string,
    private readonly tenantId: string,
    private readonly secretKey: Uint8Array
  ) {}

  create(type: CapabilityType, permissions: Permission[], partial: Partial<Constraints> = {}): CapabilityRef {
    const id = crypto.randomUUID();
    const rec: CapabilityRecord = {
      id,
      sessionId: this.sessionId,
      tenantId: this.tenantId,
      type,
      permissions,
      constraints: {
        timeoutMs: partial.timeoutMs ?? 30_000,
        maxCalls: partial.maxCalls ?? 10_000,
        maxBytes: partial.maxBytes ?? 1_000_000,
        allowedHosts: partial.allowedHosts ?? [],
        purpose: partial.purpose,
        expiresAt: partial.expiresAt,
      },
      revoked: false,
      createdAt: Date.now(),
    };
    this.records.set(id, rec);
    return this.sign(rec);
  }

  resolve(ref: CapabilityRef): CapabilityRecord {
    const parts = ref.split(':');
    if (parts.length !== 3 || parts[0] !== 'cap') throw new Error('Invalid capability ref');
    const [, id, mac] = parts as ['cap', string, string];

    const rec = this.records.get(id);
    if (!rec) throw new Error('Unknown capability');
    if (rec.revoked) throw new Error('Revoked capability');
    if (rec.constraints.expiresAt && Date.now() > rec.constraints.expiresAt) throw new Error('Expired capability');

    const expected = this.macFor(rec);
    if (!timingSafeEqualHex(mac, expected)) throw new Error('Invalid capability MAC');
    return rec;
  }

  revoke(ref: CapabilityRef): void {
    const rec = this.resolve(ref);
    rec.revoked = true;
  }

  private sign(rec: CapabilityRecord): CapabilityRef {
    return `cap:${rec.id}:${this.macFor(rec)}`;
  }

  private macFor(rec: CapabilityRecord): string {
    // Bind to tenant+session+type+constraints (not permissions to keep payload small; permissions are server-side anyway)
    const payload = JSON.stringify({
      id: rec.id,
      sessionId: rec.sessionId,
      tenantId: rec.tenantId,
      type: rec.type,
      constraints: rec.constraints,
    });
    return hmacSha256Hex(this.secretKey, payload);
  }
}
```

### Modify `src/runtime/ffi.ts`

* Treat `opts.capability` as `CapabilityRef` (string) not object.
* All privileged FFI implementations resolve capability refs via `CapabilityManager`.

### Modify `src/ext/world/*` and `src/ext/llm/*`

* Update signatures: `(world.read path :cap capRef)` etc.
* Inside host implementation: resolve capRef, verify permission/resource match.

## Enforcement semantics

* Untrusted code may **hold** and **replay** a `CapabilityRef`, but cannot **forge** one.
* Capability refs are **tenant+session scoped**; cross-session replay fails.

## Tests

* `test/unit/capability-ref.test.ts`

  * rejects malformed refs
  * rejects wrong MAC
  * rejects after revocation
  * rejects after expiry
  * accepts correct ref
* `test/integration/world-capability.test.ts`

  * allowed read within glob
  * denied read outside glob
  * denied read with missing capability even if op allowed (depending on your chosen rule)

## Versioning

* This is potentially breaking if your Lisp surface previously accepted capability objects.

  * Do a two-step deprecation:

    * **vNext MINOR**: accept both object + ref (normalize to ref internally)
    * **next MAJOR**: remove object acceptance

---

# Change Set 2 — Policy bundles + PDP + obligations (real enterprise policy, not a static whitelist)

## Intent

Your current `SessionPolicy` is a good starting PEP, but enterprises want:

* centrally managed policy bundles (versioned)
* auditability: “policy hash X was in force”
* obligations (“require approval”, “redact”, “limit tokens”)

This change turns `SessionEnforcer` into a true **Policy Enforcement Point** with an externalizable **Policy Decision Point** (Strategy/Adapter).

## Add

### `src/policy/types.ts`

```ts
import { ExecutionContext } from '../runtime/context';

export type DataLabel =
  | 'public' | 'internal' | 'confidential' | 'privileged'
  | 'pii' | 'phi' | 'attorney-client' | 'trade-secret';

export interface PolicyDecision {
  allow: boolean;
  reason?: string;

  obligations: Array<
    | { kind: 'require-approval'; approval: ApprovalPolicy }
    | { kind: 'require-redaction'; profile: RedactionProfileId }
    | { kind: 'limit-budget'; tokens?: number; cost?: number; timeMs?: number }
    | { kind: 'deny-egress'; message: string }
    | { kind: 'force-sandbox'; mode: 'worker' | 'process' | 'container' }
    | { kind: 'log'; level: 'security' | 'compliance' }
  >;

  advice?: Array<{ kind: 'tag'; key: string; value: string }>;
}

export interface PolicyInput {
  ctx: ExecutionContext;
  op: string;
  resource?: string;              // path/host/etc
  argsSummary?: unknown;          // redacted summary only
  dataLabels?: DataLabel[];
}

export interface PDP {
  decide(input: PolicyInput): PolicyDecision;
}

export type RedactionProfileId =
  | 'default'
  | 'medical-strict'
  | 'legal-privileged'
  | 'finance-pci';

export interface ApprovalPolicy {
  requiredApprovals: number;      // 1 or 2 (four-eyes)
  allowedRoles: string[];
  maxAgeMs: number;
  minAssurance?: 'aal2' | 'aal3';
}
```

### `src/policy/bundle.ts`

```ts
export interface PolicyBundle {
  id: string;
  version: string;
  hash: string;
  issuedAt: string;
  issuer: string;
  rules: unknown;                 // whatever internal format you choose
}
```

### `src/policy/pdp-local.ts`

A local PDP implementation that evaluates your existing `SessionPolicy` rules plus extra obligations.

### Modify `src/runtime/session-enforcer.ts`

Current flow:

1. whitelist/denylist
2. world path checks
3. llm call checks

New flow (Chain of Responsibility):

1. **Fast local gate** (existing)
2. Build `PolicyInput` from ctx + op + resource + argsSummary + dataLabels
3. `decision = pdp.decide(input)`
4. If deny → throw `PolicyViolationError`
5. If allow → enforce obligations:

   * require approval (block into `NeedsOutcome`)
   * require redaction (force a DLP pipeline for egress)
   * limit budgets (shrink session budget for the remainder of the turn)
   * force sandbox (choose runner; see Change Set 6)
   * log at compliance level

This is exactly a PEP with obligations (XACML-like semantics), but you can keep it minimal.

## Enforcement semantics

* Policy is now **portable**, **hash-addressable**, and **auditable**.
* You can finally answer: “Why was this allowed?” (reason + decision hash).

## Tests

* `test/unit/policy-obligations.test.ts`

  * require-approval turns operation into `(needs ...)` outcome
  * limit-budget reduces available tokens/time
  * deny-egress blocks llm/network
* `test/integration/policy-hash-audit.test.ts`

  * audit contains policy hash on every operation

## Versioning

* Minor if you default PDP to “mirror existing SessionPolicy behavior” (no semantic change unless you opt in).

---

# Change Set 3 — Binding approvals: proposal hash + signed approvals + four-eyes

## Intent

“Human-in-the-loop” is not credible unless approvals are:

* attributable (identity + role)
* bound to exact change set (hash)
* policy-scoped (policy hash)
* non-replayable (expiry + session binding)
* non-repudiable (signature)

This is the core of “legal defensibility” for agent edits.

## Add

### `src/runtime/approvals.ts`

```ts
import { PolicyRef, Principal } from './context';
import { ApprovalPolicy } from '../policy/types';

export interface Approval {
  id: string;
  tenantId: string;
  sessionId: string;

  approver: { subject: string; roles: string[] };
  policyHash: string;
  proposalsHash: string;

  justification: string;
  issuedAt: string;
  expiresAt: string;

  signature: string;              // signature over canonical payload
}

export interface ProposalDigest {
  proposalsHash: string;
  count: number;
}

export function hashProposals(proposals: Array<{type: string; ref: string; contentHash?: string; commandHash?: string}>): ProposalDigest {
  const canon = proposals.slice().sort((a,b)=> (a.type+a.ref).localeCompare(b.type+b.ref));
  const proposalsHash = sha256Hex(JSON.stringify(canon));
  return { proposalsHash, count: canon.length };
}

export function verifyApprovals(params: {
  approvals: Approval[];
  approvalPolicy: ApprovalPolicy;
  ctx: { tenantId: string; sessionId: string; policy: PolicyRef };
  proposalsHash: string;
  // pluggable signature verifier
  verifySignature: (approval: Approval) => boolean;
}): { ok: boolean; reason?: string } {
  const { approvals, approvalPolicy, ctx, proposalsHash, verifySignature } = params;

  const fresh = approvals.filter(a => {
    if (a.tenantId !== ctx.tenantId) return false;
    if (a.sessionId !== ctx.sessionId) return false;
    if (a.policyHash !== ctx.policy.hash) return false;
    if (a.proposalsHash !== proposalsHash) return false;
    if (!verifySignature(a)) return false;
    if (Date.now() > Date.parse(a.expiresAt)) return false;
    return true;
  });

  // Role / assurance checks (roles are on approval; assurance can be checked externally)
  const allowed = fresh.filter(a =>
    a.approver.roles.some(r => approvalPolicy.allowedRoles.includes(r))
  );

  if (allowed.length < approvalPolicy.requiredApprovals) {
    return { ok: false, reason: 'Insufficient valid approvals' };
  }
  return { ok: true };
}
```

### Modify `src/runtime/staged-world.ts`

* Ensure each proposal includes a deterministic `contentHash` or `commandHash`.
* Keep proposals stable across serialization.

### Modify Session commit path (host-controlled)

You likely have `sessionManager.commit(session.id)` as host API; change it to require approvals when policy says so:

```ts
commit(id: string, approvals: Approval[]): void
```

If missing, return an outcome:

* `(needs 'needs-approval "..." ctx)` rather than throwing

## Enforcement semantics

* Commit is now a **two-phase protocol**: propose → approve → commit.
* Approvals cannot be replayed across sessions or policy versions.

## Tests

* `test/integration/approvals-commit.test.ts`

  * commit denied without approvals if obligation present
  * commit denied with wrong proposalsHash
  * commit denied with expired approval
  * commit allowed with required number of approvals
  * four-eyes: requires 2 distinct approvers

## Versioning

* Minor if default policy does not require approvals.
* Add policy rule: “world.write requires approval at riskTier>=high”.

---

# Change Set 4 — Replace audit “log” with an append-only, tamper-evident, signed audit ledger

## Intent

Enterprises don’t trust mutable logs. They want tamper evidence and provable integrity (SOC2-ish expectations, incident forensics, legal eDiscovery posture).

Your current audit logging is a good start; this change makes it **cryptographically accountable**.

## Add

### `src/runtime/audit-ledger.ts`

```ts
import { ExecutionContext } from './context';

export interface AuditPayload {
  op: string;
  resource?: string;
  argsSummary?: unknown;              // MUST be redacted/minimized
  resultSummary?: unknown;
  outcomeTag?: string;
  evidenceIds?: string[];
  policyHash: string;
  principal: { subject: string; roles: string[] };
  obligations?: unknown[];
}

export interface AuditEnvelope {
  schemaVersion: number;
  tenantId: string;
  sessionId: string;
  turnNumber: number;
  timestamp: string;

  prevHash: string;
  payload: AuditPayload;

  payloadHash: string;
  envelopeHash: string;
  signature: string;
}

export interface AuditSigner {
  sign(hashHex: string): string;
  verify(hashHex: string, signature: string): boolean;
  keyId(): string;
}

export class AuditLedger {
  private lastHash: string = '0'.repeat(64);
  private envelopes: AuditEnvelope[] = [];

  constructor(private signer: AuditSigner) {}

  append(ctx: ExecutionContext, payload: Omit<AuditPayload, 'policyHash'|'principal'> & Partial<Pick<AuditPayload,'policyHash'|'principal'>>): AuditEnvelope {
    const normalized: AuditPayload = {
      ...payload,
      policyHash: payload.policyHash ?? ctx.policy.hash,
      principal: payload.principal ?? { subject: ctx.principal.subject, roles: ctx.principal.roles },
    };

    const payloadHash = sha256Hex(canonicalJson(normalized));
    const base = canonicalJson({
      schemaVersion: 1,
      tenantId: ctx.principal.tenantId,
      sessionId: ctx.sessionId,
      turnNumber: ctx.turnNumber,
      timestamp: new Date().toISOString(),
      prevHash: this.lastHash,
      payloadHash,
    });

    const envelopeHash = sha256Hex(base);
    const signature = this.signer.sign(envelopeHash);

    const env: AuditEnvelope = {
      schemaVersion: 1,
      tenantId: ctx.principal.tenantId,
      sessionId: ctx.sessionId,
      turnNumber: ctx.turnNumber,
      timestamp: new Date().toISOString(),
      prevHash: this.lastHash,
      payload: normalized,
      payloadHash,
      envelopeHash,
      signature,
    };

    this.envelopes.push(env);
    this.lastHash = env.envelopeHash;
    return env;
  }

  export(): AuditEnvelope[] { return [...this.envelopes]; }

  verifyChain(): { ok: boolean; index?: number; reason?: string } {
    let prev = '0'.repeat(64);
    for (let i=0;i<this.envelopes.length;i++) {
      const e = this.envelopes[i];
      if (e.prevHash !== prev) return { ok:false, index:i, reason:'prevHash mismatch' };
      const payloadHash = sha256Hex(canonicalJson(e.payload));
      if (payloadHash !== e.payloadHash) return { ok:false, index:i, reason:'payloadHash mismatch' };
      const base = canonicalJson({
        schemaVersion: e.schemaVersion,
        tenantId: e.tenantId,
        sessionId: e.sessionId,
        turnNumber: e.turnNumber,
        timestamp: e.timestamp,
        prevHash: e.prevHash,
        payloadHash: e.payloadHash,
      });
      const envelopeHash = sha256Hex(base);
      if (envelopeHash !== e.envelopeHash) return { ok:false, index:i, reason:'envelopeHash mismatch' };
      if (!this.signer.verify(e.envelopeHash, e.signature)) return { ok:false, index:i, reason:'signature invalid' };
      prev = e.envelopeHash;
    }
    return { ok:true };
  }
}
```

### Modify `src/runtime/session.ts`

* Replace `AuditLog` usage with `AuditLedger`.
* Every `FFI.call` append an envelope (allowed and denied).
* Include policy decision result in payload (decision hash), if Change Set 2 is done.

## Enforcement semantics

* Audit trail becomes **append-only** + **tamper-evident** + **signed**.
* You can store it WORM-style externally later without changing semantics.

## Tests

* `test/unit/audit-ledger.test.ts`

  * verify chain passes
  * mutation breaks chain
  * deletion breaks chain
* `test/integration/audit-deny-events.test.ts`

  * denied ops still create envelope with reason

## Versioning

* Internal; protocol export op may be new later.

---

# Change Set 5 — Data governance primitives: labels, redaction, and egress gate around llm/network

## Intent

The most common “enterprise whining” is *data exfiltration*:

* “Does it ever send PHI/PII to a third party?”
* “Can we enforce redaction?”
* “Can we demonstrate minimization?”
* “Can we enforce ‘no network’ in regulated mode?”

You already have sandbox + capabilities. This change adds **DLP and egress governance** as a first-class pipeline.

## Add

### `src/security/redaction.ts`

```ts
import { RedactionProfileId } from '../policy/types';
import { DataLabel } from '../policy/types';

export interface RedactionResult {
  redacted: string;
  labels: DataLabel[];
  findings: Array<{ kind: string; span: [number, number] }>;
}

export interface RedactionEngine {
  redact(text: string, profile: RedactionProfileId): RedactionResult;
}

export class HeuristicRedactionEngine implements RedactionEngine {
  redact(text: string, profile: RedactionProfileId): RedactionResult {
    // Start minimal and deterministic; you can swap implementation later.
    // Example: email, phone, SSN-ish patterns, MRN-ish patterns.
    // The goal is enforcement hooks, not perfect NLP.
    const findings: RedactionResult['findings'] = [];
    let out = text;

    // ...apply regex-based transforms deterministically...
    // Tag labels conservatively
    const labels: DataLabel[] = profile.includes('medical') ? ['phi'] : ['pii'];

    return { redacted: out, labels, findings };
  }
}
```

### `src/ext/llm/egress-gate.ts`

Decorator (GoF Decorator / Proxy) around your LLM adapter:

```ts
import { PDP } from '../../policy/types';
import { RedactionEngine } from '../../security/redaction';
import { AuditLedger } from '../../runtime/audit-ledger';
import { ExecutionContext } from '../../runtime/context';

export class EgressGatedLLMAdapter implements LLMAdapter {
  constructor(
    private base: LLMAdapter,
    private pdp: PDP,
    private redactor: RedactionEngine,
    private audit: AuditLedger,
    private ctx: ExecutionContext
  ) {}

  async complete(prompt: string, options?: LLMOptions & { dataLabels?: string[]; purpose?: string; redactionProfile?: string }): Promise<string> {
    // 1) Policy decide
    const decision = this.pdp.decide({
      ctx: this.ctx,
      op: 'llm.complete',
      resource: options?.model ?? 'default',
      argsSummary: { chars: prompt.length, model: options?.model },
      dataLabels: options?.dataLabels as any,
    });

    if (!decision.allow) {
      this.audit.append(this.ctx, {
        op: 'llm.complete',
        argsSummary: { chars: prompt.length, model: options?.model },
        outcomeTag: 'denied',
        obligations: decision.obligations,
      });
      throw new PolicyViolationError(decision.reason ?? 'Denied by policy');
    }

    // 2) Redaction obligation
    let finalPrompt = prompt;
    const redactionOb = decision.obligations.find(o => o.kind === 'require-redaction');
    if (redactionOb && redactionOb.kind === 'require-redaction') {
      const r = this.redactor.redact(prompt, redactionOb.profile);
      finalPrompt = r.redacted;
    }

    // 3) Call base
    const result = await this.base.complete(finalPrompt, options);

    // 4) Audit
    this.audit.append(this.ctx, {
      op: 'llm.complete',
      argsSummary: { chars: finalPrompt.length, model: options?.model, redacted: !!redactionOb },
      resultSummary: { chars: result.length },
    });

    return result;
  }
}
```

### Modify Session wiring

When constructing the session’s `llm` adapter:

* wrap base adapter in `EgressGatedLLMAdapter`
* feed it the session PDP + redactor + ledger + ctx

## Enforcement semantics

* All egress is policy-mediated and auditable.
* Redaction is enforceable, not “best practice”.
* You can implement “deny all egress at riskTier=regulated” in policy.

## Tests

* `test/integration/llm-egress-redaction.test.ts`

  * policy denies → no base call invoked; audit envelope emitted
  * policy requires redaction → redacted prompt used
  * ledger contains redaction flag
* `test/property/redaction-determinism.test.ts`

  * same input yields same redacted output (important for replay)

## Versioning

* Minor. You’re adding enforcement but can default policy to “allow” initially.

---

# Change Set 6 — Hard isolation runner: WorkerRunner first, then ProcessRunner/ContainerRunner

## Intent

A step counter is not a sandbox. Enterprises will ask:

* “Can you actually stop it?”
* “What’s the blast radius of infinite loops?”
* “DoS via allocation?”
* “Can we isolate untrusted modules?”

You already have sandbox configuration; this change implements a **real containment boundary** via a Runner abstraction.

## Add

### `src/runtime/sandbox-runner.ts`

```ts
import { ExecutionContext } from './context';
import { Outcome } from './outcomes';

export type RunnerKind = 'in-process' | 'worker' | 'process' | 'container';

export interface SandboxRunner {
  kind: RunnerKind;
  eval(code: string, ctx: ExecutionContext): Promise<{ outcome: Outcome; journal: JournalEvent[] }>;
  kill(reason: string): Promise<void>;
}
```

### `src/runtime/runners/worker-runner.ts`

* Spawn a Worker thread that hosts the evaluator + FFI.
* Enforce timeout by terminating Worker.
* Memory limits are weaker in Worker than process/container, but kill semantics are strong.

### Modify Session `executeTurn`

* Choose runner based on:

  * sandbox config
  * policy obligations (from Change Set 2: `force-sandbox`)
* Default to WorkerRunner for untrusted code, InProcessRunner for trusted local dev.

## Enforcement semantics

* You now have **preemptive termination** for runaway code.
* This is a necessary precondition for “production safe”.

## Tests

* `test/e2e/runner-timeout.test.ts`

  * `(fixpoint (begin ...) :max-iters very-large)` is killed by timeout
* `test/integration/runner-kill.test.ts`

  * kill terminates execution and returns `(error ...)` or `(nonconverged ...)` outcome with reason

## Versioning

* Minor; implementation detail under Session.

---

# Change Set 7 — Deterministic journal + replay mode (event sourcing for trust)

## Intent

Even with provenance, regulators ask:

* “Can you reproduce the exact recommendation?”
* “Did the model output change?”
* “Was the environment different?”

You need deterministic replay via a journal of nondeterminism.

## Add

### `src/runtime/journal.ts`

```ts
export type JournalEvent =
  | { kind: 'world.read'; ref: string; fingerprint: string; bytes: number }
  | { kind: 'world.list'; pattern: string; resultsHash: string }
  | { kind: 'llm.complete'; requestHash: string; responseHash: string; model: string }
  | { kind: 'time.now'; value: number }
  | { kind: 'random'; seed: string; value: string }
  | { kind: 'policy.decision'; op: string; decisionHash: string; policyHash: string }
  | { kind: 'proposal'; proposalsHash: string };

export interface Journal {
  record(e: JournalEvent): void;
  export(): JournalEvent[];
}
```

### Modify World + LLM adapters

* On every `world.read`, record fingerprint+bytes.
* On every `llm.complete`, record request hash + response hash (and optionally store full payload in encrypted store; see next change).
* On policy decisions, record decision hash.

### Add Replay adapters

* `ReplayWorld`: fails if fingerprint mismatch.
* `ReplayLLM`: returns recorded outputs rather than calling network.

### Modify Session to support `ctx.mode = 'replay'`

## Enforcement semantics

* You can reproduce and prove divergences:

  * changed world
  * changed policy
  * changed model output

## Tests

* `test/integration/replay-roundtrip.test.ts`

  * run live, export journal
  * run replay with same journal, assert identical outcome
* `test/integration/replay-divergence.test.ts`

  * mutate file, replay fails with explicit divergence

## Versioning

* Minor; new capability is additive.

---

# Change Set 8 — Secure Evidence Store: redacted snippet in reports, encrypted blob for authorized retrieval

## Intent

Your current Evidence stores `snippet` inline. In regulated environments that turns:

* audit logs,
* snapshots,
* evidence registries

…into PHI/PII/privileged data stores. That is often unacceptable.

You need **split evidence**:

* safe redacted snippet for reporting
* encrypted blob retrievable only with `evidence-read` capability + policy allow

## Add

### `src/runtime/evidence-store.ts`

```ts
export type EncryptedBlobRef = `evblob:${string}`;

export interface EvidenceBlobStore {
  putEncrypted(tenantId: string, plaintext: Uint8Array, labels: string[]): Promise<EncryptedBlobRef>;
  getDecrypted(tenantId: string, ref: EncryptedBlobRef): Promise<Uint8Array>;
  delete(tenantId: string, ref: EncryptedBlobRef): Promise<void>;
}

export interface KeyProvider {
  // envelope encryption hook; integrate with KMS/HSM later
  encryptForTenant(tenantId: string, plaintext: Uint8Array): Promise<{ ciphertext: Uint8Array; keyInfo: unknown }>;
  decryptForTenant(tenantId: string, ciphertext: Uint8Array, keyInfo: unknown): Promise<Uint8Array>;
}
```

### Modify `Evidence` schema (22-PROVENANCE.md implementation)

Replace `snippet` with:

* `redactedSnippet`
* `encryptedBlobRef?`
* `labels`
* `retention`

And ensure evidence capture runs through redaction engine.

### Modify evidence retrieval path

* retrieving decrypted blob requires:

  * valid `CapabilityRef` of type `evidence-read`
  * policy allow
  * audit envelope for access

## Enforcement semantics

* Default reporting cannot leak raw sensitive content.
* Sensitive evidence is still available under controlled access (which auditors like).

## Tests

* `test/integration/evidence-redaction.test.ts`

  * evidence.snippet is redacted
  * blob exists and can be retrieved with capability
  * retrieval denied without capability / policy

## Versioning

* This changes Evidence shape → treat as schema bump:

  * bump `snapshot.version.snapshot` and provide migration (you already have migration machinery).

---

# Change Set 9 — Protocol extensions: policy hash, principal context, approvals, audit export, journal export

## Intent

At enterprise scale, “trust” means integrations:

* governance UI needs pending proposals
* compliance needs audit export
* replay tooling needs journal export
* approvals need structured transport

You already have Protocol-first architecture; this is where it shines.

## Modify handshake (08-PROTOCOL.md implementation)

Add fields in `HelloRequest`/`HelloResponse`:

* `tenant-id`
* `principal` (or a `principal-hash` if you don’t want to expose details)
* `policy` (bundle id + hash)

Add new ops:

* `op: 'proposals'` → returns proposals + proposalsHash
* `op: 'commit'` → commit proposals with approvals
* `op: 'audit.export'` → stream ledger envelopes
* `op: 'journal.export'` → stream journal events

**Important:** these ops are *host-level*, not Lisp primitives; they belong in protocol handlers, not evaluator.

## Tests

* protocol integration tests verifying:

  * `commit` fails without approvals when policy requires it
  * audit export verifies chain
  * journal export used for replay

---

# Change Set 10 — Assurance case artifacts + proof obligations (regulated mode)

## Intent

This is the “final boss” for legal/medical: you need a way to express and enforce:

* required citations
* required human signoff
* required guideline constraints
* prohibited actions (autonomous diagnosis, autonomous filing, etc.)

You already have Facts + Outcomes + Provenance. Add an **Assurance Case** artifact that is machine-checkable.

## Add

### `src/assurance/assurance-case.ts`

```ts
export interface AssuranceClaim {
  id: string;
  claim: string;                           // e.g. "No PHI egress without redaction"
  argument: string;                        // structured rationale
  evidence: string[];                      // EvidenceId, AuditEnvelope hashes, Policy hashes
  status: 'satisfied' | 'unsatisfied' | 'unknown';
}

export interface AssuranceCase {
  schemaVersion: number;
  tenantId: string;
  sessionId: string;
  policyHash: string;
  claims: AssuranceClaim[];
  generatedAt: string;
}
```

### Lisp helpers (stdlib)

* `(require-guideline "ADA-2025" :evidence ev)` asserts a fact
* `(require-approval ...)` yields `(needs ...)`

### Policy obligations

* For riskTier `regulated`, policy can require certain assurance claims to be satisfied before returning `ok`.

This is “Design by Contract” + “Proof Obligation” integrated with your monotone FactStore.

## Tests

* scenario tests: missing guideline citation → `needs-evidence`
* satisfied guideline + approval → `proposed` outcome allowed

---

# Change Set 11 — Evaluation harness + drift monitoring gates in CI

## Intent

Architecture alone does not make outputs “medically correct” or “legally correct.” Enterprises want:

* regression suites
* thresholds
* drift detection
* rollback triggers

You already have a testing pyramid; extend it with “behavioral suites” for LLM-mediated workflows, with replay journals.

## Add

* `test/scenarios/` with:

  * fixtures: world snapshots + journals
  * expected outcomes (structured)
* CI step:

  * run scenario suite in replay mode
  * fail build if drift in outcomes/assurance claims

This is essentially **Golden Master Testing** + **Snapshot Testing**, but done in a replay-safe way.

---

# Change Set 12 — Observability + incident controls: kill switch, revocation, rate limiting

## Intent

Operational trust means:

* SLOs
* incident response
* emergency revocation (“stop all egress now”)
* rate limiting (anti-abuse)

## Add

* OpenTelemetry spans per turn and per op (Adapter pattern around FFI)
* `PolicyBundle` includes a “global deny egress” emergency rule
* capability manager supports bulk revocation by tenant/session
* per-tenant budgets/quotas (Budget becomes quota-enforced)

---

## Implementation order (summary)

If you want the *least rework* order, do this exactly:

1. **Change Set 0** (ExecutionContext)
2. **Change Set 1** (CapabilityRef unforgeability)
3. **Change Set 2** (PDP + obligations)
4. **Change Set 4** (AuditLedger)
5. **Change Set 3** (Approvals bound to proposalsHash+policyHash)
6. **Change Set 5** (DLP/redaction + egress gate)
7. **Change Set 6** (WorkerRunner kill semantics)
8. **Change Set 7** (Journal + replay)
9. **Change Set 8** (Secure evidence store + schema migration)
10. **Change Set 9** (Protocol ops for approvals/audit/journal)
11. **Change Set 10** (Assurance cases + proof obligations)
12. **Change Set 11–12** (Evaluation + observability + incident controls)

This order minimizes “ripples”:

* context first,
* then unforgeable authority,
* then policy,
* then audit integrity,
* then governance (approvals),
* then data governance,
* then containment,
* then replay,
* then evidence hardening,
* then protocol surfacing,
* then assurance/evaluation/ops maturity.

---
