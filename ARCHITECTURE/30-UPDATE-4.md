Continuing with **changes-only** (everything below is additive or a modification to what you already implemented), and ordered so you can build while reading.

What follows is the next tranche that closes the remaining “enterprise trust” gaps that aren’t solved by provenance/facts/fixpoint/budget/sessions alone:

* **Signed, distributable policy bundles** (so “policy” is a governed artifact, not a code smell)
* **Verified publishing pipeline** (so nothing “escapes” as an external claim unless it is evidence-grounded + fresh)
* **Assurance cases / safety cases** (so legal/medical stakeholders get an auditable argument structure, not just logs)
* **Export/interop patterns** (so you can integrate with SIEM/GRC/EMR systems with idempotent, tamper-evident delivery)
* **Model governance hooks** (model inventory, allowed-use contracts, evaluation gates)

---

# Change Set 10 — Policy Bundles as Signed, Versioned, Distributable Artifacts

Right now you have “policy” as an in-memory structure (SessionPolicy + PDP logic). Enterprises will insist policy be:

* **content-addressed** (hash-stable)
* **signed** (origin + integrity)
* **versioned** (SemVer + rollout control)
* **distributable** (fetched/pushed + cached)
* **auditable** (policy hash appears in every decision)

This turns policy into a **first-class artifact** (Repository pattern + Signed Artifact pattern), not “some config object”.

## 10.1 Add `PolicyBundle` schema (policy-as-data, not code)

### New file: `src/policy/policy-types.ts`

```ts
import { canonicalJson, sha256Hex } from '../runtime/crypto';

export type PolicyVersion = `${number}.${number}.${number}`; // SemVer

export type Effect = 'allow' | 'deny';

export type Obligation =
  | { kind: 'require-approval'; policy: { allowedRoles: string[]; minAssurance?: 'aal2'|'aal3'; expiresInSeconds: number } }
  | { kind: 'require-redaction'; profile: 'default'|'medical-strict'|'legal-privileged'|'finance-pci' }
  | { kind: 'token-cap'; maxTokens: number }
  | { kind: 'deny-egress'; reason: string }
  | { kind: 'log-level'; level: 'security'|'audit'|'debug' };

export interface SubjectConstraint {
  rolesAny?: string[];
  rolesAll?: string[];
  assuranceAtLeast?: 'aal2'|'aal3';
}

export interface ResourceConstraint {
  // canonical resource string, often path/host/model/op-specific
  equals?: string;
  glob?: string;
  notGlob?: string;
}

export interface LabelConstraint {
  any?: string[];
  all?: string[];
  none?: string[];
}

export interface PurposeConstraint {
  equals?: string;
  anyOf?: string[];
}

export interface RuleCondition {
  subject?: SubjectConstraint;
  resource?: ResourceConstraint;
  labels?: LabelConstraint;
  purpose?: PurposeConstraint;
  riskTierAny?: Array<'low'|'moderate'|'high'|'regulated'>;
}

export interface PolicyRule {
  id: string;                 // stable identifier
  description: string;
  priority: number;           // higher wins (or lower; pick one and document)
  op: string;                 // e.g. "world.write" / "llm.complete" / "network.http"
  when?: RuleCondition;
  effect: Effect;
  obligations?: Obligation[];
  reasonCode?: string;        // e.g. "PHI_EGRESS_BLOCK"
}

export interface PolicyBundle {
  schemaVersion: 1;

  tenantId: string;
  name: string;
  version: PolicyVersion;

  // governance metadata
  issuedAt: string;           // ISO
  issuedBy: string;           // "policy-admin" / CI identity
  changelog?: string;

  // rules
  rules: PolicyRule[];

  // computed
  policyHash: string;         // sha256(canonical bundle minus policyHash + signature)
}

export interface SignedPolicyBundle {
  bundle: PolicyBundle;
  signature: {
    keyId: string;
    alg: 'ed25519' | 'hmac-sha256';
    sig: string;
  };
}

export function computePolicyHash(bundle: Omit<PolicyBundle,'policyHash'>): string {
  return sha256Hex(canonicalJson(bundle));
}
```

**Important pattern choice:** this is a classic **Specification pattern** (rules + conditions) with an explicit **Priority** / conflict strategy.

---

## 10.2 Add Policy signing + verification (adapters over your `KeyProvider`)

### New file: `src/policy/policy-signing.ts`

```ts
import { SignedPolicyBundle, PolicyBundle, computePolicyHash } from './policy-types';
import { KeyProvider } from '../runtime/keys';
import { canonicalJson, sha256Hex } from '../runtime/crypto';

export function finalizeBundle(
  bundle: Omit<PolicyBundle,'policyHash'>
): PolicyBundle {
  return { ...bundle, policyHash: computePolicyHash(bundle) };
}

export function signPolicyBundle(
  keys: KeyProvider,
  bundle: PolicyBundle,
  keyPurpose: 'audit'|'approval'|'journal' = 'audit'
): SignedPolicyBundle {
  // Sign hash of canonical policy bundle (including policyHash, excluding signature)
  const payloadHash = sha256Hex(canonicalJson(bundle));
  const keyId = keys.currentKeyId(keyPurpose);
  const sig = keys.sign(keyId, payloadHash);
  return { bundle, signature: { keyId, alg: 'ed25519', sig } };
}

export function verifyPolicyBundle(
  keys: KeyProvider,
  signed: SignedPolicyBundle
): { ok: true } | { ok: false; reason: string } {
  // Check internal policyHash matches content
  const { policyHash, ...rest } = signed.bundle as any;
  const recomputed = computePolicyHash(rest);
  if (recomputed !== policyHash) return { ok:false, reason:'policy-hash-mismatch' };

  const payloadHash = sha256Hex(canonicalJson(signed.bundle));
  const ok = keys.verify(signed.signature.keyId, payloadHash, signed.signature.sig);
  return ok ? { ok:true } : { ok:false, reason:'signature-invalid' };
}
```

---

## 10.3 Add `PolicyRegistry` (Repository pattern + cache + tenant scoping)

This becomes the single source of truth for policy bundles *inside the runtime*.

### New file: `src/policy/policy-registry.ts`

```ts
import { SignedPolicyBundle } from './policy-types';
import { verifyPolicyBundle } from './policy-signing';
import { KeyProvider } from '../runtime/keys';

export interface PolicyRegistry {
  put(signed: SignedPolicyBundle): { ok:true } | { ok:false; reason:string };
  get(tenantId: string, name: string): SignedPolicyBundle | undefined;
  list(tenantId: string): SignedPolicyBundle[];
}

export class InMemoryPolicyRegistry implements PolicyRegistry {
  private store = new Map<string, SignedPolicyBundle>();
  constructor(private keys: KeyProvider) {}

  private key(t: string, n: string) { return `${t}::${n}`; }

  put(signed: SignedPolicyBundle) {
    const v = verifyPolicyBundle(this.keys, signed);
    if (!v.ok) return v;

    const k = this.key(signed.bundle.tenantId, signed.bundle.name);
    this.store.set(k, signed);
    return { ok:true as const };
  }

  get(tenantId: string, name: string) {
    return this.store.get(this.key(tenantId, name));
  }

  list(tenantId: string) {
    return Array.from(this.store.values()).filter(p => p.bundle.tenantId === tenantId);
  }
}
```

---

## 10.4 Modify PDP to be driven by `PolicyBundle` (not hardcoded policy)

You already have a PDP concept in the earlier changes. Now make it evaluate `PolicyRule[]`.

### New file: `src/policy/pdp-bundle.ts`

```ts
import { PolicyBundle, PolicyRule } from './policy-types';
import { minimatch } from '../runtime/minimatch'; // or your existing
import { canonicalJson, sha256Hex } from '../runtime/crypto';

export interface PDPRequest {
  ctx: any;
  op: string;
  resource?: string;
  labels?: string[];
  purpose?: string;
  argsSummary?: any;
}

export interface PDPDecision {
  allow: boolean;
  policyHash: string;
  decisionHash: string;
  ruleIds: string[];
  obligations: any[];
  reason?: string;
}

export class BundlePDP {
  constructor(private bundle: PolicyBundle) {}

  decide(req: PDPRequest): PDPDecision {
    const applicable = this.bundle.rules
      .filter(r => r.op === req.op)
      .filter(r => ruleMatches(r, req))
      .sort((a,b) => b.priority - a.priority);

    // Default deny is typical for regulated. You can parameterize.
    const winning = applicable[0];
    const allow = winning ? winning.effect === 'allow' : false;

    const ruleIds = winning ? [winning.id] : [];
    const obligations = winning?.obligations ?? [];

    const decisionMaterial = {
      policyHash: this.bundle.policyHash,
      op: req.op,
      resource: req.resource ?? null,
      labels: req.labels ?? [],
      purpose: req.purpose ?? null,
      ruleIds,
      allow,
      obligations
    };

    return {
      allow,
      policyHash: this.bundle.policyHash,
      decisionHash: sha256Hex(canonicalJson(decisionMaterial)),
      ruleIds,
      obligations,
      reason: winning?.reasonCode ?? (allow ? undefined : 'DEFAULT_DENY')
    };
  }
}

function ruleMatches(rule: PolicyRule, req: PDPRequest): boolean {
  const w = rule.when;
  if (!w) return true;

  // Subject constraints
  if (w.subject) {
    const roles = req.ctx?.principal?.roles ?? [];
    const assurance = req.ctx?.principal?.assurance;
    if (w.subject.rolesAny && !roles.some((r:string)=>w.subject!.rolesAny!.includes(r))) return false;
    if (w.subject.rolesAll && !w.subject.rolesAll.every(r=>roles.includes(r))) return false;
    if (w.subject.assuranceAtLeast) {
      if (!assurance) return false;
      if (w.subject.assuranceAtLeast === 'aal3' && assurance !== 'aal3') return false;
    }
  }

  // Resource constraints
  if (w.resource) {
    const res = req.resource ?? '';
    if (w.resource.equals && res !== w.resource.equals) return false;
    if (w.resource.glob && !minimatch(res, w.resource.glob)) return false;
    if (w.resource.notGlob && minimatch(res, w.resource.notGlob)) return false;
  }

  // Label constraints
  if (w.labels) {
    const labels = req.labels ?? [];
    if (w.labels.any && !labels.some(l => w.labels!.any!.includes(l))) return false;
    if (w.labels.all && !w.labels.all.every(l => labels.includes(l))) return false;
    if (w.labels.none && labels.some(l => w.labels!.none!.includes(l))) return false;
  }

  // Purpose constraints
  if (w.purpose) {
    const p = req.purpose ?? '';
    if (w.purpose.equals && p !== w.purpose.equals) return false;
    if (w.purpose.anyOf && !w.purpose.anyOf.includes(p)) return false;
  }

  // Risk constraints
  if (w.riskTierAny) {
    const t = req.ctx?.riskTier;
    if (!t || !w.riskTierAny.includes(t)) return false;
  }

  return true;
}
```

**Net effect:** policy becomes a signed artifact; PDP is a deterministic evaluator.

---

## 10.5 Modify Session creation to load policy bundles by tenant

### Modify: `src/runtime/session-manager.ts`

* Add fields to SessionConfig:

  * `tenantId`
  * `policyName`
  * `policyVersion?` (optional, for pinning)
* On create:

  * registry.get(tenantId, policyName)
  * verify signature already done by registry
  * instantiate `BundlePDP` with bundle
  * store `policyHash` in session for auditing

This makes your “host controls the loop” story complete: the host selects an *approved policy artifact*.

---

## 10.6 Protocol handshake includes policy hash (enterprise auditors love this)

### Modify: `HelloResponse`

Add:

```ts
'policy-hash'?: string;
'policy-name'?: string;
'policy-version'?: string;
```

Now every client knows which governed policy is in force.

---

## 10.7 Tests to add

1. `policy-signing.test.ts`

* bundle finalization computes stable policyHash
* signature verify fails if any rule changes

2. `pdp-bundle.test.ts`

* priority ordering works
* default deny works
* obligations propagate

3. `integration/session-policy-hash-in-hello.test.ts`

* hello includes policy metadata

---

# Change Set 11 — Verified Publishing: A “No External Claims Without Fresh Evidence” Gate

You already have provenance and epistemic modes. What’s missing is a **hard gate** that prevents a report/answer from being “published” (returned to host / UI / API) unless:

* every claim is `observed` / `measured` / `derived`
* every cited evidence object is present
* every cited evidence object is **fresh** (fingerprint match)
* optional: redaction obligations satisfied
* optional: approvals satisfied (for regulated outputs)

This is the **Correctness-by-Construction** move: enforce at the boundary, not by convention.

## 11.1 Add a “VerifiedReport” envelope + signing

### New file: `src/reporting/verified-report.ts`

```ts
import { canonicalJson, sha256Hex } from '../runtime/crypto';
import { KeyProvider } from '../runtime/keys';

export type EpistemicMode = 'observed'|'measured'|'derived'|'inferred'|'hypothesized'|'assumed';

export interface ReportFinding {
  id: string;
  description: string;
  severity: 'critical'|'high'|'medium'|'low'|'info';
  mode: EpistemicMode;
  evidence: string[]; // EvidenceId[]
  location?: { file: string; line?: number };
}

export interface ReportBody {
  schemaVersion: 1;
  tenantId: string;
  sessionId: string;
  policyHash: string;

  title: string;
  summary: string;
  findings: ReportFinding[];

  // Full evidence registry ids used (for completeness checks)
  evidenceRegistry: string[];

  createdAt: string;
}

export interface VerifiedReport {
  body: ReportBody;
  reportHash: string;   // sha256(canonical body)
  signature: {
    keyId: string;
    alg: 'ed25519';
    sig: string;
  };
}

export function computeReportHash(body: ReportBody): string {
  return sha256Hex(canonicalJson(body));
}

export function signReport(keys: KeyProvider, body: ReportBody): VerifiedReport {
  const reportHash = computeReportHash(body);
  const keyId = keys.currentKeyId('audit');
  const sig = keys.sign(keyId, reportHash);
  return { body, reportHash, signature: { keyId, alg: 'ed25519', sig } };
}

export function verifyReport(keys: KeyProvider, r: VerifiedReport): boolean {
  const h = computeReportHash(r.body);
  if (h !== r.reportHash) return false;
  return keys.verify(r.signature.keyId, r.reportHash, r.signature.sig);
}
```

---

## 11.2 Add `PublishingGate` (Facade + Guard + Fail-Fast)

### New file: `src/reporting/publishing-gate.ts`

```ts
import { EvidenceRegistry } from '../provenance/evidence-registry';
import { World } from '../world/interface';
import { ReportBody, VerifiedReport, signReport } from './verified-report';
import { KeyProvider } from '../runtime/keys';

const ALLOWED_EXTERNAL_MODES = new Set(['observed','measured','derived']);

export class PublishingGate {
  constructor(
    private keys: KeyProvider,
    private world: World,
    private evidence: EvidenceRegistry
  ) {}

  verifyOrNeeds(body: ReportBody): { ok:true; report: VerifiedReport } | { ok:false; need: any } {
    // 1) Modes
    for (const f of body.findings) {
      if (!ALLOWED_EXTERNAL_MODES.has(f.mode)) {
        return { ok:false, need: { tag:'needs', needType:'needs-evidence', description:`Finding ${f.id} mode ${f.mode} not publishable`, context: f } };
      }
      if (!f.evidence?.length) {
        return { ok:false, need: { tag:'needs', needType:'needs-evidence', description:`Finding ${f.id} lacks evidence`, context: f } };
      }
    }

    // 2) Evidence existence + freshness
    for (const evId of body.evidenceRegistry) {
      const ev = this.evidence.get(evId);
      if (!ev) {
        return { ok:false, need: { tag:'needs', needType:'needs-evidence', description:`Missing evidence ${evId}`, context: { evidenceId: evId } } };
      }
      const vr = this.evidence.verify(evId, this.world); // you already have verifyAll; expose single verify
      if (!vr.valid) {
        return { ok:false, need: { tag:'needs', needType:'needs-evidence', description:`Stale evidence ${evId}`, context: vr } };
      }
    }

    // 3) Completeness: every registered evidence in body is cited by some finding
    const cited = new Set(body.findings.flatMap(f => f.evidence));
    for (const evId of body.evidenceRegistry) {
      if (!cited.has(evId)) {
        return { ok:false, need: { tag:'needs', needType:'needs-evidence', description:`Evidence ${evId} registered but not cited`, context: { evidenceId: evId } } };
      }
    }

    // 4) Sign
    return { ok:true, report: signReport(this.keys, body) };
  }
}
```

**Net effect:** “verified publishing” is a boundary object.preventing “theater” at the egress boundary.

---

## 11.3 Modify protocol outputs to optionally require VerifiedReport

In regulated environments you typically enforce: “agent results must be structured + signed”.

### Modify: `SessionPolicy` (or PolicyBundle obligations)

Add a policy obligation:

* `{ kind: 'require-verified-report' }` for ops that produce user-facing content (e.g., `op: 'eval'` when it returns report-like structures, or `op: 'report.publish'`).

Then introduce a new protocol op:

* `op: 'report.publish'` accepts a `ReportBody` (unsigned) and returns `VerifiedReport` or `NeedsOutcome`.

This creates a clean CQRS split:

* **Command side:** session executes analysis, accumulates evidence
* **Query/publish side:** PublishingGate verifies + signs

---

## 11.4 Lisp surface: `(report/publish ...)` special form or stdlib function

### New file: `stdlib/report.lisp`

```lisp
(ns lambdallm.report
  (:require [lambdallm.core :refer [ffi]]))

(define (report/publish report-body)
  ;; Delegates to protocol/ffi boundary that runs PublishingGate
  (ffi.call "report.publish" (list report-body)))
```

---

## 11.5 Tests to add

1. `publishing-gate.test.ts`

* rejects inferred/hypothesized
* rejects missing evidence
* rejects stale evidence
* signs successful reports and verifyReport passes

2. `integration/report-publish-roundtrip.test.ts`

* session generates evidence + report body
* publish returns VerifiedReport
* signature verifies

---

# Change Set 12 — Assurance Cases (GSN-style): “Argumentation Graph” for Legal/Medical Trust

Logs and reports are necessary but not recognized as a **safety case**. Regulated orgs want:

* a structured argument: *Goals → Strategies → Solutions (evidence)*
* traceability from top-level claims down to evidence
* explicit assumptions and contexts

This is effectively a **Goal Structuring Notation (GSN)** or a minimal SACM-like structure.

You already have the building blocks (Evidence + ProvenanceGraph). This change makes an explicit **AssuranceCase** artifact, with machine-checkable semantics.

## 12.1 Add `AssuranceCase` schema

### New file: `src/assurance/assurance-case.ts`

```ts
import { canonicalJson, sha256Hex } from '../runtime/crypto';

export type NodeId = string;

export type CaseNode =
  | { kind:'goal'; id: NodeId; statement: string; context?: string[]; assumptions?: string[]; supportedBy: NodeId[] }
  | { kind:'strategy'; id: NodeId; statement: string; supportedBy: NodeId[] }
  | { kind:'solution'; id: NodeId; statement: string; evidence: string[] } // EvidenceIds
  | { kind:'context'; id: NodeId; statement: string }
  | { kind:'assumption'; id: NodeId; statement: string };

export interface AssuranceCase {
  schemaVersion: 1;
  tenantId: string;
  sessionId: string;
  policyHash: string;

  title: string;
  topGoalId: NodeId;

  nodes: CaseNode[];

  createdAt: string;
  caseHash: string; // sha256(canonical form of case without caseHash)
}

export function computeCaseHash(c: Omit<AssuranceCase,'caseHash'>): string {
  return sha256Hex(canonicalJson(c));
}
```

---

## 12.2 Add `AssuranceChecker` (graph validation + evidence freshness)

### New file: `src/assurance/assurance-checker.ts`

```ts
import { AssuranceCase, CaseNode } from './assurance-case';
import { EvidenceRegistry } from '../provenance/evidence-registry';
import { World } from '../world/interface';

export interface CaseCheckResult {
  valid: boolean;
  errors: string[];
}

export class AssuranceChecker {
  constructor(private evidence: EvidenceRegistry, private world: World) {}

  check(c: AssuranceCase): CaseCheckResult {
    const errors: string[] = [];
    const byId = new Map<string, CaseNode>();

    for (const n of c.nodes) {
      if ((byId as any).has(n.id)) errors.push(`Duplicate node id ${n.id}`);
      byId.set(n.id, n);
    }

    // Reachability from top goal
    const visited = new Set<string>();
    const dfs = (id: string) => {
      if (visited.has(id)) return;
      visited.add(id);
      const n = byId.get(id);
      if (!n) { errors.push(`Missing node ${id}`); return; }
      if ((n as any).supportedBy) {
        for (const child of (n as any).supportedBy as string[]) dfs(child);
      }
    };
    dfs(c.topGoalId);

    // Ensure all nodes reachable (optional strictness)
    for (const n of c.nodes) {
      if (!visited.has(n.id)) errors.push(`Unreachable node ${n.id}`);
    }

    // Evidence checks for solutions
    for (const n of c.nodes) {
      if (n.kind === 'solution') {
        if (!n.evidence?.length) errors.push(`Solution ${n.id} missing evidence`);
        for (const evId of n.evidence) {
          const ev = this.evidence.get(evId);
          if (!ev) { errors.push(`Missing evidence ${evId} for solution ${n.id}`); continue; }
          const vr = this.evidence.verify(evId, this.world);
          if (!vr.valid) errors.push(`Stale evidence ${evId} for solution ${n.id}`);
        }
      }
    }

    return { valid: errors.length === 0, errors };
  }
}
```

**Net effect:** you can produce a *machine-checkable* safety/assurance case where the only admissible “solutions” are evidence-backed and fresh.

---

## 12.3 Add optional signing for assurance cases (“certificate” concept)

You already started “certificates” in the provenance doc as an integration concept. Now formalize it.

### New file: `src/assurance/assurance-signing.ts`

```ts
import { AssuranceCase, computeCaseHash } from './assurance-case';
import { KeyProvider } from '../runtime/keys';

export interface SignedAssuranceCase {
  case: AssuranceCase;
  signature: { keyId: string; alg:'ed25519'; sig: string };
}

export function finalizeCase(c: Omit<AssuranceCase,'caseHash'>): AssuranceCase {
  return { ...c, caseHash: computeCaseHash(c) };
}

export function signCase(keys: KeyProvider, c: AssuranceCase): SignedAssuranceCase {
  const keyId = keys.currentKeyId('audit');
  const sig = keys.sign(keyId, c.caseHash);
  return { case: c, signature: { keyId, alg:'ed25519', sig } };
}

export function verifyCase(keys: KeyProvider, s: SignedAssuranceCase): boolean {
  const h = computeCaseHash((() => {
    const { caseHash, ...rest } = s.case as any;
    return rest;
  })());
  if (h !== s.case.caseHash) return false;
  return keys.verify(s.signature.keyId, s.case.caseHash, s.signature.sig);
}
```

---

## 12.4 Protocol ops: `assurance.check`, `assurance.sign`, `assurance.export`

* `assurance.check` returns structured errors
* `assurance.sign` checks then signs
* `assurance.export` returns JSON (or an artifact handle)

This is very “enterprise integration patterns”-friendly: downstream GRC tools ingest these.

---

## 12.5 Tests to add

* `assurance-checker.test.ts`:

  * fails on missing nodes
  * fails on unreachable nodes
  * fails on stale evidence
  * passes for valid case

* `integration/assurance-signing.test.ts`:

  * sign only after successful check
  * signature verify passes

---

# Change Set 13 — Export Pipelines: SIEM/GRC/EMR Integration with Idempotent Delivery

An enterprise runtime doesn’t just *have* audit logs; it must **emit them** to external systems reliably, without duplication, and with tamper evidence.

This is where Enterprise Integration Patterns matter:

* **Outbox pattern** (write local event + mark deliverable)
* **Idempotent Consumer**
* **At-least-once delivery** with **dedupe keys**
* **Dead letter queue** semantics for failures

## 13.1 Add an “Outbox” for audit/journal/report/case exports

### New file: `src/export/outbox.ts`

```ts
export type OutboxItemKind = 'audit'|'journal'|'verified-report'|'assurance-case';

export interface OutboxItem {
  id: string;            // uuid
  kind: OutboxItemKind;
  tenantId: string;
  sessionId: string;
  createdAt: string;

  // dedupe key for idempotent downstream consumers
  dedupeKey: string;     // e.g. sha256(payloadHash + destinationId)

  payloadRef: string;    // points to artifact storage or inline small payload
  payloadHash: string;   // sha256 of payload
  status: 'pending'|'sent'|'failed';
  attempts: number;
  lastError?: string;
}

export interface OutboxStore {
  put(item: OutboxItem): void;
  nextPending(limit: number): OutboxItem[];
  markSent(id: string): void;
  markFailed(id: string, err: string): void;
}
```

Implement with an in-memory version (tests) and a file/db-backed version (prod).

---

## 13.2 Add `Exporter` interface + destinations

### New file: `src/export/exporter.ts`

```ts
import { OutboxItem } from './outbox';

export interface ExportDestination {
  id: string;
  kind: 'http'|'s3'|'syslog'|'kafka'|'splunk-hec';
  send(item: OutboxItem, payload: Uint8Array): Promise<void>;
}

export class Exporter {
  constructor(private outbox: any, private vault: any, private destinations: ExportDestination[]) {}

  async flush(limit = 50): Promise<void> {
    const pending = this.outbox.nextPending(limit);
    for (const item of pending) {
      try {
        const payload = await loadPayload(item.payloadRef); // artifact store read
        await this.destinations[0].send(item, payload);      // or fanout
        this.outbox.markSent(item.id);
      } catch (e) {
        this.outbox.markFailed(item.id, (e as Error).message);
      }
    }
  }
}
```

**Key:** you now have a reliable “delivery subsystem” separated from runtime execution (Separation of Concerns + Ports/Adapters).

---

## 13.3 Add policy controls for export

In PolicyBundle, add rules for:

* `op: export.send`
* resource is destination id
* labels gate (e.g., disallow PHI exports to non-HIPAA destinations)
* require approvals for certain exports

Now “report publishing” and “assurance signing” can stage an outbox item; a host flush can require approvals.

---

## 13.4 Tests to add

* `outbox-idempotency.test.ts`: same dedupeKey doesn’t produce duplicate sends (simulate downstream idempotency)
* `exporter-retries.test.ts`: failed sends reattempt with attempts counter increments

---

# Change Set 14 — Model Governance: Inventory, Contracts, and Evaluation Gates

Regulated orgs will ask:

* “Which model version was used?”
* “Is that model approved for PHI?”
* “Do we have a contract / BAA / DPA for that endpoint?”
* “Do we have eval coverage and drift monitoring?”

You don’t need to implement the whole governance universe, but you *must* provide the core abstractions so policy can enforce them.

## 14.1 Add `ModelRegistry` (Repository pattern)

### New file: `src/llm/model-registry.ts`

```ts
export interface ModelRecord {
  id: string;                 // "openai:gpt-4.1" etc.
  vendor: string;
  family: string;
  version?: string;

  allowedLabels: string[];    // e.g. ["public"] or ["pii"] or ["phi"] if contract exists
  allowedPurposes?: string[]; // optional

  contract: {
    hipaaEligible?: boolean;
    dpaSigned?: boolean;
    dataRetention?: 'none'|'30d'|'vendor-default';
    trainingOptOut?: boolean;
    region?: string;
  };

  createdAt: string;
  deprecatedAt?: string;
}

export interface ModelRegistry {
  get(id: string): ModelRecord | undefined;
  list(): ModelRecord[];
}
```

---

## 14.2 Modify EgressGate to consult ModelRegistry

### Modify: `src/security/egress-gate.ts`

Add `modelRegistry` dependency and enforce:

* if labels include `phi` then require `contract.hipaaEligible === true`
* if labels include `credentials/secrets` then hard deny
* if model is deprecated, require approval

This becomes a policy obligation or a hard guard. In regulated environments, make it a hard guard.

---

## 14.3 Add evaluation gate hooks (Policy-driven)

Introduce an op like:

* `op: 'llm.model.use'` resource is model id

Policy can then enforce:

* only approved models for tenant
* require approvals if risk tier is regulated

Also add a metadata field to LLM calls:

* `modelId`
* `requestId`
* `policyHash`
* `decisionHash`

So your audit/journal can later attribute which model produced what.

---

## 14.4 Tests

* `model-registry-egress.test.ts`: PHI prompt blocked for non-eligible model
* `deprecated-model-requires-approval.test.ts`

---

# Implementation Order (continuation)

To keep this implementable without thrash:

1. **PolicyBundle types + signing + PolicyRegistry**
2. **BundlePDP** and wire Session creation to load bundle
3. **Handshake adds policy metadata**
4. **PublishingGate + VerifiedReport signing + report.publish op**
5. **AssuranceCase schema + checker + signing + protocol ops**
6. **Outbox + Exporter skeleton** (no real destinations needed yet)
7. **ModelRegistry + integrate into EgressGate**

---

# The “trust ceiling” after these changes

With these additions, you can credibly claim (in the technical/legal sense) that the runtime supports:

* **Governed policy** (signed, versioned, hash-stable)
* **Non-theatrical publishing** (external outputs must be evidence-grounded and fresh)
* **Assurance argumentation** (auditor-readable + machine-checkable)
* **Operational integration** (export with idempotency + outbox semantics)
* **Model governance** (contractual eligibility and label gating)

This still doesn’t magically make *every* use case acceptable in medicine/law (because those domains also require organizational controls, clinician oversight, validation studies, SOPs, etc.), but it gets you **as far as an agent runtime can get** purely on the software architecture axis.

If you want me to keep going, the next best “incremental but high leverage” change set is:

* **Change Set 15 — Deterministic Prompt/Context Canonicalization + Secure Prompt Templates**

  * eliminate ambient prompt injection surfaces by making prompts structured ASTs with typed slots, then rendering with a canonical template engine
  * enforce allowlisted template identifiers by policy
  * journal the *template id + slot hashes*, not raw text

Say “continue” again and I’ll write that as changes (files + protocol + tests) in the same style.
