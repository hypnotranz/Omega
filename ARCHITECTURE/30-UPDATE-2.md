Alright — I’m going to continue by **fully specifying the next implementation chunks** in the order that gives you maximum “enterprise-grade trust surface area” early, with minimal architectural churn.

Given your prior stack, the highest leverage sequence is:

1. **Policy bundles + PDP + obligations** (so you can express “regulated mode”, approvals, redaction, and deny-egress as *policy*, not ad‑hoc ifs)
2. **Tamper‑evident, signed audit ledger** (so every decision and egress is defensible)
3. **Hard isolation runner (WorkerRunner) + kill semantics** (so “sandbox” is a containment boundary, not just counters)

I’ll write this as **changes**: new modules, modified interfaces, and test deltas. I will also keep Layer 0/1 pure.

---

# Change Set 2 Deepening — Policy Bundles + Local PDP + Obligations

You already have `SessionPolicy` which is a **static whitelist/denylist**. This is necessary but insufficient for real enterprise: you need a **portable, versioned, auditable policy bundle** + a **PDP** that returns **decisions + obligations**.

This adds a *policy plane* without contaminating the evaluator.

## 2.1 Add a canonical PolicyBundle with a deterministic hash

### New file: `src/policy/policy-bundle.ts`

```ts
import { sha256Hex, canonicalJson } from '../runtime/crypto';

export type CombineAlgorithm = 'deny-overrides' | 'permit-overrides' | 'first-applicable';

export interface PolicyBundleMeta {
  id: string;              // "tenantA/policy"
  version: string;         // "12" or semver
  issuedAt: string;        // ISO
  issuer: string;          // "policy-service" | "admin:alice"
  description?: string;
}

export interface PolicyBundle {
  meta: PolicyBundleMeta;
  combine: CombineAlgorithm;
  defaults: {
    effect: 'allow' | 'deny';
  };
  rules: PolicyRule[];
  hash: string;            // sha256(canonical bundle without hash)
}

export interface PolicyRule {
  id: string;
  priority: number;        // higher wins in first-applicable / obligation resolution
  effect: 'allow' | 'deny';

  match: PolicyMatch;

  obligations?: PolicyObligation[];
  advice?: PolicyAdvice[];
}

export interface PolicyMatch {
  // operation match
  ops?: string[];          // exact op names, e.g. ["world.read","llm.complete"]
  opPrefixes?: string[];   // e.g. ["world.","llm."]

  // resource match (path/host/model/etc)
  resourceGlobs?: string[]; // e.g. ["/workspace/**","!**/.env"]

  // identity match
  rolesAny?: string[];
  rolesAll?: string[];
  subjectAny?: string[];

  // risk posture
  riskTierAny?: Array<'low'|'moderate'|'high'|'regulated'>;

  // data governance
  labelsAny?: string[];    // ["phi","pii"]
  purposeAny?: string[];   // e.g. ["clinical-decision-support"]
}

export type PolicyObligation =
  | { kind: 'require-approval'; policy: ApprovalPolicy }
  | { kind: 'require-redaction'; profile: RedactionProfileId }
  | { kind: 'limit-budget'; tokens?: number; cost?: number; timeMs?: number }
  | { kind: 'force-sandbox'; runner: 'worker'|'process'|'container' }
  | { kind: 'log'; level: 'security'|'compliance' }
  | { kind: 'deny-egress'; message: string };

export type PolicyAdvice =
  | { kind: 'tag'; key: string; value: string };

export type RedactionProfileId = 'default'|'medical-strict'|'legal-privileged'|'finance-pci';

export interface ApprovalPolicy {
  requiredApprovals: number;    // 1 or 2 for four-eyes
  allowedRoles: string[];
  maxAgeMs: number;
  minAssurance?: 'aal2'|'aal3';
}

// Deterministic hash: remove hash field, canonicalize JSON, sha256
export function computePolicyHash(bundle: Omit<PolicyBundle, 'hash'>): string {
  const canon = canonicalJson(bundle);
  return sha256Hex(canon);
}

export function finalizePolicyBundle(bundle: Omit<PolicyBundle,'hash'>): PolicyBundle {
  return { ...bundle, hash: computePolicyHash(bundle) };
}
```

### Required support: canonical JSON

If you don’t already have canonical JSON for evidence IDs, add it now (you will reuse it for audit ledger hashing).

#### New file: `src/runtime/crypto.ts`

```ts
import { createHash, createHmac, timingSafeEqual } from 'crypto';

// Keep minimal; later swap to WebCrypto in browser builds if needed.

export function sha256Hex(s: string): string {
  return createHash('sha256').update(s).digest('hex');
}

export function hmacSha256Hex(key: Uint8Array, s: string): string {
  return createHmac('sha256', key).update(s).digest('hex');
}

export function timingSafeEqualHex(a: string, b: string): boolean {
  const ab = Buffer.from(a, 'hex');
  const bb = Buffer.from(b, 'hex');
  if (ab.length !== bb.length) return false;
  return timingSafeEqual(ab, bb);
}

// Minimal deterministic canonicalization: recursively sort object keys.
export function canonicalJson(value: unknown): string {
  return JSON.stringify(canonicalize(value));
}

function canonicalize(v: any): any {
  if (v === null) return null;
  if (Array.isArray(v)) return v.map(canonicalize);
  if (typeof v === 'object') {
    const keys = Object.keys(v).sort();
    const out: Record<string, any> = {};
    for (const k of keys) out[k] = canonicalize(v[k]);
    return out;
  }
  // number/string/boolean
  return v;
}
```

### Test additions

* `test/unit/policy-bundle-hash.test.ts`

  * same object → same hash regardless of key insertion order
  * modifying any rule changes hash

---

## 2.2 Add PDP interface + Decision hashing (for audit + journal)

### New file: `src/policy/pdp.ts`

```ts
import { ExecutionContext } from '../runtime/context';
import { PolicyBundle, PolicyObligation, PolicyAdvice } from './policy-bundle';
import { sha256Hex, canonicalJson } from '../runtime/crypto';

export interface PolicyInput {
  ctx: ExecutionContext;
  op: string;
  resource?: string;
  argsSummary?: unknown;     // MUST be minimized/redacted
  labels?: string[];         // data labels (pii/phi/etc)
  purpose?: string;
}

export interface PolicyDecision {
  allow: boolean;
  reason: string;
  obligations: PolicyObligation[];
  advice: PolicyAdvice[];

  // Deterministic decision hash for audit/replay
  decisionHash: string;
  policyHash: string;
  ruleIds: string[];         // rules that contributed
}

export interface PDP {
  readonly bundle: PolicyBundle;
  decide(input: PolicyInput): PolicyDecision;
}

export function hashDecision(dec: Omit<PolicyDecision,'decisionHash'>): string {
  return sha256Hex(canonicalJson(dec));
}
```

---

## 2.3 Implement LocalPDP with combining algorithm + obligation resolution lattice

### New file: `src/policy/pdp-local.ts`

```ts
import { minimatch } from 'minimatch';
import { PDP, PolicyInput, PolicyDecision, hashDecision } from './pdp';
import { PolicyBundle, PolicyRule, PolicyObligation, RedactionProfileId } from './policy-bundle';

export class LocalPDP implements PDP {
  constructor(public readonly bundle: PolicyBundle) {}

  decide(input: PolicyInput): PolicyDecision {
    const applicable = this.bundle.rules
      .filter(r => matches(r, input))
      .sort((a,b) => b.priority - a.priority);

    // Combining algorithm: default deny-overrides (enterprise sane)
    const combine = this.bundle.combine;

    let allow = this.bundle.defaults.effect === 'allow';
    let reason = `default:${this.bundle.defaults.effect}`;
    let contributing: PolicyRule[] = [];

    if (applicable.length > 0) {
      if (combine === 'first-applicable') {
        const r = applicable[0];
        allow = r.effect === 'allow';
        reason = `${r.id}:${r.effect}`;
        contributing = [r];
      } else if (combine === 'deny-overrides') {
        const denies = applicable.filter(r => r.effect === 'deny');
        if (denies.length > 0) {
          allow = false;
          contributing = denies;
          reason = `deny-overrides:${denies.map(d=>d.id).join(',')}`;
        } else {
          allow = true;
          contributing = applicable.filter(r => r.effect === 'allow');
          reason = `allow:${contributing.map(r=>r.id).join(',')}`;
        }
      } else if (combine === 'permit-overrides') {
        const allows = applicable.filter(r => r.effect === 'allow');
        if (allows.length > 0) {
          allow = true;
          contributing = allows;
          reason = `permit-overrides:${allows.map(a=>a.id).join(',')}`;
        } else {
          allow = false;
          contributing = applicable.filter(r => r.effect === 'deny');
          reason = `deny:${contributing.map(r=>r.id).join(',')}`;
        }
      }
    }

    // Merge obligations/advice deterministically from contributing rules
    const obligations = mergeObligations(contributing.flatMap(r => r.obligations ?? []));
    const advice = contributing.flatMap(r => r.advice ?? []);

    const base: Omit<PolicyDecision,'decisionHash'> = {
      allow,
      reason,
      obligations,
      advice,
      policyHash: this.bundle.hash,
      ruleIds: contributing.map(r => r.id),
    };

    return { ...base, decisionHash: hashDecision(base) };
  }
}

function matches(rule: PolicyRule, input: PolicyInput): boolean {
  const m = rule.match;
  const { ctx } = input;

  // op
  const opOk =
    (!m.ops && !m.opPrefixes) ||
    (m.ops?.includes(input.op) ?? false) ||
    (m.opPrefixes?.some(p => input.op.startsWith(p)) ?? false);

  if (!opOk) return false;

  // resource globs
  if (m.resourceGlobs && m.resourceGlobs.length > 0) {
    const res = input.resource ?? '';
    // allow negations via "!pattern" (like your security doc allowedPaths example)
    let allowed = false;
    for (const g of m.resourceGlobs) {
      if (g.startsWith('!')) {
        if (minimatch(res, g.slice(1))) return false;
      } else {
        if (minimatch(res, g)) allowed = true;
      }
    }
    if (!allowed) return false;
  }

  // roles
  if (m.rolesAny && m.rolesAny.length > 0) {
    if (!ctx.principal.roles.some(r => m.rolesAny!.includes(r))) return false;
  }
  if (m.rolesAll && m.rolesAll.length > 0) {
    if (!m.rolesAll.every(r => ctx.principal.roles.includes(r))) return false;
  }

  // subject
  if (m.subjectAny && m.subjectAny.length > 0) {
    if (!m.subjectAny.includes(ctx.principal.subject)) return false;
  }

  // risk tier
  if (m.riskTierAny && m.riskTierAny.length > 0) {
    if (!m.riskTierAny.includes(ctx.riskTier)) return false;
  }

  // labels/purpose
  if (m.labelsAny && m.labelsAny.length > 0) {
    const labels = input.labels ?? [];
    if (!labels.some(l => m.labelsAny!.includes(l))) return false;
  }
  if (m.purposeAny && m.purposeAny.length > 0) {
    const p = input.purpose ?? ctx.purpose ?? '';
    if (!m.purposeAny.includes(p)) return false;
  }

  return true;
}

// Obligation merge: deterministically “strictest wins” for some kinds
function mergeObligations(obs: PolicyObligation[]): PolicyObligation[] {
  const out: PolicyObligation[] = [];

  // limit-budget: pick minimum across contributors (most restrictive)
  const limits = obs.filter(o => o.kind === 'limit-budget') as Array<Extract<PolicyObligation,{kind:'limit-budget'}>>;
  if (limits.length > 0) {
    out.push({
      kind: 'limit-budget',
      tokens: minDefined(limits.map(x=>x.tokens)),
      cost: minDefined(limits.map(x=>x.cost)),
      timeMs: minDefined(limits.map(x=>x.timeMs)),
    });
  }

  // require-redaction: choose strictest profile
  const reds = obs.filter(o => o.kind === 'require-redaction') as Array<Extract<PolicyObligation,{kind:'require-redaction'}>>;
  if (reds.length > 0) {
    out.push({ kind: 'require-redaction', profile: strictestRedaction(reds.map(r=>r.profile)) });
  }

  // require-approval: max requiredApprovals, intersection-ish roles (or keep union; choose your governance stance)
  const apps = obs.filter(o => o.kind === 'require-approval') as Array<Extract<PolicyObligation,{kind:'require-approval'}>>;
  if (apps.length > 0) {
    const requiredApprovals = Math.max(...apps.map(a => a.policy.requiredApprovals));
    const maxAgeMs = Math.min(...apps.map(a => a.policy.maxAgeMs));
    const allowedRoles = union(apps.flatMap(a => a.policy.allowedRoles));
    const minAssurance = strictestAssurance(apps.map(a=>a.policy.minAssurance).filter(Boolean) as any);

    out.push({ kind:'require-approval', policy:{ requiredApprovals, maxAgeMs, allowedRoles, minAssurance }});
  }

  // deny-egress: if any, keep it (deny overrides should already block, but obligation is useful for message)
  for (const d of obs.filter(o => o.kind === 'deny-egress')) out.push(d);

  // force-sandbox: choose strongest (container > process > worker)
  const fs = obs.filter(o => o.kind === 'force-sandbox') as Array<Extract<PolicyObligation,{kind:'force-sandbox'}>>;
  if (fs.length > 0) out.push({ kind:'force-sandbox', runner: strongestRunner(fs.map(x=>x.runner)) });

  // log: choose strongest (compliance > security)
  const logs = obs.filter(o => o.kind === 'log') as Array<Extract<PolicyObligation,{kind:'log'}>>;
  if (logs.length > 0) out.push({ kind:'log', level: logs.some(l=>l.level==='compliance') ? 'compliance' : 'security' });

  return out;
}

function minDefined<T extends number>(vals: Array<T|undefined>): T|undefined {
  const xs = vals.filter((v): v is T => typeof v === 'number');
  if (xs.length === 0) return undefined;
  return xs.reduce((a,b)=> Math.min(a,b)) as T;
}

function union(xs: string[]): string[] {
  return Array.from(new Set(xs)).sort();
}

function strictestRedaction(ps: RedactionProfileId[]): RedactionProfileId {
  const order: RedactionProfileId[] = ['default','finance-pci','legal-privileged','medical-strict'];
  return ps.reduce((a,b)=> order.indexOf(a) >= order.indexOf(b) ? a : b, 'default');
}

function strictestAssurance(xs: Array<'aal2'|'aal3'>): 'aal2'|'aal3'|undefined {
  if (xs.length === 0) return undefined;
  return xs.includes('aal3') ? 'aal3' : 'aal2';
}

function strongestRunner(rs: Array<'worker'|'process'|'container'>): 'worker'|'process'|'container' {
  const order = ['worker','process','container'] as const;
  return rs.reduce((a,b)=> order.indexOf(a) >= order.indexOf(b) ? a : b, 'worker');
}
```

### Tests to add

* `test/unit/pdp-local.test.ts`

  * deny-overrides works
  * obligations merge is deterministic and “most restrictive wins”
  * resource glob negation `!**/.env` denies

---

## 2.4 Modify SessionEnforcer: from “static allow/deny” to PEP + obligations

You will **keep** `SessionEnforcer.checkOperation` as the fast path gate, but you add a second stage:

* Build `PolicyInput`
* `decision = pdp.decide(input)`
* If deny: produce `ErrorOutcome` or `NeedsOutcome` based on obligation (usually error)
* If allow: enforce obligations (budget clamp, require approval => `needs`, force-sandbox => runner selection, etc.)

### Modify: `src/runtime/session-enforcer.ts` (existing)

**Add injected PDP** and make enforcement return a structured *verdict* rather than just throw.

#### New file: `src/runtime/enforcement.ts`

```ts
import { PolicyDecision } from '../policy/pdp';

export type EnforcementVerdict =
  | { tag: 'allow'; decision: PolicyDecision }
  | { tag: 'needs'; decision: PolicyDecision; needType: string; description: string; context?: unknown }
  | { tag: 'deny';  decision: PolicyDecision; reason: string };
```

#### Update enforcer to produce verdict

```ts
import { PDP } from '../policy/pdp';
import { EnforcementVerdict } from './enforcement';

export class SessionEnforcer {
  constructor(private staticPolicy: SessionPolicy, private pdp: PDP) {}

  decide(op: string, args: unknown[], ctx: ExecutionContext): EnforcementVerdict {
    // 1) existing static checks (whitelist/denylist, world rules quick checks)
    this.checkOperationStatic(op, args);

    // 2) Build policy input with *minimized* args summary (never raw content)
    const { resource, argsSummary, labels, purpose } = summarizeForPolicy(op, args, ctx);

    const decision = this.pdp.decide({ ctx, op, resource, argsSummary, labels, purpose });

    if (!decision.allow) {
      return { tag: 'deny', decision, reason: decision.reason };
    }

    // 3) Obligations that block become needs outcomes
    const approvalOb = decision.obligations.find(o => o.kind === 'require-approval');
    if (approvalOb && approvalOb.kind === 'require-approval') {
      return {
        tag: 'needs',
        decision,
        needType: 'needs-approval',
        description: `Operation ${op} requires approval`,
        context: { op, resource, approvalPolicy: approvalOb.policy, policyHash: decision.policyHash, ruleIds: decision.ruleIds }
      };
    }

    return { tag: 'allow', decision };
  }

  private checkOperationStatic(op: string, args: unknown[]) {
    // your existing checkOperation logic moved here unchanged
  }
}
```

#### New helper: policy summarization (data minimization)

### New file: `src/runtime/policy-summarize.ts`

```ts
import { sha256Hex } from './crypto';
import { ExecutionContext } from './context';

export function summarizeForPolicy(op: string, args: unknown[], ctx: ExecutionContext): {
  resource?: string;
  argsSummary?: unknown;
  labels?: string[];
  purpose?: string;
} {
  // DO NOT include raw code, file contents, prompts.
  // Prefer size + fingerprint + type.

  // Resource extraction is operation-specific
  if (op === 'world.read' || op === 'world.write' || op === 'world.fingerprint') {
    const ref = String(args[0] ?? '');
    return {
      resource: ref,
      argsSummary: { ref, argCount: args.length },
      labels: (args as any).labels,
      purpose: ctx.purpose,
    };
  }

  if (op === 'llm.complete') {
    const prompt = String(args[0] ?? '');
    const model = typeof (args[1] as any)?.model === 'string' ? (args[1] as any).model : undefined;
    return {
      resource: model ?? 'default',
      argsSummary: { promptChars: prompt.length, promptHash: sha256Hex(prompt), model },
      labels: (args[1] as any)?.dataLabels,
      purpose: (args[1] as any)?.purpose ?? ctx.purpose,
    };
  }

  // default
  return { argsSummary: { argCount: args.length }, purpose: ctx.purpose };
}
```

### Modify Session execution path

In `executeTurn`:

* before executing an FFI op, call `enforcer.decide(...)`
* if verdict is `needs`, return `NeedsOutcome` immediately
* if `deny`, return `ErrorOutcome` (policy-violation)
* if `allow`, apply obligations (budget clamp, force-sandbox selection, log level)

### Tests

* `test/integration/policy-needs-approval.test.ts`

  * policy marks `world.write` as require-approval => session returns `(needs ...)`
* `test/integration/policy-budget-clamp.test.ts`

  * policy limits tokens/time for `llm.complete`, ensure adapter enforces

---

## 2.5 Add a default “Enterprise baseline” policy bundle

You should ship a baseline policy that aligns with the typical enterprise expectation:

* `regulated` risk tier:

  * deny all `network` by default
  * deny `llm.complete` unless redaction strict + approved models
  * require approval for `world.write`
  * force WorkerRunner (or stronger) for all untrusted eval

### New file: `src/policy/bundles/enterprise-baseline.ts`

```ts
import { finalizePolicyBundle } from '../policy-bundle';

export const ENTERPRISE_BASELINE = finalizePolicyBundle({
  meta: {
    id: 'baseline/enterprise',
    version: '1',
    issuedAt: new Date().toISOString(),
    issuer: 'lambdallm',
    description: 'Secure-by-default baseline policy',
  },
  combine: 'deny-overrides',
  defaults: { effect: 'deny' },
  rules: [
    {
      id: 'allow-world-read-workspace',
      priority: 100,
      effect: 'allow',
      match: { ops: ['world.read','world.list','world.fingerprint'], resourceGlobs: ['/workspace/**','!**/.env'] },
      obligations: [{ kind:'log', level:'security' }]
    },
    {
      id: 'deny-world-read-outside',
      priority: 99,
      effect: 'deny',
      match: { opPrefixes: ['world.'], resourceGlobs: ['/**'] }, // will hit everything else
      obligations: [{ kind:'log', level:'compliance' }]
    },
    {
      id: 'require-approval-for-write-high',
      priority: 200,
      effect: 'allow',
      match: { ops: ['world.write'], riskTierAny: ['high','regulated'] },
      obligations: [
        { kind:'require-approval', policy:{ requiredApprovals: 1, allowedRoles:['admin','security-reviewer'], maxAgeMs: 60*60*1000 } },
        { kind:'log', level:'compliance' }
      ]
    },
    {
      id: 'llm-complete-medical-strict',
      priority: 300,
      effect: 'allow',
      match: { ops: ['llm.complete'], riskTierAny:['regulated'], labelsAny:['phi','pii'] },
      obligations: [
        { kind:'require-redaction', profile:'medical-strict' },
        { kind:'limit-budget', tokens: 2000, timeMs: 10_000 },
        { kind:'log', level:'compliance' }
      ]
    },
  ]
});
```

This makes “trust posture” **declarative**.

---

# Change Set 4 Deepening — Tamper‑Evident, Signed Audit Ledger

Your existing audit log is mutable and unverifiable. The next change is to implement a **hash‑chained, signed ledger**. This is a standard pattern: *tamper‑evident log* (hash chain) + *digital signature* (non-repudiation).

## 4.1 Add AuditEnvelope schema with canonical hashing

### New file: `src/runtime/audit-ledger.ts`

(You already have the high-level from my prior message; here is the “production-leaning” delta.)

Key improvements over a naive implementation:

* include `keyId` so you can rotate signing keys
* include `policyHash` and `decisionHash` always
* include an explicit `schemaVersion`
* include an explicit `integrity` block to avoid accidental omission
* store **only minimized summaries** (never raw prompt/code/file contents)

```ts
import { ExecutionContext } from './context';
import { canonicalJson, sha256Hex } from './crypto';

export interface AuditSigner {
  keyId(): string;
  sign(hashHex: string): string;
  verify(hashHex: string, signature: string): boolean;
}

export interface AuditPayload {
  op: string;
  resource?: string;

  argsSummary?: unknown;
  resultSummary?: unknown;

  // enforcement
  policyHash: string;
  decisionHash?: string;
  ruleIds?: string[];

  // outcomes
  outcomeTag?: string;            // ok|proposed|needs|error|denied

  // provenance ties
  evidenceIds?: string[];

  // governance ties
  proposalsHash?: string;

  // classification
  labels?: string[];
  purpose?: string;
}

export interface AuditEnvelope {
  schemaVersion: number;

  tenantId: string;
  sessionId: string;
  turnNumber: number;
  timestamp: string;

  integrity: {
    prevHash: string;
    payloadHash: string;
    envelopeHash: string;
    signature: string;
    keyId: string;
  };

  payload: AuditPayload;
}

export class AuditLedger {
  private prevHash = '0'.repeat(64);
  private envelopes: AuditEnvelope[] = [];

  constructor(private signer: AuditSigner) {}

  append(ctx: ExecutionContext, payload: Omit<AuditPayload,'policyHash'> & Partial<Pick<AuditPayload,'policyHash'>>): AuditEnvelope {
    const normalized: AuditPayload = {
      ...payload,
      policyHash: payload.policyHash ?? ctx.policy.hash,
      purpose: payload.purpose ?? ctx.purpose,
    };

    const payloadHash = sha256Hex(canonicalJson(normalized));

    // Hash only the immutable header + payloadHash to avoid signature instability
    const header = {
      schemaVersion: 1,
      tenantId: ctx.principal.tenantId,
      sessionId: ctx.sessionId,
      turnNumber: ctx.turnNumber,
      timestamp: new Date().toISOString(),
      prevHash: this.prevHash,
      payloadHash,
      keyId: this.signer.keyId(),
    };

    const envelopeHash = sha256Hex(canonicalJson(header));
    const signature = this.signer.sign(envelopeHash);

    const env: AuditEnvelope = {
      schemaVersion: 1,
      tenantId: header.tenantId,
      sessionId: header.sessionId,
      turnNumber: header.turnNumber,
      timestamp: header.timestamp,
      integrity: {
        prevHash: header.prevHash,
        payloadHash,
        envelopeHash,
        signature,
        keyId: header.keyId,
      },
      payload: normalized,
    };

    this.envelopes.push(env);
    this.prevHash = envelopeHash;
    return env;
  }

  export(): AuditEnvelope[] { return [...this.envelopes]; }

  verifyChain(): { ok: boolean; index?: number; reason?: string } {
    let prev = '0'.repeat(64);
    for (let i=0;i<this.envelopes.length;i++) {
      const e = this.envelopes[i];
      if (e.integrity.prevHash !== prev) return { ok:false, index:i, reason:'prevHash mismatch' };

      const payloadHash = sha256Hex(canonicalJson(e.payload));
      if (payloadHash !== e.integrity.payloadHash) return { ok:false, index:i, reason:'payloadHash mismatch' };

      const header = {
        schemaVersion: e.schemaVersion,
        tenantId: e.tenantId,
        sessionId: e.sessionId,
        turnNumber: e.turnNumber,
        timestamp: e.timestamp,
        prevHash: e.integrity.prevHash,
        payloadHash: e.integrity.payloadHash,
        keyId: e.integrity.keyId,
      };

      const envelopeHash = sha256Hex(canonicalJson(header));
      if (envelopeHash !== e.integrity.envelopeHash) return { ok:false, index:i, reason:'envelopeHash mismatch' };

      if (!this.signer.verify(envelopeHash, e.integrity.signature)) return { ok:false, index:i, reason:'signature invalid' };

      prev = e.integrity.envelopeHash;
    }
    return { ok:true };
  }
}
```

## 4.2 Provide a signer implementation that works in Bun now

You can ship two signers:

* **HMAC signer** (integrity, not non-repudiation; good for dev/testing)
* **Ed25519 signer** (real non-repudiation; best for production)

### New file: `src/runtime/signers/hmac-signer.ts`

```ts
import { AuditSigner } from '../audit-ledger';
import { hmacSha256Hex, timingSafeEqualHex } from '../crypto';

export class HmacSigner implements AuditSigner {
  constructor(private key: Uint8Array, private id: string = 'hmac-dev') {}

  keyId(): string { return this.id; }

  sign(hashHex: string): string {
    // signature = HMAC(key, envelopeHashHex)
    return hmacSha256Hex(this.key, hashHex);
  }

  verify(hashHex: string, signature: string): boolean {
    const expected = this.sign(hashHex);
    return timingSafeEqualHex(expected, signature);
  }
}
```

### Test additions

* `test/unit/audit-ledger.test.ts`

  * append 3 envelopes, verifyChain ok
  * mutate payload, verifyChain fails
  * remove envelope 2, verifyChain fails
  * reorder envelopes, verifyChain fails

## 4.3 Integrate ledger into the FFI dispatch path (Decorator/Proxy)

The ledger is most valuable when it is **impossible** to bypass. The correct pattern is:

* wrap the base FFI with an `AuditedFFI` (Decorator)
* `AuditedFFI.call`:

  * runs enforcement decision
  * logs allow/deny and metadata
  * delegates to base implementation
  * logs completion result summary

### New file: `src/runtime/ffi-audited.ts`

```ts
import { ContextualFFI, CallOptions, FFI } from './ffi';
import { AuditLedger } from './audit-ledger';
import { SessionEnforcer } from './session-enforcer';
import { ExecutionContext } from './context';
import { summarizeForPolicy } from './policy-summarize';

export class AuditedFFI implements ContextualFFI {
  constructor(
    private base: FFI,
    public readonly ctx: ExecutionContext,
    private enforcer: SessionEnforcer,
    private audit: AuditLedger
  ) {}

  call(name: string, args: unknown[], opts?: CallOptions): unknown {
    const summary = summarizeForPolicy(name, args, this.ctx);

    const verdict = this.enforcer.decide(name, args, this.ctx);

    if (verdict.tag === 'deny') {
      this.audit.append(this.ctx, {
        op: name,
        resource: summary.resource,
        argsSummary: summary.argsSummary,
        labels: summary.labels,
        purpose: summary.purpose,
        decisionHash: verdict.decision.decisionHash,
        ruleIds: verdict.decision.ruleIds,
        outcomeTag: 'denied',
      });
      throw new Error(`PolicyViolation: ${verdict.reason}`);
    }

    if (verdict.tag === 'needs') {
      this.audit.append(this.ctx, {
        op: name,
        resource: summary.resource,
        argsSummary: summary.argsSummary,
        labels: summary.labels,
        purpose: summary.purpose,
        decisionHash: verdict.decision.decisionHash,
        ruleIds: verdict.decision.ruleIds,
        outcomeTag: 'needs',
      });
      // you likely transform this into a NeedsOutcome at session level;
      // here we throw a typed error to unwind to session boundary.
      throw new Error(`Needs:${verdict.needType}:${verdict.description}`);
    }

    // allow
    this.audit.append(this.ctx, {
      op: name,
      resource: summary.resource,
      argsSummary: summary.argsSummary,
      labels: summary.labels,
      purpose: summary.purpose,
      decisionHash: verdict.decision.decisionHash,
      ruleIds: verdict.decision.ruleIds,
      outcomeTag: 'allowed',
    });

    const result = this.base.call(name, args, opts);

    // Result summary: only type/size/hashes, never full content
    this.audit.append(this.ctx, {
      op: `${name}.result`,
      resource: summary.resource,
      resultSummary: summarizeResult(result),
      decisionHash: verdict.decision.decisionHash,
      ruleIds: verdict.decision.ruleIds,
      outcomeTag: 'done',
    });

    return result;
  }
}

function summarizeResult(result: unknown): unknown {
  if (typeof result === 'string') return { type:'string', chars: result.length };
  if (typeof result === 'number') return { type:'number' };
  if (typeof result === 'boolean') return { type:'boolean' };
  if (result === null) return { type:'null' };
  if (Array.isArray(result)) return { type:'list', length: result.length };
  if (typeof result === 'object') return { type:'object' };
  return { type: typeof result };
}
```

### Session change

In `executeTurn`, instead of `new EnforcedFFI(...)`, you now construct:

* `AuditedFFI(baseFFI, ctx, enforcer, ledger)`

Then session catches `Needs:*` and converts to `NeedsOutcome`.

---

# Change Set 6 Deepening — WorkerRunner (Hard Isolation) + RPC’d FFI

Now you want real containment: in Node/Bun, that means **Worker threads** at minimum. You will treat worker code as untrusted; the host remains the reference monitor.

## 6.1 Introduce a Runner interface and select it via policy obligation

### New file: `src/runtime/runner.ts`

```ts
import { ExecutionContext } from './context';
import { Outcome } from './outcomes';

export interface RunnerResult {
  outcome: Outcome;
  // optionally: journal events, audit envelope hashes, etc.
}

export interface SandboxRunner {
  kind: 'in-process'|'worker';
  eval(code: string, ctx: ExecutionContext): Promise<RunnerResult>;
  kill(reason: string): Promise<void>;
}
```

### Modify Session

* pick runner for each turn:

  * default: in-process for dev
  * if policy obligation `force-sandbox` runner is `worker`, use WorkerRunner

This is a Strategy selection.

---

## 6.2 Worker message protocol (command pattern)

You need an RPC protocol between host and worker. Keep it explicit and versioned.

### New file: `src/runtime/runners/worker-protocol.ts`

```ts
export type WorkerMsg =
  | EvalRequest
  | EvalResponse
  | FfiRequest
  | FfiResponse
  | Interrupt
  | Ready
  ;

export interface Ready { kind:'ready'; }

export interface EvalRequest {
  kind: 'eval';
  requestId: string;
  code: string;
  ctx: any;                 // serialized ExecutionContext (safe subset)
}

export interface EvalResponse {
  kind: 'eval-result';
  requestId: string;
  outcome: any;             // serialized Outcome
}

export interface FfiRequest {
  kind: 'ffi-call';
  requestId: string;
  op: string;
  args: any;                // serialized Value[]
  opts?: any;
}

export interface FfiResponse {
  kind: 'ffi-result';
  requestId: string;
  ok: boolean;
  value?: any;              // serialized Value
  error?: { type: string; message: string };
}

export interface Interrupt { kind:'interrupt'; reason: string; }
```

You already have serialization for Values; reuse it (Adapter).

---

## 6.3 WorkerRunner implementation (host side)

### New file: `src/runtime/runners/worker-runner.ts`

```ts
import { Worker } from 'worker_threads';
import { SandboxRunner, RunnerResult } from '../runner';
import { ExecutionContext } from '../context';
import { WorkerMsg, EvalRequest, EvalResponse, FfiRequest, FfiResponse } from './worker-protocol';

export class WorkerRunner implements SandboxRunner {
  kind: 'worker' = 'worker';

  private worker: Worker | null = null;
  private inflight = new Map<string, { resolve: (r:any)=>void; reject:(e:any)=>void }>();

  constructor(private workerEntry: string, private timeoutMs: number = 30_000) {}

  async eval(code: string, ctx: ExecutionContext): Promise<RunnerResult> {
    await this.ensureWorker();

    const requestId = crypto.randomUUID();
    const req: EvalRequest = {
      kind: 'eval',
      requestId,
      code,
      ctx: sanitizeCtx(ctx),
    };

    const p = new Promise<RunnerResult>((resolve, reject) => {
      this.inflight.set(requestId, { resolve, reject });
    });

    this.worker!.postMessage(req);

    const timeout = new Promise<RunnerResult>((_, reject) =>
      setTimeout(() => reject(new Error(`RunnerTimeout:${this.timeoutMs}`)), this.timeoutMs)
    );

    try {
      return await Promise.race([p, timeout]);
    } catch (e) {
      await this.kill(`timeout:${(e as Error).message}`);
      throw e;
    }
  }

  async kill(reason: string): Promise<void> {
    if (!this.worker) return;
    try { this.worker.postMessage({ kind:'interrupt', reason } as WorkerMsg); } catch {}
    try { await this.worker.terminate(); } catch {}
    this.worker = null;
    this.inflight.clear();
  }

  private async ensureWorker(): Promise<void> {
    if (this.worker) return;

    this.worker = new Worker(this.workerEntry);
    this.worker.on('message', (msg: WorkerMsg) => this.onMessage(msg));
    this.worker.on('error', (err) => this.onError(err));
    this.worker.on('exit', (code) => this.onExit(code));

    // optionally: wait for Ready
  }

  private onMessage(msg: WorkerMsg) {
    if (msg.kind === 'eval-result') {
      const m = msg as EvalResponse;
      const entry = this.inflight.get(m.requestId);
      if (!entry) return;
      this.inflight.delete(m.requestId);
      entry.resolve({ outcome: m.outcome } as RunnerResult);
      return;
    }

    // FFI calls are handled by separate bridging layer (see below)
  }

  private onError(err: any) {
    for (const [, entry] of this.inflight) entry.reject(err);
    this.inflight.clear();
  }

  private onExit(code: number) {
    const err = new Error(`WorkerExited:${code}`);
    for (const [, entry] of this.inflight) entry.reject(err);
    this.inflight.clear();
    this.worker = null;
  }
}

function sanitizeCtx(ctx: ExecutionContext) {
  // never send secrets; the worker is untrusted
  return {
    sessionId: ctx.sessionId,
    turnNumber: ctx.turnNumber,
    principal: { subject: ctx.principal.subject, tenantId: ctx.principal.tenantId, roles: ctx.principal.roles },
    policy: ctx.policy,
    riskTier: ctx.riskTier,
    mode: ctx.mode,
    purpose: ctx.purpose,
    trace: ctx.trace,
  };
}
```

### Critical missing piece: FFI bridging

The worker cannot directly access world/network/llm; it must RPC to host, and host enforces policy/capabilities/audit.

That means in the worker evaluator you bind an `FFI` whose `call()` posts `ffi-call` messages to host and awaits a response.

---

## 6.4 Worker-side “RemoteFFI” (Adapter + Proxy)

### New file: `src/runtime/runners/worker-remote-ffi.ts` (worker environment)

```ts
import { parentPort } from 'worker_threads';
import { FfiRequest, FfiResponse } from './worker-protocol';

export class RemoteFFI {
  private inflight = new Map<string, { resolve:(v:any)=>void; reject:(e:any)=>void }>();

  constructor() {
    parentPort!.on('message', (msg: any) => this.onMessage(msg));
  }

  async call(name: string, args: any[], opts?: any): Promise<any> {
    const requestId = crypto.randomUUID();
    const req: FfiRequest = { kind:'ffi-call', requestId, op: name, args, opts };

    const p = new Promise<any>((resolve, reject) => {
      this.inflight.set(requestId, { resolve, reject });
    });

    parentPort!.postMessage(req);
    return p;
  }

  private onMessage(msg: any) {
    if (msg.kind !== 'ffi-result') return;
    const res = msg as FfiResponse;
    const entry = this.inflight.get(res.requestId);
    if (!entry) return;
    this.inflight.delete(res.requestId);

    if (res.ok) entry.resolve(res.value);
    else entry.reject(new Error(`${res.error?.type ?? 'FFIError'}:${res.error?.message ?? ''}`));
  }
}
```

---

## 6.5 Host-side FFI handler for worker requests (Reference Monitor)

Host must:

1. receive `ffi-call`
2. route to `AuditedFFI` (which already enforces policy and appends to ledger)
3. send `ffi-result`

### New file: `src/runtime/runners/worker-host-bridge.ts`

```ts
import { Worker } from 'worker_threads';
import { WorkerMsg, FfiRequest, FfiResponse } from './worker-protocol';
import { FFI } from '../ffi';

export function attachWorkerBridge(worker: Worker, ffi: FFI) {
  worker.on('message', async (msg: WorkerMsg) => {
    if (msg.kind !== 'ffi-call') return;
    const req = msg as FfiRequest;

    try {
      const value = await Promise.resolve(ffi.call(req.op, req.args, req.opts));
      const res: FfiResponse = { kind:'ffi-result', requestId: req.requestId, ok: true, value };
      worker.postMessage(res);
    } catch (e) {
      const err = e as Error;
      const res: FfiResponse = {
        kind:'ffi-result',
        requestId: req.requestId,
        ok: false,
        error: { type: err.name ?? 'Error', message: err.message ?? String(err) }
      };
      worker.postMessage(res);
    }
  });
}
```

Now you have a real trust boundary:

* the worker executes untrusted eval
* the host mediates all side effects under policy + capability + audit

---

## 6.6 Worker entrypoint

### New file: `src/runtime/runners/worker-entry.ts`

This loads your evaluator and runs requested code.

```ts
import { parentPort } from 'worker_threads';
import { WorkerMsg, EvalRequest, EvalResponse } from './worker-protocol';
import { RemoteFFI } from './worker-remote-ffi';
// import { read } from '../../core/reader'; etc.
// import { evalExpr } from '../../core/eval'; etc.

const ffi = new RemoteFFI();

parentPort!.postMessage({ kind:'ready' } as WorkerMsg);

parentPort!.on('message', async (msg: WorkerMsg) => {
  if (msg.kind === 'eval') {
    const req = msg as EvalRequest;
    try {
      // You likely already have: runtime.eval(code)
      // But inside worker we need a “Runtime” wired to RemoteFFI.
      const outcome = await runEval(req.code, ffi, req.ctx);

      const res: EvalResponse = { kind:'eval-result', requestId: req.requestId, outcome };
      parentPort!.postMessage(res);
    } catch (e) {
      const err = e as Error;
      const res: EvalResponse = {
        kind:'eval-result',
        requestId: req.requestId,
        outcome: ['error', err.message, err.name] // or your Outcome shape
      };
      parentPort!.postMessage(res);
    }
  }

  if (msg.kind === 'interrupt') {
    // Optional: set a global aborted flag if your evaluator checks it.
    // Primary kill semantics come from host terminating the worker.
  }
});

async function runEval(code: string, ffi: RemoteFFI, ctx: any): Promise<any> {
  // Construct runtime with RemoteFFI. Keep core pure.
  // This function is a small composition root for worker.
  // Implementation will depend on your existing runtime wiring.
  return ['ok', null, {}];
}
```

**Note:** the worker’s runtime should not hold policy secrets or signing keys. Those remain in host.

---

## 6.7 Tests for WorkerRunner containment

### Add E2E: `test/e2e/worker-timeout.test.ts`

* run `(fixpoint (begin ...) :max-iters 999999999)` with small timeout
* assert host returns `(error ...)` or `(nonconverged ...)` with reason “timeout”
* assert worker terminated and no further messages

### Add Integration: `test/integration/worker-ffi-enforcement.test.ts`

* policy denies `world.read "/etc/passwd"`; in worker call triggers deny at host bridge; worker receives error; session outcome is policy-violation
* audit ledger includes deny envelope

---

# What this gets you (in enterprise reviewer language)

After you implement just these three deepened changes (Policy + Ledger + WorkerRunner):

### You can credibly claim:

* **Reference Monitor** exists (host bridge + audited FFI)
* **Policy is versioned + hash-addressable** (bundle hash in every decision)
* **Policy decisions are replayable** (decisionHash, ruleIds)
* **Egress is governable** (policy can deny network/llm; later add redaction obligation in the llm adapter)
* **Audit trail is tamper-evident** (hash chain) and **signed** (non-repudiation once you move to Ed25519/KMS)
* **Untrusted evaluation is preemptively terminable** (worker terminate) — not just cooperative step limits

These are exactly the “blocking issues” that keep agents from being treated like first-class enterprise components.

---

# Next changes to generate (I’ll continue immediately after this)

Now that policy/audit/containment are real, the next most valuable continuation is:

1. **Approval objects bound to proposalsHash + policyHash with four-eyes** (Change Set 3 deepening)
2. **Egress-gated LLM adapter with enforced redaction profiles** (Change Set 5 deepening)
3. **Deterministic journal + replay adapters** (Change Set 7 deepening)

I’m going to proceed with **Change Set 3 (Approvals)** next, because it plugs directly into your existing Proposal Mode and is the main legal/compliance lever for “agent writes are safe.”
