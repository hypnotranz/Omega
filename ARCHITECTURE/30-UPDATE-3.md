Continuing in the same “**changes-only** / **implementable in-order**” style, the next tranche that gets you the furthest toward *enterprise/legal/medical trust semantics* is:

1. **Approvals** as signed, context-bound authorizations (and commit gating)
2. **Egress governance**: redaction + classification + outbound allowlisting + output filtering
3. **Deterministic journaling + replay** (event-sourced execution + divergence detection)
4. **Identity / assurance / key management hooks** (so approvals and audit signatures are meaningful)
5. **Retention + encryption at rest** for any sensitive journal payloads

Everything below is described as **additions/modifications** to your existing stack.

---

# Change Set 3 — Approvals as First-Class, Signed, Context-Bound Authorizations

Your current `NeedsOutcome` concept is the right “control-plane surface”. What’s missing is a **cryptographically attributable approval object** that is **non-replayable**, **scoped**, and **provably bound** to the exact operation/proposal set being authorized.

This is the classic separation:

* **PDP/PEP** decide “approval required”
* **Approval authority** issues a signed approval
* **PEP** verifies approval, then allows the effect

This is basically an **Authorization Code / Macaroon-like** mechanism but tailored to agent operations.

## 3.1 Add ApprovalRequest + ApprovalEnvelope schema

### New file: `src/approval/approval-types.ts`

```ts
import { sha256Hex, canonicalJson } from '../runtime/crypto';

export type ApprovalScope =
  | { kind: 'operation'; op: string; resource?: string }
  | { kind: 'proposals'; proposalsHash: string }
  | { kind: 'session'; sessionId: string };

export interface ApprovalRequest {
  schemaVersion: 1;

  tenantId: string;
  sessionId: string;
  turnNumber: number;

  // What is being authorized (narrow!)
  scope: ApprovalScope;

  // Bind to policy decision
  policyHash: string;
  decisionHash: string;
  ruleIds: string[];

  // Optional bind to evidence / provenance context
  evidenceIds?: string[];

  // Risk / governance metadata
  riskTier: 'low'|'moderate'|'high'|'regulated';
  purpose?: string;
  labels?: string[];

  // Human-facing prompt (NOT used for binding; just UX)
  summary: string;

  // Deterministic binding
  requestHash: string; // sha256(canonical request without requestHash)
}

export interface ApprovalEnvelope {
  schemaVersion: 1;

  // Who approved (attributable identity)
  approver: {
    subject: string;        // "user:alice" or email sub
    roles: string[];        // ["security-reviewer"]
    assurance?: 'aal2'|'aal3';
  };

  tenantId: string;

  // What they approved (must match ApprovalRequest.requestHash)
  requestHash: string;

  // Time bounds
  issuedAt: string;         // ISO
  expiresAt: string;        // ISO

  // Optional human justification (auditable)
  justification?: string;

  // Signature over canonical form of this envelope (minus signature)
  signature: {
    keyId: string;
    alg: 'hmac-sha256' | 'ed25519';
    sig: string;            // hex/base64 (choose one; be consistent)
  };
}

export function computeApprovalRequestHash(req: Omit<ApprovalRequest,'requestHash'>): string {
  return sha256Hex(canonicalJson(req));
}

export function finalizeApprovalRequest(req: Omit<ApprovalRequest,'requestHash'>): ApprovalRequest {
  return { ...req, requestHash: computeApprovalRequestHash(req) };
}

export function computeApprovalEnvelopeHash(env: Omit<ApprovalEnvelope,'signature'>): string {
  return sha256Hex(canonicalJson(env));
}
```

### Why this matters

* You can prove the approver authorized **exactly** the request (requestHash).
* You can expire approvals and enforce “freshness”.
* You can require assurance level (AAL2/AAL3) for regulated contexts.

---

## 3.2 Add ApprovalSigner / ApprovalVerifier (dev + production hooks)

### New file: `src/approval/approval-signer.ts`

```ts
import { ApprovalEnvelope, computeApprovalEnvelopeHash } from './approval-types';

export interface ApprovalSigner {
  keyId(): string;
  alg(): ApprovalEnvelope['signature']['alg'];
  sign(hashHex: string): string;
}

export interface ApprovalVerifier {
  verify(hashHex: string, signature: ApprovalEnvelope['signature']): boolean;
}

export function signApprovalEnvelope(
  signer: ApprovalSigner,
  env: Omit<ApprovalEnvelope,'signature'>
): ApprovalEnvelope {
  const hash = computeApprovalEnvelopeHash(env);
  return {
    ...env,
    signature: {
      keyId: signer.keyId(),
      alg: signer.alg(),
      sig: signer.sign(hash),
    }
  };
}
```

### New file: `src/approval/hmac-approval.ts`

```ts
import { ApprovalSigner, ApprovalVerifier } from './approval-signer';
import { hmacSha256Hex, timingSafeEqualHex } from '../runtime/crypto';

export class HmacApproval implements ApprovalSigner, ApprovalVerifier {
  constructor(private key: Uint8Array, private id: string = 'hmac-approval-dev') {}

  keyId(): string { return this.id; }
  alg(): 'hmac-sha256' { return 'hmac-sha256'; }

  sign(hashHex: string): string {
    return hmacSha256Hex(this.key, hashHex);
  }

  verify(hashHex: string, sig: { alg: string; sig: string }): boolean {
    if (sig.alg !== 'hmac-sha256') return false;
    const expected = this.sign(hashHex);
    return timingSafeEqualHex(expected, sig.sig);
  }
}
```

> Production path: add an `Ed25519Approval` that uses `crypto.sign/verify` (Node) or WebCrypto in browser. Keep interface identical.

---

## 3.3 Add ApprovalStore (monotone, queryable, replay-safe)

You want:

* idempotent registration
* matching by `requestHash`
* enforcement of expiration, role constraints, assurance constraints

### New file: `src/approval/approval-store.ts`

```ts
import { ApprovalEnvelope, computeApprovalEnvelopeHash } from './approval-types';
import { ApprovalVerifier } from './approval-signer';

export interface ApprovalConstraints {
  requiredRolesAny?: string[];
  minAssurance?: 'aal2'|'aal3';
  nowMs?: number; // injectable clock for tests
}

export class ApprovalStore {
  private byRequest = new Map<string, ApprovalEnvelope>();

  constructor(private verifier: ApprovalVerifier) {}

  register(env: ApprovalEnvelope): { ok: true } | { ok: false; reason: string } {
    // Verify signature
    const unsigned = { ...env } as any;
    delete unsigned.signature;

    const hash = computeApprovalEnvelopeHash(unsigned);
    if (!this.verifier.verify(hash, env.signature)) {
      return { ok: false, reason: 'invalid-signature' };
    }

    // Validate time window
    const now = Date.now();
    const issued = Date.parse(env.issuedAt);
    const expires = Date.parse(env.expiresAt);
    if (!Number.isFinite(issued) || !Number.isFinite(expires)) {
      return { ok: false, reason: 'bad-timestamps' };
    }
    if (now < issued) return { ok: false, reason: 'not-yet-valid' };
    if (now > expires) return { ok: false, reason: 'expired' };

    // Idempotent: first wins (or replace if newer; choose policy)
    if (!this.byRequest.has(env.requestHash)) {
      this.byRequest.set(env.requestHash, env);
    }
    return { ok: true };
  }

  has(requestHash: string, constraints?: ApprovalConstraints): boolean {
    const env = this.byRequest.get(requestHash);
    if (!env) return false;

    const now = constraints?.nowMs ?? Date.now();
    if (now > Date.parse(env.expiresAt)) return false;

    if (constraints?.requiredRolesAny?.length) {
      if (!env.approver.roles.some(r => constraints.requiredRolesAny!.includes(r))) return false;
    }

    if (constraints?.minAssurance) {
      const a = env.approver.assurance;
      if (!a) return false;
      if (constraints.minAssurance === 'aal3' && a !== 'aal3') return false;
    }

    return true;
  }

  get(requestHash: string): ApprovalEnvelope | undefined {
    return this.byRequest.get(requestHash);
  }

  list(): ApprovalEnvelope[] {
    return Array.from(this.byRequest.values());
  }
}
```

---

## 3.4 Extend ExecutionContext with principal assurance + policy handle

You already carry principal/roles; add assurance + policy pointer so approval logic doesn’t “guess”.

### Modify: `src/runtime/context.ts` (or wherever `ExecutionContext` lives)

Add:

```ts
export interface Principal {
  subject: string;
  tenantId: string;
  roles: string[];
  assurance?: 'aal2'|'aal3';
}

export interface ExecutionContext {
  sessionId: string;
  turnNumber: number;
  principal: Principal;

  riskTier: 'low'|'moderate'|'high'|'regulated';
  purpose?: string;

  policy: { hash: string }; // lightweight handle; actual PDP is session-owned
  // ...
}
```

---

## 3.5 Wire approvals into enforcement: “needs-approval” becomes a concrete ApprovalRequest

### Modify: `src/runtime/session-enforcer.ts`

**Add** `ApprovalStore` dependency and produce deterministic approval request hashes.

```ts
import { finalizeApprovalRequest, ApprovalRequest } from '../approval/approval-types';

export class SessionEnforcer {
  constructor(
    private staticPolicy: SessionPolicy,
    private pdp: PDP,
    private approvals: ApprovalStore
  ) {}

  decide(op: string, args: unknown[], ctx: ExecutionContext): EnforcementVerdict {
    this.checkOperationStatic(op, args);

    const summary = summarizeForPolicy(op, args, ctx);
    const decision = this.pdp.decide({ ctx, op, resource: summary.resource, argsSummary: summary.argsSummary, labels: summary.labels, purpose: summary.purpose });

    if (!decision.allow) {
      return { tag:'deny', decision, reason: decision.reason };
    }

    const approvalOb = decision.obligations.find(o => o.kind === 'require-approval');
    if (approvalOb?.kind === 'require-approval') {
      const req: ApprovalRequest = finalizeApprovalRequest({
        schemaVersion: 1,
        tenantId: ctx.principal.tenantId,
        sessionId: ctx.sessionId,
        turnNumber: ctx.turnNumber,
        scope: { kind:'operation', op, resource: summary.resource },
        policyHash: decision.policyHash,
        decisionHash: decision.decisionHash,
        ruleIds: decision.ruleIds,
        evidenceIds: [], // optionally include if available
        riskTier: ctx.riskTier,
        purpose: ctx.purpose,
        labels: summary.labels,
        summary: `Approve operation ${op} on ${summary.resource ?? '<none>'}`,
      });

      // If already approved, allow
      const ok = this.approvals.has(req.requestHash, {
        requiredRolesAny: approvalOb.policy.allowedRoles,
        minAssurance: approvalOb.policy.minAssurance,
      });

      if (!ok) {
        return {
          tag:'needs',
          decision,
          needType: 'needs-approval',
          description: `Operation ${op} requires approval`,
          context: {
            approvalRequest: req,
            approvalPolicy: approvalOb.policy,
          }
        };
      }
    }

    return { tag:'allow', decision };
  }
}
```

---

## 3.6 Gate proposal commits on approval (make compliance unavoidable)

Right now your host can “just commit”. For regulated environments, your **API should make it impossible to commit staged writes without approvals** (even if the host is “trusted”, enterprise auditors want the *mechanism*, not promises).

### New helper: hash proposals deterministically

#### New file: `src/runtime/proposals-hash.ts`

```ts
import { canonicalJson, sha256Hex } from './crypto';

export function computeProposalsHash(proposals: any[]): string {
  return sha256Hex(canonicalJson(proposals));
}
```

### Modify: `src/runtime/session-manager.ts` (or wherever commit happens)

Change commit signature:

```ts
commit(id: string, approvals?: { requestHash: string }[]): void
```

Implementation sketch:

* compute `proposalsHash`
* create an ApprovalRequest **for the proposals** (scope = proposalsHash)
* require a matching ApprovalEnvelope exists in ApprovalStore
* only then commit

#### New file: `src/approval/approval-requests.ts`

```ts
import { finalizeApprovalRequest } from './approval-types';

export function buildProposalsCommitRequest(args: {
  tenantId: string;
  sessionId: string;
  turnNumber: number;
  proposalsHash: string;
  policyHash: string;
  decisionHash: string;
  ruleIds: string[];
  riskTier: 'low'|'moderate'|'high'|'regulated';
  purpose?: string;
}){
  return finalizeApprovalRequest({
    schemaVersion: 1,
    tenantId: args.tenantId,
    sessionId: args.sessionId,
    turnNumber: args.turnNumber,
    scope: { kind:'proposals', proposalsHash: args.proposalsHash },
    policyHash: args.policyHash,
    decisionHash: args.decisionHash,
    ruleIds: args.ruleIds,
    riskTier: args.riskTier,
    purpose: args.purpose,
    summary: `Approve commit of proposals ${args.proposalsHash.slice(0,12)}…`,
  });
}
```

Then in commit:

* if policy mandates approval for `world.write`, you will have already emitted a needs outcome. But still, commit gating ensures nothing slips.

**Audit integration:** append ledger events:

* `commit.requested` with proposalsHash
* `commit.denied` if no approval
* `commit.approved` with approver subject
* `commit.done`

---

## 3.7 Protocol surface: approve + list approvals

You want a stable protocol op for hosts:

### Add to protocol handlers:

* `op: 'approve'` — register an ApprovalEnvelope
* `op: 'approvals'` — list approvals (for debugging/audit UIs)

```ts
interface ApproveRequest {
  op: 'approve';
  id: string;
  approval: ApprovalEnvelope;
}

interface ApproveResponse {
  id: string;
  status: ['done'] | ['error'];
  error?: string;
}
```

---

## 3.8 Tests you should add now

1. **Unit:** `approval-store.test.ts`

* invalid signature rejected
* expired approval rejected
* role constraints enforced
* assurance constraints enforced

2. **Integration:** `policy-needs-approval-then-approve.test.ts`

* `world.write` triggers `needs`
* host submits `approve` op
* rerun same evaluation succeeds and proposals can be committed

3. **Integration:** `commit-requires-approval.test.ts`

* stage proposals
* attempt commit without approval => denied
* approve proposalsHash => commit succeeds

---

# Change Set 5 — Egress Governance: Classification + Redaction + Allowlisting + Output Filtering

This is the second “enterprise blocker”: **data exfiltration and PHI/PII leakage**. You already have a capability model and policy. What’s missing is an **egress pipeline** that:

* classifies outbound payloads (labels)
* redacts according to profile
* enforces destination/model allowlists
* optionally filters outputs to prevent prompt-injection tool escalation

## 5.1 Add a Data Classification Interface (Strategy Pattern)

### New file: `src/security/classifier.ts`

```ts
export type DataLabel =
  | 'pii'
  | 'phi'
  | 'pci'
  | 'credentials'
  | 'secrets'
  | 'ip'
  | 'public';

export interface Classification {
  labels: DataLabel[];
  // optional signals for audit/debug
  matches?: Array<{ label: DataLabel; count: number }>;
  confidence?: number; // heuristic
}

export interface Classifier {
  classifyText(text: string): Classification;
}
```

### Provide a baseline heuristic classifier (replaceable)

#### New file: `src/security/classifier-regex.ts`

```ts
import { Classifier, Classification, DataLabel } from './classifier';

export class RegexClassifier implements Classifier {
  classifyText(text: string): Classification {
    const matches: Record<DataLabel, number> = {
      pii: 0, phi: 0, pci: 0, credentials: 0, secrets: 0, ip: 0, public: 0
    };

    // intentionally conservative heuristics
    if (/\b\d{3}-\d{2}-\d{4}\b/.test(text)) matches.pii++;               // SSN-like
    if (/\b(?:\d[ -]*?){13,19}\b/.test(text)) matches.pci++;            // card-ish
    if (/\b[A-Z0-9._%+-]+@[A-Z0-9.-]+\.[A-Z]{2,}\b/i.test(text)) matches.pii++;
    if (/\b(?:MRN|Medical Record|Patient)\b/i.test(text)) matches.phi++;
    if (/\bAKIA[0-9A-Z]{16}\b/.test(text)) matches.secrets++;           // AWS access key-ish
    if (/\b-----BEGIN (?:RSA|EC|OPENSSH) PRIVATE KEY-----\b/.test(text)) matches.secrets++;

    const labels = (Object.keys(matches) as DataLabel[])
      .filter(l => matches[l] > 0);

    return { labels, matches: labels.map(l => ({ label: l, count: matches[l] })), confidence: labels.length ? 0.6 : 0.2 };
  }
}
```

**Design note:** this is explicitly a replaceable Strategy; enterprises will swap in DLP providers.

---

## 5.2 Add Redaction Engine (Template Method + Profiles)

### New file: `src/security/redaction.ts`

```ts
import { canonicalJson, sha256Hex } from '../runtime/crypto';
import { DataLabel } from './classifier';

export type RedactionProfileId = 'default'|'medical-strict'|'legal-privileged'|'finance-pci';

export interface RedactionResult {
  redactedText: string;
  report: {
    profile: RedactionProfileId;
    removed: Array<{ kind: string; count: number }>;
    inputHash: string;
    outputHash: string;
  };
}

export interface Redactor {
  redact(text: string, profile: RedactionProfileId, labels?: DataLabel[]): RedactionResult;
}

export class RegexRedactor implements Redactor {
  redact(text: string, profile: RedactionProfileId): RedactionResult {
    const inputHash = sha256Hex(text);

    const rules = profileRules(profile);

    let out = text;
    const removed: Record<string, number> = {};

    for (const r of rules) {
      const before = out;
      out = out.replace(r.pattern, r.replacement);
      if (out !== before) removed[r.kind] = (removed[r.kind] ?? 0) + 1;
    }

    const outputHash = sha256Hex(out);

    return {
      redactedText: out,
      report: {
        profile,
        removed: Object.entries(removed).map(([kind, count]) => ({ kind, count })),
        inputHash,
        outputHash,
      }
    };
  }
}

function profileRules(profile: RedactionProfileId): Array<{ kind: string; pattern: RegExp; replacement: string }> {
  // Make strict profiles supersets.
  const base = [
    { kind: 'email', pattern: /\b[A-Z0-9._%+-]+@[A-Z0-9.-]+\.[A-Z]{2,}\b/gi, replacement: '[REDACTED:EMAIL]' },
    { kind: 'ssn', pattern: /\b\d{3}-\d{2}-\d{4}\b/g, replacement: '[REDACTED:SSN]' },
    { kind: 'aws-key', pattern: /\bAKIA[0-9A-Z]{16}\b/g, replacement: '[REDACTED:KEY]' },
    { kind: 'private-key', pattern: /\b-----BEGIN (?:RSA|EC|OPENSSH) PRIVATE KEY-----[\s\S]*?-----END (?:RSA|EC|OPENSSH) PRIVATE KEY-----\b/g, replacement: '[REDACTED:PRIVATE_KEY]' },
  ];

  if (profile === 'default') return base;

  if (profile === 'finance-pci') {
    return [
      ...base,
      { kind: 'card', pattern: /\b(?:\d[ -]*?){13,19}\b/g, replacement: '[REDACTED:CARD]' }
    ];
  }

  if (profile === 'legal-privileged') {
    return [
      ...base,
      { kind: 'privileged', pattern: /\b(ATTORNEY-CLIENT|PRIVILEGED|WORK PRODUCT)\b/gi, replacement: '[REDACTED:PRIVILEGED]' }
    ];
  }

  // medical-strict
  return [
    ...base,
    { kind: 'dob', pattern: /\b\d{4}-\d{2}-\d{2}\b/g, replacement: '[REDACTED:DOB]' },
    { kind: 'name-ish', pattern: /\bPatient\s+[A-Z][a-z]+(?:\s+[A-Z][a-z]+)?\b/g, replacement: 'Patient [REDACTED:NAME]' }
  ];
}
```

---

## 5.3 Add an EgressGate (Proxy + Chain of Responsibility)

This is the single place where outbound payloads are:

* classified
* redacted (if obligated)
* checked against allowlists
* logged

### New file: `src/security/egress-gate.ts`

```ts
import { PDP } from '../policy/pdp';
import { ExecutionContext } from '../runtime/context';
import { Classifier } from './classifier';
import { Redactor, RedactionProfileId } from './redaction';

export interface LlmEgressRequest {
  prompt: string;
  model?: string;
  purpose?: string;
  // optionally carry explicit labels if already known
  labels?: string[];
}

export interface EgressDecision {
  allowed: boolean;
  redactedPrompt: string;
  labels: string[];
  redactionReport?: any;
  policyDecisionHash?: string;
  policyHash: string;
  obligations: any[];
}

export class EgressGate {
  constructor(
    private pdp: PDP,
    private classifier: Classifier,
    private redactor: Redactor
  ) {}

  gateLLM(ctx: ExecutionContext, req: LlmEgressRequest): EgressDecision {
    // 1) classify if not provided
    const cls = req.labels?.length ? { labels: req.labels } : this.classifier.classifyText(req.prompt);
    const labels = cls.labels as string[];

    // 2) policy decision
    const dec = this.pdp.decide({
      ctx,
      op: 'llm.complete',
      resource: req.model ?? 'default',
      argsSummary: { promptChars: req.prompt.length },
      labels,
      purpose: req.purpose ?? ctx.purpose,
    });

    if (!dec.allow) {
      return { allowed:false, redactedPrompt:'', labels, policyHash: dec.policyHash, policyDecisionHash: dec.decisionHash, obligations: dec.obligations };
    }

    // 3) apply obligations (redaction)
    const redOb = dec.obligations.find(o => o.kind === 'require-redaction') as any;
    let redacted = req.prompt;
    let redactionReport: any | undefined;

    if (redOb?.kind === 'require-redaction') {
      const res = this.redactor.redact(req.prompt, redOb.profile as RedactionProfileId, cls.labels as any);
      redacted = res.redactedText;
      redactionReport = res.report;
    }

    // 4) final allow
    return {
      allowed: true,
      redactedPrompt: redacted,
      labels,
      redactionReport,
      policyDecisionHash: dec.decisionHash,
      policyHash: dec.policyHash,
      obligations: dec.obligations,
    };
  }
}
```

---

## 5.4 Modify LLM adapter to enforce the gate (never bypassable)

Your `LLMAdapter.complete(prompt)` is too “raw”. Keep it for backward compatibility, but add a structured request and route everything through the gate.

### Modify: `src/ext/llm/adapter.ts`

Add:

```ts
export interface LLMRequest {
  prompt: string;
  model?: string;
  purpose?: string;
  labels?: string[];
  // optional trace ids
  sessionId?: string;
  turnNumber?: number;
}

export interface LLMAdapter {
  complete(prompt: string, options?: any): Promise<string>;
  completeRequest(req: LLMRequest): Promise<string>; // NEW
}
```

### Modify: `src/ext/llm/openai.ts` (and any others)

Implementation pattern:

* `complete(prompt)` becomes a thin wrapper calling `completeRequest({prompt})`
* `completeRequest(req)` is called only from FFI under policy/audit context

But the adapter itself needs `ExecutionContext` to gate. You already have session-owned ctx; so do this:

**Change the FFI primitive implementation** for `llm.complete` so it calls `EgressGate` before invoking adapter.

### Modify: `src/runtime/ffi.ts` registry for `llm.complete`

Pseudo-implementation:

```ts
ffi.register('llm.complete', async (args, callCtx) => {
  const [prompt, opts] = args;
  const model = opts?.model;
  const purpose = opts?.purpose;
  const labels = opts?.labels;

  const gate = callCtx.services.egressGate;
  const d = gate.gateLLM(callCtx.ctx, { prompt, model, purpose, labels });

  if (!d.allowed) {
    throw new Error(`PolicyDenied: llm.complete blocked (policy=${d.policyHash})`);
  }

  // Optional: budget clamp obligation
  // Optional: log redaction report to audit/journal
  callCtx.audit.append(callCtx.ctx, {
    op: 'llm.egress',
    resource: model,
    argsSummary: { promptChars: prompt.length },
    resultSummary: { redactedChars: d.redactedPrompt.length, labels: d.labels, redaction: d.redactionReport },
    policyHash: d.policyHash,
    decisionHash: d.policyDecisionHash,
    outcomeTag: 'ok',
  });

  return await callCtx.services.llm.completeRequest({
    prompt: d.redactedPrompt,
    model,
    purpose,
    labels: d.labels,
    sessionId: callCtx.ctx.sessionId,
    turnNumber: callCtx.ctx.turnNumber,
  });
});
```

**Key invariant:** no outbound prompt is sent without passing through `EgressGate`.

---

## 5.5 Add output filtering (prompt injection mitigation for tool escalation)

Enterprise complaint: “model output can smuggle instructions that cause tool misuse.”

You can’t solve prompt injection “in theory”, but you *can* enforce an **output policy** before the output influences tool calls, using a **Guard / Filter / Sanitizer**.

### New file: `src/security/output-filter.ts`

```ts
export interface OutputFilter {
  filter(text: string): { ok: true; text: string } | { ok: false; reason: string };
}

export class DefaultOutputFilter implements OutputFilter {
  filter(text: string) {
    // Disallow patterns that look like secret material / private keys etc.
    if (/-----BEGIN (?:RSA|EC|OPENSSH) PRIVATE KEY-----/.test(text)) {
      return { ok:false, reason:'private-key-material' };
    }
    // Optionally: cap size
    if (text.length > 200_000) return { ok:false, reason:'oversize-output' };
    return { ok:true, text };
  }
}
```

Then, in your `llm.complete` primitive:

* run `OutputFilter` before returning result to Lisp
* if blocked, return `(error ...)` or `(needs ...)` depending on policy

---

## 5.6 Tests you should add now

* `unit/redaction.test.ts` (profiles redact expected patterns; deterministic hashes)
* `unit/egress-gate.test.ts` (policy requires redaction, ensures redactedPrompt differs)
* `integration/llm-egress-blocked.test.ts` (policy denies; adapter never called — mock verifies 0 calls)
* `integration/llm-output-filter.test.ts` (output blocked triggers error outcome)

---

# Change Set 7 — Deterministic Journal + Replay (Event Sourcing + Divergence Detection)

Your audit ledger is **tamper-evident metadata**. What enterprises also ask for is:

* “Can you **reconstruct** what happened?”
* “Can you **replay** the agent deterministically for root-cause analysis?”
* “Can you show **exactly which tool call returned what**?”

This requires a **journal**. But you must treat it as sensitive and possibly encrypted.

## 7.1 Add Journal Events (separate from audit)

### New file: `src/runtime/journal.ts`

```ts
import { canonicalJson, sha256Hex } from './crypto';

export type JournalEvent =
  | { kind:'turn-start'; sessionId: string; turn: number; timestamp: string; ctx: any }
  | { kind:'ffi-call';   requestId: string; op: string; resource?: string; args: any; policyHash: string; decisionHash?: string; timestamp: string }
  | { kind:'ffi-result'; requestId: string; ok: boolean; value?: any; error?: any; timestamp: string }
  | { kind:'turn-end';   sessionId: string; turn: number; outcome: any; timestamp: string }
  ;

export interface JournalEnvelope {
  schemaVersion: 1;
  event: JournalEvent;
  eventHash: string; // sha256(canonical event)
}

export function envelopeEvent(event: JournalEvent): JournalEnvelope {
  const eventHash = sha256Hex(canonicalJson(event));
  return { schemaVersion: 1, event, eventHash };
}

export interface JournalStore {
  append(env: JournalEnvelope): void;
  export(): JournalEnvelope[];
}
```

### Provide an in-memory store (tests) and file-backed store (ops)

#### New file: `src/runtime/journal-store-memory.ts`

```ts
import { JournalEnvelope, JournalStore } from './journal';

export class MemoryJournalStore implements JournalStore {
  private items: JournalEnvelope[] = [];
  append(e: JournalEnvelope) { this.items.push(e); }
  export() { return [...this.items]; }
}
```

---

## 7.2 Add a ReplayFFI (Proxy / Test Double)

This is a **Record/Replay** pattern: in record mode you call the real FFI and journal the result; in replay mode you return recorded results and detect divergence.

### New file: `src/runtime/ffi-replay.ts`

```ts
import { FFI } from './ffi';
import { JournalStore, envelopeEvent } from './journal';

export class RecordingFFI implements FFI {
  constructor(private base: FFI, private journal: JournalStore, private ctx: any) {}

  call(name: string, args: any[], opts?: any): any {
    const requestId = crypto.randomUUID();
    this.journal.append(envelopeEvent({
      kind:'ffi-call',
      requestId,
      op: name,
      resource: args?.[0],
      args,
      policyHash: this.ctx.policyHash ?? 'unknown',
      decisionHash: this.ctx.decisionHash,
      timestamp: new Date().toISOString(),
    }));

    try {
      const value = this.base.call(name, args, opts);
      this.journal.append(envelopeEvent({
        kind:'ffi-result',
        requestId,
        ok: true,
        value,
        timestamp: new Date().toISOString(),
      }));
      return value;
    } catch (e) {
      const err = e as Error;
      this.journal.append(envelopeEvent({
        kind:'ffi-result',
        requestId,
        ok: false,
        error: { type: err.name, message: err.message },
        timestamp: new Date().toISOString(),
      }));
      throw e;
    }
  }
}

export class ReplayFFI implements FFI {
  private i = 0;

  constructor(private recorded: any[]) {}

  call(name: string, args: any[], opts?: any): any {
    // Expect next two events: ffi-call then ffi-result
    const call = this.recorded[this.i++]; 
    const res  = this.recorded[this.i++];

    if (call.event.kind !== 'ffi-call') throw new Error('ReplayDesync: expected ffi-call');
    if (call.event.op !== name) throw new Error(`ReplayMismatch: op ${name} != ${call.event.op}`);

    // Optional: verify args equality (or hash only, for privacy)
    // if (canonicalJson(call.event.args) !== canonicalJson(args)) throw ...

    if (res.event.kind !== 'ffi-result') throw new Error('ReplayDesync: expected ffi-result');
    if (!res.event.ok) throw new Error(`ReplayError:${res.event.error?.type}:${res.event.error?.message}`);

    return res.event.value;
  }
}
```

**Divergence detection:** you can tighten checks gradually:

* start by checking `op` and `resource`
* then check `argsSummary`
* then check full args (only in non-regulated dev environments)

---

## 7.3 Add protocol ops: export-journal, replay

* `op: 'journal.export'` returns journal envelopes (or a handle to a stored artifact)
* `op: 'journal.replay'` loads a journal and re-executes using ReplayFFI

This will be *immensely valuable* for regulated audits.

---

## 7.4 Make journal payload storage encryptable (hooks now, actual crypto next)

Because journals may contain:

* redacted prompts
* tool args
* tool outputs

You need a **Vault** abstraction.

### New file: `src/runtime/vault.ts`

```ts
export interface Vault {
  encrypt(plaintext: Uint8Array, aad?: Uint8Array): Uint8Array;
  decrypt(ciphertext: Uint8Array, aad?: Uint8Array): Uint8Array;
  keyId(): string;
}
```

Then your `JournalStore` can store either:

* plaintext events (dev)
* encrypted event bodies + cleartext eventHash (prod)

---

## 7.5 Tests you should add now

* `integration/journal-replay.test.ts`

  * run a session with RecordingFFI + MemoryJournalStore
  * export journal
  * replay with ReplayFFI and verify the final outcome matches (or at least that replay completes with same `ok/error` tag)

* `integration/replay-divergence.test.ts`

  * mutate a recorded event’s op or args
  * replay fails with ReplayMismatch

---

# Change Set 8 — Identity / Assurance / Key Management Hooks

The moment you sign approvals and audit logs, the first enterprise question is:

> “Where do keys live, how do you rotate them, and how do you bind them to tenant identity?”

You don’t need to ship KMS, but you must ship **interfaces** and **key rotation semantics**.

## 8.1 Add KeyProvider interface (Factory Method)

### New file: `src/runtime/keys.ts`

```ts
export interface KeyProvider {
  currentKeyId(purpose: 'audit'|'approval'|'journal'): string;
  getPublicKey(keyId: string): Uint8Array;   // for ed25519 verify
  sign(keyId: string, payloadHashHex: string): string;
  verify(keyId: string, payloadHashHex: string, signature: string): boolean;
}
```

Then:

* `AuditSigner` becomes a thin adapter over `KeyProvider`
* `ApprovalSigner/Verifier` become adapters over `KeyProvider`

This lets enterprises plug in:

* AWS KMS
* GCP KMS
* Azure Key Vault
* HSM

## 8.2 Add tenant-scoped principals + session attestation fields

Add to `ExecutionContext`:

* `principal.tenantId`
* `principal.subject`
* `principal.assurance`
* `principal.roles`
* `sessionAttestation?: { issuer, issuedAt, claimsHash }` (optional)

This gives you a place to attach OIDC claims without hardwiring an IdP.

---

# Change Set 9 — Retention + Encryption + Redaction-Aware Storage

Enterprises will ask:

* “What do you store?”
* “How long?”
* “Can we delete?”
* “Is it encrypted at rest?”
* “Can we prove PHI wasn’t persisted?”

You can get far by enforcing:

* **Audit ledger**: minimal metadata only, never raw payload
* **Journal**: configurable, and in regulated mode: encrypted + retention-limited

## 9.1 Add RetentionPolicy (Policy-as-Data)

### New file: `src/runtime/retention.ts`

```ts
export interface RetentionPolicy {
  auditDays: number;       // e.g. 365
  journalDays: number;     // e.g. 30
  allowJournalPayload: boolean; // false in strict regulated mode
  encryptJournal: boolean;
}
```

Wire it into `SessionConfig`.

## 9.2 Ensure journaling never stores raw unredacted prompt

Hard rule:

* journal stores **redacted prompt** only (if prompts stored at all)
* store hashes of raw prompts if you must correlate without persisting content

Add an invariant test:

* when `llm.complete` is called, journal payload must not contain unredacted prompt (compare hashes)

---

# Implementation Order Checklist (so you can implement while reading)

1. **Approval types + signer/verifier + store**
2. **SessionEnforcer decides needs with ApprovalRequest**
3. **Protocol op approve + store in session**
4. **Commit gating on proposalsHash approval**
5. **Classifier + Redactor + EgressGate**
6. **Route llm.complete through EgressGate and OutputFilter**
7. **Journal event envelopes + RecordingFFI / ReplayFFI**
8. **Protocol export/replay**
9. **KeyProvider + retention hooks**

---

# What this closes (specifically the “whining” categories)

After these additions, you can answer enterprise/legal/medical objections with concrete mechanisms:

* **“Agents can take actions without permission.”**
  → No: actions can be made to require approvals; approvals are signed and scoped to exact request/proposalsHash; commit is gated.

* **“Audit logs aren’t legally defensible.”**
  → No: you have hash-chained signed audit ledger; approvals are signed; decisions are hashed.

* **“PHI/PII can leak to models.”**
  → Not by default: egress is gated; classification + mandated redaction profiles + allowlists; output filtering.

* **“We can’t reproduce what happened.”**
  → You can: deterministic journaling + replay; divergence detection; evidence staleness checks already exist.

---

If you want, I can continue next with **two more “trust multipliers”** that enterprises love:

1. **Policy bundles as signed artifacts + remote policy distribution** (supply-chain hardening for policy)
2. **“Verified report” pipeline**: a compiler that refuses to emit user-facing conclusions unless every claim is `observed/measured/derived` with fresh evidence (turning your provenance system into an enforceable publishing gate)

Say “continue” again and I’ll write those as the next change set (files + protocol + tests).
