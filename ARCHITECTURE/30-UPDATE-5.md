Continuing with **changes-only**, and still ordered so you can implement in-flight. This tranche targets the remaining high-friction enterprise objections: **prompt injection / context integrity**, **data classification + redaction/DLP**, **tamper-evident auditability**, **deterministic replay**, **cryptographic identity + approvals**, **supply-chain attestation**, **standard telemetry**, and **eval-gated governance**.

---

# Change Set 15 — Deterministic Prompt IR, Canonicalization, Secure Templates

## Why this is still missing

Even with provenance/facts/fixpoint/budget/session policy, the dominant “agents aren’t enterprise-trustable” complaint is still:

* prompts are ad‑hoc strings (non‑governed, non‑reviewable)
* untrusted data can commingle with control instructions (prompt injection)
* prompts aren’t hashed/signed (no forensic reproducibility)
* policy can’t reason about “which prompt template produced this call”

So: **strings are not prompts**. Prompts become a typed intermediate representation (IR) with:

* **Segments** (trusted vs untrusted)
* **Template IDs** (governed allowlist)
* **Canonicalization** (stable hash)
* **Policy enforcement** (template usage is an operation)
* **Audit/journal linkage** (promptHash is cryptographically chained)

This is a composite of **Builder**, **Strategy**, and **Interpreter** patterns.

---

## 15.1 Add Prompt IR types

### New file: `src/prompt/prompt-ir.ts`

```ts
import { canonicalJson, sha256Hex } from '../runtime/crypto';

export type PromptRole = 'system' | 'developer' | 'user' | 'tool';

export type SegmentKind =
  | 'directive'         // normative constraints
  | 'contract'          // tool contract / output format
  | 'role-overlay'      // expert overlay
  | 'task'              // task envelope
  | 'untrusted'         // file/web/user payload (data only)
  | 'evidence-ref'      // EvidenceIds only (not content)
  | 'metadata';         // policy/session/model context

export interface PromptSegment {
  kind: SegmentKind;
  title?: string;
  role: PromptRole;
  payload: string;

  trust: 'trusted' | 'untrusted';
  labels?: string[]; // e.g. ['phi','pii','privileged']
  source?: { ref: string; fingerprint?: string; evidenceId?: string };
}

export interface PromptIR {
  schemaVersion: 1;

  templateId: string;       // e.g. "tool.intent.compile.v1"
  templateVersion: string;  // e.g. "1.0.0"

  tenantId: string;
  sessionId: string;
  policyHash: string;

  segments: PromptSegment[];

  // hash of canonical IR (no timestamps)
  promptHash: string;
}

export function computePromptHash(ir: Omit<PromptIR,'promptHash'>): string {
  return sha256Hex(canonicalJson(ir));
}

export function finalizePromptIR(ir: Omit<PromptIR,'promptHash'>): PromptIR {
  return { ...ir, promptHash: computePromptHash(ir) };
}
```

Design intent: prompt integrity is based on **IR**, not a renderer’s incidental string formatting.

---

## 15.2 Add PromptTemplate schema + slot validation

### New file: `src/prompt/templates.ts`

```ts
export type SlotType =
  | 'string'
  | 'json'
  | 'evidence-ids'
  | 'code-lisp'
  | 'code-ts'
  | 'path'
  | 'enum';

export interface SlotSpec {
  name: string;
  type: SlotType;
  required: boolean;
  maxLen?: number;
  enumValues?: string[];
  trust: 'trusted'|'untrusted';
  labels?: string[];
}

export interface PromptTemplate {
  id: string;
  version: string; // SemVer

  description: string;
  slots: SlotSpec[];

  // Template Method: defines the skeletal segment construction
  build: (slots: Record<string, unknown>) => Array<{
    kind: string;
    role: 'system'|'developer'|'user'|'tool';
    title?: string;
    payload: string;
    trust: 'trusted'|'untrusted';
    labels?: string[];
  }>;
}
```

---

## 15.3 Add TemplateRegistry (Repository + allowlisting substrate)

### New file: `src/prompt/template-registry.ts`

```ts
import { PromptTemplate } from './templates';

export interface TemplateRegistry {
  register(t: PromptTemplate): void;
  get(id: string, version?: string): PromptTemplate | undefined;
  list(): PromptTemplate[];
}

export class InMemoryTemplateRegistry implements TemplateRegistry {
  private byId = new Map<string, PromptTemplate[]>();

  register(t: PromptTemplate) {
    const arr = this.byId.get(t.id) ?? [];
    arr.push(t);
    arr.sort((a,b) => (a.version < b.version ? 1 : -1)); // replace with proper semver
    this.byId.set(t.id, arr);
  }

  get(id: string, version?: string) {
    const arr = this.byId.get(id);
    if (!arr) return undefined;
    if (!version) return arr[0];
    return arr.find(t => t.version === version);
  }

  list() {
    return Array.from(this.byId.values()).flat();
  }
}
```

---

## 15.4 Add CanonicalRenderer with strict untrusted boundaries

### New file: `src/prompt/renderers/canonical-renderer.ts`

```ts
import { PromptIR, PromptSegment } from '../prompt-ir';

export interface RenderedMessage {
  role: 'system'|'developer'|'user'|'tool';
  content: string;
}

export interface PromptRenderer {
  render(ir: PromptIR): RenderedMessage[];
}

function renderSegment(seg: PromptSegment): string {
  if (seg.trust === 'untrusted') {
    return [
      `<<BEGIN_UNTRUSTED ${seg.title ?? seg.kind}>>`,
      `SOURCE=${seg.source?.ref ?? 'unknown'}`,
      seg.payload,
      `<<END_UNTRUSTED>>`
    ].join('\n');
  }
  return seg.payload;
}

export class CanonicalRenderer implements PromptRenderer {
  render(ir: PromptIR): RenderedMessage[] {
    return ir.segments.map(s => ({
      role: s.role,
      content: renderSegment(s),
    }));
  }
}
```

This is your anti-prompt-injection boundary object. It’s not a silver bullet, but it prevents the most common “untrusted text becomes instruction” failure mode.

---

## 15.5 Add PromptBuilder (Builder pattern)

### New file: `src/prompt/prompt-builder.ts`

```ts
import { PromptIR, finalizePromptIR } from './prompt-ir';
import { TemplateRegistry } from './template-registry';

export interface PromptBuildContext {
  tenantId: string;
  sessionId: string;
  policyHash: string;
}

export class PromptBuilder {
  constructor(private templates: TemplateRegistry) {}

  build(
    ctx: PromptBuildContext,
    templateId: string,
    slots: Record<string, unknown>,
    templateVersion?: string
  ): PromptIR {
    const t = this.templates.get(templateId, templateVersion);
    if (!t) throw new Error(`Unknown prompt template ${templateId}@${templateVersion ?? 'latest'}`);

    for (const s of t.slots) {
      const v = slots[s.name];
      if (s.required && (v === undefined || v === null)) {
        throw new Error(`Missing slot ${s.name} for template ${t.id}`);
      }
      if (typeof v === 'string' && s.maxLen && v.length > s.maxLen) {
        throw new Error(`Slot ${s.name} too long`);
      }
      if (s.type === 'enum' && s.enumValues && typeof v === 'string' && !s.enumValues.includes(v)) {
        throw new Error(`Slot ${s.name} invalid enum value ${v}`);
      }
    }

    const segments = t.build(slots).map(s => ({
      kind: s.kind as any,
      role: s.role,
      title: s.title,
      payload: s.payload,
      trust: s.trust,
      labels: s.labels,
    }));

    return finalizePromptIR({
      schemaVersion: 1,
      templateId: t.id,
      templateVersion: t.version,
      tenantId: ctx.tenantId,
      sessionId: ctx.sessionId,
      policyHash: ctx.policyHash,
      segments: segments as any,
    });
  }
}
```

---

## 15.6 Policy integration: template usage is an operation

Add policy rules for:

* `op: "llm.prompt.template.use"`
* `resource: "prompt-template:<templateId>"`

Then enforce: **no LLM call unless template usage is allowed** (and approvals satisfied).

### Change: in your PDP usage

* When building a prompt, do `pdp.decide(op='llm.prompt.template.use', resource='prompt-template:…')`.
* If deny → PolicyViolation
* If obligations include approvals → NeedsOutcome (or require receipt, see Change Set 19)

---

## 15.7 Modify LLM adapter interface to accept PromptIR

### Change: `src/ext/llm/adapter.ts`

```ts
import { PromptIR } from '../../prompt/prompt-ir';

export interface LLMAdapter {
  complete(prompt: PromptIR, options?: any): Promise<string>;
}
```

### Change: `src/ext/llm/openai.ts` and other adapters

* render PromptIR with `CanonicalRenderer` (or adapter-specific renderer)
* include `promptHash`, `templateId`, `templateVersion` in audit/journal entry

---

## 15.8 Tests

Add:

1. `prompt-hash-stability.test.ts`
2. `canonical-renderer-untrusted-boundary.test.ts`
3. `policy-template-allowlist.test.ts`

---

---

# Change Set 16 — Data Classification + Boundary Labeling + Redaction/DLP

## Why this is still missing

You already *have* labels in policy rules, but not a coherent system for:

* where labels originate (classification)
* how they attach to Evidence and outputs
* how they trigger redaction/DLP
* how they block egress/publish

Enterprises expect a DLP-style control plane. Minimal viable path: label at **boundaries** (world reads, evidence capture, publish/export), without rewriting Lisp value semantics.

---

## 16.1 Add label taxonomy + classification rules

### New file: `src/security/labels.ts`

```ts
export type DataLabel =
  | 'public'
  | 'internal'
  | 'confidential'
  | 'pii'
  | 'phi'
  | 'privileged'
  | 'credentials'
  | 'trade-secret'
  | 'source-code'
  | 'regulated';

export interface ClassificationRule {
  id: string;
  description: string;
  resourceGlob: string;
  addLabels: DataLabel[];
}

export interface ClassificationPolicy {
  rules: ClassificationRule[];
}
```

---

## 16.2 Add Classifier

### New file: `src/security/classifier.ts`

```ts
import { minimatch } from '../runtime/minimatch';
import { ClassificationPolicy, DataLabel } from './labels';

export class Classifier {
  constructor(private policy: ClassificationPolicy) {}

  classifyResource(ref: string): DataLabel[] {
    const labels: DataLabel[] = [];
    for (const r of this.policy.rules) {
      if (minimatch(ref, r.resourceGlob)) {
        for (const l of r.addLabels) if (!labels.includes(l)) labels.push(l);
      }
    }
    return labels;
  }
}
```

---

## 16.3 Extend Evidence objects to carry labels

### Change: `Evidence` interface

Add:

* `labels?: string[]`
* `classification?: { rulesApplied?: string[] }`

### Change: `captureEvidence(...)`

* call `Classifier.classifyResource(ref)`
* store into evidence.labels

Now your “fresh evidence” is also “classified evidence”.

---

## 16.4 Redaction engine (Strategy pattern)

### New file: `src/security/redaction.ts`

```ts
export type RedactionProfile =
  | 'default'
  | 'medical-strict'
  | 'legal-privileged'
  | 'finance-pci';

export interface Redactor {
  redact(text: string): string;
}

export class RegexRedactor implements Redactor {
  constructor(private patterns: Array<{ re: RegExp; replace: string }>) {}
  redact(text: string): string {
    let t = text;
    for (const p of this.patterns) t = t.replace(p.re, p.replace);
    return t;
  }
}

export function redactorFor(profile: RedactionProfile): Redactor {
  switch (profile) {
    case 'medical-strict':
      return new RegexRedactor([
        { re: /\b\d{3}-\d{2}-\d{4}\b/g, replace: '[REDACTED:SSN]' },
        { re: /\b[A-Z0-9._%+-]+@[A-Z0-9.-]+\.[A-Z]{2,}\b/gi, replace: '[REDACTED:EMAIL]' },
        { re: /\b\d{10}\b/g, replace: '[REDACTED:PHONE]' },
      ]);
    case 'legal-privileged':
      return new RegexRedactor([{ re: /attorney-client privileged/gi, replace: '[REDACTED:PRIVILEGED]' }]);
    default:
      return new RegexRedactor([]);
  }
}
```

---

## 16.5 DLP scanner hook

### New file: `src/security/dlp.ts`

```ts
export interface DLPScanResult {
  ok: boolean;
  hits: Array<{ ruleId: string; description: string }>;
}

export interface DLPScanner {
  scan(text: string, labels: string[]): DLPScanResult;
}

export class DefaultDLPScanner implements DLPScanner {
  scan(text: string, labels: string[]): DLPScanResult {
    const hits: DLPScanResult['hits'] = [];

    if (/-----BEGIN (RSA|EC|OPENSSH) PRIVATE KEY-----/.test(text)) {
      hits.push({ ruleId:'private-key', description:'Private key material detected' });
    }
    if (/AKIA[0-9A-Z]{16}/.test(text)) {
      hits.push({ ruleId:'aws-access-key', description:'Possible AWS access key pattern' });
    }

    return { ok: hits.length === 0, hits };
  }
}
```

---

## 16.6 Integrate labels + redaction + DLP into PublishingGate

### Change: `PublishingGate.verifyOrNeeds(...)`

After evidence freshness checks:

* compute `effectiveLabels = union(evidence.labels for cited evidence)`
* if policy obligations include `require-redaction`:

  * redact `summary` and `finding.description` **before signing**
* run DLP scan on the redacted report
* if DLP hits remain → return NeedsOutcome (block publish)

This makes “publishing” a **verified, sanitized egress boundary**.

---

## 16.7 Integrate into EgressGate / Exporter

Before sending any outbox payload:

* determine payload labels (from referenced evidence and/or explicit classification)
* DLP scan if labels include `confidential|pii|phi|credentials`
* deny egress if policy says so

---

## 16.8 Tests

* `evidence-classification.test.ts`
* `publishing-redaction-dlp.test.ts`
* `export-egress-block.test.ts`

---

---

# Change Set 17 — Tamper-Evident Audit Journal (Hash Chain + Signed Checkpoints)

## Why this matters

Audit logs that can be modified are not legally persuasive. The standard expectation is:

* append-only
* tamper-evident
* verifiable
* exportable

Implement as a **hash chain** (and optionally Merkle batching). This is essentially Event Sourcing plus integrity.

---

## 17.1 Journal entry schema

### New file: `src/audit/journal.ts`

```ts
import { canonicalJson, sha256Hex } from '../runtime/crypto';

export interface JournalEntry {
  seq: number;
  timestamp: string;
  tenantId: string;
  sessionId: string;
  turnNumber: number;

  op: string;
  argsSummary?: any;

  prevHash: string | null;
  entryHash: string;

  policyHash?: string;
  decisionHash?: string;
  promptHash?: string;
  modelId?: string;
}

export interface JournalCheckpoint {
  seq: number;
  rootHash: string; // entryHash of last entry (or Merkle root)
  timestamp: string;
  signature: { keyId: string; alg:'ed25519'; sig: string };
}

export function computeEntryHash(e: Omit<JournalEntry,'entryHash'>): string {
  return sha256Hex(canonicalJson(e));
}
```

---

## 17.2 JournalStore interface

### New file: `src/audit/journal-store.ts`

```ts
import { JournalEntry, JournalCheckpoint } from './journal';

export interface JournalStore {
  append(entry: Omit<JournalEntry,'seq'|'prevHash'|'entryHash'>): JournalEntry;
  checkpoint(): JournalCheckpoint;
  readFrom(seq: number, limit: number): JournalEntry[];
  latestSeq(): number;
  latestHash(): string | null;
  verify(): { valid: boolean; errors: string[] };
}
```

Provide `journal-store-memory.ts` now; add file-backed later.

---

## 17.3 Wire Session execution to append journal entries

### Change: `Session.executeTurn`

After each operation (or at least each FFI call), append:

* `policyHash` (session)
* `decisionHash` (PDP decision)
* `promptHash` and `modelId` for LLM calls

---

## 17.4 Signed checkpoints

Every turn (or every N entries):

* compute checkpoint rootHash
* sign with audit key

This yields offline verifiability.

---

## 17.5 Protocol ops

* `journal.verify`
* `journal.export`

---

## 17.6 Tests

* `journal-tamper-detect.test.ts`
* `journal-checkpoint-signature.test.ts`

---

---

# Change Set 18 — Deterministic Replay (Record/Replay) for Nondeterministic Effects

You have snapshot/restore for interpreter state, but not for LLM/time/random/network. Replay is mandatory for serious incident response.

Add an **EffectTape** (recorded side-effects) and a Decorator adapter.

---

## 18.1 Add Session execution modes

### Change: `SessionConfig`

Add:

* `executionMode?: 'live'|'record'|'replay'`
* `replaySourceSessionId?: string`

---

## 18.2 EffectTape schema

### New file: `src/replay/effect-tape.ts`

```ts
import { canonicalJson, sha256Hex } from '../runtime/crypto';

export type EffectKind = 'llm.complete'|'time.now'|'random.u64'|'network.http';

export interface EffectRecord {
  seq: number;
  kind: EffectKind;

  request: any;
  response: any;

  requestHash: string;
  responseHash: string;
}

export function hashEffectPayload(p: any): string {
  return sha256Hex(canonicalJson(p));
}
```

---

## 18.3 EffectTapeStore

### New file: `src/replay/effect-tape-store.ts`

```ts
import { EffectRecord } from './effect-tape';

export interface EffectTapeStore {
  append(kind: string, request: any, response: any): EffectRecord;
  next(kind: string, requestHash: string): EffectRecord | undefined;
  resetCursor(): void;
}
```

---

## 18.4 Recorded LLM adapter (Decorator)

### New file: `src/replay/recorded-llm.ts`

```ts
import { LLMAdapter } from '../ext/llm/adapter';
import { PromptIR } from '../prompt/prompt-ir';
import { EffectTapeStore } from './effect-tape-store';
import { hashEffectPayload } from './effect-tape';

export class RecordedLLMAdapter implements LLMAdapter {
  constructor(
    private mode: 'live'|'record'|'replay',
    private base: LLMAdapter,
    private tape: EffectTapeStore
  ) {}

  async complete(prompt: PromptIR, options?: any): Promise<string> {
    const request = { promptHash: prompt.promptHash, templateId: prompt.templateId, options: options ?? {} };
    const requestHash = hashEffectPayload(request);

    if (this.mode === 'replay') {
      const rec = this.tape.next('llm.complete', requestHash);
      if (!rec) throw new Error(`Replay missing llm.complete effect for ${requestHash}`);
      return rec.response.text;
    }

    const text = await this.base.complete(prompt, options);
    if (this.mode === 'record') {
      this.tape.append('llm.complete', request, { text });
    }
    return text;
  }
}
```

Add similar wrappers for `time.now`, `random`, and `network`.

---

## 18.5 Tests

* `record-replay-llm.test.ts`
* `replay-requesthash-mismatch.test.ts`

---

---

# Change Set 19 — Cryptographic Identity + Strong Approvals + Separation of Duties

You already have approvals conceptually. Now make them legally meaningful:

* bound to verified principal identity
* signed receipts
* separation-of-duties enforced
* scoped + expiring

---

## 19.1 Principal + assertion schema

### New file: `src/identity/principal.ts`

```ts
export interface Principal {
  subject: string;
  displayName?: string;
  roles: string[];
  assurance?: 'aal2'|'aal3';
  tenantId: string;
  idp: { issuer: string; keyId: string };
}

export interface PrincipalAssertion {
  principal: Principal;
  issuedAt: string;
  expiresAt: string;
  signature: { alg:'ed25519'|'rs256'; keyId: string; sig: string };
}
```

Add `IdentityVerifier` to verify assertion signature and expiry.

---

## 19.2 Bind session to PrincipalAssertion

### Change: `SessionConfig`

Add: `principalAssertion?: PrincipalAssertion`

Session verifies it on creation and stores principal in `session.ctx.principal`. PDP can now reliably evaluate subject constraints.

---

## 19.3 ApprovalReceipt schema (signed)

### New file: `src/identity/approval.ts`

```ts
import { canonicalJson, sha256Hex } from '../runtime/crypto';
import { Principal } from './principal';

export interface ApprovalScope {
  op: string;
  resource?: string;
  decisionHash?: string;
  policyHash: string;
  sessionId: string;
  expiresAt: string;
}

export interface ApprovalReceipt {
  schemaVersion: 1;
  tenantId: string;
  approver: Principal;

  scope: ApprovalScope;

  issuedAt: string;
  receiptHash: string;

  signature: { keyId: string; alg:'ed25519'; sig: string };
}

export function computeReceiptHash(r: Omit<ApprovalReceipt,'receiptHash'|'signature'>): string {
  return sha256Hex(canonicalJson(r));
}
```

---

## 19.4 Enforce separation of duties

Policy obligation `require-approval` should be augmented with:

* `disallowSelfApproval: true`
* `minAssurance`

Then approval verification checks:

* approver != actor/author
* roles satisfied
* assurance satisfied
* scope matches decisionHash + sessionId + policyHash
* expiry not passed

---

## 19.5 Protocol op `approve`

Input: `ApprovalReceipt`
Output: `(ok ...)` or `(error ...)`

On success:

* journal append approval event
* attach receipt to pending proposal commit

---

## 19.6 Tests

* `approval-sod.test.ts`
* `approval-scope-binding.test.ts`
* `approval-expiry.test.ts`

---

---

# Change Set 20 — Runtime Attestation + Signed Releases + SBOM hooks

Enterprises will also ask: “How do we know the runtime is genuine?”

Add runtime attestation fields in `versionInfo` and `hello`, plus CI scaffolding for SBOM + signing.

---

## 20.1 Extend `versionInfo` with attestation fields

### Change: `versionInfo`

Add:

```ts
build: {
  artifactHash: string;
  sbomHash?: string;
  builderId?: string;
  provenanceSig?: string;
  keyId?: string;
}
```

---

## 20.2 HelloResponse includes attestation summary

Add:

* `runtime-artifact-hash`
* `sbom-hash`
* `build-key-id`

Clients can pin expected hashes.

---

## 20.3 CI workflow changes (release pipeline)

Add steps:

* generate SBOM
* compute artifact hash
* sign provenance
* publish artifacts

(Exact tool commands are your choice; this is the structural addition.)

---

## 20.4 Tests

* `attestation-present.test.ts`
* `hello-attestation.test.ts`

---

---

# Change Set 21 — OpenTelemetry-grade Observability + Correlation IDs

Your metrics exist, but enterprises want OTLP/trace correlation.

---

## 21.1 Tracer interface

### New file: `src/telemetry/tracer.ts`

```ts
export interface Span {
  end(): void;
  setAttribute(k: string, v: string|number|boolean): void;
}

export interface Tracer {
  startSpan(name: string, attrs?: Record<string, any>): Span;
}
```

Provide `NoopTracer` + adapter later.

---

## 21.2 Instrument Session + FFI + LLM calls

Add spans around:

* `executeTurn`
* each `ffi.call`
* `llm.complete`

Set attributes:

* tenantId, sessionId, turnNumber
* op
* policyHash, decisionHash
* promptHash, templateId
* journalSeq

Now SIEM can correlate telemetry to journal integrity chain.

---

## 21.3 Tests

* `tracing-smoke.test.ts` (ensures spans are created)

---

---

# Change Set 22 — Eval Suites as Governed Artifacts + Policy Quality Gates

Even with safe runtime semantics, enterprise trust collapses if prompts/models regress. Add eval-gated usage:

* EvalSuite (versioned)
* EvalRun (signed pass/fail)
* Policy obligation: require passing eval for a template/model

---

## 22.1 EvalSuite schema

### New file: `src/evals/eval-suite.ts`

```ts
import { canonicalJson, sha256Hex } from '../runtime/crypto';

export interface EvalCase {
  id: string;
  description: string;
  templateId: string;
  slots: Record<string, unknown>;
  expected: {
    mustContain?: string[];
    mustNotContain?: string[];
    jsonSchema?: any;
  };
  labels?: string[];
  riskTier?: 'low'|'moderate'|'high'|'regulated';
}

export interface EvalSuite {
  schemaVersion: 1;
  id: string;
  version: string;
  description: string;
  cases: EvalCase[];
  suiteHash: string;
}

export function computeSuiteHash(s: Omit<EvalSuite,'suiteHash'>): string {
  return sha256Hex(canonicalJson(s));
}
```

---

## 22.2 EvalRun (signed)

### New file: `src/evals/eval-result.ts`

```ts
import { canonicalJson, sha256Hex } from '../runtime/crypto';

export interface EvalRun {
  suiteId: string;
  suiteHash: string;
  modelId: string;
  templateVersions: Record<string,string>;
  startedAt: string;
  finishedAt: string;

  passed: boolean;
  failures: Array<{ caseId: string; reason: string; outputHash?: string }>;

  runHash: string;
  signature?: { keyId: string; alg:'ed25519'; sig: string };
}

export function computeRunHash(r: Omit<EvalRun,'runHash'|'signature'>): string {
  return sha256Hex(canonicalJson(r));
}
```

---

## 22.3 Policy obligation: require eval pass

Add obligation type:

* `{ kind:'require-eval-pass'; suiteId: string; minVersion?: string }`

Enforce before:

* template usage
* model usage

---

## 22.4 Tests

* `eval-gate-blocks-untested.test.ts`
* `eval-gate-allows-tested.test.ts`

---

---

# Implementation Order (this continuation tranche)

1. **Prompt IR + templates + adapter changes** (Change Set 15)
2. **Classification → Evidence labels → publishing redaction/DLP** (Change Set 16)
3. **Hash‑chained journal + signed checkpoints** (Change Set 17)
4. **Record/replay nondeterministic effects** (Change Set 18)
5. **Principal assertions + approval receipts + SoD** (Change Set 19)
6. **Runtime attestation metadata + hello** (Change Set 20)
7. **OpenTelemetry-style tracing** (Change Set 21)
8. **Eval suites + policy quality gates** (Change Set 22)

---

If you want me to continue again, the next highest-leverage additions (still changes-only) are:

* **Change Set 23 — Stronger transactional World: 2-phase commit + patchsets + structured diffs + merge/conflict policy**
* **Change Set 24 — Policy verification: rule-unit-tests + mutation testing + property tests for PDP determinism and completeness**
* **Change Set 25 — Tenant-grade crypto: envelope encryption, key rotation, secret-zero bootstrapping, and encrypted artifact storage**

Say **continue** and I’ll write those out with the same concrete “new files / modifications / tests” treatment.
