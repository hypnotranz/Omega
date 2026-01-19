Below is the next tranche of **changes-only** work, sequenced so you can implement incrementally while reading. This continues from the previous tranche (Prompt IR → DLP → Journal hash chain → Replay → Identity approvals → Attestation → Telemetry → Eval gating).

This tranche addresses what still blocks “enterprise / legal / medical trustability” in practice:

* **Change control** beyond “staged writes”: patchsets, preconditions, diffs, merge/conflict, 2‑phase commit, signed commit receipts
* **Governance of the policy layer itself**: determinism, explainability, testability, mutation tests, signed policy bundles
* **Encryption / key management / crypto-shredding**: at-rest confidentiality + legally meaningful deletion
* **Retention and legal hold**: records management without violating your monotone semantics inside a live session
* **Multi-tenant isolation and ABAC**: strong boundaries across tenants and principals
* **Regulated-domain safety profiles**: enforceable “clinical / legal / finance” modes as policy + validators, not vibes
* **Output contracts**: schema-validated, evidence-linked, stale-proof outputs
* **Connector framework**: controlled ingress with provenance + caching, not “random HTTP in the middle of eval”

---

# Change Set 23 — Transactional World v2: PatchSets, Preconditions, Diff/Merge, Two‑Phase Commit, Signed Commit Receipts

## What this adds (beyond current `StagedWorld` proposals)

Your current proposal mode is a good **Unit of Work**, but enterprises will still ask:

* “Show me the diff” (for review, compliance, change tickets)
* “What did you assume was the base?” (preconditions / optimistic concurrency)
* “Detect conflicts; don’t just overwrite” (merge policy)
* “Separate prepare from commit” (2PC, approvals, CI gates)
* “Give me a tamper-evident commit receipt” (signed, journal-linked)

So we upgrade proposals into first-class **PatchSets** (Command objects) with explicit **preconditions** and stable diffs.

---

## 23.1 New PatchSet model

### New file: `src/world/patchset.ts`

```ts
import { canonicalJson, sha256Hex } from '../runtime/crypto';

export type PatchOpKind = 'write' | 'delete' | 'rename';

export interface BasePrecondition {
  // optimistic concurrency control
  expectedFingerprint: string; // fingerprint(ref) observed at proposal time
  observedAt: string;          // ISO
}

export interface PatchOpBase {
  kind: PatchOpKind;
  ref: string;
  labels?: string[];           // data labels (pii/phi/confidential/etc.)
  precondition?: BasePrecondition;
}

export interface WriteOp extends PatchOpBase {
  kind: 'write';
  content: string;             // canonical content
  // optional review artifact
  unifiedDiff?: string;
  contentHash: string;
}

export interface DeleteOp extends PatchOpBase {
  kind: 'delete';
}

export interface RenameOp extends PatchOpBase {
  kind: 'rename';
  toRef: string;
}

export type PatchOp = WriteOp | DeleteOp | RenameOp;

export interface PatchSet {
  schemaVersion: 1;

  id: string;
  tenantId: string;
  sessionId: string;
  createdAt: string;

  // hash of the policy bundle that allowed creating it
  policyHash: string;

  // stable representation; useful for deterministic approvals
  patchHash: string;

  ops: PatchOp[];
}

export function computePatchHash(ps: Omit<PatchSet,'patchHash'>): string {
  return sha256Hex(canonicalJson(ps));
}

export function finalizePatchSet(ps: Omit<PatchSet,'patchHash'>): PatchSet {
  return { ...ps, patchHash: computePatchHash(ps) };
}
```

Notes:

* This is essentially **Command** + **Memento**: operations + enough metadata to replay/verify.
* `precondition.expectedFingerprint` enables optimistic concurrency and safe merge.

---

## 23.2 Diff generator (review artifact)

You can implement a minimal line diff now; later swap for a stronger diff algorithm without changing PatchSet schema.

### New file: `src/world/diff.ts`

```ts
// Minimal unified diff generator (line-based; not Myers-optimal but stable)
// You can replace implementation later behind same interface.

export function unifiedDiff(oldText: string, newText: string, fileName: string): string {
  const oldLines = oldText.split('\n');
  const newLines = newText.split('\n');

  // naive algorithm: emit full replacement if differs
  if (oldText === newText) return `--- a/${fileName}\n+++ b/${fileName}\n@@ -0,0 +0,0 @@\n`;

  const header = `--- a/${fileName}\n+++ b/${fileName}\n`;
  const body = [
    `@@ -1,${oldLines.length} +1,${newLines.length} @@`,
    ...oldLines.map(l => `-${l}`),
    ...newLines.map(l => `+${l}`),
  ].join('\n');

  return header + body + '\n';
}
```

---

## 23.3 TransactionWorld (Unit of Work + 2PC)

### New file: `src/world/transaction-world.ts`

```ts
import { World } from '../ext/world/interface';
import { finalizePatchSet, PatchOp, PatchSet } from './patchset';
import { unifiedDiff } from './diff';
import { sha256Hex } from '../runtime/crypto';

export type TxStatus = 'open' | 'prepared' | 'committed' | 'aborted';

export interface TxOptions {
  tenantId: string;
  sessionId: string;
  policyHash: string;
  mergePolicy?: 'fail'|'ours'|'theirs'; // expand later
}

export class TransactionWorld {
  private ops: PatchOp[] = [];
  private status: TxStatus = 'open';

  constructor(private base: World, private opts: TxOptions) {}

  getStatus(): TxStatus { return this.status; }

  write(ref: string, content: string): void {
    this.assertOpen();

    const old = this.safeRead(ref);
    const expectedFp = this.base.fingerprint(ref);

    this.ops.push({
      kind: 'write',
      ref,
      content,
      unifiedDiff: old === null ? undefined : unifiedDiff(old, content, ref),
      contentHash: sha256Hex(content),
      precondition: { expectedFingerprint: expectedFp, observedAt: new Date().toISOString() },
    });
  }

  delete(ref: string): void {
    this.assertOpen();
    const expectedFp = this.base.fingerprint(ref);
    this.ops.push({
      kind: 'delete',
      ref,
      precondition: { expectedFingerprint: expectedFp, observedAt: new Date().toISOString() },
    });
  }

  rename(ref: string, toRef: string): void {
    this.assertOpen();
    const expectedFp = this.base.fingerprint(ref);
    this.ops.push({
      kind: 'rename',
      ref,
      toRef,
      precondition: { expectedFingerprint: expectedFp, observedAt: new Date().toISOString() },
    });
  }

  exportPatchSet(): PatchSet {
    return finalizePatchSet({
      schemaVersion: 1,
      id: `patch-${crypto.randomUUID()}`,
      tenantId: this.opts.tenantId,
      sessionId: this.opts.sessionId,
      createdAt: new Date().toISOString(),
      policyHash: this.opts.policyHash,
      ops: [...this.ops],
    });
  }

  // Phase 1: prepare (validate preconditions)
  prepare(): { ok: true } | { ok: false; conflicts: Array<{ ref: string; expected: string; actual: string }> } {
    this.assertOpen();
    const conflicts: Array<{ ref: string; expected: string; actual: string }> = [];

    for (const op of this.ops) {
      const pre = op.precondition;
      if (!pre) continue;
      const actual = this.base.fingerprint(op.ref);
      if (actual !== pre.expectedFingerprint) {
        conflicts.push({ ref: op.ref, expected: pre.expectedFingerprint, actual });
      }
    }

    if (conflicts.length > 0) {
      return { ok: false, conflicts };
    }

    this.status = 'prepared';
    return { ok: true };
  }

  // Phase 2: commit (after approvals / gates)
  commit(): void {
    if (this.status !== 'prepared') {
      throw new Error(`Cannot commit tx in status ${this.status}`);
    }

    for (const op of this.ops) {
      switch (op.kind) {
        case 'write':
          this.base.write(op.ref, op.content);
          break;
        case 'delete':
          // if your World supports delete; else represent as write empty + tombstone in metadata
          (this.base as any).delete?.(op.ref) ?? this.base.write(op.ref, '');
          break;
        case 'rename':
          (this.base as any).rename?.(op.ref, op.toRef);
          break;
      }
    }

    this.status = 'committed';
  }

  abort(): void {
    if (this.status === 'committed') throw new Error('Cannot abort committed tx');
    this.status = 'aborted';
    this.ops = [];
  }

  private safeRead(ref: string): string | null {
    try { return this.base.read(ref); } catch { return null; }
  }

  private assertOpen(): void {
    if (this.status !== 'open') throw new Error(`Tx not open: ${this.status}`);
  }
}
```

This is explicit **Two‑Phase Commit**:

* `prepare()` = “I can commit safely; base hasn’t changed”
* `commit()` = “apply”
* approvals and CI gates occur between the phases.

---

## 23.4 Merge policies (pluggable Strategy)

Initially implement `'fail'|'ours'|'theirs'`. Later add true 3‑way merge for text files and AST merge for TS/Lisp.

### New file: `src/world/merge.ts`

```ts
export type MergePolicy = 'fail'|'ours'|'theirs';

export function resolveConflict(policy: MergePolicy): 'use-new'|'use-old'|'fail' {
  switch (policy) {
    case 'ours': return 'use-new';
    case 'theirs': return 'use-old';
    default: return 'fail';
  }
}
```

Hook this into `prepare()` if you want auto-resolution (careful for regulated modes; usually require manual review).

---

## 23.5 Signed Commit Receipt (ties together PatchSet + Journal + Approval)

### New file: `src/world/commit-receipt.ts`

```ts
import { canonicalJson, sha256Hex } from '../runtime/crypto';
import { PatchSet } from './patchset';

export interface CommitReceipt {
  schemaVersion: 1;

  tenantId: string;
  sessionId: string;

  patchHash: string;
  policyHash: string;

  preparedAt: string;
  committedAt: string;

  journalCheckpointHash: string; // from Change Set 17
  approvals?: string[];          // receipt hashes from Change Set 19

  receiptHash: string;
  signature: { keyId: string; alg:'ed25519'; sig: string };
}

export function computeCommitReceiptHash(r: Omit<CommitReceipt,'receiptHash'|'signature'>): string {
  return sha256Hex(canonicalJson(r));
}
```

---

## 23.6 Session integration changes

### Change: `Session.executeTurn`

* If `policy.worldRules.proposalMode === true`, do **not** directly call `world.write`.
* Instead route to a per-turn `TransactionWorld` (Unit of Work) and emit a `proposed` Outcome that includes:

  * PatchSet
  * conflicts (if prepare fails)
  * diffs (from patch ops)
* Only after approvals + gates: commit transaction, append commit receipt to journal.

This makes “writes” reviewable and safe.

---

## 23.7 Lisp surface API (changes)

Add transactional primitives (do **not** remove old ones; deprecate gradually):

```lisp
;; Begin a tx (returns tx handle)
(world.tx/begin)

(world.tx/write tx "src/a.ts" new-content)
(world.tx/delete tx "src/old.ts")
(world.tx/rename tx "src/a.ts" "src/b.ts")

;; Phase 1
(world.tx/prepare tx)
;; => (ok) | (conflict ((:ref ... :expected ... :actual ...) ...))

;; Export patchset for review
(world.tx/patchset tx)

;; Phase 2
(world.tx/commit tx)   ;; only host allows after approvals
(world.tx/abort tx)
```

---

## 23.8 Tests

* `test/integration/tx-prepare-conflict.test.ts`
* `test/integration/tx-diff-generated.test.ts`
* `test/integration/tx-two-phase-commit.test.ts`
* `test/security/tx-deny-write-outside-allowedpaths.test.ts` (ensure policy still gates)

---

# Change Set 24 — Policy-as-Code v2: Deterministic PDP, Explainability, Linters, Property Tests, Mutation Testing, Signed Policy Bundles

## Why this is still missing

You already enforce ops via `SessionEnforcer`, but enterprises will ask:

* “Prove the policy is complete” (no op escapes enforcement)
* “Policy decisions must be deterministic” (same inputs → same decisionHash)
* “Explain why something was denied” (for ticketing and audits)
* “Version and sign policy bundles” (change management)
* “Test the policy layer like production code” (property tests + mutation tests)

This is the **Specification** pattern applied to governance.

---

## 24.1 Formalize PolicyBundle + decision hashing

### New file: `src/policy/policy-bundle.ts`

```ts
import { canonicalJson, sha256Hex } from '../runtime/crypto';

export interface PolicyRule {
  id: string;
  effect: 'allow'|'deny';
  op: string;          // e.g. "world.read"
  resource?: string;   // glob or structured name
  when?: Record<string, any>; // ABAC conditions (principal roles, labels, etc.)
  obligations?: Array<Record<string, any>>;
}

export interface PolicyBundle {
  schemaVersion: 1;
  id: string;
  version: string;           // SemVer
  tenantId: string;

  rules: PolicyRule[];

  // computed
  policyHash: string;

  // signed bundle
  signature?: { keyId: string; alg:'ed25519'; sig: string };
}

export function computePolicyHash(b: Omit<PolicyBundle,'policyHash'|'signature'>): string {
  // Ensure determinism: canonical JSON + stable ordering
  return sha256Hex(canonicalJson(b));
}
```

---

## 24.2 PDP with deterministic evaluation + explanation

### New file: `src/policy/pdp.ts`

```ts
import { PolicyBundle, PolicyRule } from './policy-bundle';
import { canonicalJson, sha256Hex } from '../runtime/crypto';

export interface DecisionInput {
  tenantId: string;
  sessionId: string;
  principal?: { subject: string; roles: string[] };
  op: string;
  resource?: string;
  labels?: string[];
  context?: Record<string, any>;
}

export interface Decision {
  effect: 'allow'|'deny';
  matchedRuleIds: string[];
  obligations: Array<Record<string, any>>;
  reason: string;
  decisionHash: string;
}

export function computeDecisionHash(d: Omit<Decision,'decisionHash'>): string {
  return sha256Hex(canonicalJson(d));
}

export class PDP {
  constructor(private bundle: PolicyBundle) {}

  decide(input: DecisionInput): Decision {
    // Determinism requirements:
    // - stable rule ordering (sort by rule.id)
    // - stable matching semantics

    const rules = [...this.bundle.rules].sort((a,b) => a.id.localeCompare(b.id));

    const matched: PolicyRule[] = [];
    for (const r of rules) {
      if (r.op !== input.op) continue;
      if (r.resource && input.resource && !this.matchResource(r.resource, input.resource)) continue;
      if (r.when && !this.evalWhen(r.when, input)) continue;
      matched.push(r);
    }

    // Deny-overrides (typical enterprise semantics)
    const denies = matched.filter(r => r.effect === 'deny');
    const allows = matched.filter(r => r.effect === 'allow');

    const effect = denies.length > 0 ? 'deny' : (allows.length > 0 ? 'allow' : 'deny');
    const matchedRuleIds = matched.map(r => r.id);
    const obligations = matched.flatMap(r => r.obligations ?? []);
    const reason =
      effect === 'allow'
        ? (allows.length > 0 ? `allowed by ${allows[0].id}` : 'allowed by default') // usually not used
        : (denies.length > 0 ? `denied by ${denies[0].id}` : 'denied: no matching allow');

    const decisionNoHash: Omit<Decision,'decisionHash'> = {
      effect, matchedRuleIds, obligations, reason
    };

    return { ...decisionNoHash, decisionHash: computeDecisionHash(decisionNoHash) };
  }

  private matchResource(rulePattern: string, resource: string): boolean {
    // replace with minimatch or structured resource parser
    if (rulePattern === resource) return true;
    if (rulePattern.endsWith('*')) return resource.startsWith(rulePattern.slice(0, -1));
    return false;
  }

  private evalWhen(when: Record<string, any>, input: DecisionInput): boolean {
    // Minimal ABAC now; expand in Change Set 27
    if (when.role) return (input.principal?.roles ?? []).includes(when.role);
    if (when.label) return (input.labels ?? []).includes(when.label);
    return true;
  }
}
```

---

## 24.3 Policy linting and completeness checks

### New file: `src/policy/lint.ts`

```ts
import { PolicyBundle } from './policy-bundle';

export interface PolicyLintResult {
  ok: boolean;
  errors: string[];
  warnings: string[];
}

export function lintPolicy(bundle: PolicyBundle, knownOps: string[]): PolicyLintResult {
  const errors: string[] = [];
  const warnings: string[] = [];

  const ruleIds = new Set<string>();
  for (const r of bundle.rules) {
    if (ruleIds.has(r.id)) errors.push(`Duplicate rule id: ${r.id}`);
    ruleIds.add(r.id);

    if (!knownOps.includes(r.op)) warnings.push(`Unknown op in rule ${r.id}: ${r.op}`);
  }

  // Completeness: every known op should have at least one rule
  for (const op of knownOps) {
    if (!bundle.rules.some(r => r.op === op)) {
      warnings.push(`No explicit rule for op: ${op} (will be implicit deny unless you special-case)`);
    }
  }

  return { ok: errors.length === 0, errors, warnings };
}
```

---

## 24.4 Policy bundle signing / verification (ties into Change Set 25 keys)

Add:

* `PolicyBundleStore` that only loads bundles with valid signatures.
* `policyHash` is computed and stored; session references this hash in PromptIR/journal.

---

## 24.5 Property tests + mutation tests

### New tests

* `test/policy/pdp-determinism.property.test.ts`
  Generate random bundles and inputs; ensure `decide(input)` stable across repeated invocations.
* `test/policy/deny-overrides.test.ts`
  Ensure any deny match results in deny even if allow matches.
* `test/policy/mutation.test.ts`
  Implement a mutation harness that flips random rule effects; ensure at least one test fails (classic mutation testing gate).

This is how you elevate policy from “config” to “governed program artifact.”

---

# Change Set 25 — Tenant-Grade Cryptography: Envelope Encryption, Key Rotation, Crypto‑Shredding

## Why this is still missing

Without encryption/key rotation, you’ll fail security review even if you’re “safe by policy,” because:

* logs, evidence snippets, effect tapes, and artifacts contain sensitive data
* “delete” is legally required (retention schedules, right-to-erasure)
* **tamper-evident** isn’t the same as **confidential**

So you add:

* AES‑GCM envelope encryption for persisted stores
* tenant-scoped KEKs (Key Encryption Keys) with versioning
* DEKs (Data Encryption Keys) per object
* rotation + crypto-shredding for deletion

---

## 25.1 Key management abstraction

### New file: `src/security/keys.ts`

```ts
export interface KeyRef {
  tenantId: string;
  purpose: 'evidence'|'artifacts'|'journal'|'snapshots'|'effects';
  version: number;
}

export interface WrappedKey {
  keyRef: KeyRef;
  wrapped: Uint8Array; // DEK encrypted by KEK
}

export interface KeyProvider {
  // returns KEK material (or a handle if using external KMS)
  getKEK(ref: KeyRef): Promise<CryptoKey>;
  // returns current version for purpose
  currentVersion(tenantId: string, purpose: KeyRef['purpose']): Promise<number>;
}
```

---

## 25.2 Envelope crypto

### New file: `src/security/envelope.ts`

```ts
import { KeyProvider, KeyRef, WrappedKey } from './keys';

export interface EncryptedBlob {
  schemaVersion: 1;
  keyRef: KeyRef;
  wrappedDEK: Uint8Array;
  iv: Uint8Array;
  ciphertext: Uint8Array;
  aad?: Uint8Array;
}

export class EnvelopeCrypto {
  constructor(private keys: KeyProvider) {}

  async encrypt(tenantId: string, purpose: KeyRef['purpose'], plaintext: Uint8Array, aad?: Uint8Array): Promise<EncryptedBlob> {
    const version = await this.keys.currentVersion(tenantId, purpose);
    const keyRef: KeyRef = { tenantId, purpose, version };

    // Generate DEK
    const dek = await crypto.subtle.generateKey({ name: 'AES-GCM', length: 256 }, true, ['encrypt','decrypt']);
    const rawDEK = new Uint8Array(await crypto.subtle.exportKey('raw', dek));

    // Wrap DEK with KEK (AES-KW or AES-GCM wrapping; simplest: encrypt rawDEK under KEK with GCM)
    const kek = await this.keys.getKEK(keyRef);
    const wrapIv = crypto.getRandomValues(new Uint8Array(12));
    const wrapped = new Uint8Array(await crypto.subtle.encrypt({ name:'AES-GCM', iv: wrapIv }, kek, rawDEK));

    // Encrypt payload with DEK
    const iv = crypto.getRandomValues(new Uint8Array(12));
    const ct = new Uint8Array(await crypto.subtle.encrypt({ name:'AES-GCM', iv, additionalData: aad }, dek, plaintext));

    // Store wrapIv inside aad or extend schema; simplest: concatenate (wrapIv || wrapped)
    const wrappedDEK = new Uint8Array(wrapIv.length + wrapped.length);
    wrappedDEK.set(wrapIv, 0);
    wrappedDEK.set(wrapped, wrapIv.length);

    return { schemaVersion: 1, keyRef, wrappedDEK, iv, ciphertext: ct, aad };
  }

  async decrypt(blob: EncryptedBlob): Promise<Uint8Array> {
    const kek = await this.keys.getKEK(blob.keyRef);

    const wrapIv = blob.wrappedDEK.slice(0, 12);
    const wrapped = blob.wrappedDEK.slice(12);

    const rawDEK = new Uint8Array(await crypto.subtle.decrypt({ name:'AES-GCM', iv: wrapIv }, kek, wrapped));
    const dek = await crypto.subtle.importKey('raw', rawDEK, { name:'AES-GCM' }, false, ['decrypt']);

    const pt = new Uint8Array(await crypto.subtle.decrypt(
      { name:'AES-GCM', iv: blob.iv, additionalData: blob.aad },
      dek,
      blob.ciphertext
    ));
    return pt;
  }
}
```

---

## 25.3 Encrypt persisted stores (Decorator)

Add encrypted wrappers for:

* `JournalStore`
* `ArtifactStore.serialize()`
* `EvidenceRegistry.serialize()`
* `EffectTapeStore`

This is GoF **Decorator**: same interface, encryption applied at storage boundary.

---

## 25.4 Rotation + rewrap-on-read

Add to `KeyProvider`:

* `rotate(tenantId, purpose): newVersion`

Then implement “rewrap on read”:

* when decrypting an old-version blob, re-encrypt using current version and write back.

This yields rolling compliance.

---

## 25.5 Crypto-shredding (deletion without rewriting append-only logs)

If you must “delete,” you delete the KEK version (or revoke it) for that tenant+purpose+version; all blobs under it become undecryptable.

This gives you a real deletion story without breaking tamper-evident append-only journaling.

---

## 25.6 Tests

* `test/crypto/envelope-roundtrip.test.ts`
* `test/crypto/rewrap-on-read.test.ts`
* `test/crypto/crypto-shred.test.ts`

---

# Change Set 26 — Retention & Legal Hold: Records Management without Breaking Session Monotonicity

## Why this is still missing

Even perfect auditability fails legal review if you can’t answer:

* “How long do you retain PII/PHI?”
* “Can you place a legal hold?”
* “Can you prove deletion happened (or crypto-shredded)?”
* “Can you export for eDiscovery with redaction?”

The trick: keep session semantics monotone, but make **persistence** subject to retention policy.

---

## 26.1 Add retention metadata to persisted objects

### New file: `src/retention/retention.ts`

```ts
export type RetentionClass =
  | 'ephemeral'
  | 'standard'
  | 'regulated'
  | 'audit';

export interface RetentionMeta {
  retentionClass: RetentionClass;
  labels: string[];
  createdAt: string;
  expiresAt?: string;
  legalHold?: { holdId: string; reason: string; placedAt: string };
}
```

---

## 26.2 RetentionPolicy + evaluator

### New file: `src/retention/policy.ts`

```ts
export interface RetentionRule {
  id: string;
  whenLabel: string; // e.g. 'phi'
  retentionClass: 'regulated';
  ttlDays: number;
}

export interface RetentionPolicy {
  rules: RetentionRule[];
  defaultTtlDays: number;
}
```

### New file: `src/retention/manager.ts`

```ts
import { RetentionPolicy } from './policy';
import { RetentionMeta } from './retention';

export class RetentionManager {
  constructor(private policy: RetentionPolicy) {}

  computeMeta(labels: string[]): RetentionMeta {
    const now = new Date();
    const matched = this.policy.rules.find(r => labels.includes(r.whenLabel));
    const ttlDays = matched?.ttlDays ?? this.policy.defaultTtlDays;

    const expires = new Date(now.getTime() + ttlDays * 24*60*60*1000);
    return {
      retentionClass: matched?.retentionClass ?? 'standard',
      labels,
      createdAt: now.toISOString(),
      expiresAt: expires.toISOString(),
    };
  }
}
```

---

## 26.3 Purge workflow + legal hold

Add operations:

* `retention.placeHold(holdId, predicate)`
* `retention.releaseHold(holdId)`
* `retention.purgeExpired()`

In purge:

* if encrypted: crypto-shred key versions for expired blobs (Change Set 25)
* else delete files from storage

Journal still records purge events (tamper-evident).

---

## 26.4 Tests

* `test/retention/ttl-computation.test.ts`
* `test/retention/legal-hold-prevents-purge.test.ts`
* `test/retention/purge-emits-journal-events.test.ts`

---

# Change Set 27 — Multi‑Tenant Isolation + ABAC: Tenant Namespacing, Resource IDs, and Policy Conditions

## Why this is still missing

You have `tenantId` on sessions and prompts, but enterprise auditors will ask for *enforced isolation*:

* a tenant cannot read/write another tenant’s artifacts/journal/evidence
* policies are tenant-scoped and signed
* ABAC conditions depend on principal, labels, environment, time, etc.

---

## 27.1 Canonical resource naming

### New file: `src/runtime/resource.ts`

```ts
export type ResourceScheme = 'file'|'connector'|'artifact'|'evidence'|'journal';

export interface ResourceId {
  scheme: ResourceScheme;
  tenantId: string;
  path: string; // normalized; no ".."
}

export function formatResource(r: ResourceId): string {
  return `${r.scheme}://tenant/${r.tenantId}/${r.path.replace(/^\/+/,'')}`;
}

export function parseResource(s: string): ResourceId {
  // implement strict parser; reject ambiguity
  // keep minimal here
  const m = /^([a-z]+):\/\/tenant\/([^/]+)\/(.+)$/.exec(s);
  if (!m) throw new Error(`Invalid resource: ${s}`);
  return { scheme: m[1] as any, tenantId: m[2], path: m[3] };
}
```

---

## 27.2 World enforcement wrapper (Facade)

Wrap any `World` so every operation is tenant-scoped.

### New file: `src/world/tenant-world.ts`

```ts
import { World } from '../ext/world/interface';

export class TenantWorld implements World {
  constructor(private tenantId: string, private base: World) {}

  read(ref: string): string {
    this.assertTenantPath(ref);
    return this.base.read(ref);
  }
  write(ref: string, content: string): void {
    this.assertTenantPath(ref);
    this.base.write(ref, content);
  }
  list(pattern: string): string[] {
    this.assertTenantPath(pattern);
    return this.base.list(pattern);
  }
  fingerprint(ref: string): string {
    this.assertTenantPath(ref);
    return this.base.fingerprint(ref);
  }

  private assertTenantPath(ref: string): void {
    // enforce “tenant root”
    if (!ref.startsWith(`/tenants/${this.tenantId}/`)) {
      throw new Error(`Cross-tenant access denied: ${ref}`);
    }
    if (ref.includes('..')) throw new Error(`Path traversal denied: ${ref}`);
  }
}
```

---

## 27.3 ABAC condition language (Specification pattern)

Upgrade `when` from ad-hoc object to a small DSL that can be evaluated deterministically and hashed.

### New file: `src/policy/conditions.ts`

```ts
export type Cond =
  | { all: Cond[] }
  | { any: Cond[] }
  | { not: Cond }
  | { hasRole: string }
  | { hasLabel: string }
  | { equals: { key: string; value: string|number|boolean } };

export function evalCond(c: Cond, ctx: { roles: string[]; labels: string[]; kv: Record<string, any> }): boolean {
  if ('all' in c) return c.all.every(x => evalCond(x, ctx));
  if ('any' in c) return c.any.some(x => evalCond(x, ctx));
  if ('not' in c) return !evalCond(c.not, ctx);
  if ('hasRole' in c) return ctx.roles.includes(c.hasRole);
  if ('hasLabel' in c) return ctx.labels.includes(c.hasLabel);
  if ('equals' in c) return ctx.kv[c.equals.key] === c.equals.value;
  return false;
}
```

Then `PolicyRule.when` becomes `Cond`.

---

## 27.4 Tests

* `test/tenant/isolation-read-write.test.ts`
* `test/policy/abac-conditions.test.ts`

---

# Change Set 28 — Regulated Domain Safety Profiles: “Medical / Legal / Finance” as Enforced Modes

## What this does (and what it doesn’t)

This is not “a disclaimer.” This is enforceable gating:

* strict template allowlists
* stricter DLP/redaction profiles
* higher approval requirements
* eval-suite gating is mandatory
* output validators reject prohibited or insufficiently grounded responses

Think of this as **Policy + Contract + Validation Pipeline**.

---

## 28.1 Add RegulatoryProfile to SessionConfig

### Change: `SessionConfig`

Add:

```ts
regulatoryProfile?: 'general'|'medical'|'legal'|'finance';
```

Store it in session context and include in:

* PromptIR metadata segment
* journal entries
* policy decision context

---

## 28.2 Safety validators (Chain of Responsibility)

### New file: `src/safety/validators.ts`

```ts
export interface ValidationFailure {
  code: string;
  message: string;
}

export interface ValidatorContext {
  regulatoryProfile: 'general'|'medical'|'legal'|'finance';
  templateId: string;
  promptHash: string;
  citedEvidenceIds?: string[];
}

export interface OutputValidator {
  validate(output: string, ctx: ValidatorContext): ValidationFailure[];
}

export class RequiresCitationsValidator implements OutputValidator {
  validate(output: string, ctx: ValidatorContext): ValidationFailure[] {
    // For structured outputs: require evidence ids present (upgrade in Change Set 29)
    if (ctx.regulatoryProfile !== 'general') {
      if (!ctx.citedEvidenceIds || ctx.citedEvidenceIds.length === 0) {
        return [{ code:'missing-evidence', message:'Regulated mode requires evidence citations' }];
      }
    }
    return [];
  }
}

export class ProhibitedContentValidator implements OutputValidator {
  validate(output: string, ctx: ValidatorContext): ValidationFailure[] {
    // conservative patterns; tune per domain
    if (ctx.regulatoryProfile === 'medical') {
      if (/prescribe|dosage|take\s+\d+\s*mg/i.test(output)) {
        return [{ code:'treatment-directive', message:'Medical mode disallows unreviewed treatment directives' }];
      }
    }
    if (ctx.regulatoryProfile === 'legal') {
      if (/this constitutes legal advice/i.test(output) === false && /you should sue|file a lawsuit/i.test(output)) {
        return [{ code:'legal-opinion', message:'Legal mode disallows unqualified legal directives' }];
      }
    }
    return [];
  }
}
```

---

## 28.3 Integrate validator pipeline into LLM completion

### Change: `RecordedLLMAdapter.complete` (or session layer)

After model returns text:

* run validators based on session profile + templateId
* on failures: return `NeedsOutcome` (or `ErrorOutcome`) with failures list
* journal the validation failure event

---

## 28.4 Tests

* `test/safety/medical-prohibited-treatment.test.ts`
* `test/safety/regulated-requires-evidence.test.ts`

---

# Change Set 29 — Output Contracts v2: Schema‑Validated S‑Expressions, Evidence Linking, Staleness Proofs

## Why this is missing

You have REPORT/PLAN/ANALYSIS modes, but you need **Design by Contract** at runtime:

* output must be syntactically valid
* must conform to an expected schema
* evidence IDs must exist in registry
* evidence must verify fresh at time of output
* report must be publishable (redaction + DLP already exists)

---

## 29.1 Output contract definitions

### New file: `src/contracts/output-contracts.ts`

```ts
export interface OutputContract {
  id: string;            // e.g. "report.v2"
  version: string;
  headSymbol: string;    // e.g. "report"
  requiredKeys: string[]; // e.g. [":title",":summary",":findings"]
}

export const REPORT_V2: OutputContract = {
  id: 'report.v2',
  version: '2.0.0',
  headSymbol: 'report',
  requiredKeys: [':title', ':summary', ':findings'],
};
```

---

## 29.2 S-expression schema validation

### New file: `src/contracts/sexp-validator.ts`

```ts
import { Value } from '../core/types'; // whatever your Value type is
import { OutputContract } from './output-contracts';

export interface ContractViolation { code: string; message: string; }

function symName(x: any): string | null {
  if (typeof x === 'symbol') return Symbol.keyFor(x) ?? x.toString();
  return null;
}

export function validateContract(expr: Value, c: OutputContract): ContractViolation[] {
  if (!Array.isArray(expr) || expr.length === 0) {
    return [{ code:'not-list', message:'Expected list S-expression' }];
  }
  const head = symName(expr[0]);
  if (head !== c.headSymbol) {
    return [{ code:'wrong-head', message:`Expected (${c.headSymbol} ...) got (${head ?? 'unknown'})` }];
  }

  // naive keyword scan
  const present = new Set<string>();
  for (const el of expr) {
    const n = symName(el);
    if (n && n.startsWith(':')) present.add(n);
  }

  const violations: ContractViolation[] = [];
  for (const k of c.requiredKeys) {
    if (!present.has(k)) violations.push({ code:'missing-key', message:`Missing key ${k}` });
  }
  return violations;
}
```

---

## 29.3 Evidence ID validation + staleness check

### Change: output pipeline

When output is a REPORT/PLAN:

* collect referenced `EvidenceId`s
* verify each exists in registry
* run `verifyEvidence` against current world fingerprints
* if stale → `NeedsOutcome(needs-evidence)` or `ErrorOutcome(stale-evidence)`

This prevents “report cites stale evidence,” which is exactly the kind of thing that destroys legal credibility.

---

## 29.4 Tests

* `test/contracts/report-schema-required-keys.test.ts`
* `test/contracts/report-stale-evidence-rejected.test.ts`

---

# Change Set 30 — Connector Framework: Controlled Ingress with Provenance, Normalization, and Memoization

## Why this matters

Enterprises do not run agents solely on local files. They need connectors:

* ticketing (Jira)
* docs (Confluence/Drive)
* chat (Slack/Teams)
* clinical systems (EHR) — highly regulated
* code hosts (GitHub)

If “world.read” becomes a proxy for “fetch arbitrary URLs,” you’re dead on arrival.

So: connectors are first-class, capability-scoped, policy-gated, evidence-producing.

---

## 30.1 Connector interface

### New file: `src/connectors/connector.ts`

```ts
import { Evidence } from '../provenance/evidence';

export interface ConnectorReadResult {
  text: string;
  evidence: Evidence;
  labels?: string[];
}

export interface Connector {
  scheme(): string; // e.g. "http", "jira", "slack"
  read(ref: string, options?: any): Promise<ConnectorReadResult>;
}
```

---

## 30.2 WorldResolver (Abstract Factory)

### New file: `src/connectors/world-resolver.ts`

```ts
import { Connector } from './connector';

export class WorldResolver {
  private byScheme = new Map<string, Connector>();

  register(c: Connector): void { this.byScheme.set(c.scheme(), c); }

  get(scheme: string): Connector | undefined { return this.byScheme.get(scheme); }
}
```

---

## 30.3 Extend World to accept URI refs (without breaking file refs)

### Change: `world.read`

* if ref matches `<scheme>://...` route to connector
* policy checks:

  * allowed scheme
  * allowed host/resource
  * capability required
* evidence capture:

  * evidence.ref = normalized URI
  * evidence.obtainedVia.op = `connector.read`
  * evidence.fingerprint = hash(text + headers + etag) (deterministic representation)

Then register evidence in EvidenceRegistry automatically for connector reads.

---

## 30.4 Normalization pipeline (Strategy)

Add optional normalizers:

* HTML → text
* Markdown → canonical text
* JSON → stable pretty format
* (PDF, images) — only if you can do it safely; for regulated mode default deny unless explicitly enabled

---

## 30.5 Memoization integration

Connector reads should be artifact-cached via:

* dependency fingerprint = etag/version header or content hash
* `memo/auto` already exists; integrate connector read into dependency tracker

---

## 30.6 Tests

* `test/connectors/uri-routing.test.ts`
* `test/connectors/policy-blocks-disallowed-scheme.test.ts`
* `test/connectors/evidence-created-for-remote-read.test.ts`

---

# Implementation Order for This Continuation

1. **PatchSet + TransactionWorld + prepare/commit + diffs** (Change Set 23)
2. **PDP determinism + signed policy bundles + lint + tests** (Change Set 24)
3. **Envelope encryption + encrypted stores + rotation/shredding** (Change Set 25)
4. **Retention + legal hold + purge + journaling** (Change Set 26)
5. **TenantWorld + ABAC conditions** (Change Set 27)
6. **Regulated profiles + validator chain** (Change Set 28)
7. **Output contracts + stale-proof schema validation** (Change Set 29)
8. **Connector framework** (Change Set 30)

---

If you want me to continue again, the next tranche that closes remaining “first-class enterprise” gaps is:

* **Change Set 31 — CI/CD integration: change tickets, PR linking, codeowner routing, gated commit pipelines (prepare → CI attest → approve → commit)**
* **Change Set 32 — Typed “Safety Case” generator: produce a structured assurance case graph (claims → evidence → argument), signed and replayable**
* **Change Set 33 — Model registry + capability negotiation: pin model families, safety characteristics, evaluation provenance, and rollout policies**

Just say **continue**.
