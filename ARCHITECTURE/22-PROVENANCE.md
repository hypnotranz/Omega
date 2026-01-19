# 22: Provenance (Evidence-Based Reasoning)

## The Core Problem

AI agents can fabricate plausible-sounding claims. Without provenance:

```
Agent: "The bug is on line 42 of auth.ts"
Human: "Is that real or hallucinated?"
Agent: "...I don't know"
```

**Provenance solves this.** Every claim must cite evidence. Evidence can be verified.

---

## Evidence Objects

Evidence is a first-class immutable data structure:

```typescript
interface Evidence {
  // Unique identifier (content-addressed)
  id: EvidenceId;  // "ev:sha256:abc123..."

  // Where the evidence came from
  ref: string;     // File path or world reference
  span?: Span;     // Line range (1-based, inclusive)

  // Content at capture time
  fingerprint: string;  // SHA256 of content
  snippet: string;      // Actual captured text

  // How evidence was obtained
  obtainedVia: {
    op: string;         // "world.read", "world.search", etc.
    params: Record<string, unknown>;
  };

  // World state context
  worldView: 'base' | 'staged';  // Before or after proposals
  capturedAt: number;            // Timestamp
}

interface Span {
  startLine: number;  // 1-based, inclusive
  endLine: number;    // 1-based, inclusive
  startColumn?: number;
  endColumn?: number;
}

type EvidenceId = `ev:sha256:${string}`;
```

### Creating Evidence

```typescript
function captureEvidence(
  world: World,
  ref: string,
  span?: Span,
  meta?: Record<string, unknown>
): Evidence {
  // Read content
  const content = world.read(ref);
  const fingerprint = sha256(content);

  // Extract snippet if span provided
  const snippet = span
    ? extractLines(content, span.startLine, span.endLine)
    : content.slice(0, 500);  // First 500 chars as preview

  // Generate ID from content hash + span
  const id = generateEvidenceId(fingerprint, span);

  return {
    id,
    ref,
    span,
    fingerprint,
    snippet,
    obtainedVia: {
      op: 'world.read',
      params: { ref, span },
    },
    worldView: world.isStaged ? 'staged' : 'base',
    capturedAt: Date.now(),
  };
}

function generateEvidenceId(fingerprint: string, span?: Span): EvidenceId {
  const spanStr = span ? `${span.startLine}-${span.endLine}` : 'full';
  const hash = sha256(`${fingerprint}:${spanStr}`).slice(0, 16);
  return `ev:sha256:${hash}`;
}
```

---

## Staleness Detection

**The Theater-Killer**: Evidence becomes stale when the source changes.

```typescript
function verifyEvidence(evidence: Evidence, world: World): VerificationResult {
  const currentFingerprint = world.fingerprint(evidence.ref);

  if (currentFingerprint !== evidence.fingerprint) {
    return {
      valid: false,
      reason: 'stale',
      message: `Evidence ${evidence.id} is stale: file changed since capture`,
      evidence,
      currentFingerprint,
    };
  }

  // Optionally verify snippet matches
  if (evidence.span) {
    const content = world.read(evidence.ref);
    const currentSnippet = extractLines(
      content,
      evidence.span.startLine,
      evidence.span.endLine
    );

    if (currentSnippet !== evidence.snippet) {
      return {
        valid: false,
        reason: 'content-mismatch',
        message: `Evidence ${evidence.id}: snippet no longer matches`,
        evidence,
      };
    }
  }

  return { valid: true, evidence };
}

// Batch verification for reports
function verifyAllEvidence(
  evidenceRegistry: Map<EvidenceId, Evidence>,
  world: World
): VerificationReport {
  const results: VerificationResult[] = [];
  let allValid = true;

  for (const evidence of evidenceRegistry.values()) {
    const result = verifyEvidence(evidence, world);
    results.push(result);
    if (!result.valid) allValid = false;
  }

  return {
    valid: allValid,
    totalChecked: results.length,
    stale: results.filter(r => r.reason === 'stale').length,
    mismatched: results.filter(r => r.reason === 'content-mismatch').length,
    results,
  };
}
```

---

## Epistemic Modes

Not all knowledge is equal. Epistemic modes enforce truth semantics:

```typescript
type EpistemicMode =
  | 'observed'      // Directly read from source
  | 'measured'      // Computed from source (metrics, counts)
  | 'derived'       // Logically derived from observations
  | 'inferred'      // Probabilistic inference
  | 'hypothesized'  // Proposed but not verified
  | 'assumed'       // Taken as given (axiom)
  ;

interface TypedClaim {
  claim: string;
  mode: EpistemicMode;
  evidence: EvidenceId[];
  confidence?: number;  // 0-1, optional
}

// Validation rules
const EPISTEMIC_RULES: Record<EpistemicMode, {
  requiresEvidence: boolean;
  allowedInCode: boolean;  // Can be used in code-level assertions
  transitive: boolean;     // Can derive from other claims of this mode
}> = {
  observed:     { requiresEvidence: true,  allowedInCode: true,  transitive: false },
  measured:     { requiresEvidence: true,  allowedInCode: true,  transitive: false },
  derived:      { requiresEvidence: true,  allowedInCode: true,  transitive: true },
  inferred:     { requiresEvidence: false, allowedInCode: false, transitive: true },
  hypothesized: { requiresEvidence: false, allowedInCode: false, transitive: false },
  assumed:      { requiresEvidence: false, allowedInCode: false, transitive: false },
};

function validateClaim(claim: TypedClaim, registry: EvidenceRegistry): ValidationResult {
  const rules = EPISTEMIC_RULES[claim.mode];

  if (rules.requiresEvidence && claim.evidence.length === 0) {
    return {
      valid: false,
      error: `${claim.mode} claims require evidence`,
    };
  }

  // Verify all cited evidence exists and is fresh
  for (const evidenceId of claim.evidence) {
    if (!registry.has(evidenceId)) {
      return {
        valid: false,
        error: `Evidence ${evidenceId} not found in registry`,
      };
    }
  }

  return { valid: true };
}
```

---

## Evidence Registry

Every evaluation context maintains an evidence registry:

```typescript
class EvidenceRegistry {
  private evidence: Map<EvidenceId, Evidence> = new Map();
  private citations: Map<EvidenceId, Set<string>> = new Map();

  // Add evidence to registry
  register(evidence: Evidence): void {
    this.evidence.set(evidence.id, evidence);
    this.citations.set(evidence.id, new Set());
  }

  // Record that something cites this evidence
  cite(evidenceId: EvidenceId, citedBy: string): void {
    const cites = this.citations.get(evidenceId);
    if (!cites) {
      throw new Error(`Cannot cite unknown evidence: ${evidenceId}`);
    }
    cites.add(citedBy);
  }

  // Get evidence by ID
  get(id: EvidenceId): Evidence | undefined {
    return this.evidence.get(id);
  }

  // Check if evidence is cited
  isCited(id: EvidenceId): boolean {
    return (this.citations.get(id)?.size ?? 0) > 0;
  }

  // Get all uncited evidence (potential dead evidence)
  getUncited(): Evidence[] {
    return Array.from(this.evidence.values()).filter(
      e => !this.isCited(e.id)
    );
  }

  // Verify all evidence against current world
  verifyAll(world: World): VerificationReport {
    return verifyAllEvidence(this.evidence, world);
  }

  // Serialize for storage
  serialize(): SerializedEvidenceRegistry {
    return {
      evidence: Array.from(this.evidence.entries()),
      citations: Array.from(this.citations.entries()).map(
        ([id, set]) => [id, Array.from(set)]
      ),
    };
  }
}
```

---

## Lisp Integration

```lisp
;; Capture evidence
(define ev (evidence/capture "src/auth.ts" :lines 42 50))
;; => #<Evidence ev:sha256:abc123... src/auth.ts:42-50>

;; Access evidence properties
(evidence/id ev)          ;; => "ev:sha256:abc123..."
(evidence/ref ev)         ;; => "src/auth.ts"
(evidence/snippet ev)     ;; => "function login(user, pass) {...}"
(evidence/fingerprint ev) ;; => "sha256:..."

;; Verify evidence is still fresh
(evidence/verify ev)      ;; => #t or #f
(evidence/stale? ev)      ;; => #f

;; Make claim with evidence
(claim/observed "login function exists"
  :evidence (list ev))

;; Register evidence in context
(evidence/register ev)

;; Cite evidence in report
(finding "Security: password stored in plaintext"
  :evidence (list ev)
  :mode 'observed)
```

---

## Report Schema Integration

Reports MUST cite evidence for all claims:

```typescript
interface Finding {
  description: string;
  severity: 'critical' | 'high' | 'medium' | 'low' | 'info';
  evidence: EvidenceId[];  // REQUIRED
  mode: EpistemicMode;
  location?: {
    file: string;
    line?: number;
  };
}

interface ReportV2 {
  id: string;
  title: string;
  summary: string;
  findings: Finding[];
  evidenceRegistry: EvidenceId[];  // All evidence used
}

// Validation
function validateReport(report: ReportV2, registry: EvidenceRegistry): ValidationResult {
  const errors: string[] = [];

  // All findings must cite evidence
  for (const finding of report.findings) {
    if (finding.evidence.length === 0) {
      errors.push(`Finding "${finding.description}" has no evidence`);
    }

    // All cited evidence must exist
    for (const evidenceId of finding.evidence) {
      if (!registry.get(evidenceId)) {
        errors.push(`Finding cites unknown evidence: ${evidenceId}`);
      }
    }
  }

  // All registered evidence should be cited
  const citedIds = new Set(report.findings.flatMap(f => f.evidence));
  for (const evidenceId of report.evidenceRegistry) {
    if (!citedIds.has(evidenceId)) {
      errors.push(`Evidence ${evidenceId} registered but never cited`);
    }
  }

  return {
    valid: errors.length === 0,
    errors,
  };
}
```

---

## Provenance Chain

Track how evidence was derived:

```typescript
interface ProvenanceNode {
  id: string;
  type: 'evidence' | 'derivation' | 'inference';

  // For evidence nodes
  evidence?: Evidence;

  // For derived nodes
  derivedFrom?: string[];  // IDs of source nodes
  derivationRule?: string; // How it was derived

  // For inference nodes
  inferenceMethod?: string;
  confidence?: number;
}

class ProvenanceGraph {
  private nodes: Map<string, ProvenanceNode> = new Map();

  // Add raw evidence
  addEvidence(evidence: Evidence): string {
    const node: ProvenanceNode = {
      id: evidence.id,
      type: 'evidence',
      evidence,
    };
    this.nodes.set(node.id, node);
    return node.id;
  }

  // Add derived knowledge
  addDerivation(
    sources: string[],
    rule: string,
    description: string
  ): string {
    const id = `derived:${sha256(sources.join(':') + rule).slice(0, 8)}`;
    const node: ProvenanceNode = {
      id,
      type: 'derivation',
      derivedFrom: sources,
      derivationRule: rule,
    };
    this.nodes.set(id, node);
    return id;
  }

  // Trace back to original evidence
  traceToEvidence(nodeId: string): Evidence[] {
    const node = this.nodes.get(nodeId);
    if (!node) return [];

    if (node.type === 'evidence') {
      return node.evidence ? [node.evidence] : [];
    }

    // Recursively trace derivations
    const sources = node.derivedFrom ?? [];
    return sources.flatMap(s => this.traceToEvidence(s));
  }
}
```

---

## Why Provenance is First-Class

| Without Provenance | With Provenance |
|-------------------|-----------------|
| "The bug is on line 42" | Evidence proves it |
| "I checked the file" | Fingerprint verifies it |
| "This is definitely true" | Mode declares confidence |
| Hallucinations pass | Staleness detection catches |
| No audit trail | Full provenance chain |
| "Trust me" | "Verify this" |

**Provenance is the anti-theater mechanism.** It prevents AI agents from making unfounded claims by requiring all assertions to be grounded in verifiable evidence.

---

## Integration with Other Systems

```
┌─────────────────────────────────────────────────────────────────┐
│                    Provenance Integration                       │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  FACTS System                                                   │
│  ├── Facts can carry evidence payloads                         │
│  └── (assert '(file-exists "auth.ts") :evidence ev)           │
│                                                                 │
│  CERTIFICATES                                                   │
│  ├── Certificate events cite evidence                          │
│  └── Confidence calculated from evidence chain                 │
│                                                                 │
│  REPORTS/PLANS                                                  │
│  ├── All findings must cite evidence                           │
│  └── Schema validation enforces this                           │
│                                                                 │
│  FIXPOINT                                                       │
│  ├── Evidence fingerprints in state signature                  │
│  └── Convergence accounts for evidence changes                 │
│                                                                 │
│  SESSION                                                        │
│  ├── Evidence registry persists across turns                   │
│  └── Staleness checked before each decision                    │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```
