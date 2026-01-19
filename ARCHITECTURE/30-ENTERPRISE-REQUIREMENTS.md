# Enterprise Security Architecture Requirements Tree

This document organizes the enterprise security changes from UPDATE-1, UPDATE-2, and UPDATE-3 into parallelizable work items with clear dependencies.

## Source Documents

- **30-UPDATE-1**: Change Sets 0-12 overview + ExecutionContext, CapabilityRef, Policy/PDP, Audit, Data Governance, WorkerRunner, Journal, Evidence, Protocol, Assurance, Evaluation, Observability
- **30-UPDATE-2**: Deep implementation specs for PolicyBundle, LocalPDP, AuditLedger, WorkerRunner/FFI bridging
- **30-UPDATE-3**: Approvals, EgressGate, Journal/Replay, KeyProvider, Retention

---

## Dependency Graph

```
Phase 7: ENTERPRISE FOUNDATION (after FFI #06)
----------------------------------------------
20-CRYPTO -----> 21-CONTEXT
    |               |
    v               v
22-CAPABILITY   23-POLICY-BUNDLE
    |               |
    v               v
24-AUDIT-SCHEMA 25-APPROVAL-SCHEMA
                    |
                    v
26-CLASSIFIER   27-JOURNAL-SCHEMA
26-REDACTOR         |
    |               v
    v          28-RUNNER-INTERFACE
27-EGRESS-GATE


Phase 8: ENTERPRISE CORE (depends on Phase 7)
---------------------------------------------
29-PDP (depends on 23-POLICY-BUNDLE)
    |
    v
30-AUDIT-LEDGER (depends on 24-AUDIT-SCHEMA, 20-CRYPTO)
    |
    v
31-APPROVAL-STORE (depends on 25-APPROVAL-SCHEMA)
    |
    v
32-ENFORCER (depends on 29-PDP, 31-APPROVAL-STORE, 21-CONTEXT)


Phase 9: ISOLATION (depends on Phase 7, can parallel with Phase 8)
------------------------------------------------------------------
33-WORKER-PROTOCOL (independent)
    |
    v
34-WORKER-RUNNER (depends on 33, 28-RUNNER-INTERFACE)
    |
    v
35-WORKER-FFI (depends on 33, 34)


Phase 10: INTEGRATION (depends on Phases 8 & 9 + FFI #06)
---------------------------------------------------------
36-AUDITED-FFI (depends on 30, 32, FFI #06)
    |
    v
37-EGRESS-INTEGRATION (depends on 27-EGRESS-GATE, 29-PDP, 36)
    |
    v
38-JOURNAL-IMPL (depends on 27-JOURNAL-SCHEMA, FFI #06)


Phase 11: PROTOCOL & FINAL (depends on Phase 10 + Protocol #15)
---------------------------------------------------------------
39-PROTOCOL-APPROVE (depends on 31, Protocol #15)
40-PROTOCOL-AUDIT (depends on 30, Protocol #15)
41-PROTOCOL-JOURNAL (depends on 38, Protocol #15)
42-KEY-PROVIDER (independent)
43-EVIDENCE-STORE (depends on 26, 22)
44-ASSURANCE (depends on many)
45-EVAL-OBSERVABILITY (depends on 38)
```

---

## Work Item Summary

### Phase 7: Enterprise Foundation (7 jobs, highly parallel)

| Job | Name | Depends On | Blocks | Effort |
|-----|------|------------|--------|--------|
| 20 | Crypto Utilities | - | 22,23,24,25,27,30 | S |
| 21 | ExecutionContext | #01 Types | 32,36 | S |
| 22 | CapabilityRef | 20 | 36,43 | M |
| 23 | PolicyBundle Schema | 20 | 29 | M |
| 24 | AuditEnvelope Schema | 20,21 | 30 | S |
| 25 | ApprovalTypes Schema | 20 | 31 | M |
| 26 | Classifier + Redactor | - | 27,43 | M |

### Phase 8: Enterprise Core (4 jobs, sequential dependency chain)

| Job | Name | Depends On | Blocks | Effort |
|-----|------|------------|--------|--------|
| 27 | JournalTypes + EgressGate | 20,26 | 37,38 | M |
| 28 | Runner Interface | 21 | 34 | S |
| 29 | LocalPDP | 23 | 32,37 | M |
| 30 | AuditLedger | 20,24 | 36,40 | M |

### Phase 9: Isolation (3 jobs, parallel with Phase 8)

| Job | Name | Depends On | Blocks | Effort |
|-----|------|------------|--------|--------|
| 31 | ApprovalStore | 25 | 32,39 | M |
| 33 | WorkerProtocol | - | 34,35 | S |
| 34 | WorkerRunner | 28,33 | 35 | M |

### Phase 10: Integration (4 jobs)

| Job | Name | Depends On | Blocks | Effort |
|-----|------|------------|--------|--------|
| 32 | SessionEnforcer Ext | 21,29,31 | 36 | M |
| 35 | WorkerFFI Bridge | 33,34 | - | M |
| 36 | AuditedFFI | 30,32,#06 | 37 | M |
| 37 | EgressIntegration | 27,29,36 | - | M |

### Phase 11: Protocol & Final (7 jobs)

| Job | Name | Depends On | Blocks | Effort |
|-----|------|------------|--------|--------|
| 38 | JournalImpl | 27,#06 | 41,45 | M |
| 39 | ProtocolApprove | 31,#15 | - | M |
| 40 | ProtocolAudit | 30,#15 | - | S |
| 41 | ProtocolJournal | 38,#15 | - | S |
| 42 | KeyProvider | - | - | S |
| 43 | EvidenceStore | 22,26 | - | M |
| 44 | AssuranceCases | many | - | M |

---

## Critical Dependencies on Existing Jobs

The enterprise security work depends on these existing jobs:

- **#01 Types** - ExecutionContext uses core Value types
- **#06 FFI** - AuditedFFI wraps base FFI, EgressGate wraps LLM adapter
- **#15 Protocol** - Protocol extensions for approve, audit.export, journal.export

### Parallelization Strategy

**Can start immediately (no deps on in-progress work):**
- 20-CRYPTO (independent)
- 26-CLASSIFIER (independent)
- 33-WORKER-PROTOCOL (independent)
- 42-KEY-PROVIDER (independent)

**Can start after Types #01 complete:**
- 21-CONTEXT
- All jobs depending on 21

**Can start after FFI #06 complete:**
- 36-AUDITED-FFI
- 37-EGRESS-INTEGRATION
- 38-JOURNAL-IMPL

**Can start after Protocol #15 complete:**
- 39-PROTOCOL-APPROVE
- 40-PROTOCOL-AUDIT
- 41-PROTOCOL-JOURNAL

---

## Implementation Order for Minimal Enterprise Trust Surface

If implementing incrementally, this order gives maximum enterprise trust value earliest:

1. **20-CRYPTO** - Foundation for all cryptographic operations
2. **21-CONTEXT** - ExecutionContext for audit/policy threading
3. **23-POLICY-BUNDLE** - Portable, versioned, hashable policy
4. **29-PDP** - Real policy decisions with obligations
5. **24-AUDIT-SCHEMA + 30-AUDIT-LEDGER** - Tamper-evident audit
6. **22-CAPABILITY** - Unforgeable capability refs
7. **25-APPROVAL + 31-APPROVAL-STORE** - Signed approvals
8. **26-CLASSIFIER + 27-EGRESS** - Data governance
9. **33-34-35 WORKER** - Hard isolation
10. **38 JOURNAL** - Deterministic replay

After this order, you can credibly claim:
- Policy is versioned and auditable
- Audit trail is tamper-evident and signed
- Capabilities are unforgeable
- Approvals are cryptographically attributable
- Egress is governed (classification + redaction)
- Untrusted code runs in isolated workers
- Execution is reproducible via replay
