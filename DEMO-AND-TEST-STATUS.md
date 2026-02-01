# Demo and Test Status

**Date**: 2026-01-30

---

## ✅ Demo Infrastructure - WORKING

### Demo Runner CLI

**Location**: [demo/runWow.ts](demo/runWow.ts)

**Usage**:
```bash
# List all demos
npx tsx demo/runWow.ts --list

# Run a specific demo
npx tsx demo/runWow.ts --demo oracle-repl-stack --profile pragmatic --seed 7

# Run all demos
npx tsx demo/runWow.ts --all --profile pragmatic --seed 7

# Verbose output
npx tsx demo/runWow.ts --demo backtracking --verbose

# Save report to JSON
npx tsx demo/runWow.ts --all --report results.json
```

**Available profiles**: `explore`, `pragmatic`, `strict`, `airgap`

---

## ✅ All Demos Pass (9/9)

### Omega Wow Pack Demos

1. **oracle-repl-stack** ✅ - Interactive Oracle REPL in Call Stack (5 invariants)
2. **multi-shot-backtracking** ✅ - Multi-shot Backtracking over Semantic Choices (5 invariants)
3. **concurrency-cost-collapse** ✅ - Concurrency Collapses Semantic Cost (5 invariants)
4. **generic-miss-synthesis** ✅ - Generic Operations Synthesize Missing Methods (5 invariants)
5. **constraint-diagnosis-repair** ✅ - Constraint Diagnosis and Repair (5 invariants)
6. **semantic-macro-pipeline** ✅ - Semantic Macros: DSL to Verified Pipeline (5 invariants)
7. **compilation-inference-plane** ✅ - Compilation Preserves Inference Plane (5 invariants)
8. **meta-circular-repair** ✅ - Meta-circular Closure: Oracle Repairs eval0 (5 invariants)
9. **opr-callbacks** ✅ - OPR Callback Wiring - THE KEYSTONE (5 invariants)

**Result**: All 9 demos passed! 45/45 invariants verified.

---

## 🏃 Quick Start

### Run Everything

```bash
# Build project
npm run build

# Run all Wow Pack demos
npx tsx demo/runWow.ts --all

# Run tests
npm test

# Start REPL
npm run omega-repl

# Start debug server
npm run omega-debugger
```

### Run Specific Demo

```bash
# Run TypeScript demo
npm run omega-fast -- --file demo/by-chapter/ch02-llm-calls.ts

# Run Lisp demo
npm run omega-repl -- --file demo/lisp/ch09-agentic-repl.lisp

# Run Wow Pack demo
npx tsx demo/runWow.ts --demo opr-callbacks --verbose
```

---

## ✅ Status Summary

| Component | Status | Count |
|-----------|--------|-------|
| **Omega Wow Demos** | ✅ PASS | 9/9 |
| **TypeScript Chapters** | ✅ READY | 27 demos |
| **Lisp Demos** | ✅ READY | 34 demos |
| **Live LLM Demo** | ✅ WORKING | 1 demo |
| **Build** | ✅ PASS | 506KB |

**Total**: 71 working demos, all infrastructure functional!

