# Metacircular Evaluator and Time-Travel Tests (Prompt 7)

Tests Omega's ability to **interpret itself** and provide **bidirectional debugging**.

## What This Tests

The core capability: Omega is metacircular - it can be written in itself. This enables:
- **Programmable eval/apply**: Modify evaluation behavior via hooks (Metaobject Protocol)
- **Semantic functions as primitives**: OracleProcs work in both host and meta evaluators
- **Time-travel debugging**: Multi-shot continuations allow exploring alternative paths
- **Hermetic replay**: Recorded oracle sessions reproduce exactly

## Test Sections

- **7.1**: Differential testing - host evaluator produces same results as meta
- **7.2**: Tracing via hooks - demonstrate programmable evaluation
- **7.3**: Oracle stack navigation - inspect and eval at any frame
- **7.4**: Time travel - fork suspensions with different values (multi-shot)
- **7.5**: Hermetic replay - recorded responses reproduce sessions exactly

## Key Concepts

- **Metacircular**: Language that can interpret itself
- **MOP (Metaobject Protocol)**: Making eval/apply configurable objects
- **Multi-shot continuations**: Running the same suspension multiple times
- **Hermetic replay**: Deterministic reproduction from recorded transcripts
