# Semantic Program Rewriting Tests (Prompt 6)

Tests Omega's ability to **transform programs while preserving meaning**.

## What This Tests

The oracle proposes code transformations (refactoring, optimization, security hardening) and the runtime applies them **deterministically** using pattern-matching rules.

## Use Cases

- **Security hardening**: Eliminate dangerous patterns like `eval`
- **Refactoring**: Transform ad-hoc code to use proper abstractions
- **Optimization**: Stream fusion, constant folding
- **Semantic macros**: Define new language constructs via rewrite rules

## Test Sections

- **6.1**: Security hardening - eliminating unsafe eval
- **6.2**: Refactoring fold-based code to generic abstractions
- **6.3**: Stream fusion optimization
- **6.4**: Conflict detection (when rules overlap)
- **6.5**: Partial evaluation / residualization
- **6.6**: Semantic macros as rewrite rules
- **6.7**: Deterministic rule application
