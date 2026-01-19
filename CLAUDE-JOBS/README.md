# CLAUDE-JOBS

This folder contains detailed job specifications for fixing issues in OmegaLLM. Each job is documented with enough detail that someone unfamiliar with the codebase can execute the fix.

## Job Format

Each job file includes:
- **Executive Summary**: What's broken and why
- **Root Cause Analysis**: The specific files and code causing the issue
- **The Fix**: Step-by-step instructions with code changes
- **Verification Steps**: How to confirm the fix works
- **Files Modified Summary**: Quick reference of all files touched
- **Checklist**: Task list for tracking progress

## Current Jobs

| Job ID | Title | Priority | Status |
|--------|-------|----------|--------|
| [001](./001-FIX-PRODUCTION-PRIMITIVES.md) | Fix Production Primitives | P0 - CRITICAL | ✅ DONE |
| [002](./002-FIX-PRIMITIVE-NAMING-CONVENTIONS.md) | Fix Primitive Naming Conventions | P1 - Important | ✅ DONE |

## Job Status Legend

- **NOT STARTED**: Job documented, no work begun
- **IN PROGRESS**: Someone is working on it
- **BLOCKED**: Waiting on external dependency
- **DONE**: Fix completed and verified

## Creating New Jobs

When documenting a new job:

1. Use sequential numbering: `002-SHORT-DESCRIPTION.md`
2. Follow the format of existing jobs
3. Include ALL file paths as relative links (e.g., `[file.ts](../src/file.ts)`)
4. Include code snippets showing BEFORE and AFTER
5. Add verification steps that can be run
6. Update this README with the new job

## Related Documents

- [LAMBDA-LLM--OMEGA-LLM-FEATURES.md](../docs/LAMBDA-LLM--OMEGA-LLM-FEATURES.md) - Feature audit and issue tracking
- [ARCHITECTURE-EXPLANATION.md](../docs/ARCHITECTURE-EXPLANATION.md) - System architecture
- [REFERENCE-ALGEBRA.md](../docs/REFERENCE-ALGEBRA.md) - Formal specification
