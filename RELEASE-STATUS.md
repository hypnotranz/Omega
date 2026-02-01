# Release Status

## Production-Ready (demo/lisp/)

All 35 chapter demos are working and ready for release:

- ✅ ch01-getting-started.lisp
- ✅ ch02-llm-calls.lisp
- ✅ ch03-composition.lisp
- ✅ ch04-higher-order.lisp
- ✅ ch05-nondeterministic.lisp
- ✅ ch06-multi-shot.lisp
- ✅ ch07-lazy-streams.lisp
- ✅ ch08-debugger.lisp
- ✅ ch09-agentic-repl.lisp
- ✅ ch10-api-reference.lisp
- ✅ ch11-semantic-procedures.lisp
- ✅ ch12-inference-processes.lisp
- ✅ ch13-higher-order-inference.lisp
- ✅ ch14-semantic-data.lisp
- ✅ ch15-sequences.lisp
- ✅ ch16-symbolic-semantic.lisp
- ✅ ch17-multiple-representations.lisp
- ✅ ch18-generic-semantic.lisp
- ✅ ch19-conversational-state.lisp
- ✅ ch20-metalinguistic.lisp
- ✅ ch21-metacircular.lisp
- ✅ ch22-lazy-evaluator.lisp
- ✅ ch23-nondeterministic-evaluator.lisp
- ✅ ch24-logic-programming.lisp
- ✅ ch25-query-systems.lisp
- ✅ ch26-concurrent-inference.lisp
- ✅ ch27-logic-programming.lisp
- ✅ ch28-register-machines.lisp
- ✅ ch29-compilation.lisp
- ✅ ch30-garbage-collection.lisp
- ✅ ch31-distributed-inference.lisp
- ✅ ch32-memory-systems.lisp
- ✅ ch33-budget-llm.lisp
- ✅ ch34-sandbox.lisp
- ✅ auto-traceability.lisp

## Experimental (experimental/)

### rtm-generator/
❌ **NOT READY** - has runtime issues

Requirements Traceability Matrix generation tools. Moved to experimental due to:
- Runtime hangs with file operations
- Timeout issues
- Needs debugging

**Do not include in release.**

## Core System

- ✅ src/ - All core TypeScript implementation
- ✅ test/ - Unit and integration tests
- ✅ ARCHITECTURE/ - 50+ specification documents
- ✅ bin/ - CLI tools (omega, omega-repl, manual-runner)

## .gitignore

Updated to exclude `/experimental/` folder from git tracking.
