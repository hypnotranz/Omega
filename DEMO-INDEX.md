# Complete Demo Index

All runnable OmegaLLM demos organized by category.

## 🚀 Featured Showcase

**Start here - shows multiple features:**
- `ch00-showcase.lisp` - Map, filter, amb, streams, pipelines (RECOMMENDED)
- `npm run demo` - Runs the featured showcase

## 🤖 Agent Demos (For Coding Agents)

**Real patterns agents can use:**
- `agent-security-audit.lisp` - Map security analysis over codebase (vs serial tool calls)
- `auto-traceability.lisp` - Requirements traceability matrix with map/filter/streams
- `ch09-agentic-repl.lisp` - LLM queries runtime for facts
- `ch19-conversational-state.lisp` - Persistent state across interactions

**Run:** `npm run manual 9` (for ch09)

## 📚 Learning Path (SICP-Based)

### Getting Started
- `ch01-getting-started.lisp` - Basic syntax
- `ch02-llm-calls.lisp` - First LLM inference  
- `ch03-composition.lisp` - Composing LLM calls
- `ch04-higher-order.lisp` - map/filter/fold

**Run:** `npm run manual 1` through `npm run manual 4`

### Core Features
- `ch05-nondeterministic.lisp` - Backtracking with amb
- `ch06-multi-shot.lisp` - Multi-shot sampling
- `ch07-lazy-streams.lisp` - Infinite sequences
- `ch08-debugger.lisp` - Step-through debugging

**Run:** `npm run manual 5` through `npm run manual 8`

### Advanced (Ch10-27)
- `ch10-api-reference.lisp` - Full API demo
- `ch11-semantic-procedures.lisp` - LLM predicates
- `ch12-inference-processes.lisp` - Recursive vs iterative
- `ch13-higher-order-inference.lisp` - LLMs over LLMs
- `ch14-semantic-data.lisp` - Data structures
- `ch15-sequences.lisp` - Sequence operations
- `ch16-symbolic-semantic.lisp` - Symbolic processing
- `ch17-multiple-representations.lisp` - Data abstraction
- `ch18-generic-semantic.lisp` - Generic operations
- `ch19-conversational-state.lisp` - Stateful conversations
- `ch20-semantic-environment.lisp` - Environment manipulation
- `ch21-mutable-semantic.lisp` - Mutable state
- `ch22-concurrent-inference.lisp` - Parallel LLM calls
- `ch23-streams-of-inference.lisp` - Infinite semantic expansion
- `ch24-metacircular.lisp` - Evaluator in Omega
- `ch25-lazy-semantic.lisp` - Memoization
- `ch26-amb-inference.lisp` - Constraint satisfaction
- `ch27-logic-programming.lisp` - Logic with semantic facts

**Run:** `npm run manual <number>`

## 🎯 Use Case Demos

**Real-world applications:**
- `usecase-coding-agent.lisp` - Code generation agent
- `usecase-code-review.lisp` - Automated code review
- `usecase-research-agent.lisp` - Research and analysis
- `usecase-data-pipeline.lisp` - Data processing pipeline
- `usecase-interactive-tutor.lisp` - Educational assistant
- `usecase-workflow-orchestrator.lisp` - Multi-step workflows

**Run:** `omega --file demo/lisp/usecase-<name>.lisp`

## 📊 Complete List (39 demos)

| Demo | Category | Description | Run |
|------|----------|-------------|-----|
| ch00-showcase.lisp | Featured | All killer features | `npm run demo` |
| agent-security-audit.lisp | Agent | Map over codebase | Manual |
| auto-traceability.lisp | Agent | Requirements matrix | Manual |
| ch01-ch27 | Learning | SICP chapters | `npm run manual N` |
| usecase-* | Applications | Real use cases | Manual |

## 🧪 Testing Demos

**All chapter demos (ch01-ch27) are tested via:**
```bash
npm run manual 1  # Test chapter 1
npm run manual 5  # Test chapter 5 (amb)
# etc.
```

**Agent demos require capabilities:**
```bash
omega --file demo/lisp/agent-security-audit.lisp --caps shell,file.read
omega --file demo/lisp/auto-traceability.lisp --caps shell,file.read,file.write
```

## 📖 Full Documentation

See [DEMO-GALLERY.md](MANUAL--STRUCTURE-AND-INTERPRETATION-OF-LINGUISTIC-PROGRAMS/DEMO-GALLERY.md) for:
- Complete code listings
- Live LLM outputs
- Detailed explanations
