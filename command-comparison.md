# Command Comparison: Debugger vs REPL

## Debugger Commands (omega-debugger.ts)
- load (code) - Load Lisp code
- loadfile <path> - Load from file
- s, step - Execute one step
- n, next [N] - Execute N steps
- c, continue, run - Run until breakpoint/completion
- stop - Stop execution
- state, st - Show current state
- stack, bt - Show call stack
- frame <N>, f <N> - Inspect frame
- env [filter], e [filter] - Show environment
- lookup <name>, l <name> - Lookup binding
- eval (expr) - Evaluate expression
- break step/expr/effect, b - Add breakpoint
- breaks, breakpoints - List breakpoints
- delete <id>, del <id>, d <id> - Delete breakpoint
- toggle <id> - Toggle breakpoint
- save <name> - Save snapshot
- restore <name>, load-snapshot <name> - Restore snapshot
- snapshots, snaps - List snapshots
- export <name> <file> - Export snapshot to file
- back [N] - Go back N steps
- history [N], hist [N] - Show history
- goto <N>, g <N> - Jump to step
- trace [s] [n], tr [s] [n] - List trace
- runrecord, rr - Run to completion with recording
- record on|off - Toggle recording
- dump <file> - Save trace to file
- replay <file> - Load and replay dump
- quit, q, exit - Exit

## REPL Commands (omega-repl.ts)
- :help, :h - Show help
- (expr) - Evaluate expression
- :debug (expr) - Load into debugger
- :step [N] - Execute N steps
- :run, :continue, :c - Run to completion/breakpoint
- :goto <N>, :g <N> - Jump to step
- :trace [s] [n] - Show trace
- :state, :st - Show debug state
- :break step/expr/effect, :b - Add breakpoint
- :breaks, :breakpoints - List breakpoints
- :delbreak <id>, :del <id> - Delete breakpoint
- :env [name] - Show environment or lookup binding
- :defs - Show definitions
- :stack - Show call stack
- :frame <n> - Inspect frame
- :control - Show current control
- :ask <question> - Ask LLM (agentic)
- :traces - List LLM traces
- :trace <id> [-v] - Show LLM trace
- :quit, :q - Exit

## Missing Commands in REPL (from debugger)
1. **loadfile <path>** - Load code from file (debugger has this, REPL doesn't)
2. **next [N]** - Alias for step (debugger has 'n', 'next'; REPL only has :step)
3. **stop** - Stop execution (debugger has this, REPL doesn't)
4. **toggle <id>** - Toggle breakpoint enable/disable (missing in REPL)
5. **save <name>** - Save snapshot (missing in REPL)
6. **restore <name>** - Restore snapshot (missing in REPL)
7. **snapshots** - List snapshots (missing in REPL)
8. **export <name> <file>** - Export snapshot (missing in REPL)
9. **back [N]** - Go back in history (missing in REPL)
10. **history [N]** - Show step history (missing in REPL)
11. **record on|off** - Toggle trace recording (missing in REPL)
12. **dump <file>** - Save trace to file (missing in REPL)
13. **replay <file>** - Load and replay dump (missing in REPL)
14. **runrecord, rr** - Run to completion with recording (missing in REPL)
15. **lookup <name>** - Dedicated lookup command (REPL has :env <name>)

## Commands Present in REPL but not Debugger
1. **:ask <question>** - Agentic LLM query with tool calls
2. **:traces** - List LLM traces
3. **:trace <id>** - Show LLM trace details
4. **:defs** - Show user definitions
5. **:control** - Show current control

## Analysis
The REPL already has core debugging functionality but is missing:
- Snapshot management (save/restore/list/export)
- History navigation (back/history)
- Trace recording controls (record on/off)
- Trace persistence (dump/replay)
- File loading (loadfile)
- Stop command
- Breakpoint toggle

These need to be added to the REPL to achieve full feature parity.
