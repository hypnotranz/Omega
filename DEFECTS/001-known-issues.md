# Known Defects and Issues

## DEFECT-001: LLM Traces Don't Persist Across REPL Sessions

**Status**: Open
**Severity**: Medium
**Found**: 2026-01-31

### Description
The `:traces` command only shows traces from the current REPL session. When you quit and restart the REPL, all trace history is lost.

### Steps to Reproduce
```text
# Session 1
Omega> :ask "What is 2+2?"
; ... response with Trace ID: t-abc123 ...
Omega> :traces
Recent LLM Traces:
  t-abc123: ...
Omega> :quit

# Session 2
Omega> :traces
(no traces recorded yet)
Omega> :trace t-abc123
Trace 't-abc123' not found.
```

### Expected Behavior
Traces should persist to disk (like sessions do) and be viewable across REPL restarts.

### Workaround
None. You must view traces in the same REPL session where they were created.

---

## DEFECT-002: `--session` Flag Doesn't Auto-Restore State

**Status**: By Design (but confusing)
**Severity**: Low (documentation issue)
**Found**: 2026-01-31

### Description
The `--session <name>` CLI flag only names the session for recording. It does NOT automatically restore state from a previous session with that name.

### Steps to Reproduce
```bash
# Session 1
npm run omega-fast -- --session foo
Omega> (define x 42)
Omega> :quit

# Session 2 - expects x to exist
npm run omega-fast -- --session foo
Omega> x
error: unbound var x
```

### Expected Behavior (from user perspective)
Using the same `--session` name should restore the environment.

### Actual Behavior
You must manually run `:session load <name>` then `:session goto <seq>` to restore.

### Resolution
This is by design, but the README has been updated to make this clear. See "Common Gotchas" section.

---

## DEFECT-003: OPR Kernel JSON Parsing Fails with Special Characters

**Status**: Open
**Severity**: Low
**Found**: 2026-01-31

### Description
When using `:opr-run` via piped input, JSON with certain special characters fails to parse.

### Steps to Reproduce
```text
Omega> :opr-run opr.classify.v1 {"text": "I'm frustrated!", "categories": ["positive", "negative"]}
Invalid JSON: Bad escaped character in JSON at position 71
```

### Workaround
Use `--cmd` flag instead of piped input, or avoid apostrophes/special characters in text:
```bash
npm run omega-fast -- --cmd ':opr-run opr.classify.v1 {"text":"angry customer","categories":["positive","negative"]}'
```

---

## DEFECT-004: Logic Programming Demo Returns Wrong Answer

**Status**: Open
**Severity**: Low
**Found**: 2026-01-31

### Description
Chapter 27 (logic programming) demo incorrectly answers "Is Alice the grandparent of Carol?" as `#f` (false), when the facts clearly establish Alice → Bob → Carol.

### Steps to Reproduce
```bash
npm run manual 27
```

### Output
```
(is-grandparent? "Alice" "Carol")
=> #f
```

### Expected Output
```
=> #t   ; or "yes"
```

### Root Cause
The LLM returned something other than exactly "yes", causing the `equal?` check to fail. The predicate is too strict.

### Suggested Fix
Use case-insensitive matching or check for "yes" substring:
```lisp
(define (is-grandparent? a c)
  (string-contains?
    (string-downcase (effect infer.op ...))
    "yes"))
```

---

## DEFECT-005: Some LLM Responses Have Leaked Tool Call Wrappers

**Status**: Open
**Severity**: Low
**Found**: 2026-01-31

### Description
Occasionally LLM responses include raw tool call JSON like `functions.return({"value":"..."})` instead of just the content.

### Steps to Reproduce
Run Chapter 8 demo:
```bash
npm run manual 8
```

### Output (sometimes)
```
=> ("functions.return({\"value\":\"This debugging step involves...\")" "Verifying...")
```

### Expected Output
```
=> ("This debugging step involves..." "Verifying...")
```

### Root Cause
Response parsing doesn't strip tool call wrapper in some edge cases.

---

## Notes

- Session files are stored in `.omega-session/sessions/` (relative to working directory)
- Use `:help` to see all available commands
- See README.md "Common Gotchas" section for known UX issues
