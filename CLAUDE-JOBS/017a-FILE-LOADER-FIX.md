# JOB-017a: Fix File Loader for Multi-Line S-Expressions

**Priority**: P1 - Critical Bug Fix
**Estimated Effort**: 30 minutes
**Skills Required**: TypeScript
**Status**: NOT STARTED
**Depends On**: None
**Blocks**: 017d (CLI Unification)

> **MUST READ**: [LOGISTICS.md](LOGISTICS.md) before starting.

---

## Executive Summary

The REPL's `--file` flag breaks on multi-line Lisp files because it splits input by newline. The `extractSexpressions()` function exists but isn't used in file loading.

**Bug Location**: `bin/omega-repl.ts:2057`

**Example broken file** (`demo/lisp/ch03-composition.lisp`):
```lisp
(define (is-complaint? text)
  (equal? "yes"
    (effect infer.op
      (list "Is this customer note a complaint? yes/no: " text))))
```

When loaded with `--file`, this becomes 4 separate commands, each failing.

---

## Root Cause

**Current code at line 2057:**
```typescript
const commands = fs.readFileSync(filePath, "utf8").split("\n");
for (const cmd of commands) {
  const trimmed = cmd.trim();
  if (!trimmed || trimmed.startsWith(";")) continue;
  // Each LINE is processed separately - breaks multi-line S-expressions!
  const { replState: newState, output, shouldExit } = await processReplCommand(trimmed, replState);
  ...
}
```

**Problem**: `.split("\n")` breaks S-expressions that span multiple lines.

**Solution**: Use `extractSexpressions()` which is already defined at line 1096 and handles parenthesis balancing, string escaping, and multi-line expressions.

---

## Implementation

### Step 1: Locate the Bug

Open `bin/omega-repl.ts` and find the `--file` handling section (around line 2050-2088).

Look for:
```typescript
if (args.file) {
  const filePath = path.resolve(args.file);
  ...
  const commands = fs.readFileSync(filePath, "utf8").split("\n");  // <-- BUG
```

### Step 2: Apply the Fix

**REPLACE** lines 2057-2067 with:

```typescript
// Read file and extract complete S-expressions (handles multi-line)
const fileContent = fs.readFileSync(filePath, "utf8");

// Strip comment lines before parsing
const cleanedContent = fileContent
  .split("\n")
  .filter(line => !line.trim().startsWith(";"))
  .join("\n");

// Extract balanced S-expressions
const sexprs = extractSexpressions(cleanedContent);
const outputs: string[] = [];

for (const sexpr of sexprs) {
  const trimmed = sexpr.trim();
  if (!trimmed) continue;

  const { replState: newState, output, shouldExit } = await processReplCommand(trimmed, replState);
  replState = newState;
  if (output) outputs.push(output);
  if (shouldExit) break;
}
```

### Step 3: Verify extractSexpressions Exists

The function is at line 1096. Confirm it has this signature:

```typescript
function extractSexpressions(text: string): string[]
```

It:
- Tracks parenthesis depth
- Handles strings with escapes
- Returns an array of complete S-expressions

**DO NOT MODIFY** `extractSexpressions()` - it already works correctly.

---

## Final State

### File Changes

**`bin/omega-repl.ts`** - 1 change:

| Location | Before | After |
|----------|--------|-------|
| Lines 2057-2067 | `split("\n")` loop | `extractSexpressions()` loop |

### Code Diff

```diff
     // If we have a file, run all commands from it
     if (args.file) {
       const filePath = path.resolve(args.file);
       if (!fs.existsSync(filePath)) {
         console.error(`File not found: ${filePath}`);
         process.exit(1);
       }

-      const commands = fs.readFileSync(filePath, "utf8").split("\n");
-      const outputs: string[] = [];
-
-      for (const cmd of commands) {
-        const trimmed = cmd.trim();
-        if (!trimmed || trimmed.startsWith(";")) continue; // skip empty and comments
-
-        const { replState: newState, output, shouldExit } = await processReplCommand(trimmed, replState);
+      // Read file and extract complete S-expressions (handles multi-line)
+      const fileContent = fs.readFileSync(filePath, "utf8");
+
+      // Strip comment lines before parsing
+      const cleanedContent = fileContent
+        .split("\n")
+        .filter(line => !line.trim().startsWith(";"))
+        .join("\n");
+
+      // Extract balanced S-expressions
+      const sexprs = extractSexpressions(cleanedContent);
+      const outputs: string[] = [];
+
+      for (const sexpr of sexprs) {
+        const trimmed = sexpr.trim();
+        if (!trimmed) continue;
+
+        const { replState: newState, output, shouldExit } = await processReplCommand(trimmed, replState);
         replState = newState;
         if (output) outputs.push(output);
         if (shouldExit) break;
       }
```

---

## Verification

### Test 1: Multi-line Lisp File

```bash
cd OmegaLLM

# Create test file with multi-line S-expression
cat > /tmp/test-multiline.lisp << 'EOF'
;; Test multi-line S-expressions
(define (greet name)
  (list "Hello"
        name
        "!"))

(greet "World")
EOF

# Run it
npx tsx bin/omega-repl.ts --file /tmp/test-multiline.lisp

# Expected output:
# ("Hello" "World" "!")
```

### Test 2: Chapter 3 Demo

```bash
npx tsx bin/omega-repl.ts --file demo/lisp/ch03-composition.lisp

# Should execute without errors (may fail on LLM calls if no API key,
# but should NOT fail with "unexpected token" or syntax errors)
```

### Test 3: Comment Handling

```bash
cat > /tmp/test-comments.lisp << 'EOF'
;; This is a comment
(define x 42)
; Another comment
(+ x 1)
EOF

npx tsx bin/omega-repl.ts --file /tmp/test-comments.lisp

# Expected output:
# 43
```

### Test 4: Existing Functionality

```bash
# Ensure single-line files still work
echo "(+ 1 2 3)" > /tmp/simple.lisp
npx tsx bin/omega-repl.ts --file /tmp/simple.lisp
# Expected: 6

# Ensure --cmd still works
npx tsx bin/omega-repl.ts --cmd "(* 7 6)"
# Expected: 42
```

---

## Checklist

- [ ] Located bug at `bin/omega-repl.ts:2057`
- [ ] Verified `extractSexpressions` exists at line 1096
- [ ] Replaced `.split("\n")` with `extractSexpressions()` approach
- [ ] Comment stripping added before parsing
- [ ] Test 1 passes (multi-line S-expression)
- [ ] Test 2 passes (ch03-composition.lisp)
- [ ] Test 3 passes (comment handling)
- [ ] Test 4 passes (single-line still works)

---

## Proof of Completion

```bash
# 1. Multi-line file loads
npx tsx bin/omega-repl.ts --file demo/lisp/ch03-composition.lisp 2>&1 | head -5
# Should NOT show "unexpected token" errors

# 2. Simple expressions still work
npx tsx bin/omega-repl.ts --cmd "(+ 1 2)"
# Should output: 3
```

---

## Footer

| Field | Value |
|-------|-------|
| Created | 2025-01-21 |
| Author | Claude |
| Part Of | Job 017 (CLI Unification) |
| Related Files | `bin/omega-repl.ts` |
