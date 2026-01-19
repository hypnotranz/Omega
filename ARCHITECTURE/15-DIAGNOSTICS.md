# 15: Error Diagnostics

## Philosophy

Good error messages are **critical** for usability. They should:
1. Say what went wrong
2. Say where it happened
3. Suggest how to fix it

---

## Error Message Format

```
┌─────────────────────────────────────────────────────────────────┐
│  Error Format                                                   │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  error[E0123]: unbound variable 'foo'                          │
│    --> src/main.lisp:42:5                                      │
│     |                                                           │
│  42 |   (define result (foo x))                                │
│     |                   ^^^ not found in scope                 │
│     |                                                           │
│  help: did you mean 'foe' or 'food'?                           │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

---

## Error Types

### Read Errors (Syntax)

| Code | Error | Example |
|------|-------|---------|
| E0001 | Unclosed parenthesis | `(define x` |
| E0002 | Unexpected closing paren | `x)` |
| E0003 | Unterminated string | `"hello` |
| E0004 | Invalid number | `12.34.56` |
| E0005 | Invalid character | `@#$` |
| E0006 | Invalid escape sequence | `"\q"` |

### Evaluation Errors (Runtime)

| Code | Error | Example |
|------|-------|---------|
| E0100 | Unbound variable | `foo` not defined |
| E0101 | Not a procedure | `(42 1 2)` |
| E0102 | Wrong arity | `(+ 1)` needs 2+ args |
| E0103 | Type mismatch | `(+ "a" 1)` |
| E0104 | Division by zero | `(/ 1 0)` |
| E0105 | Index out of bounds | `(nth '(a b) 5)` |

### Condition Errors

| Code | Error | Example |
|------|-------|---------|
| E0200 | Unhandled condition | No handler for signal |
| E0201 | Unknown restart | Restart not found |
| E0202 | Circular dependency | Module cycle |

### FFI Errors

| Code | Error | Example |
|------|-------|---------|
| E0300 | File not found | `(read-file "nope.txt")` |
| E0301 | Permission denied | Can't write to path |
| E0302 | Network error | LLM call failed |
| E0303 | Timeout | Operation took too long |

---

## Implementation

### Error Structure

```typescript
interface DiagnosticError {
  code: string;           // "E0100"
  severity: 'error' | 'warning' | 'info';
  message: string;        // "unbound variable 'foo'"
  location: SourceLoc;    // file:line:col
  span: SourceSpan;       // start to end
  notes: Note[];          // Additional context
  help?: string;          // Suggestion
  related?: RelatedInfo[];// Related locations
}

interface Note {
  message: string;
  location?: SourceLoc;
}

interface RelatedInfo {
  message: string;
  location: SourceLoc;
}
```

### Error Generation

```typescript
function unboundVariable(name: string, loc: SourceLoc, env: Environment): DiagnosticError {
  const similar = findSimilarNames(name, env);

  return {
    code: 'E0100',
    severity: 'error',
    message: `unbound variable '${name}'`,
    location: loc,
    span: { start: loc, end: { ...loc, column: loc.column + name.length } },
    notes: [],
    help: similar.length > 0
      ? `did you mean '${similar.join("' or '")}'?`
      : undefined
  };
}

function findSimilarNames(name: string, env: Environment): string[] {
  const allNames = env.getAllNames();
  return allNames
    .filter(n => levenshteinDistance(n, name) <= 2)
    .slice(0, 3);
}
```

### Pretty Printing

```typescript
function formatError(error: DiagnosticError, source: string): string {
  const lines = source.split('\n');
  const lineNum = error.location.line;
  const line = lines[lineNum - 1];
  const col = error.location.column;

  let output = '';

  // Header
  output += `error[${error.code}]: ${error.message}\n`;
  output += `  --> ${error.location.file}:${lineNum}:${col}\n`;
  output += `   |\n`;

  // Source line
  const lineNumStr = String(lineNum).padStart(3);
  output += `${lineNumStr} | ${line}\n`;

  // Pointer
  const pointer = ' '.repeat(col - 1) + '^'.repeat(error.span.end.column - col);
  output += `   | ${pointer}\n`;

  // Notes
  for (const note of error.notes) {
    output += `   = note: ${note.message}\n`;
  }

  // Help
  if (error.help) {
    output += `   = help: ${error.help}\n`;
  }

  return output;
}
```

---

## Stack Traces

### Format

```
error[E0104]: division by zero
  --> src/math.lisp:10:3
   |
10 |   (/ result count)
   |   ^^^^^^^^^^^^^^^^ attempted division by zero
   |
stack trace (most recent call first):
  at divide-safely (src/math.lisp:10:3)
  at compute-average (src/stats.lisp:25:5)
  at main (src/main.lisp:8:1)
```

### Continuation-Based Stack

```typescript
function formatStackTrace(cont: Continuation): string {
  const frames: string[] = [];

  for (const frame of cont.frames) {
    if (frame.sourceLoc) {
      const funcName = frame.marks.get('function-name') ?? '<anonymous>';
      frames.push(`  at ${funcName} (${formatLoc(frame.sourceLoc)})`);
    }
  }

  return 'stack trace (most recent call first):\n' + frames.join('\n');
}
```

---

## Warnings

### Format

```
warning[W0001]: unused variable 'temp'
  --> src/main.lisp:15:7
   |
15 |   (let ((temp (compute)))
   |         ^^^^ defined but never used
   |
```

### Types

| Code | Warning | Description |
|------|---------|-------------|
| W0001 | Unused variable | Defined but not used |
| W0002 | Shadowed variable | Shadows outer binding |
| W0003 | Deprecated function | Use newer alternative |
| W0004 | Unreachable code | Code after return |
| W0005 | Redundant condition | Always true/false |

---

## IDE Integration

### JSON Format for LSP

```json
{
  "severity": 1,
  "range": {
    "start": { "line": 41, "character": 4 },
    "end": { "line": 41, "character": 7 }
  },
  "message": "unbound variable 'foo'",
  "code": "E0100",
  "source": "lambdallm",
  "relatedInformation": [
    {
      "location": {
        "uri": "file:///src/main.lisp",
        "range": { "start": { "line": 10 }, "end": { "line": 10 } }
      },
      "message": "similar name 'foe' defined here"
    }
  ]
}
```

### Quick Fixes

```typescript
interface CodeAction {
  title: string;
  kind: string;
  diagnostics: Diagnostic[];
  edit: WorkspaceEdit;
}

// Generate quick fix for typo
function typoQuickFix(error: DiagnosticError, suggestion: string): CodeAction {
  return {
    title: `Change to '${suggestion}'`,
    kind: 'quickfix',
    diagnostics: [error],
    edit: {
      changes: {
        [error.location.file]: [{
          range: error.span,
          newText: suggestion
        }]
      }
    }
  };
}
```

---

## Condition Messages

### Formatted Condition

```
condition[file-not-found]: cannot read file 'config.json'
  --> src/loader.lisp:20:3
   |
20 |   (read-file "config.json")
   |   ^^^^^^^^^^^^^^^^^^^^^^^^^ file does not exist
   |
available restarts:
  [1] use-default - Use default configuration
  [2] try-another - Specify different path
  [3] abort       - Cancel operation

Enter restart number:
```

### Implementation

```typescript
function formatCondition(condition: Condition, cont: Continuation): string {
  let output = `condition[${symbolName(condition.type)}]: ${condition.message}\n`;

  // Source location from continuation
  const frame = cont.currentFrame;
  if (frame?.sourceLoc) {
    output += `  --> ${formatLoc(frame.sourceLoc)}\n`;
  }

  // Available restarts
  if (condition.restarts.length > 0) {
    output += '\navailable restarts:\n';
    for (let i = 0; i < condition.restarts.length; i++) {
      const r = condition.restarts[i];
      output += `  [${i + 1}] ${symbolName(r.name)} - ${r.description}\n`;
    }
  }

  return output;
}
```

---

## Debugging Tips

Generated automatically based on error:

```typescript
const debuggingTips: Record<string, string[]> = {
  'E0100': [
    'Check the spelling of the variable name',
    'Make sure the variable is defined before use',
    'Check if it should be imported from another namespace'
  ],
  'E0102': [
    'Check the function documentation for expected arguments',
    'Some functions are variadic (accept any number of args)',
    'Consider if you\'re calling the right function'
  ],
  'E0300': [
    'Verify the file path is correct',
    'Check if the file exists: (file-exists? "path")',
    'Use absolute paths to avoid ambiguity'
  ]
};
```
