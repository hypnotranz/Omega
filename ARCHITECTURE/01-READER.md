# 01: Reader (S-Expression Parser)

## What Is The Reader?

The Reader transforms text into data structures. In Lisp, code IS data (homoiconicity), so the reader produces the same structures used at runtime.

```
┌─────────────────────────────────────────────────────────────────┐
│                                                                 │
│  Source Text              Reader                AST/Data        │
│  ───────────              ──────                ────────        │
│                                                                 │
│  "(+ 1 2)"      ────▶    tokenize    ────▶    ['+', 1, 2]     │
│                          parse                                  │
│                                                                 │
│  "(define x 5)" ────▶    tokenize    ────▶    ['define', 'x', 5]│
│                          parse                                  │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

## Why Reader Is Separate

Real Lisps have the reader as a completely separate phase:

1. **Macros operate on AST** - after reading, before evaluation
2. **Multiple readers possible** - JSON reader, EDN reader, custom syntax
3. **Source locations** - reader tracks file:line:col for errors
4. **Read-time evaluation** - `#.(...)` in Common Lisp

## S-Expression Syntax

```lisp
;; Atoms
42                    ; Integer
3.14                  ; Float
"hello"               ; String
foo                   ; Symbol
:keyword              ; Keyword (optional)
#t #f                 ; Booleans

;; Compounds
(a b c)               ; List
[a b c]               ; Vector (optional)
{:a 1 :b 2}           ; Map (optional)

;; Special syntax
'x                    ; Quote: (quote x)
`(a ,b ,@c)          ; Quasiquote
;; comment            ; Comment (ignored)
```

## TypeScript Implementation

```typescript
// reader.ts

// Source location tracking
interface SourceLoc {
  file: string;
  line: number;
  column: number;
  offset: number;  // Character offset in file
}

interface LocatedValue {
  value: Value;
  loc: SourceLoc;
  endLoc: SourceLoc;
}

// Tokenizer
type Token =
  | { type: 'lparen'; loc: SourceLoc }
  | { type: 'rparen'; loc: SourceLoc }
  | { type: 'quote'; loc: SourceLoc }
  | { type: 'quasiquote'; loc: SourceLoc }
  | { type: 'unquote'; loc: SourceLoc }
  | { type: 'unquote-splicing'; loc: SourceLoc }
  | { type: 'number'; value: number; loc: SourceLoc }
  | { type: 'string'; value: string; loc: SourceLoc }
  | { type: 'symbol'; value: string; loc: SourceLoc }
  | { type: 'boolean'; value: boolean; loc: SourceLoc };

function tokenize(source: string, file: string = '<input>'): Token[] {
  const tokens: Token[] = [];
  let pos = 0;
  let line = 1;
  let column = 1;

  function currentLoc(): SourceLoc {
    return { file, line, column, offset: pos };
  }

  function advance(n: number = 1): void {
    for (let i = 0; i < n; i++) {
      if (source[pos] === '\n') {
        line++;
        column = 1;
      } else {
        column++;
      }
      pos++;
    }
  }

  while (pos < source.length) {
    const ch = source[pos];

    // Whitespace
    if (/\s/.test(ch)) {
      advance();
      continue;
    }

    // Comment
    if (ch === ';') {
      while (pos < source.length && source[pos] !== '\n') {
        advance();
      }
      continue;
    }

    // Parentheses
    if (ch === '(') {
      tokens.push({ type: 'lparen', loc: currentLoc() });
      advance();
      continue;
    }
    if (ch === ')') {
      tokens.push({ type: 'rparen', loc: currentLoc() });
      advance();
      continue;
    }

    // Quote shortcuts
    if (ch === "'") {
      tokens.push({ type: 'quote', loc: currentLoc() });
      advance();
      continue;
    }
    if (ch === '`') {
      tokens.push({ type: 'quasiquote', loc: currentLoc() });
      advance();
      continue;
    }
    if (ch === ',') {
      if (source[pos + 1] === '@') {
        tokens.push({ type: 'unquote-splicing', loc: currentLoc() });
        advance(2);
      } else {
        tokens.push({ type: 'unquote', loc: currentLoc() });
        advance();
      }
      continue;
    }

    // String
    if (ch === '"') {
      const loc = currentLoc();
      advance(); // opening quote
      let value = '';
      while (pos < source.length && source[pos] !== '"') {
        if (source[pos] === '\\') {
          advance();
          const escape = source[pos];
          if (escape === 'n') value += '\n';
          else if (escape === 't') value += '\t';
          else if (escape === 'r') value += '\r';
          else if (escape === '\\') value += '\\';
          else if (escape === '"') value += '"';
          else value += escape;
        } else {
          value += source[pos];
        }
        advance();
      }
      advance(); // closing quote
      tokens.push({ type: 'string', value, loc });
      continue;
    }

    // Number or Symbol
    const loc = currentLoc();
    let atom = '';
    while (pos < source.length && !/[\s()'`,;]/.test(source[pos])) {
      atom += source[pos];
      advance();
    }

    // Booleans
    if (atom === '#t' || atom === 'true') {
      tokens.push({ type: 'boolean', value: true, loc });
    } else if (atom === '#f' || atom === 'false') {
      tokens.push({ type: 'boolean', value: false, loc });
    }
    // Numbers
    else if (/^-?\d+(\.\d+)?$/.test(atom)) {
      tokens.push({ type: 'number', value: parseFloat(atom), loc });
    }
    // Symbols
    else {
      tokens.push({ type: 'symbol', value: atom, loc });
    }
  }

  return tokens;
}

// Parser
function parse(tokens: Token[]): LocatedValue[] {
  let pos = 0;

  function parseExpr(): LocatedValue {
    const token = tokens[pos];

    // Quote shortcuts
    if (token.type === 'quote') {
      pos++;
      const inner = parseExpr();
      return {
        value: [Symbol.for('quote'), inner.value],
        loc: token.loc,
        endLoc: inner.endLoc
      };
    }
    if (token.type === 'quasiquote') {
      pos++;
      const inner = parseExpr();
      return {
        value: [Symbol.for('quasiquote'), inner.value],
        loc: token.loc,
        endLoc: inner.endLoc
      };
    }
    if (token.type === 'unquote') {
      pos++;
      const inner = parseExpr();
      return {
        value: [Symbol.for('unquote'), inner.value],
        loc: token.loc,
        endLoc: inner.endLoc
      };
    }
    if (token.type === 'unquote-splicing') {
      pos++;
      const inner = parseExpr();
      return {
        value: [Symbol.for('unquote-splicing'), inner.value],
        loc: token.loc,
        endLoc: inner.endLoc
      };
    }

    // List
    if (token.type === 'lparen') {
      pos++;
      const elements: Value[] = [];
      const startLoc = token.loc;
      let endLoc = startLoc;

      while (pos < tokens.length && tokens[pos].type !== 'rparen') {
        const elem = parseExpr();
        elements.push(elem.value);
        endLoc = elem.endLoc;
      }

      if (pos >= tokens.length) {
        throw new ReadError('Unclosed parenthesis', startLoc);
      }
      endLoc = tokens[pos].loc;
      pos++; // consume ')'

      return { value: elements, loc: startLoc, endLoc };
    }

    // Atoms
    if (token.type === 'number') {
      pos++;
      return { value: token.value, loc: token.loc, endLoc: token.loc };
    }
    if (token.type === 'string') {
      pos++;
      return { value: token.value, loc: token.loc, endLoc: token.loc };
    }
    if (token.type === 'boolean') {
      pos++;
      return { value: token.value, loc: token.loc, endLoc: token.loc };
    }
    if (token.type === 'symbol') {
      pos++;
      return { value: Symbol.for(token.value), loc: token.loc, endLoc: token.loc };
    }

    throw new ReadError(`Unexpected token: ${token.type}`, token.loc);
  }

  const exprs: LocatedValue[] = [];
  while (pos < tokens.length) {
    exprs.push(parseExpr());
  }
  return exprs;
}

// Public API
export function read(source: string, file?: string): Value[] {
  const tokens = tokenize(source, file);
  const located = parse(tokens);
  return located.map(l => l.value);
}

export function readOne(source: string, file?: string): Value {
  const exprs = read(source, file);
  if (exprs.length !== 1) {
    throw new ReadError(`Expected one expression, got ${exprs.length}`);
  }
  return exprs[0];
}

// With source locations (for error messages, debugging)
export function readLocated(source: string, file?: string): LocatedValue[] {
  const tokens = tokenize(source, file);
  return parse(tokens);
}
```

## Pretty Printer

```typescript
// printer.ts
export function print(value: Value): string {
  if (value === null || value === undefined) return 'nil';
  if (typeof value === 'boolean') return value ? '#t' : '#f';
  if (typeof value === 'number') return String(value);
  if (typeof value === 'string') return JSON.stringify(value);
  if (typeof value === 'symbol') return Symbol.keyFor(value) ?? value.toString();

  if (Array.isArray(value)) {
    // Check for quote
    if (value.length === 2 && value[0] === Symbol.for('quote')) {
      return `'${print(value[1])}`;
    }
    return `(${value.map(print).join(' ')})`;
  }

  return String(value);
}
```

## Error Messages with Source Locations

```typescript
class ReadError extends Error {
  constructor(message: string, public loc?: SourceLoc) {
    super(loc ? `${loc.file}:${loc.line}:${loc.column}: ${message}` : message);
    this.name = 'ReadError';
  }
}

// Example error:
// ReadError: main.lisp:42:15: Unclosed parenthesis
```

## Comparison to Python LambdaRLM

| Feature | Python (sexp.py) | TypeScript (reader.ts) |
|---------|------------------|------------------------|
| Tokenization | ✅ Regex-based | ✅ Manual (cleaner) |
| Parsing | ✅ Recursive | ✅ Same |
| Source locations | ❌ Not tracked | ✅ Full tracking |
| Error messages | ⚠️ Generic | ✅ File:line:col |
| Quote shortcuts | ✅ Has them | ✅ Same |
| Unicode | ⚠️ Basic | ✅ Full support |

## Integration Points

The reader connects to:
- **Evaluator**: Produces AST for evaluation
- **Macros**: AST transformed before evaluation
- **Debugger**: Source locations for stack traces
- **Editor**: Source locations for "go to definition"
