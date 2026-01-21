import type { Diagnostic } from "../../outcome/diagnostic";
import { errorDiag } from "../../outcome/diagnostic";
import type { Form, FormMeta, ReadResult } from "./types";

type SpanState = { line: number; col: number };

const ERR_UNBALANCED = "E0002";
const ERR_STRING = "E0003";

export function readForms(source: string, filename = "<stdin>"): ReadResult {
  const diagnostics: Diagnostic[] = [];
  const forms: Form[] = [];

  let idx = 0;
  let pos: SpanState = { line: 1, col: 1 };

  const peek = () => source[idx];
  const eof = () => idx >= source.length;

  function advance(): string {
    const ch = source[idx++];
    if (ch === "\n") {
      pos = { line: pos.line + 1, col: 1 };
    } else {
      pos = { line: pos.line, col: pos.col + 1 };
    }
    return ch;
  }

  function skipWhitespace(): void {
    while (!eof()) {
      const ch = peek();
      if (/\s/.test(ch)) {
        advance();
        continue;
      }
      if (ch === ";") {
        // comment to end of line
        while (!eof() && advance() !== "\n") {
          /* skip */
        }
        continue;
      }
      break;
    }
  }

  function spanFrom(start: SpanState): FormMeta["span"] {
    return {
      file: filename,
      startLine: start.line,
      startCol: start.col,
      endLine: pos.line,
      endCol: pos.col - 1,
    };
  }

  function parseString(start: SpanState): Form | null {
    // consume opening quote
    advance();
    let value = "";
    while (!eof()) {
      const ch = advance();
      if (ch === `"`) {
        return {
          tag: "String",
          value,
          meta: { span: spanFrom(start) },
        };
      }
      if (ch === "\\") {
        const next = advance();
        switch (next) {
          case "n":
            value += "\n";
            break;
          case "t":
            value += "\t";
            break;
          case "\\":
          case `"`:
            value += next;
            break;
          default:
            value += next;
        }
      } else {
        value += ch;
      }
    }

    diagnostics.push(
      errorDiag(ERR_STRING, "Invalid string literal: unterminated", { span: spanFrom(start) })
    );
    return null;
  }

  function parseNumber(start: SpanState, firstChar: string): Form {
    let numStr = firstChar;
    while (!eof() && /[0-9._-]/.test(peek())) {
      numStr += advance();
    }
    const num = Number(numStr);
    return {
      tag: "Number",
      value: num,
      meta: { span: spanFrom(start) },
    };
  }

  function parseSymbol(start: SpanState, firstChar: string): Form {
    let sym = firstChar;
    while (
      !eof() &&
      !/\s/.test(peek()) &&
      !"()[]{}".includes(peek())
    ) {
      sym += advance();
    }
    return {
      tag: sym.startsWith(":") ? "Keyword" : "Symbol",
      value: sym,
      meta: { span: spanFrom(start) },
    };
  }

  function parseDelimited(kind: "List" | "Vector" | "Map", endCh: string): Form | null {
    const start = { ...pos };
    advance(); // consume opener
    const children: Form[] = [];

    while (true) {
      skipWhitespace();
      if (eof()) {
        diagnostics.push(
          errorDiag(ERR_UNBALANCED, "Unbalanced parentheses", { span: spanFrom(start) })
        );
        return null;
      }
      if (peek() === endCh) {
        advance();
        return { tag: kind, value: null, children, meta: { span: spanFrom(start) } };
      }
      const child = parseForm();
      if (!child) return null;
      children.push(child);
    }
  }

  function parseForm(): Form | null {
    skipWhitespace();
    if (eof()) return null;
    const start = { ...pos };
    const ch = peek();

    if (ch === "(") return parseDelimited("List", ")");
    if (ch === "[") return parseDelimited("Vector", "]");
    if (ch === "{") return parseDelimited("Map", "}");
    if (ch === `"`) return parseString(start);
    if (/[0-9]/.test(ch) || (ch === "-" && /[0-9]/.test(source[idx + 1]))) {
      return parseNumber(start, advance());
    }

    return parseSymbol(start, advance());
  }

  while (true) {
    skipWhitespace();
    if (eof()) break;
    const form = parseForm();
    if (!form) {
      break;
    }
    forms.push(form);
  }

  return { ok: diagnostics.length === 0, forms, diagnostics };
}
