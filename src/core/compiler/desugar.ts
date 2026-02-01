import type { Diagnostic } from "../../outcome/diagnostic";
import { errorDiag } from "../../outcome/diagnostic";
import type { CoreForm, CoreFormTag, DesugarResult, Form, ValueLiteral } from "./types";

const ERR_UNKNOWN = "E0001";

export function desugarForm(form: Form): DesugarResult {
  const diagnostics: Diagnostic[] = [];
  const coreForm = desugar(form, diagnostics);

  return {
    ok: diagnostics.length === 0,
    coreForm,
    diagnostics,
  };
}

function desugar(form: Form, diagnostics: Diagnostic[]): CoreForm {
  switch (form.tag) {
    case "Number":
    case "String":
    case "Keyword":
    case "Symbol":
    case "Atom":
      return quoteLiteral(literal(form), metaFor(form));
    default:
      break;
  }

  if (form.tag !== "List" || !form.children || form.children.length === 0) {
    return quoteLiteral(literal(form), metaFor(form));
  }

  const head = form.children[0];
  const headSym = head.tag === "Symbol" ? String(head.value) : null;
  const args = form.children.slice(1);

  switch (headSym) {
    case "quote":
      return { tag: "quote", meta: metaFor(form), args: [literal(args[0])] };
    case "pure":
    case "fail":
    case "emit":
    case "observe":
    case "commit":
    case "validate":
    case "with-budget":
    case "with-timeout":
    case "infer":
    case "tool-call":
      return simpleCore(headSym, metaFor(form), args, diagnostics);
    case "bind":
    case "catch":
      return simpleCore(headSym, metaFor(form), args, diagnostics);
    case "all":
    case "any":
    case "race":
    case "sequence":
      return { tag: headSym as CoreFormTag, meta: metaFor(form), args: args.map(arg => desugar(arg, diagnostics)) };
    case "if":
      return {
        tag: "branch",
        meta: metaFor(form),
        args: args.map(arg => desugar(arg, diagnostics)),
      };
    case "lambda": {
      const paramsForm = args[0];
      const params =
        paramsForm?.children?.map(p => String(p.value)) ?? [];
      const body = args[1] ? desugar(args[1], diagnostics) : quoteLiteral(literalAtom(null, form.meta), form.meta);
      return { tag: "lambda", meta: metaFor(form), args: [literalAtom(params, form.meta), body] };
    }
    case "let":
    case "letrec": {
      const bindingsForm = args[0];
      const bodyForm = args[1];
      const bindings: [string, CoreForm][] = [];
      const pairs = bindingsForm?.children ?? [];
      for (const pair of pairs) {
        const [nameForm, exprForm] = pair.children ?? [];
        bindings.push([String(nameForm?.value ?? ""), desugar(exprForm, diagnostics)]);
      }
      return {
        tag: headSym as CoreFormTag,
        meta: metaFor(form),
        args: [literalAtom(bindings, form.meta), desugar(bodyForm, diagnostics)],
      };
    }
    case "let*": {
      const bindingsForm = args[0];
      const bodyForm = args[1];
      const pairs = bindingsForm?.children ?? [];
      let body = desugar(bodyForm, diagnostics);
      for (let i = pairs.length - 1; i >= 0; i--) {
        const pair = pairs[i];
        const [nameForm, exprForm] = pair.children ?? [];
        body = {
          tag: "let",
          meta: metaFor(pair),
          args: [literalAtom([[String(nameForm?.value ?? ""), desugar(exprForm, diagnostics)]], pair.meta), body],
        };
      }
      return body;
    }
    case "begin": {
      const forms = args.map(arg => desugar(arg, diagnostics));
      return { tag: "begin", meta: metaFor(form), args: forms };
    }
    case "and": {
      if (args.length === 0) return quoteLiteral(literalAtom(true, form.meta), form.meta);
      const base = desugar(args[args.length - 1], diagnostics);
      const remaining = args.slice(0, -1);
      return remaining.reduceRight<CoreForm>((acc, arg) => {
        return {
          tag: "branch",
          meta: metaFor(form),
          args: [desugar(arg, diagnostics), acc, quoteLiteral(literalAtom(false, form.meta), form.meta)],
        };
      }, base);
    }
    case "or": {
      if (args.length === 0) return quoteLiteral(literalAtom(false, form.meta), form.meta);
      const [first, ...rest] = args;
      if (rest.length === 0) return desugar(first, diagnostics);
      const tempName = "__or_tmp";
      const restBranch = rest
        .slice(0, -1)
        .reduceRight<CoreForm>(
        (acc, arg) => ({
          tag: "branch",
          meta: form.meta,
          args: [desugar(arg, diagnostics), desugar(arg, diagnostics), acc],
        }),
        desugar(rest[rest.length - 1], diagnostics)
      );
      return {
        tag: "let",
        meta: metaFor(form),
        args: [
          literalAtom([[tempName, desugar(first, diagnostics)]], form.meta),
          {
            tag: "branch",
            meta: metaFor(form),
            args: [quoteLiteral(literalAtom(tempName, form.meta), form.meta), quoteLiteral(literalAtom(tempName, form.meta), form.meta), restBranch],
          },
        ],
      };
    }
    case "cond": {
      // (cond (test1 e1...) (test2 e2...) ... (else en...))
      // Desugars to nested if: (if test1 (begin e1...) (if test2 (begin e2...) ...))
      if (args.length === 0) {
        // Empty cond returns #f
        return quoteLiteral(literalAtom(false, form.meta), form.meta);
      }

      // Process clauses from right to left to build nested if
      let result: CoreForm = quoteLiteral(literalAtom(false, form.meta), form.meta); // default: #f if no else

      for (let i = args.length - 1; i >= 0; i--) {
        const clause = args[i];
        if (!clause.children || clause.children.length === 0) {
          diagnostics.push(errorDiag(ERR_UNKNOWN, "cond: empty clause", { span: clause.meta?.span }));
          continue;
        }

        const testForm = clause.children[0];
        const bodyForms = clause.children.slice(1);

        // Check for 'else' clause
        const isElse = testForm.tag === "Symbol" && String(testForm.value) === "else";

        if (isElse) {
          // (else e1 e2 ...) → (begin e1 e2 ...)
          if (bodyForms.length === 0) {
            result = quoteLiteral(literalAtom(null, form.meta), form.meta); // unit
          } else if (bodyForms.length === 1) {
            result = desugar(bodyForms[0], diagnostics);
          } else {
            result = { tag: "begin", meta: metaFor(clause), args: bodyForms.map(b => desugar(b, diagnostics)) };
          }
        } else {
          // (test e1 e2 ...) → (if test (begin e1 e2 ...) <rest>)
          let body: CoreForm;
          if (bodyForms.length === 0) {
            // (test) with no body - return test value (like Scheme)
            body = desugar(testForm, diagnostics);
          } else if (bodyForms.length === 1) {
            body = desugar(bodyForms[0], diagnostics);
          } else {
            body = { tag: "begin", meta: metaFor(clause), args: bodyForms.map(b => desugar(b, diagnostics)) };
          }

          result = {
            tag: "branch",
            meta: metaFor(clause),
            args: [desugar(testForm, diagnostics), body, result],
          };
        }
      }

      return result;
    }
    default:
      diagnostics.push(errorDiag(ERR_UNKNOWN, `Unknown form: ${headSym ?? head.tag}`, { span: form.meta.span }));
      return quoteLiteral(literal(form), metaFor(form));
  }
}

function simpleCore(tag: string, meta: Form["meta"], args: Form[], diagnostics: Diagnostic[]): CoreForm {
  return {
    tag: tag as CoreFormTag,
    meta,
    args: args.map(arg => desugar(arg, diagnostics)),
  };
}

function literalAtom(value: unknown, meta: Form["meta"]): ValueLiteral {
  return { tag: "literal", meta, value };
}

function literal(form: Form | undefined): ValueLiteral {
  if (!form) return { tag: "literal", meta: { span: { startLine: 0, startCol: 0, endLine: 0, endCol: 0 } as any }, value: null };
  return { tag: "literal", meta: form.meta, value: form.tag === "Number" ? Number(form.value) : form.value };
}

function quoteLiteral(value: ValueLiteral, meta: Form["meta"]): CoreForm {
  return { tag: "quote", meta, args: [value] };
}

function metaFor(form: Form): Form["meta"] {
  return { ...form.meta, originalForm: form };
}
