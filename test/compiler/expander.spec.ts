import { describe, expect, it } from "vitest";
import { readForms } from "../../src/core/compiler/reader";
import {
  createMacroEnv,
  defineMacro,
  expandModule,
  gensym,
} from "../../src/core/compiler/expander";

const dummySpan = { file: "test", startLine: 1, startCol: 1, endLine: 1, endCol: 1 };

describe("compiler macroexpander", () => {
  it("expands syntax-rules style macros", () => {
    const env = createMacroEnv();
    const pattern = readForms("(when test body)").forms[0];
    const template = readForms("(if test body (quote nil))").forms[0];

    defineMacro(env, "when", {
      patterns: [{ pattern, template }],
    });

    const forms = readForms("(when true 1)").forms;
    const [result] = expandModule(forms, env);

    expect(result.ok).toBe(true);
    const expanded = result.form;
    expect(expanded.tag).toBe("List");
    expect(expanded.children?.[0].value).toBe("if");
    expect(expanded.meta.macroExpanded).toBe(true);
  });

  it("generates fresh symbols to preserve hygiene", () => {
    const env = createMacroEnv();

    defineMacro(env, "fresh", {
      transformer: () => ({
        tag: "Symbol",
        meta: { span: dummySpan, macroExpanded: true },
        value: gensym(env, "tmp"),
      }),
    });

    const [first] = expandModule(readForms("(fresh)").forms, env);
    const [second] = expandModule(readForms("(fresh)").forms, env);

    expect(first.form.value).not.toBe(second.form.value);
  });
});
