import { describe, expect, it } from "vitest";
import { readForms } from "../../src/core/compiler/reader";

describe("compiler reader", () => {
  it("parses atoms and lists with spans", () => {
    const src = "(add 1 2)";
    const result = readForms(src, "test.scm");

    expect(result.ok).toBe(true);
    expect(result.forms).toHaveLength(1);

    const list = result.forms[0];
    expect(list.tag).toBe("List");
    expect(list.children?.map(child => child.tag)).toEqual(["Symbol", "Number", "Number"]);
    expect(list.meta.span.startLine).toBe(1);
    expect(list.meta.span.startCol).toBe(1);

    const sym = list.children?.[0];
    expect(sym?.meta.span.startCol).toBe(2);
  });

  it("decodes string escapes", () => {
    const res = readForms('(say "hi\\nthere")');
    expect(res.ok).toBe(true);
    const stringForm = res.forms[0]?.children?.[1];
    expect(stringForm?.tag).toBe("String");
    expect(stringForm?.value).toBe("hi\nthere");
  });

  it("reports unbalanced parentheses", () => {
    const res = readForms("(foo");
    expect(res.ok).toBe(false);
    expect(res.diagnostics[0]?.code).toBe("E0002");
  });
});
