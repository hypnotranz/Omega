import { describe, expect, it } from "vitest";
import { readForms } from "../../src/core/compiler/reader";
import { desugarForm } from "../../src/core/compiler/desugar";

describe("compiler desugarer", () => {
  it("desugars and into nested branches", () => {
    const form = readForms("(and a b c)").forms[0];
    const result = desugarForm(form);

    expect(result.ok).toBe(true);
    expect(result.coreForm.tag).toBe("branch");
    const thenBranch = result.coreForm.args[1] as any;
    expect(thenBranch.tag).toBe("branch");
  });

  it("desugars let* into nested lets", () => {
    const form = readForms("(let* ([x 1] [y x]) y)").forms[0];
    const result = desugarForm(form);

    expect(result.ok).toBe(true);
    expect(result.coreForm.tag).toBe("let");
    const inner = (result.coreForm.args[1] as any);
    expect(inner.tag).toBe("let");
  });
});
