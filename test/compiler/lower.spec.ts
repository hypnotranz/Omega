import { describe, expect, it } from "vitest";
import type { CoreForm } from "../../src/core/compiler/types";
import { createLowerEnv, lowerCoreForm } from "../../src/core/compiler/lower";
import { CURRENT_IR_VERSION } from "../../src/frameir/version";

const span = { file: "test", startLine: 1, startCol: 1, endLine: 1, endCol: 1 };
const lit = (value: unknown): any => ({ tag: "literal", meta: { span }, value });

describe("compiler lowerer", () => {
  it("lowers pure to FPure", () => {
    const form: CoreForm = {
      tag: "pure",
      meta: { span },
      args: [lit(42)],
    };

    const env = createLowerEnv();
    const result = lowerCoreForm(form, env);

    expect(result.ok).toBe(true);
    const ir = result.ir as any;
    expect(ir.tag).toBe("FPure");
    expect(ir.v).toBe(CURRENT_IR_VERSION);
    expect(ir.value.tag).toBe("VInt");
  });

  it("creates function definitions for bind continuations", () => {
    const pureForm: CoreForm = { tag: "pure", meta: { span }, args: [lit(0)] };
    const lambdaForm: CoreForm = {
      tag: "lambda",
      meta: { span },
      args: [
        lit(["x"]),
        { tag: "pure", meta: { span }, args: [lit("done")] } as CoreForm,
      ],
    };
    const bindForm: CoreForm = {
      tag: "bind",
      meta: { span },
      args: [pureForm, lambdaForm],
    };

    const env = createLowerEnv();
    const result = lowerCoreForm(bindForm, env);

    expect(result.ok).toBe(true);
    expect(result.fnDefs.length).toBe(1);
    const ir = result.ir as any;
    expect(ir.tag).toBe("FBind");
    expect(ir.k.tag).toBe("VRef");
  });
});
