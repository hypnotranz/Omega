import { COWStore } from "./src/core/eval/store";
import type { State } from "./src/core/eval/machine";
import type { Val } from "./src/core/eval/values";
import { runToCompletion } from "./src/core/eval/run";
import { compileTextToExpr } from "./src/core/pipeline/compileText";
import { installPrims } from "./src/core/prims";
import { createTestRuntime } from "./test/helpers/runtime";

function initialState(src: string): State {
  const expr = compileTextToExpr(src);
  const store0 = new COWStore();
  const prim = installPrims(store0);
  return {
    control: { tag: "Expr", e: expr },
    env: prim.env,
    store: prim.store,
    kont: [],
    handlers: [],
  };
}

async function evalOmega(src: string): Promise<Val> {
  const runtime = createTestRuntime();
  const st0 = initialState(src);
  return runToCompletion(runtime, st0);
}

const src = `
  (begin
    (define saved #f)
    (define resumed? #f)
    (call/cc (lambda (k) (set! saved k) 0))
    (if resumed?
        7
        (begin
          (set! resumed? #t)
          (if (procedure? saved)
              (apply saved (list 7))
              'not-procedure))))
`;

evalOmega(src).then(result => {
  console.log("Result:", JSON.stringify(result, null, 2));
}).catch(err => {
  console.error("Error:", err);
});
