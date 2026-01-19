// SOURCE: ARCHITECTURE/32-LANGUAGE-OFFICIAL-13-IMPLEMENTATION-11.md
// AUTO-EXTRACTED - Do not edit directly. Edit the source document.

import type { Runtime } from "../../eval/runtime";
import type { State } from "../../eval/machine";
import type { Val } from "../../eval/values";
import { stepOnce } from "../../eval/machineStep";

export type NondetMode = "first" | "all";
export type Frontier = "bfs" | "dfs";

export type NondetOptions = {
  mode: NondetMode;
  frontier: Frontier;
  quantumSteps: number;
  maxTotalSteps: number;
  maxJobs: number;
};

export type NondetResult =
  | { tag: "None" }
  | { tag: "One"; value: Val }
  | { tag: "Many"; values: Val[] };

type Job = { state: State };

function isVectorVal(v: Val): v is { tag: "Vector"; items: Val[] } {
  return v.tag === "Vector";
}

function enqueue(frontier: Frontier, q: Job[], j: Job): Job[] {
  if (frontier === "bfs") return q.concat([j]);
  // dfs
  return [j].concat(q);
}

export async function runNondet(runtime: Runtime, initial: State, opts: NondetOptions): Promise<NondetResult> {
  let queue: Job[] = [{ state: initial }];
  const solutions: Val[] = [];
  let totalSteps = 0;

  while (queue.length > 0) {
    if (queue.length > opts.maxJobs) throw new Error("runNondet: maxJobs exceeded");

    const job = queue[0];
    queue = queue.slice(1);

    let st = job.state;

    for (let q = 0; q < opts.quantumSteps; q++) {
      totalSteps++;
      if (totalSteps > opts.maxTotalSteps) throw new Error("runNondet: maxTotalSteps exceeded");

      const out = stepOnce(st);

      if (out.tag === "State") {
        st = out.state;
        continue;
      }

      if (out.tag === "Done") {
        solutions.push(out.value);
        if (opts.mode === "first") return { tag: "One", value: out.value };
        break; // finish this job
      }

      if (out.tag === "Op") {
        const op = out.opcall.op;

        if (op === "amb.op") {
          // Convention: amb.op args:
          //   either a single Vector of choices, or a variadic list of choices.
          let choices: Val[] = [];
          if (out.opcall.args.length === 1 && isVectorVal(out.opcall.args[0])) {
            choices = out.opcall.args[0].items;
          } else {
            choices = out.opcall.args;
          }

          // Fork: each choice resumes the continuation with that choice as the op result.
          for (const ch of choices) {
            const child = out.opcall.resumption.invoke(ch);
            queue = enqueue(opts.frontier, queue, { state: child });
          }
          break; // stop current job; replaced by children
        }

        const handled = await runtime.dispatch(out.state, out.opcall);
        if (handled === "Uncaught") throw new Error(`Uncaught op: ${out.opcall.op}`);
        st = handled;
        continue;
      }
    }

    // If quantum expired and we still have a running state, requeue it (fair scheduling).
    if (st.control.tag !== "Val" || st.kont.length > 0) {
      queue = enqueue(opts.frontier, queue, { state: st });
    }
  }

  if (solutions.length === 0) return { tag: "None" };
  return { tag: "Many", values: solutions };
}