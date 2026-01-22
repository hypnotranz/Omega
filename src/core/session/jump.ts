import { SessionReader } from "./reader";
import { stepOnce } from "../eval/machineStep";
import { captureValueResumption } from "../effects/capture";
import type { State } from "../eval/machine";
import type { Val } from "../eval/values";

export type JumpResult = {
  state: State;
  seq: number;
  replayedSteps: number;
  usedReceipts: string[];
};

function responseToVal(responseContent: string): Val {
  return { tag: "Str", s: responseContent };
}

export class JumpController {
  constructor(private reader: SessionReader) {}

  async jumpTo(targetSeq: number): Promise<JumpResult> {
    const checkpoint = this.reader.findCheckpointBefore(targetSeq);

    if (!checkpoint) {
      throw new Error(`No checkpoint found before seq ${targetSeq}`);
    }

    let state = this.reader.getCheckpointState(checkpoint.stateId);
    const usedReceipts = new Set<string>();
    let replayedSteps = 0;
    let pending: { resumption: { invoke: (v: Val) => State }; receiptKey?: string } | null = null;

    const events = this.reader.getEventsInRange(checkpoint.seq + 1, targetSeq);
    for (const event of events) {
      if (!("seq" in event)) continue;

      switch (event.type) {
        case "step": {
          const result = stepOnce(state);
          if (result.tag === "State") {
            state = result.state;
          } else if (result.tag === "Done") {
            state = result.state;
          } else if (result.tag === "Op") {
            pending = { resumption: captureValueResumption(result.state) };
            state = result.state;
          }
          replayedSteps += 1;
          break;
        }
        case "llm_req":
          if (pending) {
            pending.receiptKey = event.receiptKey;
          }
          break;
        case "llm_resp": {
          const receipt = this.reader.getReceipt(event.receiptKey);
          if (!receipt) {
            throw new Error(`Receipt not found: ${event.receiptKey}`);
          }
          usedReceipts.add(event.receiptKey);
          const content = typeof receipt.response?.content === "string"
            ? receipt.response.content
            : JSON.stringify(receipt.response ?? "");
          const cachedVal = responseToVal(content);
          if (pending) {
            state = pending.resumption.invoke(cachedVal);
            pending = null;
          } else {
            const resumption = captureValueResumption(state);
            state = resumption.invoke(cachedVal);
          }
          break;
        }
        case "checkpoint":
          state = this.reader.getCheckpointState(event.stateId);
          pending = null;
          break;
        case "resume":
        case "effect":
        case "input":
        case "result":
        case "error":
          break;
      }
    }

    return {
      state,
      seq: targetSeq,
      replayedSteps,
      usedReceipts: Array.from(usedReceipts),
    };
  }
}
