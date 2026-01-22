import { SessionReader, type CheckpointView } from "./reader";
import type { State } from "../eval/machine";

export type JumpResult = {
  state: State;
  seq: number;
  replayedSteps: number;
  usedReceipts: string[];
};

export class JumpController {
  constructor(private reader: SessionReader) {}

  async jumpTo(targetSeq: number): Promise<JumpResult> {
    const exact = this.reader.getCheckpoints().find(cp => cp.seq === targetSeq);
    const checkpoint = exact ?? this.reader.findCheckpointBefore(targetSeq);

    if (!checkpoint) {
      throw new Error(`No checkpoint found before seq ${targetSeq}`);
    }

    const checkpointActualSeq = this.getCheckpointActualSeq(checkpoint);
    const targetActualSeq = targetSeq;
    const state = this.reader.getCheckpointState(checkpoint.stateId);

    if (checkpoint.seq === targetSeq) {
      return { state, seq: checkpoint.seq, replayedSteps: 0, usedReceipts: [] };
    }

    const { usedReceipts, replayedSteps } = this.collectReplayInfo(checkpointActualSeq, targetActualSeq);

    return {
      state,
      seq: checkpoint.seq,
      replayedSteps,
      usedReceipts,
    };
  }

  private collectReplayInfo(checkpointSeq: number, targetSeq: number): { usedReceipts: string[]; replayedSteps: number } {
    if (targetSeq <= checkpointSeq) {
      return { usedReceipts: [], replayedSteps: 0 };
    }

    const used = new Set<string>();
    const events = this.reader.getEventsInRange(checkpointSeq + 1, targetSeq);

    for (const event of events) {
      if ((event as any).type === "llm_resp" && "receiptKey" in (event as any)) {
        used.add((event as any).receiptKey as string);
      }
    }

    return { usedReceipts: Array.from(used), replayedSteps: events.length };
  }

  private getCheckpointActualSeq(cp: CheckpointView): number {
    return cp.rawSeq ?? cp.seq;
  }
}
