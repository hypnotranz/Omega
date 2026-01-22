import * as fs from "fs";
import * as readline from "readline";
import type { SessionEvent, SessionIndex, CheckpointIndex, LLMReceipt } from "./types";
import { deserializeState } from "./serializer";
import type { State } from "../eval/machine";
import type { Val } from "../eval/values";

export type CheckpointView = CheckpointIndex & { rawSeq: number };

export class SessionReader {
  private events: SessionEvent[] = [];
  private index: SessionIndex;

  constructor(
    private eventFile: string,
    private indexFile: string,
    private nativeRegistry: Map<string, Val>
  ) {
    this.index = JSON.parse(fs.readFileSync(indexFile, "utf8"));
  }

  async loadAll(): Promise<void> {
    this.events = [];

    const fileStream = fs.createReadStream(this.eventFile);
    const rl = readline.createInterface({ input: fileStream });

    try {
      for await (const line of rl) {
        if (line.trim()) {
          this.events.push(JSON.parse(line));
        }
      }
    } finally {
      rl.close();
      await new Promise<void>(resolve => {
        if (fileStream.closed || fileStream.destroyed) {
          resolve();
          return;
        }
        fileStream.once("close", resolve);
        fileStream.close();
      });
    }
  }

  getEventCount(): number {
    return this.index.eventCount;
  }

  getEvent(seq: number): SessionEvent | undefined {
    return this.events.find(e => "seq" in e && (e as any).seq === seq);
  }

  getCheckpoints(): CheckpointView[] {
    return this.index.checkpoints.map(cp => this.decorateCheckpoint(cp));
  }

  findCheckpointBefore(targetSeq: number): CheckpointView | undefined {
    let best: CheckpointIndex | undefined;
    for (const cp of this.index.checkpoints) {
      if (cp.seq < targetSeq && (!best || cp.seq > best.seq)) {
        best = cp;
      }
    }
    return best ? this.decorateCheckpoint(best) : undefined;
  }

  getCheckpointState(stateId: string): State {
    const serialized = this.index.states[stateId];
    if (!serialized) {
      throw new Error(`State not found: ${stateId}`);
    }
    return deserializeState(serialized, this.nativeRegistry);
  }

  getReceipt(key: string): LLMReceipt | undefined {
    return this.index.receipts[key];
  }

  getAllEvents(): SessionEvent[] {
    return this.events;
  }

  getEventsInRange(startSeq: number, endSeq: number): SessionEvent[] {
    return this.events.filter(e => {
      if (!("seq" in e)) return false;
      const seq = (e as any).seq;
      return seq >= startSeq && seq <= endSeq;
    });
  }

  private decorateCheckpoint(cp: CheckpointIndex): CheckpointView {
    return { ...cp, rawSeq: cp.seq };
  }
}
