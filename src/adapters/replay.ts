import type { ClockPort } from "../ports/clock";
import type { OraclePort, OracleRequest, OracleResponse } from "../ports/oracle";
import type { RngPort } from "../ports/rng";
import type { ExecContext } from "../ports/types";

/**
 * Replay event log entry.
 */
export interface ReplayLogEntry {
  id: string;
  tag: string;
  request?: unknown;
  response?: unknown;
  value?: unknown;
}

/**
 * Replay log source.
 */
export interface ReplayLog {
  get(id: string): ReplayLogEntry | undefined;
  getByTag(tag: string): ReplayLogEntry[];
}

function takeEntry(entries: ReplayLogEntry[], index: { value: number }, kind: string): ReplayLogEntry {
  const entry = entries[index.value++];
  if (!entry) {
    throw new Error(`Replay log exhausted for ${kind}`);
  }
  return entry;
}

/**
 * Create replay oracle that returns logged responses.
 */
export function replayOracle(log: ReplayLog): OraclePort {
  const entries = log.getByTag("E_OracleCall");
  const index = { value: 0 };

  return {
    async infer(_req: OracleRequest, _ctx: ExecContext): Promise<OracleResponse> {
      const entry = takeEntry(entries, index, "oracle calls");
      if (entry.response === undefined) {
        throw new Error("Replay log entry missing oracle response");
      }
      return entry.response as OracleResponse;
    },
  };
}

/**
 * Create replay clock that returns logged times.
 */
export function replayClock(log: ReplayLog): ClockPort {
  const entries = log.getByTag("E_ClockRead");
  const index = { value: 0 };

  return {
    nowMs(_ctx: ExecContext): number {
      const entry = takeEntry(entries, index, "clock reads");
      if (entry.value === undefined) {
        throw new Error("Replay log entry missing clock value");
      }
      return entry.value as number;
    },
    async sleepMs(_ms: number, _ctx: ExecContext): Promise<void> {
      // No-op in replay mode
    },
  };
}

/**
 * Create replay RNG that returns logged values.
 */
export function replayRng(log: ReplayLog): RngPort {
  const entries = log.getByTag("E_RngRead");
  const index = { value: 0 };

  const take = (): number => {
    const entry = takeEntry(entries, index, "RNG reads");
    if (entry.value === undefined) {
      throw new Error("Replay log entry missing RNG value");
    }
    return entry.value as number;
  };

  return {
    nextU32(_ctx: ExecContext): number {
      return take();
    },
    nextFloat(ctx: ExecContext): number {
      const value = take();
      return value / 0xFFFFFFFF;
    },
    nextInt(min: number, max: number, _ctx: ExecContext): number {
      if (max <= min) {
        throw new Error("max must be greater than min");
      }
      return min + (take() % (max - min));
    },
  };
}
