import type { ExecContext } from "./types";

/**
 * Clock port interface.
 * MUST be used for all time access to enable deterministic replay.
 */
export interface ClockPort {
  /**
   * Get current time in milliseconds.
   * In replay mode, returns logged time.
   */
  nowMs(ctx: ExecContext): number;

  /**
   * Sleep for specified duration.
   * In replay mode, may be no-op or use logged time.
   */
  sleepMs(ms: number, ctx: ExecContext): Promise<void>;
}
