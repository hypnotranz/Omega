import type { ExecContext } from "./types";

/**
 * RNG port interface.
 * MUST be used for all randomness to enable deterministic replay.
 */
export interface RngPort {
  /**
   * Get next random 32-bit unsigned integer.
   * In replay mode, returns logged value.
   */
  nextU32(ctx: ExecContext): number;

  /**
   * Get random float in [0, 1).
   */
  nextFloat(ctx: ExecContext): number;

  /**
   * Get random integer in [min, max).
   */
  nextInt(min: number, max: number, ctx: ExecContext): number;
}
