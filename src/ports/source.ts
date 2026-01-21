import type { ExecContext } from "./types";

/**
 * Source port interface.
 * For observing from input streams.
 */
export interface SourcePort {
  /**
   * Observe from source.
   * @param query - Optional query/filter
   * @returns Observed value
   */
  observe(sourceId: string, query: unknown | undefined, ctx: ExecContext): Promise<unknown>;
}
