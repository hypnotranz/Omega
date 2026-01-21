import type { ExecContext } from "./types";

/**
 * Sink port interface.
 * For emitting items to output streams.
 */
export interface SinkPort {
  /**
   * Emit item to sink.
   */
  emit(sinkId: string, item: unknown, ctx: ExecContext): Promise<void>;
}
