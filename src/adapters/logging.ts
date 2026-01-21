import type { ClockPort } from "../ports/clock";
import type { OraclePort, OracleRequest, OracleResponse } from "../ports/oracle";
import type { RngPort } from "../ports/rng";
import type { ToolPort, ToolCall, ToolResult } from "../ports/tool";
import type { ExecContext } from "../ports/types";

function makeId(kind: string): string {
  const random = Math.random().toString(36).slice(2);
  return `${kind}:${Date.now()}:${random}`;
}

/**
 * Wrap oracle port with logging.
 */
export function loggingOracle(inner: OraclePort): OraclePort {
  return {
    async infer(req: OracleRequest, ctx: ExecContext): Promise<OracleResponse> {
      const id = makeId("oracle");
      const start = Date.now();
      try {
        const res = await inner.infer(req, ctx);
        ctx.trace.emit({
          tag: "E_OracleCall",
          id,
          durationMs: Date.now() - start,
        });
        return res;
      } catch (error) {
        ctx.trace.emit({
          tag: "E_OracleCall",
          id,
          durationMs: Date.now() - start,
        });
        throw error;
      }
    },
  };
}

/**
 * Wrap tool port with logging.
 */
export function loggingTool(inner: ToolPort): ToolPort {
  return {
    async call(call: ToolCall, ctx: ExecContext): Promise<ToolResult> {
      const id = makeId("tool");
      const start = Date.now();
      const res = await inner.call(call, ctx);
      ctx.trace.emit({
        tag: "E_ToolCall",
        id,
        tool: call.name,
        durationMs: Date.now() - start,
      });
      return res;
    },
  };
}

/**
 * Wrap clock port with logging.
 */
export function loggingClock(inner: ClockPort): ClockPort {
  return {
    nowMs(ctx: ExecContext): number {
      const value = inner.nowMs(ctx);
      const id = makeId("clock");
      ctx.trace.emit({ tag: "E_ClockRead", id, valueMs: value });
      return value;
    },
    async sleepMs(ms: number, ctx: ExecContext): Promise<void> {
      return inner.sleepMs(ms, ctx);
    },
  };
}

/**
 * Wrap RNG port with logging.
 */
export function loggingRng(inner: RngPort): RngPort {
  return {
    nextU32(ctx: ExecContext): number {
      const value = inner.nextU32(ctx);
      const id = makeId("rng");
      ctx.trace.emit({ tag: "E_RngRead", id, value });
      return value;
    },
    nextFloat(ctx: ExecContext): number {
      return inner.nextFloat(ctx);
    },
    nextInt(min: number, max: number, ctx: ExecContext): number {
      return inner.nextInt(min, max, ctx);
    },
  };
}
