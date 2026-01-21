import type { Span } from "../frameir/meta";

/**
 * Budget state for resource tracking.
 */
export interface BudgetState {
  llmCalls?: { used: number; limit: number };
  tokens?: { used: number; limit: number };
  timeMs?: { used: number; limit: number };
  toolCalls?: { used: number; limit: number };
}

/**
 * Capability tokens for object-capability security.
 */
export interface CapabilitySet {
  /** Allowed oracle models */
  oracleCap?: {
    allowedModels: string[];
    constraints?: Record<string, unknown>;
  };

  /** Allowed tool contracts */
  toolCap?: {
    allowedContracts: Set<string>;
  };

  /** Allowed stores */
  storeCap?: {
    allowedStores: Set<string>;
    readOnly?: boolean;
  };

  /** Allowed sinks */
  sinkCap?: {
    allowedSinks: Set<string>;
  };

  /** Allowed sources */
  sourceCap?: {
    allowedSources: Set<string>;
  };
}

/**
 * Trace event types for replay logging.
 */
export type TraceEvent =
  | { tag: "E_OracleCall"; id: string; durationMs: number }
  | { tag: "E_ToolCall"; id: string; tool: string; durationMs: number }
  | { tag: "E_StoreOp"; id: string; op: "get" | "put"; key: string }
  | { tag: "E_SinkEmit"; id: string; sink: string }
  | { tag: "E_SourceObserve"; id: string; source: string }
  | { tag: "E_ClockRead"; id: string; valueMs: number }
  | { tag: "E_RngRead"; id: string; value: number }
  | { tag: "E_SchedulerDecision"; fiberId: string; choice: number };

/**
 * Trace sink for logging events.
 */
export interface TraceSink {
  emit(event: TraceEvent): void;
}

/**
 * Execution context passed to all port operations.
 */
export interface ExecContext {
  /** Correlation ID for the entire execution */
  runId: string;

  /** Current span for provenance */
  span?: Span;

  /** Remaining budget */
  budget?: BudgetState;

  /** Capability tokens */
  caps: CapabilitySet;

  /** Trace event sink */
  trace: TraceSink;
}

/**
 * Create a child context with updated span.
 */
export function childContext(parent: ExecContext, span: Span): ExecContext {
  return { ...parent, span };
}

/**
 * Create a context with restricted capabilities.
 */
export function restrictCaps(ctx: ExecContext, caps: Partial<CapabilitySet>): ExecContext {
  return {
    ...ctx,
    caps: {
      ...ctx.caps,
      ...caps,
    },
  };
}
