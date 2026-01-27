/**
 * Debug Service - Clean Contract for CESK Machine Inspection
 *
 * This module defines the service layer contract for exposing
 * the CESK machine's internals to external clients (web UI, CLI, tests).
 *
 * The CESK machine is PERFECT for debugging because ALL state is explicit:
 * - C (Control): What expression/value is currently being evaluated
 * - E (Environment): Variable bindings (Ctx chain)
 * - S (Store): Heap allocations (addresses -> values)
 * - K (Kontinuation): Call stack as data structure
 *
 * Plus we have:
 * - Handlers: Effect/exception handler stack
 * - Profile: Governance (capabilities, budgets)
 * - Provenance: Execution audit trail
 */

// ============================================================
// SERVICE TYPES - What the UI receives (JSON-serializable)
// ============================================================

/**
 * Serialized control - what's currently being evaluated
 */
export type SerializedControl =
  | { tag: 'Expr'; exprType: string; exprSummary: string; source?: string }
  | { tag: 'Val'; valType: string; valSummary: string };

/**
 * Serialized environment binding
 */
export interface SerializedBinding {
  name: string;
  value: SerializedValue;
  /** Which scope level (0 = innermost) */
  depth: number;
}

/**
 * Serialized value (recursive structure)
 */
export interface SerializedValue {
  tag: string;
  summary: string;
  /** Detailed representation for expandable view */
  details?: unknown;
  /** For closures - captured environment */
  closure?: { params: string[]; bodyPreview: string };
  /** For vectors/lists - item count */
  length?: number;
  /** Store address if allocated */
  address?: number;
}

/**
 * Serialized stack frame (continuation)
 */
export interface SerializedFrame {
  /** Frame index (0 = bottom of stack) */
  index: number;
  /** Frame type (KIf, KAppFun, KCall, etc.) */
  tag: string;
  /** Human-readable description */
  description: string;
  /** What we're waiting for */
  waiting: string;
  /** Associated environment bindings (if any) */
  bindings?: SerializedBinding[];
  /** Source location if available */
  source?: { file?: string; line?: number; column?: number };
}

/**
 * Serialized handler frame
 */
export interface SerializedHandler {
  /** Handler ID */
  hid: string;
  /** Operations this handler catches */
  operations: string[];
  /** Has return clause? */
  hasReturn: boolean;
  /** Has finally clause? */
  hasFinally: boolean;
}

/**
 * Store entry (heap allocation)
 */
export interface SerializedStoreEntry {
  address: number;
  value: SerializedValue;
  /** Reference count (if tracked) */
  refCount?: number;
}

/**
 * Complete machine state snapshot
 */
export interface MachineSnapshot {
  /** Unique snapshot ID */
  snapshotId: string;
  /** Step number */
  step: number;
  /** Timestamp */
  timestamp: string;

  /** Current control */
  control: SerializedControl;

  /** Environment (visible bindings) */
  environment: SerializedBinding[];

  /** Call stack (continuation frames) */
  callStack: SerializedFrame[];

  /** Effect handlers */
  handlers: SerializedHandler[];

  /** Store (heap) - optional, can be large */
  store?: SerializedStoreEntry[];

  /** Execution status */
  status: 'running' | 'paused' | 'done' | 'error' | 'effect';

  /** If status is 'effect', what effect is pending */
  pendingEffect?: {
    op: string;
    args: SerializedValue[];
  };

  /** If status is 'done', the final value */
  result?: SerializedValue;

  /** If status is 'error', the error */
  error?: { message: string; stack?: string };

  /** Budget remaining (if governance enabled) */
  budget?: {
    stepsRemaining?: number;
    inferCallsRemaining?: number;
    oracleReqsRemaining?: number;
    toolCallsRemaining?: number;
  };
}

// ============================================================
// SERVICE INTERFACE - What operations are available
// ============================================================

/**
 * Breakpoint configuration
 */
export interface Breakpoint {
  id: string;
  type: 'step' | 'expression' | 'effect' | 'binding';
  /** For expression breakpoints - match on expr type */
  exprType?: string;
  /** For effect breakpoints - match on effect name */
  effectOp?: string;
  /** For binding breakpoints - match on variable name */
  bindingName?: string;
  /** Condition (Lisp expression that must evaluate to true) */
  condition?: string;
  /** Is breakpoint enabled? */
  enabled: boolean;
}

/**
 * Session configuration
 */
export interface SessionConfig {
  /** Session name (optional) */
  name?: string;
  /** Enable step recording */
  recording?: boolean;
  /** Max steps before auto-pause */
  maxSteps?: number;
  /** Oracle adapter to use */
  oracleAdapter?: 'openai' | 'anthropic' | 'mock';
}

/**
 * Step result
 */
export interface StepResult {
  /** New machine snapshot */
  snapshot: MachineSnapshot;
  /** What happened */
  outcome: 'stepped' | 'breakpoint' | 'effect' | 'done' | 'error';
  /** If breakpoint hit, which one */
  breakpointId?: string;
}

/**
 * The Debug Service contract
 */
export interface IDebugService {
  // ─────────────────────────────────────────────────────────────
  // Session Management
  // ─────────────────────────────────────────────────────────────

  /** Create a new debug session */
  createSession(config?: SessionConfig): Promise<string>;

  /** List active sessions */
  listSessions(): Promise<Array<{ id: string; name?: string; step: number; status: string }>>;

  /** Get session info */
  getSession(sessionId: string): Promise<{ id: string; config: SessionConfig; snapshot: MachineSnapshot }>;

  /** Close a session */
  closeSession(sessionId: string): Promise<void>;

  // ─────────────────────────────────────────────────────────────
  // Code Execution
  // ─────────────────────────────────────────────────────────────

  /** Load code into session (compile but don't run) */
  loadCode(sessionId: string, code: string): Promise<{ success: boolean; error?: string }>;

  /** Execute a single step */
  step(sessionId: string): Promise<StepResult>;

  /** Execute N steps */
  stepN(sessionId: string, n: number): Promise<StepResult>;

  /** Continue until breakpoint, effect, or completion */
  continue(sessionId: string): Promise<StepResult>;

  /** Run to completion (with optional step limit) */
  run(sessionId: string, maxSteps?: number): Promise<StepResult>;

  /** Provide value to resume from effect */
  resumeWithValue(sessionId: string, value: unknown): Promise<StepResult>;

  // ─────────────────────────────────────────────────────────────
  // State Inspection
  // ─────────────────────────────────────────────────────────────

  /** Get current machine snapshot */
  getSnapshot(sessionId: string): Promise<MachineSnapshot>;

  /** Get specific environment binding */
  getBinding(sessionId: string, name: string): Promise<SerializedValue | null>;

  /** Get store entry by address */
  getStoreEntry(sessionId: string, address: number): Promise<SerializedStoreEntry | null>;

  /** Evaluate expression in current environment (for watch expressions) */
  evaluate(sessionId: string, expr: string): Promise<{ value: SerializedValue } | { error: string }>;

  /** Get call stack with full details */
  getCallStack(sessionId: string): Promise<SerializedFrame[]>;

  // ─────────────────────────────────────────────────────────────
  // Breakpoints
  // ─────────────────────────────────────────────────────────────

  /** Add a breakpoint */
  addBreakpoint(sessionId: string, bp: Omit<Breakpoint, 'id'>): Promise<string>;

  /** Remove a breakpoint */
  removeBreakpoint(sessionId: string, breakpointId: string): Promise<void>;

  /** List breakpoints */
  listBreakpoints(sessionId: string): Promise<Breakpoint[]>;

  /** Enable/disable breakpoint */
  toggleBreakpoint(sessionId: string, breakpointId: string, enabled: boolean): Promise<void>;

  // ─────────────────────────────────────────────────────────────
  // Time Travel (if session recording enabled)
  // ─────────────────────────────────────────────────────────────

  /** Jump to specific step */
  jumpToStep(sessionId: string, step: number): Promise<MachineSnapshot>;

  /** Get execution history summary */
  getHistory(sessionId: string): Promise<Array<{ step: number; control: string; timestamp: string }>>;

  // ─────────────────────────────────────────────────────────────
  // OPR Integration
  // ─────────────────────────────────────────────────────────────

  /** List available OPR kernels */
  listKernels(): Promise<Array<{ id: string; op: string; description?: string }>>;

  /** Execute OPR kernel directly (outside Lisp) */
  executeKernel(kernelId: string, program: unknown): Promise<{ ok: boolean; result?: unknown; error?: unknown }>;
}

// ============================================================
// WEBSOCKET EVENTS - For real-time updates
// ============================================================

/**
 * Events the server sends to clients
 */
export type ServerEvent =
  | { type: 'snapshot'; snapshot: MachineSnapshot }
  | { type: 'breakpointHit'; breakpointId: string; snapshot: MachineSnapshot }
  | { type: 'effectPending'; effect: { op: string; args: SerializedValue[] }; snapshot: MachineSnapshot }
  | { type: 'done'; result: SerializedValue; snapshot: MachineSnapshot }
  | { type: 'error'; error: { message: string; stack?: string }; snapshot: MachineSnapshot }
  | { type: 'stepProgress'; step: number; total?: number };

/**
 * Commands the client sends to server
 */
export type ClientCommand =
  | { type: 'step' }
  | { type: 'stepN'; n: number }
  | { type: 'continue' }
  | { type: 'run'; maxSteps?: number }
  | { type: 'pause' }
  | { type: 'resume'; value: unknown }
  | { type: 'evaluate'; expr: string }
  | { type: 'addBreakpoint'; breakpoint: Omit<Breakpoint, 'id'> }
  | { type: 'removeBreakpoint'; breakpointId: string }
  | { type: 'jumpToStep'; step: number };
