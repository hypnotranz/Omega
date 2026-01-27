/**
 * State Serializer - Convert CESK State to JSON for web transport
 *
 * This module transforms the internal TypeScript State object into
 * the JSON-serializable types defined in debugService.ts.
 */

import type { State, Frame, HandlerFrame, Control, StepOutcome } from '../core/eval/machine';
import type { Val } from '../core/eval/values';
import type { Env } from '../core/eval/env';
import type { Expr } from '../core/ast';
import type {
  MachineSnapshot,
  SerializedControl,
  SerializedBinding,
  SerializedValue,
  SerializedFrame,
  SerializedHandler,
  SerializedStoreEntry,
  StepResult,
} from './debugService';

// ============================================================
// VALUE SERIALIZATION
// ============================================================

/**
 * Serialize a Val to JSON-transportable format
 */
export function serializeValue(v: Val, maxDepth = 3): SerializedValue {
  const base: SerializedValue = {
    tag: v.tag,
    summary: valueSummary(v),
  };

  if (maxDepth <= 0) {
    base.summary = `${v.tag}...`;
    return base;
  }

  switch (v.tag) {
    case 'Unit':
      base.summary = '()';
      break;

    case 'Num':
      base.summary = String(v.n);
      base.details = v.n;
      break;

    case 'Int':
      base.summary = `${v.value}n`;
      base.details = String(v.value);
      break;

    case 'Bool':
      base.summary = v.b ? '#t' : '#f';
      base.details = v.b;
      break;

    case 'Str':
      base.summary = v.s.length > 50 ? `"${v.s.slice(0, 47)}..."` : `"${v.s}"`;
      base.details = v.s;
      break;

    case 'Sym':
      base.summary = `'${v.name}`;
      base.details = v.name;
      break;

    case 'Vector':
      base.length = v.items.length;
      if (isConsList(v)) {
        const items = consToArray(v);
        base.summary = `(${items.slice(0, 3).map(i => valueSummary(i)).join(' ')}${items.length > 3 ? ' ...' : ''})`;
        base.details = items.map(i => serializeValue(i, maxDepth - 1));
        base.tag = 'List';
      } else {
        base.summary = `#(${v.items.slice(0, 3).map(i => valueSummary(i)).join(' ')}${v.items.length > 3 ? ' ...' : ''})`;
        base.details = v.items.map(i => serializeValue(i, maxDepth - 1));
      }
      break;

    case 'Pair':
      base.summary = `(${valueSummary(v.car)} . ${valueSummary(v.cdr)})`;
      base.details = {
        car: serializeValue(v.car, maxDepth - 1),
        cdr: serializeValue(v.cdr, maxDepth - 1),
      };
      break;

    case 'List':
      base.length = v.elements.length;
      base.summary = `(${v.elements.slice(0, 3).map(i => valueSummary(i)).join(' ')}${v.elements.length > 3 ? ' ...' : ''})`;
      base.details = v.elements.map(i => serializeValue(i, maxDepth - 1));
      break;

    case 'Map':
      base.length = v.entries.length;
      base.summary = `{map ${v.entries.length} entries}`;
      base.details = Object.fromEntries(
        v.entries.slice(0, 10).map(([k, val]) => [valueSummary(k), serializeValue(val, maxDepth - 1)])
      );
      break;

    case 'Closure':
      base.summary = `<closure ${v.params.join(' ')}>`;
      base.closure = {
        params: v.params,
        bodyPreview: exprSummary(v.body),
      };
      break;

    case 'Native':
      base.summary = `<native:${v.name}>`;
      break;

    case 'Cont':
    case 'Continuation':
      base.summary = `<continuation>`;
      break;

    case 'OracleProc':
      base.summary = `<oracle-proc>`;
      break;

    case 'Machine':
      base.summary = `<machine:${v.machineId}>`;
      base.details = { machineId: v.machineId, stepCount: v.stepCount, isDone: v.isDone };
      break;

    case 'Fiber':
      base.summary = `<fiber:${v.id}>`;
      break;

    case 'Profile':
      base.summary = `<profile:${v.profileId}>`;
      break;

    case 'Err':
      base.summary = `<error: ${v.message}>`;
      base.details = v.message;
      break;

    case 'Tagged':
      base.summary = `<${v.typeTag}: ${valueSummary(v.payload)}>`;
      base.details = { typeTag: v.typeTag, payload: serializeValue(v.payload, maxDepth - 1) };
      break;

    default:
      base.summary = `<${v.tag}>`;
  }

  return base;
}

/**
 * Quick summary of a value (single line)
 */
function valueSummary(v: Val): string {
  switch (v.tag) {
    case 'Unit': return '()';
    case 'Num': return String(v.n);
    case 'Int': return `${v.value}n`;
    case 'Bool': return v.b ? '#t' : '#f';
    case 'Str': return v.s.length > 20 ? `"${v.s.slice(0, 17)}..."` : `"${v.s}"`;
    case 'Sym': return `'${v.name}`;
    case 'Vector': return isConsList(v) ? `(list ${v.items.length > 0 ? '...' : ''})` : `#(...)`;
    case 'List': return `(${v.elements.length} items)`;
    case 'Pair': return `(pair)`;
    case 'Map': return `{map}`;
    case 'Closure': return `<λ>`;
    case 'Native': return `<native:${v.name}>`;
    case 'Cont':
    case 'Continuation': return `<k>`;
    case 'Err': return `<err>`;
    default: return `<${v.tag}>`;
  }
}

/**
 * Check if a Vector is a cons-cell list
 */
function isConsList(v: Val): boolean {
  if (v.tag !== 'Vector') return false;
  if (v.items.length !== 2) return false;
  const tail = v.items[1];
  return tail.tag === 'Unit' || isConsList(tail);
}

/**
 * Convert cons-cell list to array
 */
function consToArray(v: Val): Val[] {
  const result: Val[] = [];
  let cur = v;
  while (cur.tag === 'Vector' && cur.items.length === 2) {
    result.push(cur.items[0]);
    cur = cur.items[1];
  }
  return result;
}

// ============================================================
// EXPRESSION SERIALIZATION
// ============================================================

/**
 * Quick summary of an expression
 */
function exprSummary(e: Expr): string {
  switch (e.tag) {
    case 'Lit':
      if (e.value === null) return '()';
      if (typeof e.value === 'string') return `"${e.value.slice(0, 20)}"`;
      return String(e.value);
    case 'Var': return e.name;
    case 'If': return `(if ${exprSummary(e.test)} ...)`;
    case 'Begin': return `(begin ...)`;
    case 'Define': return `(define ${e.name} ...)`;
    case 'Set': return `(set! ${e.name} ...)`;
    case 'Lambda': return `(lambda (${e.params.join(' ')}) ...)`;
    case 'App': return `(${exprSummary(e.fn)} ...)`;
    case 'Effect': return `(effect "${e.op}" ...)`;
    case 'Handle': return `(handle ...)`;
    case 'Quote': return `'...`;
    case 'Match': return `(match ...)`;
    case 'OracleLambda': return `(oracle-lambda ...)`;
    default: return `<${(e as any).tag}>`;
  }
}

// ============================================================
// CONTROL SERIALIZATION
// ============================================================

export function serializeControl(ctrl: Control): SerializedControl {
  if (ctrl.tag === 'Expr') {
    // Note: Expr type doesn't have source property - omit it
    return {
      tag: 'Expr',
      exprType: ctrl.e.tag,
      exprSummary: exprSummary(ctrl.e),
    };
  } else {
    return {
      tag: 'Val',
      valType: ctrl.v.tag,
      valSummary: valueSummary(ctrl.v),
    };
  }
}

// ============================================================
// FRAME SERIALIZATION
// ============================================================

export function serializeFrame(frame: Frame, index: number): SerializedFrame {
  const base: SerializedFrame = {
    index,
    tag: frame.tag,
    description: frameDescription(frame),
    waiting: frameWaiting(frame),
  };

  // Extract bindings if frame has an env
  if ('env' in frame && frame.env) {
    base.bindings = extractBindings(frame.env, 5);
  }

  return base;
}

function frameDescription(frame: Frame): string {
  switch (frame.tag) {
    case 'KIf': return 'if branch';
    case 'KBegin': return `begin (${frame.rest.length} remaining)`;
    case 'KDefine': return `define ${frame.name}`;
    case 'KSet': return `set! ${frame.name}`;
    case 'KAppFun': return `apply function (${frame.args.length} args)`;
    case 'KAppArg': return `apply args (${frame.pending.length} pending, ${frame.acc.length} done)`;
    case 'KAppArgLazy': return `apply args lazy`;
    case 'KCall': return 'function call return';
    case 'KEffect': return `effect "${frame.op}"`;
    case 'KHandleBoundary': return `handle boundary`;
    case 'KHandleReturn': return `handle return (${frame.mode})`;
    case 'KPrompt': return 'prompt/control';
    case 'KMatch': return `match (${frame.clauses.length} clauses)`;
    case 'KOracleLambda': return 'oracle-lambda';
    case 'KBind': return 'bind (>>=)';
    case 'KHandlerBind': return `handler-bind (${frame.handlers.length} handlers)`;
    case 'KRestartBind': return `restart-case (${frame.restarts.length} restarts)`;
    case 'KSignaling': return `signaling condition`;
    default: return (frame as any).tag;
  }
}

function frameWaiting(frame: Frame): string {
  switch (frame.tag) {
    case 'KIf': return 'condition value';
    case 'KBegin': return 'current expression';
    case 'KDefine': return 'value to bind';
    case 'KSet': return 'new value';
    case 'KAppFun': return 'function value';
    case 'KAppArg': return 'next argument';
    case 'KAppArgLazy': return 'next argument (lazy)';
    case 'KCall': return 'function body result';
    case 'KEffect': return 'effect arguments';
    case 'KHandleBoundary': return 'handled body result';
    case 'KHandleReturn': return 'resume value';
    case 'KPrompt': return 'prompt body result';
    case 'KMatch': return 'value to match';
    case 'KOracleLambda': return 'oracle spec';
    case 'KBind': return 'monad value';
    case 'KHandlerBind': return 'body result';
    case 'KRestartBind': return 'body result';
    case 'KSignaling': return 'handler response';
    default: return 'unknown';
  }
}

// ============================================================
// ENVIRONMENT SERIALIZATION
// ============================================================

/**
 * Extract visible bindings from environment
 */
export function extractBindings(env: Env, maxPerScope = 10): SerializedBinding[] {
  const bindings: SerializedBinding[] = [];
  let depth = 0;
  let currentEnv: Env | null = env;

  while (currentEnv && depth < 10) {
    // Env is a Ctx which has bindings in its chain
    const scopeBindings = getEnvBindings(currentEnv);

    for (const [name, val] of scopeBindings.slice(0, maxPerScope)) {
      bindings.push({
        name,
        value: serializeValue(val, 2),
        depth,
      });
    }

    // Move to parent scope
    currentEnv = currentEnv.parent ?? null;
    depth++;
  }

  return bindings;
}

/**
 * Get bindings from a single Env/Ctx scope
 */
function getEnvBindings(env: Env): Array<[string, Val]> {
  // Env is a Ctx with .bindings Map<string, Val>
  if (env.bindings && env.bindings instanceof Map) {
    return Array.from(env.bindings.entries());
  }
  return [];
}

// ============================================================
// HANDLER SERIALIZATION
// ============================================================

export function serializeHandler(handler: HandlerFrame): SerializedHandler {
  return {
    hid: handler.hid,
    operations: Array.from(handler.on.keys()),
    hasReturn: !!handler.ret,
    hasFinally: !!handler.fin,
  };
}

// ============================================================
// FULL STATE SERIALIZATION
// ============================================================

let snapshotCounter = 0;

export function serializeState(
  state: State,
  stepNumber: number,
  status: MachineSnapshot['status'] = 'paused',
  options: {
    includeStore?: boolean;
    maxStoreEntries?: number;
    pendingEffect?: { op: string; args: Val[] };
    result?: Val;
    error?: Error;
  } = {}
): MachineSnapshot {
  const snapshot: MachineSnapshot = {
    snapshotId: `snap_${++snapshotCounter}_${Date.now().toString(36)}`,
    step: stepNumber,
    timestamp: new Date().toISOString(),

    control: serializeControl(state.control),

    environment: extractBindings(state.env, 20),

    callStack: state.kont.map((frame, i) => serializeFrame(frame, i)),

    handlers: state.handlers.map(serializeHandler),

    status,
  };

  // Include store if requested
  if (options.includeStore) {
    snapshot.store = serializeStore(state.store, options.maxStoreEntries ?? 100);
  }

  // Include pending effect
  if (options.pendingEffect) {
    snapshot.pendingEffect = {
      op: options.pendingEffect.op,
      args: options.pendingEffect.args.map(a => serializeValue(a, 2)),
    };
  }

  // Include result
  if (options.result) {
    snapshot.result = serializeValue(options.result, 4);
  }

  // Include error
  if (options.error) {
    snapshot.error = {
      message: options.error.message,
      stack: options.error.stack,
    };
  }

  // Include budget if present
  if (state.budget) {
    snapshot.budget = {
      stepsRemaining: state.budget.stepsLeft,
      inferCallsRemaining: state.budget.inferCallsLeft,
      oracleReqsRemaining: state.budget.oracleReqLeft,
      toolCallsRemaining: state.budget.toolCallsLeft,
    };
  }

  return snapshot;
}

/**
 * Serialize store (heap) entries
 */
function serializeStore(store: any, maxEntries: number): SerializedStoreEntry[] {
  const entries: SerializedStoreEntry[] = [];

  // Store is a COWStore with .data Map<number, Val>
  if (store.data && store.data instanceof Map) {
    let count = 0;
    for (const [addr, val] of store.data.entries()) {
      if (count >= maxEntries) break;
      entries.push({
        address: addr,
        value: serializeValue(val, 2),
      });
      count++;
    }
  }

  return entries;
}

// ============================================================
// STEP OUTCOME SERIALIZATION
// ============================================================

export function serializeStepOutcome(
  outcome: StepOutcome,
  stepNumber: number
): { snapshot: MachineSnapshot; outcomeType: 'stepped' | 'done' | 'effect' } {
  switch (outcome.tag) {
    case 'State':
      return {
        snapshot: serializeState(outcome.state, stepNumber, 'paused'),
        outcomeType: 'stepped',
      };

    case 'Done':
      return {
        snapshot: serializeState(outcome.state, stepNumber, 'done', { result: outcome.value }),
        outcomeType: 'done',
      };

    case 'Op':
      return {
        snapshot: serializeState(outcome.state, stepNumber, 'effect', {
          pendingEffect: { op: outcome.opcall.op, args: outcome.opcall.args },
        }),
        outcomeType: 'effect',
      };
  }
}
