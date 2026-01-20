// src/core/compiler/vm.ts
// SOURCE: ARCHITECTURE/32-LANGUAGE-OFFICIAL-13-IMPLEMENTATION-17.md
// Prompt 17: Bytecode VM execution with effect preservation

import type { Val } from "../eval/values";
import { VUnit, VTrue, VFalse } from "../eval/values";
import { sha256JSON } from "../artifacts/hash";
import type {
  BytecodeProgram,
  BytecodeFunction,
  Instr,
  VMState,
  VMFrame,
  VMConfig,
  DEFAULT_VM_CONFIG,
  EffectTraceEntry,
  SourceLocation,
} from "./types";

// ─────────────────────────────────────────────────────────────────
// VM Configuration
// ─────────────────────────────────────────────────────────────────

/**
 * Default VM configuration with conservative limits.
 */
export const defaultVMConfig: VMConfig = {
  maxOperations: 50,
  maxStackDepth: 1000,
  maxCallDepth: 100,
  stepping: false,
  breakpoints: new Set(),
};

// ─────────────────────────────────────────────────────────────────
// Effect Handler Interface
// ─────────────────────────────────────────────────────────────────

/**
 * Effect handler function signature.
 */
export type EffectHandler = (
  op: string,
  args: Val[],
  resume: (result: Val) => Val
) => Val;

/**
 * Effect handler registry.
 */
export type EffectHandlerRegistry = Map<string, EffectHandler>;

/**
 * Create default effect handlers.
 */
export function createDefaultHandlers(): EffectHandlerRegistry {
  const handlers = new Map<string, EffectHandler>();

  // Default infer.op handler (pass through)
  handlers.set("infer.op", (_op, _args, resume) => {
    return resume(VUnit);
  });

  // Default amb.choose handler (take first)
  handlers.set("amb.choose", (_op, args, resume) => {
    if (args.length > 0 && args[0].tag === "Vector" && args[0].items.length > 0) {
      return resume(args[0].items[0]);
    }
    return resume(VUnit);
  });

  return handlers;
}

// ─────────────────────────────────────────────────────────────────
// VM State Management
// ─────────────────────────────────────────────────────────────────

/**
 * Create initial VM state.
 */
export function createVMState(
  program: BytecodeProgram,
  args: Val[] = [],
  globals: Map<string, Val> = new Map(),
  config: VMConfig = defaultVMConfig
): VMState {
  // Initialize globals with free variables
  const globalEnv = new Map(globals);
  for (const freeVar of program.freeVars) {
    if (!globalEnv.has(freeVar)) {
      globalEnv.set(freeVar, VUnit);
    }
  }

  // Create initial frame for entry function
  const entryFn = program.functions[program.entryFn];
  const initialFrame: VMFrame = {
    fnId: program.entryFn,
    ip: 0,
    locals: new Array(entryFn.localCount).fill(VUnit),
  };

  // Set arguments in initial frame
  for (let i = 0; i < args.length && i < initialFrame.locals.length; i++) {
    initialFrame.locals[i] = args[i];
  }

  return {
    stack: [],
    frames: [initialFrame],
    currentFrame: 0,
    handlerStack: [],
    globals: globalEnv,
    effectTrace: [],
    oracleCallCount: 0,
    status: "running",
    fuel: config.maxOperations,
  };
}

/**
 * Get current frame.
 */
function currentFrame(state: VMState): VMFrame {
  return state.frames[state.currentFrame];
}

/**
 * Get current instruction.
 */
function currentInstr(state: VMState, program: BytecodeProgram): Instr | null {
  const frame = currentFrame(state);
  const fn = program.functions[frame.fnId];
  if (frame.ip >= fn.code.length) {
    return null;
  }
  return fn.code[frame.ip];
}

/**
 * Push a value onto the stack.
 */
function push(state: VMState, val: Val): void {
  state.stack.push(val);
}

/**
 * Pop a value from the stack.
 */
function pop(state: VMState): Val {
  if (state.stack.length === 0) {
    throw new Error("VM stack underflow");
  }
  return state.stack.pop()!;
}

/**
 * Peek at top of stack.
 */
function peek(state: VMState): Val {
  if (state.stack.length === 0) {
    throw new Error("VM stack underflow");
  }
  return state.stack[state.stack.length - 1];
}

// ─────────────────────────────────────────────────────────────────
// Primitive Operations
// ─────────────────────────────────────────────────────────────────

/**
 * Execute a primitive operation.
 */
function executePrim(name: string, args: Val[]): Val {
  switch (name) {
    case "+": {
      if (args.length >= 2 && args[0].tag === "Num" && args[1].tag === "Num") {
        return { tag: "Num", n: args[0].n + args[1].n };
      }
      return VUnit;
    }
    case "-": {
      if (args.length >= 2 && args[0].tag === "Num" && args[1].tag === "Num") {
        return { tag: "Num", n: args[0].n - args[1].n };
      }
      return VUnit;
    }
    case "*": {
      if (args.length >= 2 && args[0].tag === "Num" && args[1].tag === "Num") {
        return { tag: "Num", n: args[0].n * args[1].n };
      }
      return VUnit;
    }
    case "/": {
      if (args.length >= 2 && args[0].tag === "Num" && args[1].tag === "Num" && args[1].n !== 0) {
        return { tag: "Num", n: args[0].n / args[1].n };
      }
      return VUnit;
    }
    case "=":
    case "eq?": {
      if (args.length >= 2) {
        return valEqual(args[0], args[1]) ? VTrue : VFalse;
      }
      return VFalse;
    }
    case "<": {
      if (args.length >= 2 && args[0].tag === "Num" && args[1].tag === "Num") {
        return args[0].n < args[1].n ? VTrue : VFalse;
      }
      return VFalse;
    }
    case ">": {
      if (args.length >= 2 && args[0].tag === "Num" && args[1].tag === "Num") {
        return args[0].n > args[1].n ? VTrue : VFalse;
      }
      return VFalse;
    }
    case "<=": {
      if (args.length >= 2 && args[0].tag === "Num" && args[1].tag === "Num") {
        return args[0].n <= args[1].n ? VTrue : VFalse;
      }
      return VFalse;
    }
    case ">=": {
      if (args.length >= 2 && args[0].tag === "Num" && args[1].tag === "Num") {
        return args[0].n >= args[1].n ? VTrue : VFalse;
      }
      return VFalse;
    }
    case "not": {
      if (args.length >= 1) {
        return isTruthy(args[0]) ? VFalse : VTrue;
      }
      return VTrue;
    }
    case "cons": {
      if (args.length >= 2) {
        return { tag: "Pair", car: args[0], cdr: args[1] };
      }
      return VUnit;
    }
    case "car": {
      if (args.length >= 1 && args[0].tag === "Pair") {
        return args[0].car;
      }
      return VUnit;
    }
    case "cdr": {
      if (args.length >= 1 && args[0].tag === "Pair") {
        return args[0].cdr;
      }
      return VUnit;
    }
    case "null?": {
      if (args.length >= 1) {
        return args[0].tag === "Unit" ? VTrue : VFalse;
      }
      return VTrue;
    }
    case "pair?": {
      if (args.length >= 1) {
        return args[0].tag === "Pair" ? VTrue : VFalse;
      }
      return VFalse;
    }
    case "identity": {
      return args.length >= 1 ? args[0] : VUnit;
    }
    case "undefined": {
      return VUnit;
    }
    case "string-append": {
      let result = "";
      for (const arg of args) {
        if (arg.tag === "Str") {
          result += arg.s;
        }
      }
      return { tag: "Str", s: result };
    }
    case "string-length": {
      if (args.length >= 1 && args[0].tag === "Str") {
        return { tag: "Num", n: args[0].s.length };
      }
      return { tag: "Num", n: 0 };
    }
    case "vector": {
      return { tag: "Vector", items: args };
    }
    case "vector-ref": {
      if (args.length >= 2 && args[0].tag === "Vector" && args[1].tag === "Num") {
        const idx = args[1].n;
        if (idx >= 0 && idx < args[0].items.length) {
          return args[0].items[idx];
        }
      }
      return VUnit;
    }
    case "vector-length": {
      if (args.length >= 1 && args[0].tag === "Vector") {
        return { tag: "Num", n: args[0].items.length };
      }
      return { tag: "Num", n: 0 };
    }
    default:
      return VUnit;
  }
}

/**
 * Check if two values are equal.
 */
function valEqual(a: Val, b: Val): boolean {
  if (a.tag !== b.tag) return false;

  switch (a.tag) {
    case "Unit":
    case "Uninit":
      return true;
    case "Num":
      return a.n === (b as { tag: "Num"; n: number }).n;
    case "Bool":
      return a.b === (b as { tag: "Bool"; b: boolean }).b;
    case "Str":
      return a.s === (b as { tag: "Str"; s: string }).s;
    case "Sym":
      return a.name === (b as { tag: "Sym"; name: string }).name;
    case "Pair": {
      const bPair = b as { tag: "Pair"; car: Val; cdr: Val };
      return valEqual(a.car, bPair.car) && valEqual(a.cdr, bPair.cdr);
    }
    case "Vector": {
      const bVec = b as { tag: "Vector"; items: Val[] };
      if (a.items.length !== bVec.items.length) return false;
      return a.items.every((item, i) => valEqual(item, bVec.items[i]));
    }
    default:
      // For complex types, use JSON comparison
      return JSON.stringify(a) === JSON.stringify(b);
  }
}

/**
 * Check if a value is truthy.
 */
function isTruthy(val: Val): boolean {
  if (val.tag === "Bool") return val.b;
  if (val.tag === "Unit") return false;
  return true;
}

// ─────────────────────────────────────────────────────────────────
// VM Execution
// ─────────────────────────────────────────────────────────────────

/**
 * Execute a single instruction.
 */
export function step(
  state: VMState,
  program: BytecodeProgram,
  handlers: EffectHandlerRegistry = createDefaultHandlers()
): VMState {
  if (state.status !== "running") {
    return state;
  }

  if (state.fuel <= 0) {
    state.status = "error";
    state.error = "Fuel exhausted (operation limit reached)";
    return state;
  }

  const instr = currentInstr(state, program);
  if (instr === null) {
    state.status = "completed";
    return state;
  }

  const frame = currentFrame(state);
  const fn = program.functions[frame.fnId];

  // Decrement fuel
  state.fuel--;

  try {
    switch (instr.op) {
      case "CONST": {
        const val = program.constants[instr.k];
        push(state, val);
        frame.ip++;
        break;
      }

      case "LOAD": {
        const val = frame.locals[instr.slot];
        push(state, val);
        frame.ip++;
        break;
      }

      case "STORE": {
        const val = pop(state);
        frame.locals[instr.slot] = val;
        frame.ip++;
        break;
      }

      case "GLOAD": {
        const val = state.globals.get(instr.name) ?? VUnit;
        push(state, val);
        frame.ip++;
        break;
      }

      case "GSTORE": {
        const val = pop(state);
        state.globals.set(instr.name, val);
        frame.ip++;
        break;
      }

      case "CLOSURE": {
        // Create a closure value referencing the function
        const closureVal: Val = {
          tag: "Closure",
          params: [],
          body: { tag: "Lit", value: null }, // Placeholder
          env: {} as any,
        };
        // Store fnId in a way we can recover it
        (closureVal as any).vmFnId = instr.fnId;
        (closureVal as any).captured = instr.captured.map(slot => frame.locals[slot]);
        push(state, closureVal);
        frame.ip++;
        break;
      }

      case "CALL": {
        // Pop arguments in reverse order
        const args: Val[] = [];
        for (let i = 0; i < instr.argc; i++) {
          args.unshift(pop(state));
        }

        // Pop function
        const fnVal = pop(state);

        // Check if it's a VM closure
        if ((fnVal as any).vmFnId !== undefined) {
          const targetFnId = (fnVal as any).vmFnId;
          const targetFn = program.functions[targetFnId];

          // Create new frame
          const newFrame: VMFrame = {
            fnId: targetFnId,
            ip: 0,
            locals: new Array(targetFn.localCount).fill(VUnit),
            returnAddr: { fnId: frame.fnId, ip: frame.ip + 1 },
          };

          // Copy arguments to locals
          for (let i = 0; i < args.length && i < newFrame.locals.length; i++) {
            newFrame.locals[i] = args[i];
          }

          // Copy captured values
          const captured = (fnVal as any).captured ?? [];
          for (let i = 0; i < captured.length; i++) {
            if (args.length + i < newFrame.locals.length) {
              newFrame.locals[args.length + i] = captured[i];
            }
          }

          state.frames.push(newFrame);
          state.currentFrame = state.frames.length - 1;
        } else {
          // Native or external function - just push unit
          push(state, VUnit);
          frame.ip++;
        }
        break;
      }

      case "TAILCALL": {
        // Similar to CALL but reuse current frame
        const args: Val[] = [];
        for (let i = 0; i < instr.argc; i++) {
          args.unshift(pop(state));
        }

        const fnVal = pop(state);

        if ((fnVal as any).vmFnId !== undefined) {
          const targetFnId = (fnVal as any).vmFnId;
          const targetFn = program.functions[targetFnId];

          // Reuse frame
          frame.fnId = targetFnId;
          frame.ip = 0;
          frame.locals = new Array(targetFn.localCount).fill(VUnit);

          for (let i = 0; i < args.length && i < frame.locals.length; i++) {
            frame.locals[i] = args[i];
          }
        } else {
          push(state, VUnit);
          frame.ip++;
        }
        break;
      }

      case "RET": {
        const retVal = state.stack.length > 0 ? pop(state) : VUnit;

        if (state.currentFrame > 0) {
          // Return to caller
          const returnAddr = frame.returnAddr;
          state.frames.pop();
          state.currentFrame--;

          push(state, retVal);

          if (returnAddr) {
            currentFrame(state).ip = returnAddr.ip;
          }
        } else {
          // Top-level return
          push(state, retVal);
          state.status = "completed";
        }
        break;
      }

      case "POP": {
        pop(state);
        frame.ip++;
        break;
      }

      case "DUP": {
        const val = peek(state);
        push(state, val);
        frame.ip++;
        break;
      }

      case "JMP": {
        frame.ip = instr.label;
        break;
      }

      case "JMPIF": {
        const test = pop(state);
        if (isTruthy(test)) {
          frame.ip = instr.labelTrue;
        } else {
          frame.ip = instr.labelFalse;
        }
        break;
      }

      case "EFFECT": {
        // Effects are preserved! Pop args and invoke handler
        const args: Val[] = [];
        for (let i = 0; i < instr.argc; i++) {
          args.unshift(pop(state));
        }

        // Record effect in trace
        const traceEntry: EffectTraceEntry = {
          op: instr.opName,
          argsDigest: sha256JSON(args),
          seq: state.effectTrace.length,
        };

        // Track oracle calls
        if (instr.opName === "infer.op") {
          state.oracleCallCount++;
        }

        // Find and invoke handler
        const handler = handlers.get(instr.opName);
        if (handler) {
          // Simple synchronous resume - real impl would be more complex
          const result = handler(instr.opName, args, (r) => r);
          traceEntry.result = result;
          push(state, result);
        } else {
          // No handler - push unit
          push(state, VUnit);
        }

        state.effectTrace.push(traceEntry);
        frame.ip++;
        break;
      }

      case "HANDLE": {
        // Push handler onto handler stack
        state.handlerStack.push({
          handlerId: instr.handlerId,
          frameIndex: state.currentFrame,
        });
        frame.ip++;
        break;
      }

      case "UNHANDLE": {
        // Pop handler from handler stack
        if (state.handlerStack.length > 0) {
          state.handlerStack.pop();
        }
        frame.ip++;
        break;
      }

      case "PRIM": {
        const args: Val[] = [];
        for (let i = 0; i < instr.argc; i++) {
          args.unshift(pop(state));
        }
        const result = executePrim(instr.name, args);
        push(state, result);
        frame.ip++;
        break;
      }

      case "MATCH": {
        // Simplified - just jump to first clause
        if (instr.clauseLabels.length > 0) {
          frame.ip = instr.clauseLabels[0];
        } else {
          frame.ip++;
        }
        break;
      }

      case "FAIL": {
        const reason = program.constants[instr.reasonK];
        state.status = "error";
        state.error = `Fail: ${JSON.stringify(reason)}`;
        break;
      }

      case "NOP": {
        frame.ip++;
        break;
      }

      case "DEBUG": {
        // Debugging instruction - pause if stepping
        frame.ip++;
        break;
      }
    }
  } catch (e) {
    state.status = "error";
    state.error = e instanceof Error ? e.message : String(e);
  }

  return state;
}

/**
 * Run VM until completion or fuel exhaustion.
 */
export function run(
  program: BytecodeProgram,
  args: Val[] = [],
  globals: Map<string, Val> = new Map(),
  config: VMConfig = defaultVMConfig,
  handlers: EffectHandlerRegistry = createDefaultHandlers()
): VMState {
  let state = createVMState(program, args, globals, config);

  while (state.status === "running") {
    state = step(state, program, handlers);
  }

  return state;
}

/**
 * Get the result value from completed VM state.
 */
export function getResult(state: VMState): Val {
  if (state.stack.length > 0) {
    return state.stack[state.stack.length - 1];
  }
  return VUnit;
}

/**
 * Get source location for current instruction.
 */
export function getSourceLocation(
  state: VMState,
  program: BytecodeProgram
): SourceLocation | undefined {
  const frame = currentFrame(state);
  const fn = program.functions[frame.fnId];

  if (fn.sourceMap) {
    const entry = fn.sourceMap.entries.find(e => e.irPos === frame.ip);
    if (entry) {
      return entry.loc;
    }
  }

  return undefined;
}

/**
 * Get call stack for debugging.
 */
export function getCallStack(state: VMState, program: BytecodeProgram): string[] {
  const stack: string[] = [];

  for (const frame of state.frames) {
    const fn = program.functions[frame.fnId];
    const label = fn.label ?? `fn${fn.id}`;
    stack.push(`${label}:${frame.ip}`);
  }

  return stack;
}

/**
 * Set a breakpoint.
 */
export function setBreakpoint(state: VMState, ip: number): void {
  // Would need to track breakpoints in config
}

/**
 * Check if VM is at a breakpoint.
 */
export function atBreakpoint(state: VMState, config: VMConfig): boolean {
  const frame = currentFrame(state);
  return config.breakpoints.has(frame.ip);
}
