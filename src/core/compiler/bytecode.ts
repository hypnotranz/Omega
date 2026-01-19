// src/core/compiler/bytecode.ts
// SOURCE: ARCHITECTURE/32-LANGUAGE-OFFICIAL-13-IMPLEMENTATION-17.md
// Prompt 17: Bytecode generation from ANF

import type { Val } from "../eval/values";
import { VUnit } from "../eval/values";
import type {
  ANFAtom,
  ANFPrim,
  ANFExpr,
  ANFProgram,
  ANFPattern,
  ANFHandler,
  Instr,
  BytecodeFunction,
  BytecodeHandler,
  BytecodeProgram,
  SourceMap,
  SourceMapEntry,
  SourceLocation,
} from "./types";

// ─────────────────────────────────────────────────────────────────
// Bytecode Generation Context
// ─────────────────────────────────────────────────────────────────

/**
 * Context for bytecode generation.
 */
type BytecodeContext = {
  /** Constant pool */
  constants: Val[];
  /** Constant index lookup */
  constantMap: Map<string, number>;
  /** Function pool */
  functions: BytecodeFunction[];
  /** Handler definitions */
  handlers: BytecodeHandler[];
  /** Current function being compiled */
  currentFn: BytecodeFunction | null;
  /** Local variable name to slot mapping */
  locals: Map<string, number>;
  /** Next local slot */
  nextLocal: number;
  /** Label counter */
  labelCounter: number;
  /** Source map entries */
  sourceMapEntries: SourceMapEntry[];
  /** Free variables */
  freeVars: string[];
};

/**
 * Create a fresh bytecode context.
 */
function createBytecodeContext(): BytecodeContext {
  return {
    constants: [],
    constantMap: new Map(),
    functions: [],
    handlers: [],
    currentFn: null,
    locals: new Map(),
    nextLocal: 0,
    labelCounter: 0,
    sourceMapEntries: [],
    freeVars: [],
  };
}

/**
 * Add a constant to the pool, returning its index.
 */
function addConstant(ctx: BytecodeContext, val: Val): number {
  const key = JSON.stringify(val);
  const existing = ctx.constantMap.get(key);
  if (existing !== undefined) {
    return existing;
  }
  const idx = ctx.constants.length;
  ctx.constants.push(val);
  ctx.constantMap.set(key, idx);
  return idx;
}

/**
 * Get or allocate a local slot for a variable.
 */
function getLocalSlot(ctx: BytecodeContext, name: string): number {
  const existing = ctx.locals.get(name);
  if (existing !== undefined) {
    return existing;
  }
  const slot = ctx.nextLocal++;
  ctx.locals.set(name, slot);
  return slot;
}

/**
 * Fresh label number.
 */
function freshLabel(ctx: BytecodeContext): number {
  return ctx.labelCounter++;
}

/**
 * Emit an instruction to the current function.
 */
function emit(ctx: BytecodeContext, instr: Instr): void {
  if (ctx.currentFn) {
    ctx.currentFn.code.push(instr);
  }
}

/**
 * Get current instruction index.
 */
function currentIP(ctx: BytecodeContext): number {
  return ctx.currentFn ? ctx.currentFn.code.length : 0;
}

// ─────────────────────────────────────────────────────────────────
// Atom Compilation
// ─────────────────────────────────────────────────────────────────

/**
 * Compile an ANF atom (pushes value onto stack).
 */
function compileAtom(ctx: BytecodeContext, atom: ANFAtom): void {
  switch (atom.tag) {
    case "Lit": {
      const k = addConstant(ctx, atom.v);
      emit(ctx, { op: "CONST", k });
      break;
    }
    case "Var": {
      const slot = ctx.locals.get(atom.name);
      if (slot !== undefined) {
        emit(ctx, { op: "LOAD", slot });
      } else {
        // Global or free variable
        emit(ctx, { op: "GLOAD", name: atom.name });
      }
      break;
    }
  }
}

// ─────────────────────────────────────────────────────────────────
// Primitive Compilation
// ─────────────────────────────────────────────────────────────────

/**
 * Find free variables in an ANF expression.
 */
function findFreeVarsInExpr(expr: ANFExpr, bound: Set<string>): Set<string> {
  const free = new Set<string>();

  function checkAtom(atom: ANFAtom): void {
    if (atom.tag === "Var" && !bound.has(atom.name)) {
      free.add(atom.name);
    }
  }

  function checkPrim(prim: ANFPrim, innerBound: Set<string>): void {
    switch (prim.tag) {
      case "Call":
        checkAtom(prim.fn);
        prim.args.forEach(checkAtom);
        break;
      case "Effect":
      case "Prim":
        prim.args.forEach(checkAtom);
        break;
      case "Lambda": {
        const lambdaBound = new Set(innerBound);
        prim.params.forEach(p => lambdaBound.add(p));
        for (const fv of findFreeVarsInExpr(prim.body, lambdaBound)) {
          if (!bound.has(fv)) free.add(fv);
        }
        break;
      }
      case "Match":
        checkAtom(prim.scrut);
        // Patterns bind variables - simplified handling
        break;
      case "Handle":
        for (const fv of findFreeVarsInExpr(prim.body, innerBound)) {
          free.add(fv);
        }
        break;
      default:
        break;
    }
  }

  function visit(e: ANFExpr, innerBound: Set<string>): void {
    switch (e.tag) {
      case "Return":
        checkAtom(e.v);
        break;
      case "Let": {
        checkPrim(e.rhs, innerBound);
        const newBound = new Set(innerBound);
        newBound.add(e.name);
        visit(e.body, newBound);
        break;
      }
      case "LetRec": {
        const newBound = new Set(innerBound);
        e.bindings.forEach(b => newBound.add(b.name));
        e.bindings.forEach(b => checkPrim(b.rhs, newBound));
        visit(e.body, newBound);
        break;
      }
      case "If":
        checkAtom(e.test);
        visit(e.thn, innerBound);
        visit(e.els, innerBound);
        break;
      case "Seq":
        visit(e.first, innerBound);
        visit(e.second, innerBound);
        break;
      case "Set":
        checkAtom(e.rhs);
        visit(e.body, innerBound);
        break;
    }
  }

  visit(expr, bound);
  return free;
}

/**
 * Compile an ANF primitive (leaves result on stack).
 */
function compilePrim(ctx: BytecodeContext, prim: ANFPrim): void {
  switch (prim.tag) {
    case "Lambda": {
      // Find free variables in the lambda body
      const paramSet = new Set(prim.params);
      const freeVars = findFreeVarsInExpr(prim.body, paramSet);

      // Determine which free vars are available in current scope
      const captured: number[] = [];
      const capturedNames: string[] = [];
      for (const fv of freeVars) {
        const slot = ctx.locals.get(fv);
        if (slot !== undefined) {
          captured.push(slot);
          capturedNames.push(fv);
        }
      }

      // Create a new function
      const fnId = ctx.functions.length;
      const savedLocals = new Map(ctx.locals);
      const savedNextLocal = ctx.nextLocal;
      const savedFn = ctx.currentFn;

      // Create new function
      const fn: BytecodeFunction = {
        id: fnId,
        arity: prim.params.length,
        localCount: 0,
        code: [],
        label: prim.label,
      };
      ctx.functions.push(fn);
      ctx.currentFn = fn;

      // Reset locals for function
      ctx.locals = new Map();
      ctx.nextLocal = 0;

      // Allocate slots for parameters
      for (const param of prim.params) {
        getLocalSlot(ctx, param);
      }

      // Allocate slots for captured variables (after params)
      for (const name of capturedNames) {
        getLocalSlot(ctx, name);
      }

      // Compile function body
      compileExpr(ctx, prim.body);

      // Ensure return
      emit(ctx, { op: "RET" });

      fn.localCount = ctx.nextLocal;

      // Restore context
      ctx.locals = savedLocals;
      ctx.nextLocal = savedNextLocal;
      ctx.currentFn = savedFn;

      // Push closure with captured variable slots
      emit(ctx, { op: "CLOSURE", fnId, captured });
      break;
    }

    case "Call": {
      // Push function
      compileAtom(ctx, prim.fn);
      // Push args
      for (const arg of prim.args) {
        compileAtom(ctx, arg);
      }
      // Call
      emit(ctx, { op: "CALL", argc: prim.args.length });
      break;
    }

    case "Effect": {
      // Effects are preserved! Push args and emit effect instruction
      for (const arg of prim.args) {
        compileAtom(ctx, arg);
      }
      emit(ctx, { op: "EFFECT", opName: prim.op, argc: prim.args.length });
      break;
    }

    case "Quote": {
      const val = datumToVal(prim.datum);
      const k = addConstant(ctx, val);
      emit(ctx, { op: "CONST", k });
      break;
    }

    case "Prim": {
      // Built-in primitive operations
      for (const arg of prim.args) {
        compileAtom(ctx, arg);
      }
      emit(ctx, { op: "PRIM", name: prim.name, argc: prim.args.length });
      break;
    }

    case "Match": {
      compileAtom(ctx, prim.scrut);

      // For simplicity, generate if-else chain for patterns
      // A more sophisticated version would use dispatch tables
      const endLabel = freshLabel(ctx);

      for (let i = 0; i < prim.clauses.length; i++) {
        const clause = prim.clauses[i];
        const nextLabel = i < prim.clauses.length - 1 ? freshLabel(ctx) : endLabel;

        // Compile pattern test
        compilePatternTest(ctx, clause.pat, nextLabel);

        // Bind pattern variables
        compilePatternBind(ctx, clause.pat);

        // Compile body
        compileExpr(ctx, clause.body);

        // Jump to end
        emit(ctx, { op: "JMP", label: endLabel });

        // Next clause label
        if (i < prim.clauses.length - 1) {
          // Mark label position (simplified - in real impl would patch jumps)
        }
      }

      // End label
      break;
    }

    case "Handle": {
      const handlerId = ctx.handlers.length;

      // Create handler definition
      const handler: BytecodeHandler = {
        id: handlerId,
        ops: new Map(),
      };

      // Compile handler clauses into functions
      for (const clause of prim.handler.on) {
        const fnId = ctx.functions.length;
        const fn: BytecodeFunction = {
          id: fnId,
          arity: clause.params.length + 1, // +1 for continuation
          localCount: 0,
          code: [],
        };
        ctx.functions.push(fn);

        const savedLocals = new Map(ctx.locals);
        const savedNextLocal = ctx.nextLocal;
        const savedFn = ctx.currentFn;

        ctx.currentFn = fn;
        ctx.locals = new Map();
        ctx.nextLocal = 0;

        // Allocate slots for params and k
        const paramSlots: number[] = [];
        for (const p of clause.params) {
          paramSlots.push(getLocalSlot(ctx, p));
        }
        const kSlot = getLocalSlot(ctx, clause.k);

        compileExpr(ctx, clause.body);
        emit(ctx, { op: "RET" });

        fn.localCount = ctx.nextLocal;

        handler.ops.set(clause.op, { paramSlots, kSlot, codeStart: fnId });

        ctx.locals = savedLocals;
        ctx.nextLocal = savedNextLocal;
        ctx.currentFn = savedFn;
      }

      // Compile ret clause if present
      if (prim.handler.ret) {
        const fnId = ctx.functions.length;
        const fn: BytecodeFunction = {
          id: fnId,
          arity: 1,
          localCount: 0,
          code: [],
        };
        ctx.functions.push(fn);

        const savedLocals = new Map(ctx.locals);
        const savedNextLocal = ctx.nextLocal;
        const savedFn = ctx.currentFn;

        ctx.currentFn = fn;
        ctx.locals = new Map();
        ctx.nextLocal = 0;

        handler.retSlot = getLocalSlot(ctx, prim.handler.ret.v);
        compileExpr(ctx, prim.handler.ret.body);
        emit(ctx, { op: "RET" });

        fn.localCount = ctx.nextLocal;
        handler.retCodeStart = fnId;

        ctx.locals = savedLocals;
        ctx.nextLocal = savedNextLocal;
        ctx.currentFn = savedFn;
      }

      ctx.handlers.push(handler);

      // Emit handle instruction
      emit(ctx, { op: "HANDLE", handlerId });

      // Compile body
      compileExpr(ctx, prim.body);

      // End handler
      emit(ctx, { op: "UNHANDLE" });
      break;
    }

    case "MakeClosure": {
      // After closure conversion - not implemented yet
      compilePrim(ctx, prim.lambda);
      break;
    }
  }
}

/**
 * Convert datum to Val for quotes.
 */
function datumToVal(datum: unknown): Val {
  if (datum === null || datum === undefined) return VUnit;
  if (typeof datum === "boolean") return datum ? { tag: "Bool", b: true } : { tag: "Bool", b: false };
  if (typeof datum === "number") return { tag: "Num", n: datum };
  if (typeof datum === "string") return { tag: "Str", s: datum };
  if (typeof datum === "symbol") {
    return { tag: "Sym", name: datum.description ?? "symbol" };
  }
  if (Array.isArray(datum)) {
    return { tag: "Vector", items: datum.map(datumToVal) };
  }
  return { tag: "Sym", name: String(datum) };
}

/**
 * Compile pattern test (for match expressions).
 */
function compilePatternTest(ctx: BytecodeContext, pat: ANFPattern, failLabel: number): void {
  switch (pat.tag) {
    case "PWild":
      // Always matches
      break;
    case "PVar":
      // Always matches, binding happens later
      break;
    case "PLit": {
      emit(ctx, { op: "DUP" });
      const k = addConstant(ctx, pat.v);
      emit(ctx, { op: "CONST", k });
      emit(ctx, { op: "PRIM", name: "eq?", argc: 2 });
      emit(ctx, { op: "JMPIF", labelTrue: currentIP(ctx) + 1, labelFalse: failLabel });
      break;
    }
    case "PVector":
      // TODO: Implement vector pattern matching
      break;
  }
}

/**
 * Compile pattern binding (bind pattern variables).
 */
function compilePatternBind(ctx: BytecodeContext, pat: ANFPattern): void {
  switch (pat.tag) {
    case "PWild":
      emit(ctx, { op: "POP" });
      break;
    case "PVar": {
      const slot = getLocalSlot(ctx, pat.name);
      emit(ctx, { op: "STORE", slot });
      break;
    }
    case "PLit":
      emit(ctx, { op: "POP" });
      break;
    case "PVector":
      // TODO: Implement vector pattern binding
      emit(ctx, { op: "POP" });
      break;
  }
}

// ─────────────────────────────────────────────────────────────────
// Expression Compilation
// ─────────────────────────────────────────────────────────────────

/**
 * Compile an ANF expression.
 */
function compileExpr(ctx: BytecodeContext, expr: ANFExpr): void {
  switch (expr.tag) {
    case "Return": {
      compileAtom(ctx, expr.v);
      break;
    }

    case "Let": {
      // Compile RHS
      compilePrim(ctx, expr.rhs);

      // Store in local
      const slot = getLocalSlot(ctx, expr.name);
      emit(ctx, { op: "STORE", slot });

      // Compile body
      compileExpr(ctx, expr.body);
      break;
    }

    case "LetRec": {
      // Allocate slots for all bindings first
      for (const binding of expr.bindings) {
        getLocalSlot(ctx, binding.name);
      }

      // Initialize with undefined
      for (const binding of expr.bindings) {
        const k = addConstant(ctx, VUnit);
        emit(ctx, { op: "CONST", k });
        const slot = ctx.locals.get(binding.name)!;
        emit(ctx, { op: "STORE", slot });
      }

      // Compile each binding's RHS
      for (const binding of expr.bindings) {
        compilePrim(ctx, binding.rhs);
        const slot = ctx.locals.get(binding.name)!;
        emit(ctx, { op: "STORE", slot });
      }

      // Compile body
      compileExpr(ctx, expr.body);
      break;
    }

    case "If": {
      compileAtom(ctx, expr.test);

      // Emit placeholder JMPIF - we'll patch the false label
      const jmpifIdx = currentIP(ctx);
      emit(ctx, { op: "JMPIF", labelTrue: jmpifIdx + 1, labelFalse: 0 }); // placeholder

      // Then branch
      compileExpr(ctx, expr.thn);

      // Emit placeholder JMP to end - we'll patch this too
      const jmpEndIdx = currentIP(ctx);
      emit(ctx, { op: "JMP", label: 0 }); // placeholder

      // Now we know where the else branch starts
      const elseStart = currentIP(ctx);

      // Patch the JMPIF false label
      if (ctx.currentFn) {
        const jmpif = ctx.currentFn.code[jmpifIdx];
        if (jmpif.op === "JMPIF") {
          ctx.currentFn.code[jmpifIdx] = { ...jmpif, labelFalse: elseStart };
        }
      }

      // Else branch
      compileExpr(ctx, expr.els);

      // Now we know where the end is
      const endPos = currentIP(ctx);

      // Patch the JMP to end
      if (ctx.currentFn) {
        const jmp = ctx.currentFn.code[jmpEndIdx];
        if (jmp.op === "JMP") {
          ctx.currentFn.code[jmpEndIdx] = { ...jmp, label: endPos };
        }
      }

      break;
    }

    case "Seq": {
      compileExpr(ctx, expr.first);
      emit(ctx, { op: "POP" }); // Discard first result
      compileExpr(ctx, expr.second);
      break;
    }

    case "Set": {
      compileAtom(ctx, expr.rhs);

      const slot = ctx.locals.get(expr.name);
      if (slot !== undefined) {
        emit(ctx, { op: "STORE", slot });
      } else {
        emit(ctx, { op: "GSTORE", name: expr.name });
      }

      compileExpr(ctx, expr.body);
      break;
    }
  }
}

// ─────────────────────────────────────────────────────────────────
// Public API
// ─────────────────────────────────────────────────────────────────

/**
 * Compile an ANF program to bytecode.
 */
export function toBytecode(anf: ANFProgram): BytecodeProgram {
  const ctx = createBytecodeContext();
  ctx.freeVars = [...anf.freeVars];

  // Create entry function
  const entryFn: BytecodeFunction = {
    id: 0,
    arity: 0,
    localCount: 0,
    code: [],
    label: "entry",
  };
  ctx.functions.push(entryFn);
  ctx.currentFn = entryFn;

  // Compile program body
  compileExpr(ctx, anf.body);
  emit(ctx, { op: "RET" });

  entryFn.localCount = ctx.nextLocal;

  return {
    constants: ctx.constants,
    functions: ctx.functions,
    handlers: ctx.handlers,
    entryFn: 0,
    freeVars: ctx.freeVars,
    sourceMap: { entries: ctx.sourceMapEntries },
  };
}

/**
 * Count instructions in bytecode program.
 */
export function countInstructions(program: BytecodeProgram): number {
  return program.functions.reduce((sum, fn) => sum + fn.code.length, 0);
}

/**
 * Count effect instructions in bytecode program.
 */
export function countEffectInstructions(program: BytecodeProgram): number {
  let count = 0;
  for (const fn of program.functions) {
    for (const instr of fn.code) {
      if (instr.op === "EFFECT") {
        count++;
      }
    }
  }
  return count;
}

/**
 * Find all effect operations in bytecode program.
 */
export function findEffectOpsInBytecode(program: BytecodeProgram): Set<string> {
  const ops = new Set<string>();
  for (const fn of program.functions) {
    for (const instr of fn.code) {
      if (instr.op === "EFFECT") {
        ops.add(instr.opName);
      }
    }
  }
  return ops;
}

/**
 * Disassemble bytecode function (for debugging).
 */
export function disassemble(fn: BytecodeFunction): string {
  const lines: string[] = [];
  lines.push(`; Function ${fn.id}${fn.label ? ` (${fn.label})` : ""}`);
  lines.push(`; Arity: ${fn.arity}, Locals: ${fn.localCount}`);

  for (let i = 0; i < fn.code.length; i++) {
    const instr = fn.code[i];
    lines.push(`  ${i.toString().padStart(4)}: ${formatInstr(instr)}`);
  }

  return lines.join("\n");
}

/**
 * Format a single instruction.
 */
function formatInstr(instr: Instr): string {
  switch (instr.op) {
    case "CONST":
      return `CONST ${instr.k}`;
    case "LOAD":
      return `LOAD ${instr.slot}`;
    case "STORE":
      return `STORE ${instr.slot}`;
    case "GLOAD":
      return `GLOAD ${instr.name}`;
    case "GSTORE":
      return `GSTORE ${instr.name}`;
    case "CLOSURE":
      return `CLOSURE ${instr.fnId} [${instr.captured.join(", ")}]`;
    case "CALL":
      return `CALL ${instr.argc}`;
    case "TAILCALL":
      return `TAILCALL ${instr.argc}`;
    case "RET":
      return "RET";
    case "POP":
      return "POP";
    case "DUP":
      return "DUP";
    case "JMP":
      return `JMP ${instr.label}`;
    case "JMPIF":
      return `JMPIF ${instr.labelTrue} ${instr.labelFalse}`;
    case "EFFECT":
      return `EFFECT ${instr.opName} ${instr.argc}`;
    case "HANDLE":
      return `HANDLE ${instr.handlerId}`;
    case "UNHANDLE":
      return "UNHANDLE";
    case "PRIM":
      return `PRIM ${instr.name} ${instr.argc}`;
    case "MATCH":
      return `MATCH [${instr.clauseLabels.join(", ")}]`;
    case "FAIL":
      return `FAIL ${instr.reasonK}`;
    case "NOP":
      return "NOP";
    case "DEBUG":
      return `DEBUG "${instr.info}"`;
  }
}

/**
 * Disassemble entire bytecode program.
 */
export function disassembleProgram(program: BytecodeProgram): string {
  const lines: string[] = [];

  lines.push("; Constants:");
  for (let i = 0; i < program.constants.length; i++) {
    lines.push(`  ${i}: ${JSON.stringify(program.constants[i])}`);
  }
  lines.push("");

  lines.push("; Free variables:");
  lines.push(`  [${program.freeVars.join(", ")}]`);
  lines.push("");

  lines.push("; Functions:");
  for (const fn of program.functions) {
    lines.push(disassemble(fn));
    lines.push("");
  }

  return lines.join("\n");
}
