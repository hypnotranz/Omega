// test/public-api/cli-exports.spec.ts
// Tests for CLI tool public API exports (Job 017b)
//
// This test verifies that all symbols needed by CLI tools are accessible
// from the public API (src/index.ts) rather than requiring internal imports.

import { describe, it, expect } from "vitest";

// Test that all CLI exports are accessible from public API
import {
  // Core runtime (already existed)
  OmegaRuntime,
  evalOmegaCode,
  type OmegaConfig,
  type EvalResult,

  // NEW: CLI/Debugger exports - Store
  COWStore,
  type Store,

  // NEW: Single-step execution
  stepOnce,
  type StepResult,

  // NEW: Run to completion
  runToCompletionWithState,
  runToCompletion,

  // NEW: Machine state types
  type State,
  type Frame,

  // NEW: Environment type
  type Env,

  // NEW: Compilation
  compileTextToExpr,

  // NEW: Effects runtime
  RuntimeImpl,

  // NEW: Oracle state management
  SnapshotRepo,
  InMemoryReceiptStore,
  type ReceiptStore,

  // NEW: OpenAI adapter (Anthropic already exported)
  createOpenAIAdapter,

  // NEW: Primitive installation
  installPrims,
} from "../../src";

describe("Public API - CLI Exports", () => {
  describe("Store exports", () => {
    it("COWStore is a class constructor", () => {
      expect(COWStore).toBeDefined();
      expect(typeof COWStore).toBe("function");
    });

    it("can create COWStore instance", () => {
      const store = new COWStore();
      expect(store).toBeDefined();
      expect(store).toBeInstanceOf(COWStore);
    });

    it("COWStore implements Store interface", () => {
      const store = new COWStore();
      // Check Store interface methods exist
      expect(typeof store.alloc).toBe("function");
      expect(typeof store.read).toBe("function");
      expect(typeof store.write).toBe("function");
      expect(typeof store.snapshot).toBe("function");
    });

    it("COWStore allocates addresses", () => {
      let store = new COWStore();
      const [store1, addr1] = store.alloc({ tag: "Num", n: 42 });
      const [store2, addr2] = store1.alloc({ tag: "Num", n: 99 });
      expect(typeof addr1).toBe("number");
      expect(typeof addr2).toBe("number");
      expect(addr1).not.toBe(addr2);
    });

    it("COWStore can read and write values", () => {
      const store = new COWStore();
      const val = { tag: "Num", n: 42 } as const;
      const [store1, addr] = store.alloc(val);
      const retrieved = store1.read(addr);
      expect(retrieved).toEqual(val);
    });
  });

  describe("Single-step execution exports", () => {
    it("stepOnce is a function", () => {
      expect(stepOnce).toBeDefined();
      expect(typeof stepOnce).toBe("function");
    });

    it("stepOnce returns StepResult", () => {
      // Create minimal state
      const store = new COWStore();
      const prims = installPrims(store);

      const state: State = {
        control: { tag: "Val", v: { tag: "Unit" } },
        env: prims.env,
        store: prims.store,
        kont: [],
        handlers: [],
      };

      const result = stepOnce(state);
      expect(result).toBeDefined();
      expect(result.tag).toBeDefined();
      // Result should be one of: State, Done, Op
      expect(["State", "Done", "Op"]).toContain(result.tag);
    });
  });

  describe("Run to completion exports", () => {
    it("runToCompletion is a function", () => {
      expect(runToCompletion).toBeDefined();
      expect(typeof runToCompletion).toBe("function");
    });

    it("runToCompletionWithState is a function", () => {
      expect(runToCompletionWithState).toBeDefined();
      expect(typeof runToCompletionWithState).toBe("function");
    });

    it("runToCompletion executes simple expression", async () => {
      const store = new COWStore();
      const prims = installPrims(store);
      const runtime = new RuntimeImpl(undefined, new SnapshotRepo(), new InMemoryReceiptStore("off"), undefined, undefined);

      const expr = compileTextToExpr("42");
      const state: State = {
        control: { tag: "Expr", e: expr },
        env: prims.env,
        store: prims.store,
        kont: [],
        handlers: [],
      };

      const result = await runToCompletion(runtime, state, 1000);
      expect(result).toEqual({ tag: "Num", n: 42 });
    });

    it("runToCompletionWithState returns value and state", async () => {
      const store = new COWStore();
      const prims = installPrims(store);
      const runtime = new RuntimeImpl(undefined, new SnapshotRepo(), new InMemoryReceiptStore("off"), undefined, undefined);

      const expr = compileTextToExpr("42");
      const state: State = {
        control: { tag: "Expr", e: expr },
        env: prims.env,
        store: prims.store,
        kont: [],
        handlers: [],
      };

      const result = await runToCompletionWithState(runtime, state, 1000);
      expect(result).toBeDefined();
      expect(result.value).toEqual({ tag: "Num", n: 42 });
      expect(result.state).toBeDefined();
      expect(result.state.control.tag).toBe("Val");
    });
  });

  describe("Compilation exports", () => {
    it("compileTextToExpr is a function", () => {
      expect(compileTextToExpr).toBeDefined();
      expect(typeof compileTextToExpr).toBe("function");
    });

    it("compileTextToExpr compiles number literals", () => {
      const expr = compileTextToExpr("42");
      expect(expr).toBeDefined();
      expect(expr.tag).toBe("Lit");
      if (expr.tag === "Lit") {
        expect(expr.value).toBe(42);
      }
    });

    it("compileTextToExpr compiles string literals", () => {
      const expr = compileTextToExpr('"hello"');
      expect(expr).toBeDefined();
      expect(expr.tag).toBe("Lit");
      if (expr.tag === "Lit") {
        expect(expr.value).toBe("hello");
      }
    });

    it("compileTextToExpr compiles function calls", () => {
      const expr = compileTextToExpr("(+ 1 2)");
      expect(expr).toBeDefined();
      expect(expr.tag).toBe("App");
    });

    it("compileTextToExpr handles empty input", () => {
      // Empty input compiles to a begin with no forms (not an error)
      const expr = compileTextToExpr("");
      expect(expr).toBeDefined();
    });

    it("compileTextToExpr handles malformed input", () => {
      expect(() => compileTextToExpr("(+ 1")).toThrow();
    });
  });

  describe("RuntimeImpl exports", () => {
    it("RuntimeImpl is a class constructor", () => {
      expect(RuntimeImpl).toBeDefined();
      expect(typeof RuntimeImpl).toBe("function");
    });

    it("can create RuntimeImpl instance", () => {
      const snapshots = new SnapshotRepo();
      const receipts = new InMemoryReceiptStore("off");
      const runtime = new RuntimeImpl(undefined, snapshots, receipts, undefined, undefined);
      expect(runtime).toBeDefined();
      expect(runtime).toBeInstanceOf(RuntimeImpl);
    });

    it("RuntimeImpl has dispatch method", () => {
      const runtime = new RuntimeImpl(undefined, new SnapshotRepo(), new InMemoryReceiptStore("off"), undefined, undefined);
      expect(typeof runtime.dispatch).toBe("function");
    });
  });

  describe("Oracle state management exports", () => {
    it("SnapshotRepo is a class constructor", () => {
      expect(SnapshotRepo).toBeDefined();
      expect(typeof SnapshotRepo).toBe("function");
    });

    it("can create SnapshotRepo instance", () => {
      const repo = new SnapshotRepo();
      expect(repo).toBeDefined();
      expect(repo).toBeInstanceOf(SnapshotRepo);
    });

    it("InMemoryReceiptStore is a class constructor", () => {
      expect(InMemoryReceiptStore).toBeDefined();
      expect(typeof InMemoryReceiptStore).toBe("function");
    });

    it("can create InMemoryReceiptStore instance", () => {
      const store = new InMemoryReceiptStore("off");
      expect(store).toBeDefined();
      expect(store).toBeInstanceOf(InMemoryReceiptStore);
    });

    it("InMemoryReceiptStore implements ReceiptStore interface", () => {
      const store = new InMemoryReceiptStore("off");
      // Check ReceiptStore interface methods exist
      expect(typeof store.put).toBe("function");
      expect(typeof store.get).toBe("function");
    });
  });

  describe("Adapter exports", () => {
    it("createOpenAIAdapter is a function", () => {
      expect(createOpenAIAdapter).toBeDefined();
      expect(typeof createOpenAIAdapter).toBe("function");
    });

    it("createOpenAIAdapter creates adapter with default config", () => {
      const adapter = createOpenAIAdapter();
      expect(adapter).toBeDefined();
      // OpenAI adapter implements OracleAdapter interface
      expect(typeof adapter.startSession).toBe("function");
      expect(typeof adapter.capabilities).toBe("function");
    });

    it("createOpenAIAdapter accepts config overrides", () => {
      const adapter = createOpenAIAdapter({ model: "gpt-4" });
      expect(adapter).toBeDefined();
    });
  });

  describe("Primitive installation exports", () => {
    it("installPrims is a function", () => {
      expect(installPrims).toBeDefined();
      expect(typeof installPrims).toBe("function");
    });

    it("installPrims installs primitives into store", () => {
      const store = new COWStore();
      const result = installPrims(store);
      expect(result).toBeDefined();
      expect(result.env).toBeDefined();
      expect(result.store).toBeDefined();
    });

    it("installPrims env contains primitive bindings", () => {
      const store = new COWStore();
      const { env } = installPrims(store);
      // Env is a Ctx object, check it's defined and has expected structure
      expect(env).toBeDefined();
      expect(env.tag).toBe("Ctx");
      // Check that env has frame with bindings
      expect(env.frame).toBeInstanceOf(Map);
      expect(env.frame.size).toBeGreaterThan(100); // Should have 117+ primitives
    });

    it("installPrims bindings are usable", async () => {
      const store = new COWStore();
      const prims = installPrims(store);
      const runtime = new RuntimeImpl(undefined, new SnapshotRepo(), new InMemoryReceiptStore("off"), undefined, undefined);

      const expr = compileTextToExpr("(+ 10 32)");
      const state: State = {
        control: { tag: "Expr", e: expr },
        env: prims.env,
        store: prims.store,
        kont: [],
        handlers: [],
      };

      const result = await runToCompletion(runtime, state, 1000);
      expect(result).toEqual({ tag: "Num", n: 42 });
    });
  });

  describe("Integration - Complete CLI workflow", () => {
    it("can compile, install prims, and evaluate expression", async () => {
      // This is the typical CLI/REPL workflow
      const code = "(* (+ 2 3) 7)";

      // 1. Compile
      const expr = compileTextToExpr(code);

      // 2. Create store and install primitives
      const store = new COWStore();
      const prims = installPrims(store);

      // 3. Create runtime
      const runtime = new RuntimeImpl(
        undefined,
        new SnapshotRepo(),
        new InMemoryReceiptStore("off"),
        undefined,
        undefined
      );

      // 4. Create initial state
      const state: State = {
        control: { tag: "Expr", e: expr },
        env: prims.env,
        store: prims.store,
        kont: [],
        handlers: [],
      };

      // 5. Execute
      const result = await runToCompletion(runtime, state, 10000);

      expect(result).toEqual({ tag: "Num", n: 35 });
    });

    it("can single-step through execution", () => {
      const code = "(+ 1 2)";
      const expr = compileTextToExpr(code);
      const store = new COWStore();
      const prims = installPrims(store);

      let state: State = {
        control: { tag: "Expr", e: expr },
        env: prims.env,
        store: prims.store,
        kont: [],
        handlers: [],
      };

      // Step until done
      let steps = 0;
      const maxSteps = 100;
      while (steps < maxSteps) {
        const result = stepOnce(state);
        steps++;

        if (result.tag === "Done") {
          expect(result.value).toEqual({ tag: "Num", n: 3 });
          break;
        } else if (result.tag === "State") {
          state = result.state;
        } else {
          // Op tag - would need runtime dispatch in real scenario
          break;
        }
      }

      expect(steps).toBeLessThan(maxSteps);
    });
  });

  describe("Type exports", () => {
    it("Store type is available", () => {
      // TypeScript compilation test - if this compiles, type is exported
      const _typeTest: Store = new COWStore();
      expect(_typeTest).toBeDefined();
    });

    it("StepResult type is available", () => {
      // TypeScript compilation test
      const store = new COWStore();
      const prims = installPrims(store);
      const state: State = {
        control: { tag: "Val", v: { tag: "Unit" } },
        env: prims.env,
        store: prims.store,
        kont: [],
        handlers: [],
      };
      const _typeTest: StepResult = stepOnce(state);
      expect(_typeTest).toBeDefined();
    });

    it("State type is available", () => {
      // TypeScript compilation test
      const store = new COWStore();
      const prims = installPrims(store);
      const _typeTest: State = {
        control: { tag: "Val", v: { tag: "Unit" } },
        env: prims.env,
        store: prims.store,
        kont: [],
        handlers: [],
      };
      expect(_typeTest).toBeDefined();
    });

    it("Frame type is available", () => {
      // TypeScript compilation test
      const _typeTest: Frame = {
        tag: "AppL",
        e: { tag: "Lit", v: { tag: "Unit" } },
        env: {},
      };
      expect(_typeTest).toBeDefined();
    });

    it("Env type is available", () => {
      // TypeScript compilation test
      const _typeTest: Env = {};
      expect(_typeTest).toBeDefined();
    });

    it("ReceiptStore type is available", () => {
      // TypeScript compilation test
      const _typeTest: ReceiptStore = new InMemoryReceiptStore("off");
      expect(_typeTest).toBeDefined();
    });
  });

  describe("Edge cases and error handling", () => {
    it("compileTextToExpr handles multiple forms", () => {
      const expr = compileTextToExpr("(+ 1 2) (* 3 4)");
      expect(expr).toBeDefined();
      expect(expr.tag).toBe("Begin");
    });

    it("runToCompletion enforces step limit", async () => {
      const store = new COWStore();
      const prims = installPrims(store);
      const runtime = new RuntimeImpl(undefined, new SnapshotRepo(), new InMemoryReceiptStore("off"), undefined, undefined);

      // Create infinite loop
      const expr = compileTextToExpr("((lambda (f) (f f)) (lambda (f) (f f)))");
      const state: State = {
        control: { tag: "Expr", e: expr },
        env: prims.env,
        store: prims.store,
        kont: [],
        handlers: [],
      };

      await expect(runToCompletion(runtime, state, 10)).rejects.toThrow("step budget exceeded");
    });
  });
});
