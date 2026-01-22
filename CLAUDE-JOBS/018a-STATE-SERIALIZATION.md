# JOB-018a: State Serialization (Foundation)

**Priority**: P1 - Core Infrastructure
**Estimated Effort**: 3-4 hours
**Status**: NOT STARTED
**Depends On**: None
**Blocks**: 018b, 018c, 018e

> **MUST READ**: [LOGISTICS.md](LOGISTICS.md) before starting.

---

## Goal

Implement `serializeState()` and `deserializeState()` - pure functions that convert CEKS machine state to/from JSON.

**Done State**: All serializer tests pass. You can round-trip any State through JSON.

---

## TDD: Write Tests First

Create `test/session/serializer.spec.ts` and make these tests pass:

```typescript
import { describe, it, expect } from "vitest";
import { serializeState, deserializeState } from "../../src/core/session/serializer";
import { buildNativeRegistry } from "../../src/core/session/nativeRegistry";
import { COWStore } from "../../src/core/eval/store";
import { installPrims } from "../../src/core/prims";

describe("State Serializer", () => {
  function setup() {
    const store = new COWStore();
    const { env, store: primedStore } = installPrims(store);
    const nativeRegistry = buildNativeRegistry(primedStore);
    return { env, store: primedStore, nativeRegistry };
  }

  it("round-trips atomic values (Num, Bool, Str, Sym, Unit)", () => {
    const { env, store, nativeRegistry } = setup();

    for (const v of [
      { tag: "Num", n: 42 },
      { tag: "Bool", b: true },
      { tag: "Str", s: "hello" },
      { tag: "Sym", name: "foo" },
      { tag: "Unit" },
    ]) {
      const state = { control: { tag: "Val", v }, env, store, kont: [], handlers: [] };
      const json = JSON.stringify(serializeState(state));
      const restored = deserializeState(JSON.parse(json), nativeRegistry);
      expect(restored.control).toEqual(state.control);
    }
  });

  it("round-trips BigInt via string conversion", () => {
    const { env, store, nativeRegistry } = setup();
    const bigVal = { tag: "Int", value: BigInt("99999999999999999999") };
    const state = { control: { tag: "Val", v: bigVal }, env, store, kont: [], handlers: [] };

    const json = JSON.stringify(serializeState(state));
    expect(json).toContain("99999999999999999999");  // As string

    const restored = deserializeState(JSON.parse(json), nativeRegistry);
    expect(restored.control.v.value.toString()).toBe("99999999999999999999");
  });

  it("round-trips Pair and nested structures", () => {
    const { env, store, nativeRegistry } = setup();
    const pair = {
      tag: "Pair",
      car: { tag: "Num", n: 1 },
      cdr: { tag: "Pair", car: { tag: "Num", n: 2 }, cdr: { tag: "Unit" } }
    };
    const state = { control: { tag: "Val", v: pair }, env, store, kont: [], handlers: [] };

    const json = JSON.stringify(serializeState(state));
    const restored = deserializeState(JSON.parse(json), nativeRegistry);
    expect(restored.control.v.car.n).toBe(1);
    expect(restored.control.v.cdr.car.n).toBe(2);
  });

  it("round-trips Closure with captured environment", () => {
    const { env, store, nativeRegistry } = setup();
    const closure = {
      tag: "Closure",
      params: ["x", "y"],
      body: { tag: "App", fn: { tag: "Var", name: "+" }, args: [{ tag: "Var", name: "x" }, { tag: "Var", name: "y" }] },
      env: env,
    };
    const state = { control: { tag: "Val", v: closure }, env, store, kont: [], handlers: [] };

    const json = JSON.stringify(serializeState(state));
    const restored = deserializeState(JSON.parse(json), nativeRegistry);

    expect(restored.control.v.tag).toBe("Closure");
    expect(restored.control.v.params).toEqual(["x", "y"]);
    expect(restored.control.v.env).toBeDefined();
    expect(restored.control.v.env.tag).toBe("Ctx");
  });

  it("round-trips Native via registry lookup (fn not in JSON)", () => {
    const { env, store, nativeRegistry } = setup();
    const plusNative = nativeRegistry.get("+");
    const state = { control: { tag: "Val", v: plusNative }, env, store, kont: [], handlers: [] };

    const json = JSON.stringify(serializeState(state));
    expect(json).not.toContain('"fn"');  // Function not serialized
    expect(json).toContain('"name":"+"');

    const restored = deserializeState(JSON.parse(json), nativeRegistry);
    expect(restored.control.v.tag).toBe("Native");
    expect(restored.control.v.name).toBe("+");
    expect(typeof restored.control.v.fn).toBe("function");  // Restored from registry
  });

  it("handles circular Ctx.parent without throwing", () => {
    const { env, store, nativeRegistry } = setup();
    const state = { control: { tag: "Val", v: { tag: "Unit" } }, env, store, kont: [], handlers: [] };

    // Should not throw "Converting circular structure to JSON"
    expect(() => JSON.stringify(serializeState(state))).not.toThrow();

    const json = JSON.stringify(serializeState(state));
    const restored = deserializeState(JSON.parse(json), nativeRegistry);
    expect(restored.env.tag).toBe("Ctx");
  });

  it("round-trips all 18 Frame types in kont", () => {
    const { env, store, nativeRegistry } = setup();

    const frames = [
      { tag: "KIf", conseq: { tag: "Num", n: 1 }, alt: { tag: "Num", n: 2 }, env },
      { tag: "KBegin", rest: [{ tag: "Num", n: 3 }], env },
      { tag: "KDefine", name: "x", env },
      { tag: "KSet", name: "y", env },
      { tag: "KAppFun", args: [{ tag: "Num", n: 4 }], env },
      { tag: "KAppArg", fnVal: { tag: "Unit" }, pending: [], acc: [], env },
      { tag: "KCall", savedEnv: env },
      { tag: "KEffect", op: "test.op", pending: [], acc: [], env },
      { tag: "KMatch", clauses: [], env },
      { tag: "KOracleLambda", params: ["a"], env },
      { tag: "KBind", fn: { tag: "Unit" }, env },
    ];

    const state = { control: { tag: "Val", v: { tag: "Unit" } }, env, store, kont: frames, handlers: [] };

    const json = JSON.stringify(serializeState(state));
    const restored = deserializeState(JSON.parse(json), nativeRegistry);

    expect(restored.kont.length).toBe(frames.length);
    expect(restored.kont[0].tag).toBe("KIf");
    expect(restored.kont[6].tag).toBe("KCall");
    expect(restored.kont[6].savedEnv.tag).toBe("Ctx");
  });

  it("round-trips Cont with Resumption (invoke reconstructed)", () => {
    const { env, store, nativeRegistry } = setup();

    const baseState = { control: { tag: "Val", v: { tag: "Unit" } }, env, store, kont: [], handlers: [] };
    const cont = {
      tag: "Cont",
      hid: "h-123",
      boundaryIndex: 0,
      resumption: {
        rid: "r-456",
        base: baseState,
        invoke: (v) => ({ ...baseState, control: { tag: "Val", v } }),
        digest: () => "test",
      },
    };

    const state = { control: { tag: "Val", v: cont }, env, store, kont: [], handlers: [] };

    const json = JSON.stringify(serializeState(state));
    const restored = deserializeState(JSON.parse(json), nativeRegistry);

    expect(restored.control.v.tag).toBe("Cont");
    expect(restored.control.v.resumption.rid).toBe("r-456");
    expect(typeof restored.control.v.resumption.invoke).toBe("function");

    // Test that invoke actually works
    const resumed = restored.control.v.resumption.invoke({ tag: "Num", n: 99 });
    expect(resumed.control.v.n).toBe(99);
  });

  it("round-trips Store with multiple cells", () => {
    const { env, nativeRegistry } = setup();
    let store = new COWStore();
    [store] = store.alloc({ tag: "Num", n: 10 });
    [store] = store.alloc({ tag: "Str", s: "hello" });
    [store] = store.alloc({ tag: "Bool", b: true });

    const state = { control: { tag: "Val", v: { tag: "Unit" } }, env, store, kont: [], handlers: [] };

    const json = JSON.stringify(serializeState(state));
    const restored = deserializeState(JSON.parse(json), nativeRegistry);

    expect(restored.store.read(0).n).toBe(10);
    expect(restored.store.read(1).s).toBe("hello");
    expect(restored.store.read(2).b).toBe(true);
  });

  it("round-trips Dist (distribution) values", () => {
    const { env, store, nativeRegistry } = setup();
    const dist = {
      tag: "Dist",
      support: [
        { v: { tag: "Str", s: "yes" }, w: 0.7 },
        { v: { tag: "Str", s: "no" }, w: 0.3 },
      ],
      normalized: true,
    };
    const state = { control: { tag: "Val", v: dist }, env, store, kont: [], handlers: [] };

    const json = JSON.stringify(serializeState(state));
    const restored = deserializeState(JSON.parse(json), nativeRegistry);

    expect(restored.control.v.support.length).toBe(2);
    expect(restored.control.v.support[0].w).toBe(0.7);
  });
});
```

---

## Implementation

### File 1: `src/core/session/nativeRegistry.ts`

```typescript
import type { Val } from "../eval/values";
import type { Store } from "../eval/store";

/**
 * Build a registry of Native functions by scanning the primed store.
 * Used during deserialization to restore Native.fn from Native.name.
 */
export function buildNativeRegistry(store: Store): Map<string, Val> {
  const registry = new Map<string, Val>();

  for (let addr = 0; addr < store.next; addr++) {
    try {
      const val = store.read(addr);
      if (val && val.tag === "Native") {
        registry.set(val.name, val);
      }
    } catch {
      // Skip invalid addresses
    }
  }

  return registry;
}
```

### File 2: `src/core/session/serializer.ts`

See the full implementation in the parent job 018. Key points:

1. **SerializedState type** - JSON-safe version of State
2. **ctxTable** - Flattens Ctx parent chain to break circulars
3. **serializeVal()** - Handles all 50+ Val types
4. **serializeFrame()** - Handles all 18 Frame types
5. **deserializeState()** - Rebuilds everything including:
   - Ctx parent pointers
   - Native.fn from registry lookup
   - Resumption.invoke reconstruction

---

## Verification

```bash
cd OmegaLLM
npm test -- --run test/session/serializer.spec.ts

# Expected output:
# ✓ round-trips atomic values
# ✓ round-trips BigInt via string conversion
# ✓ round-trips Pair and nested structures
# ✓ round-trips Closure with captured environment
# ✓ round-trips Native via registry lookup
# ✓ handles circular Ctx.parent without throwing
# ✓ round-trips all 18 Frame types in kont
# ✓ round-trips Cont with Resumption
# ✓ round-trips Store with multiple cells
# ✓ round-trips Dist values
```

---

## Checklist

- [ ] Create `src/core/session/` directory
- [ ] Implement `nativeRegistry.ts`
- [ ] Implement `serializer.ts` with all types
- [ ] All serializer.spec.ts tests pass
- [ ] No `console.warn` for unhandled types

---

## Files Created

```
src/core/session/
├── nativeRegistry.ts    # buildNativeRegistry()
├── serializer.ts        # serializeState(), deserializeState()
└── index.ts             # exports

test/session/
└── serializer.spec.ts   # All round-trip tests
```
