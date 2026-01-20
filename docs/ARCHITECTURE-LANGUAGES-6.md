## 69) FrameIR v1 JSON Schema: a *machine-checkable* contract for bundles, nodes, and replay compatibility

If you want “optimal power” across four projects, you need the IR to be **formally validated** (structural soundness), not just informally typed. The most pragmatic “lowest common denominator” is **JSON Schema 2020-12** for the interchange format, with canonicalization rules defined normatively (because JSON Schema cannot express all canonical constraints like “no trailing zeros” reliably).

Below is a **workable** schema skeleton that is immediately implementable.

### 69.1 Canonical numeric lexical spaces (normative, not fully enforceable in schema)

* **Int lexical space**: `^-?(0|[1-9][0-9]*)$`
* **Float lexical space**: you have two viable policies:

  1. **Decimal-only** (no exponent): `^-?(0|[1-9][0-9]*)(\.[0-9]+)?$` plus *canonicalization* strips trailing zeros and normalizes `-0` to `0`.
  2. **Scientific allowed**: requires a canonical formatter (e.g., normalize to decimal with fixed rules).
     *Strong recommendation*: pick (1) for simplicity and determinism.

### 69.2 `$id` / `$schema` header + shared defs

```json
{
  "$schema": "https://json-schema.org/draft/2020-12/schema",
  "$id": "frameir.schema.json",
  "title": "FrameIR v1 Schema",
  "type": "object",
  "oneOf": [
    { "$ref": "#/$defs/IRBundle" },
    { "$ref": "#/$defs/FlowIR" },
    { "$ref": "#/$defs/PromptIR" },
    { "$ref": "#/$defs/ValueIR" }
  ],
  "$defs": {
    "IRVersion": {
      "type": "string",
      "const": "frameir@1"
    },

    "Span": {
      "type": "object",
      "properties": {
        "file": { "type": "string" },
        "startLine": { "type": "integer", "minimum": 1 },
        "startCol": { "type": "integer", "minimum": 1 },
        "endLine": { "type": "integer", "minimum": 1 },
        "endCol": { "type": "integer", "minimum": 1 }
      },
      "additionalProperties": false
    },

    "Meta": {
      "type": "object",
      "properties": {
        "span": { "$ref": "#/$defs/Span" },
        "doc": { "type": "string" },
        "attrs": { "type": "object" }
      },
      "additionalProperties": false
    },

    "NodeBase": {
      "type": "object",
      "properties": {
        "v": { "$ref": "#/$defs/IRVersion" },
        "tag": { "type": "string" },
        "meta": { "$ref": "#/$defs/Meta" }
      },
      "required": ["v", "tag"],
      "additionalProperties": true
    },

    "IntString": {
      "type": "string",
      "pattern": "^-?(0|[1-9][0-9]*)$"
    },

    "FloatString": {
      "type": "string",
      "pattern": "^-?(0|[1-9][0-9]*)(\\.[0-9]+)?$"
    }
  }
}
```

Note the deliberate choice: **NodeBase allows additionalProperties** because each node subtype will lock down additional properties via `allOf` + `additionalProperties:false`. This avoids repetition and prevents schema loopholes.

---

## 69.3 ValueIR union schema

### 69.3.1 Atom nodes

```json
{
  "$defs": {
    "ValueIR": {
      "oneOf": [
        { "$ref": "#/$defs/VNil" },
        { "$ref": "#/$defs/VBool" },
        { "$ref": "#/$defs/VInt" },
        { "$ref": "#/$defs/VFloat" },
        { "$ref": "#/$defs/VStr" },
        { "$ref": "#/$defs/VSymbol" },
        { "$ref": "#/$defs/VKeyword" },
        { "$ref": "#/$defs/VList" },
        { "$ref": "#/$defs/VRecord" },
        { "$ref": "#/$defs/VRef" },
        { "$ref": "#/$defs/VExpr" }
      ]
    },

    "VNil": {
      "allOf": [
        { "$ref": "#/$defs/NodeBase" },
        {
          "type": "object",
          "properties": { "tag": { "const": "VNil" } },
          "required": ["tag"],
          "additionalProperties": false
        }
      ]
    },

    "VBool": {
      "allOf": [
        { "$ref": "#/$defs/NodeBase" },
        {
          "type": "object",
          "properties": {
            "tag": { "const": "VBool" },
            "value": { "type": "boolean" }
          },
          "required": ["tag", "value"],
          "additionalProperties": false
        }
      ]
    },

    "VInt": {
      "allOf": [
        { "$ref": "#/$defs/NodeBase" },
        {
          "type": "object",
          "properties": {
            "tag": { "const": "VInt" },
            "value": { "$ref": "#/$defs/IntString" }
          },
          "required": ["tag", "value"],
          "additionalProperties": false
        }
      ]
    },

    "VFloat": {
      "allOf": [
        { "$ref": "#/$defs/NodeBase" },
        {
          "type": "object",
          "properties": {
            "tag": { "const": "VFloat" },
            "value": { "$ref": "#/$defs/FloatString" }
          },
          "required": ["tag", "value"],
          "additionalProperties": false
        }
      ]
    },

    "VStr": {
      "allOf": [
        { "$ref": "#/$defs/NodeBase" },
        {
          "type": "object",
          "properties": { "tag": { "const": "VStr" }, "value": { "type": "string" } },
          "required": ["tag", "value"],
          "additionalProperties": false
        }
      ]
    },

    "VSymbol": {
      "allOf": [
        { "$ref": "#/$defs/NodeBase" },
        {
          "type": "object",
          "properties": { "tag": { "const": "VSymbol" }, "name": { "type": "string" } },
          "required": ["tag", "name"],
          "additionalProperties": false
        }
      ]
    },

    "VKeyword": {
      "allOf": [
        { "$ref": "#/$defs/NodeBase" },
        {
          "type": "object",
          "properties": { "tag": { "const": "VKeyword" }, "name": { "type": "string" } },
          "required": ["tag", "name"],
          "additionalProperties": false
        }
      ]
    }
  }
}
```

### 69.3.2 Collections + references

```json
{
  "$defs": {
    "VList": {
      "allOf": [
        { "$ref": "#/$defs/NodeBase" },
        {
          "type": "object",
          "properties": {
            "tag": { "const": "VList" },
            "items": { "type": "array", "items": { "$ref": "#/$defs/ValueIR" } }
          },
          "required": ["tag", "items"],
          "additionalProperties": false
        }
      ]
    },

    "VRecordEntry": {
      "type": "object",
      "properties": {
        "k": { "$ref": "#/$defs/ValueIR" },
        "v": { "$ref": "#/$defs/ValueIR" }
      },
      "required": ["k", "v"],
      "additionalProperties": false
    },

    "VRecord": {
      "allOf": [
        { "$ref": "#/$defs/NodeBase" },
        {
          "type": "object",
          "properties": {
            "tag": { "const": "VRecord" },
            "entries": { "type": "array", "items": { "$ref": "#/$defs/VRecordEntry" } }
          },
          "required": ["tag", "entries"],
          "additionalProperties": false
        }
      ]
    },

    "VRef": {
      "allOf": [
        { "$ref": "#/$defs/NodeBase" },
        {
          "type": "object",
          "properties": {
            "tag": { "const": "VRef" },
            "ref": {
              "type": "object",
              "properties": {
                "kind": {
                  "type": "string",
                  "enum": ["Global", "Fn", "ToolContract", "Schema", "Store", "Sink", "Source"]
                },
                "id": { "type": "string" },
                "name": { "type": "string" }
              },
              "required": ["kind", "id"],
              "additionalProperties": false
            }
          },
          "required": ["tag", "ref"],
          "additionalProperties": false
        }
      ]
    }
  }
}
```

### 69.3.3 Value expression union

You don’t need to spell all of it here, but the pattern repeats:

```json
{
  "$defs": {
    "VExpr": {
      "oneOf": [
        { "$ref": "#/$defs/EIf" },
        { "$ref": "#/$defs/EEq" },
        { "$ref": "#/$defs/ENot" },
        { "$ref": "#/$defs/EAnd" },
        { "$ref": "#/$defs/EOr" },
        { "$ref": "#/$defs/EAdd" },
        { "$ref": "#/$defs/ESub" },
        { "$ref": "#/$defs/EMul" },
        { "$ref": "#/$defs/EDiv" },
        { "$ref": "#/$defs/EMod" },
        { "$ref": "#/$defs/EGet" },
        { "$ref": "#/$defs/EAssoc" },
        { "$ref": "#/$defs/ECallPrim" }
      ]
    },

    "EIf": {
      "allOf": [
        { "$ref": "#/$defs/NodeBase" },
        {
          "type": "object",
          "properties": {
            "tag": { "const": "EIf" },
            "cond": { "$ref": "#/$defs/ValueIR" },
            "then": { "$ref": "#/$defs/ValueIR" },
            "else": { "$ref": "#/$defs/ValueIR" }
          },
          "required": ["tag", "cond", "then", "else"],
          "additionalProperties": false
        }
      ]
    },

    "ECallPrim": {
      "allOf": [
        { "$ref": "#/$defs/NodeBase" },
        {
          "type": "object",
          "properties": {
            "tag": { "const": "ECallPrim" },
            "prim": { "type": "string" },
            "args": { "type": "array", "items": { "$ref": "#/$defs/ValueIR" } }
          },
          "required": ["tag", "prim", "args"],
          "additionalProperties": false
        }
      ]
    }
  }
}
```

---

## 69.4 PromptIR union schema

Same discriminated-union technique. Key design choice: **PromptDoc is the only top-level prompt** and other prompt nodes appear inside `parts` or as `inner`.

```json
{
  "$defs": {
    "PromptIR": { "$ref": "#/$defs/PromptDoc" },

    "PromptDoc": {
      "allOf": [
        { "$ref": "#/$defs/NodeBase" },
        {
          "type": "object",
          "properties": {
            "tag": { "const": "PromptDoc" },
            "parts": { "type": "array", "items": { "$ref": "#/$defs/PromptPart" } }
          },
          "required": ["tag", "parts"],
          "additionalProperties": false
        }
      ]
    },

    "PromptPart": {
      "oneOf": [
        { "$ref": "#/$defs/PSystem" },
        { "$ref": "#/$defs/PUser" },
        { "$ref": "#/$defs/PAssistant" },
        { "$ref": "#/$defs/PFewShot" },
        { "$ref": "#/$defs/PData" },
        { "$ref": "#/$defs/PXml" },
        { "$ref": "#/$defs/PCodeBlock" },
        { "$ref": "#/$defs/PNumbered" },
        { "$ref": "#/$defs/PAttachTools" },
        { "$ref": "#/$defs/PAttachSchema" },
        { "$ref": "#/$defs/PAttachFormat" },
        { "$ref": "#/$defs/PTransform" }
      ]
    },

    "PSystem": {
      "allOf": [
        { "$ref": "#/$defs/NodeBase" },
        {
          "type": "object",
          "properties": { "tag": { "const": "PSystem" }, "text": { "type": "string" } },
          "required": ["tag", "text"],
          "additionalProperties": false
        }
      ]
    },

    "PAttachSchema": {
      "allOf": [
        { "$ref": "#/$defs/NodeBase" },
        {
          "type": "object",
          "properties": {
            "tag": { "const": "PAttachSchema" },
            "schema": { "$ref": "#/$defs/VRef" },
            "inner": { "$ref": "#/$defs/PromptIR" }
          },
          "required": ["tag", "schema", "inner"],
          "additionalProperties": false
        }
      ]
    }
  }
}
```

---

## 69.5 FlowIR union schema

This is the high-value piece because it enables **compiler-grade lints** and **replay stability**.

```json
{
  "$defs": {
    "FlowIR": {
      "oneOf": [
        { "$ref": "#/$defs/FPure" },
        { "$ref": "#/$defs/FBind" },
        { "$ref": "#/$defs/FCatch" },
        { "$ref": "#/$defs/FFail" },
        { "$ref": "#/$defs/FWithBudget" },
        { "$ref": "#/$defs/FWithTimeout" },
        { "$ref": "#/$defs/FAll" },
        { "$ref": "#/$defs/FRace" },
        { "$ref": "#/$defs/FAny" },
        { "$ref": "#/$defs/FSequence" },
        { "$ref": "#/$defs/FBranch" },
        { "$ref": "#/$defs/FLoop" },
        { "$ref": "#/$defs/FInfer" },
        { "$ref": "#/$defs/FToolCall" },
        { "$ref": "#/$defs/FValidate" },
        { "$ref": "#/$defs/FCommit" },
        { "$ref": "#/$defs/FEmit" },
        { "$ref": "#/$defs/FObserve" },
        { "$ref": "#/$defs/FSuspend" }
      ]
    },

    "FPure": {
      "allOf": [
        { "$ref": "#/$defs/NodeBase" },
        {
          "type": "object",
          "properties": {
            "tag": { "const": "FPure" },
            "value": { "$ref": "#/$defs/ValueIR" }
          },
          "required": ["tag", "value"],
          "additionalProperties": false
        }
      ]
    },

    "FBind": {
      "allOf": [
        { "$ref": "#/$defs/NodeBase" },
        {
          "type": "object",
          "properties": {
            "tag": { "const": "FBind" },
            "flow": { "$ref": "#/$defs/FlowIR" },
            "k": { "$ref": "#/$defs/VRef" }
          },
          "required": ["tag", "flow", "k"],
          "additionalProperties": false
        }
      ]
    },

    "FInfer": {
      "allOf": [
        { "$ref": "#/$defs/NodeBase" },
        {
          "type": "object",
          "properties": {
            "tag": { "const": "FInfer" },
            "prompt": { "$ref": "#/$defs/PromptIR" },
            "options": { "$ref": "#/$defs/ValueIR" }
          },
          "required": ["tag", "prompt"],
          "additionalProperties": false
        }
      ]
    },

    "FToolCall": {
      "allOf": [
        { "$ref": "#/$defs/NodeBase" },
        {
          "type": "object",
          "properties": {
            "tag": { "const": "FToolCall" },
            "tool": { "$ref": "#/$defs/ValueIR" },
            "args": { "$ref": "#/$defs/ValueIR" },
            "contract": { "$ref": "#/$defs/VRef" }
          },
          "required": ["tag", "tool", "args"],
          "additionalProperties": false
        }
      ]
    }
  }
}
```

---

## 69.6 IRBundle schema

This is the “closure conversion container” + “linker unit”.

```json
{
  "$defs": {
    "IRBundle": {
      "allOf": [
        { "$ref": "#/$defs/NodeBase" },
        {
          "type": "object",
          "properties": {
            "tag": { "const": "IRBundle" },
            "entry": { "$ref": "#/$defs/FlowIR" },
            "fns": {
              "type": "object",
              "additionalProperties": { "$ref": "#/$defs/FnDefIR" }
            },
            "schemas": {
              "type": "object",
              "additionalProperties": { "$ref": "#/$defs/SchemaIR" }
            },
            "toolContracts": {
              "type": "object",
              "additionalProperties": { "$ref": "#/$defs/ToolContractIR" }
            }
          },
          "required": ["tag", "entry", "fns", "schemas", "toolContracts"],
          "additionalProperties": false
        }
      ]
    },

    "FnDefIR": {
      "allOf": [
        { "$ref": "#/$defs/NodeBase" },
        {
          "type": "object",
          "properties": {
            "tag": { "const": "FnDef" },
            "fnId": { "type": "string" },
            "params": { "type": "array", "items": { "type": "string" } },
            "body": {
              "oneOf": [
                { "$ref": "#/$defs/FlowIR" },
                { "$ref": "#/$defs/ValueIR" }
              ]
            },
            "captures": { "$ref": "#/$defs/ValueIR" }
          },
          "required": ["tag", "fnId", "params", "body"],
          "additionalProperties": false
        }
      ]
    },

    "SchemaIR": {
      "allOf": [
        { "$ref": "#/$defs/NodeBase" },
        {
          "type": "object",
          "properties": {
            "tag": { "const": "Schema" },
            "id": { "type": "string" },
            "kind": { "type": "string", "enum": ["JsonSchema", "FrameSchema"] },
            "jsonSchema": { "type": "object" },
            "frameSchema": { "type": "object" }
          },
          "required": ["tag", "id", "kind"],
          "additionalProperties": false
        }
      ]
    },

    "ToolContractIR": {
      "allOf": [
        { "$ref": "#/$defs/NodeBase" },
        {
          "type": "object",
          "properties": {
            "tag": { "const": "ToolContract" },
            "id": { "type": "string" },
            "name": { "type": "string" },
            "version": { "type": "string" },
            "inputSchema": { "$ref": "#/$defs/VRef" },
            "outputSchema": { "$ref": "#/$defs/VRef" }
          },
          "required": ["tag", "id", "name", "version", "inputSchema", "outputSchema"],
          "additionalProperties": false
        }
      ]
    }
  }
}
```

This is enough to:

* validate a bundle structurally
* refuse ill-formed nodes at boundaries (tooling, persistence, network)
* generate docs safely
* generate code bindings safely

---

# 70) PrimitiveDescriptor registry seed: bridging the four projects into one “semantic index”

Your `REFERENCE-LIBRARIES.md` is already a “human registry,” but it’s not executable. Let’s convert that into a proper **descriptor registry** that:

* drives docgen
* drives lowering
* drives lints
* drives budgeting
* drives capability gating

### 70.1 Descriptor minimal “golden fields” (must be present)

For each primitive / special form / macro:

* `id` (namespaced)
* `layer`
* `kind` (`SpecialForm | Function | Macro | ProtocolMethod`)
* `signature` (string types are fine initially)
* `effects` (closed set)
* `lowering` (intrinsic tag, macroexpand, or hook)
* `laws` (equational laws; crucial for reasoning + refactoring)
* `constraints` (lint hooks: dominated-by-budget, contract-required, etc.)

### 70.2 Example: `framelisp/infer`

```json
{
  "id": "framelisp/infer",
  "layer": "FrameLisp",
  "kind": "SpecialForm",
  "signature": {
    "params": [
      { "name": "prompt", "type": "Prompt" },
      { "name": "options", "type": "Record?", "optional": true }
    ],
    "returns": "Str"
  },
  "effects": ["Oracle"],
  "resources": {
    "model": "oracle-v1",
    "estimate": { "llmCalls": 1, "tokens": "promptTokens + maxTokens", "timeMs": "p95(model)" }
  },
  "doc": {
    "summary": "Core LLM inference primitive.",
    "laws": [
      "infer(prompt) is referentially opaque (Oracle effect).",
      "infer must be dominated by with-budget in linted bundles."
    ]
  },
  "lowering": { "kind": "Intrinsic", "irTag": "FInfer" },
  "constraints": {
    "mustBeDominatedByBudget": true
  },
  "version": "1.0.0"
}
```

### 70.3 Example: `framelisp/bind`

```json
{
  "id": "framelisp/bind",
  "layer": "FrameLisp",
  "kind": "Function",
  "signature": {
    "params": [
      { "name": "m", "type": "Flow[A]" },
      { "name": "k", "type": "Fn[A -> Flow[B]]" }
    ],
    "returns": "Flow[B]"
  },
  "effects": ["Control"],
  "doc": {
    "summary": "Monadic bind for Flow.",
    "laws": [
      "Left identity: bind(pure(x), k) ≡ k(x).",
      "Right identity: bind(m, pure) ≡ m.",
      "Associativity: bind(bind(m, f), g) ≡ bind(m, (λx. bind(f(x), g)))."
    ]
  },
  "lowering": { "kind": "Intrinsic", "irTag": "FBind" },
  "version": "1.0.0"
}
```

Those “laws” are not decoration; they are the foundation for **Refactoring to Patterns** and safe rewrites.

### 70.4 Example: `lambdarlm/compose-sequential`

This is library-level but should still have a lowering story.

```json
{
  "id": "lambdarlm/compose-sequential",
  "layer": "LambdaRLM",
  "kind": "Function",
  "signature": {
    "params": [
      { "name": "s1", "type": "Solver" },
      { "name": "s2", "type": "Solver" }
    ],
    "returns": "Solver"
  },
  "effects": ["Control"],
  "doc": {
    "summary": "Construct a solver that pipes s1 result into s2.",
    "laws": [
      "compose-sequential is associative up to observational equivalence, if solver metadata merge is associative."
    ]
  },
  "lowering": { "kind": "LowerHook", "hook": "lambdarlm.lower/composeSequential" },
  "version": "1.0.0"
}
```

LowerHook means: “this is not a primitive Flow node; it expands into a solver record whose `solve` field ultimately emits FlowIR.”

---

# 71) Port interfaces: the “hexagonal boundary” that makes determinism and replay possible

Your runtime has real effects:

* oracle calls
* tool calls
* filesystem / stores
* clocks
* randomness
* concurrency scheduling choices

If you treat these as ambient authority, you cannot replay, and you cannot lint capabilities.

So: **Object-Capability discipline** + **Ports & Adapters**.

### 71.1 The canonical Port set

#### OraclePort (LLM)

```ts
export interface OracleRequest {
  model: string;
  prompt: string;              // rendered PromptIR
  tools?: any;                 // tool spec for LLM provider, if used
  outputSchema?: any;          // structured mode constraint
  temperature?: number;
  maxTokens?: number;
  seed?: number;
  metadata?: Record<string, any>;
}

export interface OracleResponse {
  text: string;
  usage?: { promptTokens?: number; completionTokens?: number; totalTokens?: number };
  raw?: any; // provider-specific
}

export interface OraclePort {
  infer(req: OracleRequest, ctx: ExecContext): Promise<OracleResponse>;
}
```

Key: OraclePort must be the only place an LLM can be called.

#### ToolPort (external tools)

```ts
export interface ToolCall {
  name: string;
  args: unknown;               // validated against contract input schema before calling
  contractId: string;          // must be present in audited execution
}

export interface ToolResult {
  ok: boolean;
  value?: unknown;             // validated against output schema
  error?: { type: string; message: string; data?: unknown };
  raw?: unknown;
}

export interface ToolPort {
  call(tool: ToolCall, ctx: ExecContext): Promise<ToolResult>;
}
```

#### StorePort (commit/lookup)

```ts
export interface StorePort {
  get(key: string, ctx: ExecContext): Promise<unknown | null>;
  put(key: string, value: unknown, ctx: ExecContext): Promise<void>;
}
```

#### SinkPort / SourcePort (emit/observe)

```ts
export interface SinkPort {
  emit(item: unknown, ctx: ExecContext): Promise<void>;
}

export interface SourcePort {
  observe(query: unknown, ctx: ExecContext): Promise<unknown>;
}
```

#### ClockPort + RngPort (determinism requirements)

```ts
export interface ClockPort {
  nowMs(ctx: ExecContext): number;             // must be deterministic under replay
  sleepMs(ms: number, ctx: ExecContext): Promise<void>;
}

export interface RngPort {
  nextU32(ctx: ExecContext): number;           // deterministic stream
}
```

> If you don’t port clocks/RNG, you don’t have replay; you have “best effort.”

### 71.2 ExecContext: the capability + trace propagation envelope

```ts
export interface ExecContext {
  runId: string;        // correlation id for a whole execution
  span?: Span;          // current span
  budget?: BudgetState; // remaining resources
  caps: CapabilitySet;  // object-capabilities: tools allowed, stores allowed, etc.
  trace: TraceSink;     // event log sink
}
```

Capabilities should be *data*, but enforced in runtime (deny by default).

---

# 72) Replay log format: event-sourced execution with deterministic rehydration

To get “optimal power,” you want the ability to:

* run with real side-effects once
* then replay offline deterministically for debugging, auditing, and regression tests

This is **Event Sourcing** + **Deterministic Replay**.

### 72.1 Core replay principles

1. Every effect is logged as an event with enough data to reproduce the result.
2. In replay mode, ports become “replay adapters” that read from the log.
3. Scheduler decisions are logged, so concurrency is replayable.

### 72.2 Event types

A minimal but sufficient event taxonomy:

```ts
export type ReplayEvent =
  | { tag: "E_SchedulerDecision"; fiberId: string; choice: number; }
  | { tag: "E_OracleCall"; id: string; req: OracleRequest; res: OracleResponse; }
  | { tag: "E_ToolCall"; id: string; call: ToolCall; res: ToolResult; }
  | { tag: "E_StorePut"; storeId: string; key: string; valueHash: string; }
  | { tag: "E_StoreGet"; storeId: string; key: string; hit: boolean; valueHash?: string; }
  | { tag: "E_SinkEmit"; sinkId: string; itemHash: string; }
  | { tag: "E_SourceObserve"; sourceId: string; queryHash: string; resHash: string; }
  | { tag: "E_ClockNow"; valueMs: number; }
  | { tag: "E_RngNextU32"; value: number; }
  | { tag: "E_YieldPoint"; label: string; }
  | { tag: "E_Failure"; failure: any; };
```

Use **hashes** (claim checks) instead of embedding large payloads; store payload blobs separately keyed by hash. This avoids log bloat and makes content-addressability explicit.

### 72.3 Replay adapters

* `ReplayOraclePort`: asserts `req` matches logged `req` (or a weaker match policy), returns logged `res`.
* `ReplayToolPort`: same.
* `ReplayClockPort`: returns logged time values.
* `ReplayRngPort`: returns logged RNG values.
* `ReplaySchedulerPolicy`: uses logged decisions to pick next fiber deterministically.

This is basically a **Proxy** + **Decorator** stack:

* Real ports: `LoggingPort(realPort, traceSink)`
* Replay ports: `ReplayPort(traceSource)`

### 72.4 Determinism hazards (and how you prevent them)

* **Non-deterministic iteration order** in JS objects → canonicalization forbids it or normalizes it.
* **Floating point** differences → store floats as strings; parse using decimal library.
* **Clock and RNG** ambient usage → forbid; require ports (lint + runtime guard).
* **Concurrency** nondeterminism → must log scheduling decisions and use deterministic policies.

---

# 73) Capability calculus + static dominance checks: compile-time safety properties

This is where your system becomes “enterprise-grade” rather than “agent demo.”

### 73.1 Capabilities as first-class values (object-capability model)

Each execution environment carries explicit authority:

* `OracleCap(modelSet, constraints)`
* `ToolCap(allowedToolContracts)`
* `StoreCap(storeIds)`
* `SinkCap(sinkIds)`
* `SourceCap(sourceIds)`

Then:

* `PAttachTools` introduces/limits tool caps for a prompt (for tool-augmented chat)
* `FWithBudget` introduces a budget cap
* `with-tools` in LambdaLLM (spec) becomes a Flow wrapper that changes caps in the ExecContext

### 73.2 Dominator-based lint rules (compiler-grade)

Given FlowIR forms a control-flow-ish structure (tree with joins via parallel combinators), you can compute dominators conservatively.

**Example lint rules:**

* **Budget dominance**: every `FInfer` and `FToolCall` must be dominated by `FWithBudget`.
* **Timeout dominance**: optionally require `FInfer` dominated by `FWithTimeout` for production profiles.
* **Contract requirement**: every `FToolCall` must specify `contract` with `kind=ToolContract`.
* **Schema requirement**: if prompt has structured output expectation, require `PAttachSchema` or `PAttachFormat`.
* **Capability gating**: `FToolCall` contractId must be in cap-set in the path environment.

Patterns in play:

* **Static Program Analysis**
* **Design by Contract**
* **Secure by Construction**

---

# 74) Linker/Loader model: how bundles become executable units

You’re already close to a module system because you have four projects with overlapping primitives. Make it explicit:

### 74.1 Compilation products

* **LambdaLLM source** → `IRBundle` (+ registry ref hash)
* **LambdaRLM library** → emits `FnDef` + helper `Schema` + contract refs
* **Omega runtime** → executes `IRBundle` with given ports

### 74.2 Content-addressed linking

Every bundle component should be content-addressed:

* `fnId = sha256(canonical(fnDef without meta))`
* `schemaId = sha256(canonical(schema))`
* `contractId = sha256(canonical(contract))`

Then:

* you can store these in a CAS (content-addressed store)
* you can deduplicate aggressively
* you can do Merkle proof style evidence (your provenance system aligns perfectly with this)

### 74.3 Name resolution policy

Avoid “stringly typed global resolution” where possible:

* At compile time, resolve symbols to `VRef(kind="Fn", id=...)`
* For human readability, optionally carry `name` in the ref, excluded from semantic hash

This is exactly the separation:

* **semantic identity** = `id`
* **presentation** = `name`

---

# 75) Test architecture: how to keep a system like this from regressing into mud

You’ve got enough moving parts that you *must* invest in testing patterns as first-class artifacts.

### 75.1 Golden tests for lowering

For each primitive / macro:

* input: LambdaLLM form
* expected: canonical FlowIR JSON

Use snapshot testing (but with canonicalization so snapshots are stable).

### 75.2 Property-based tests for canonicalization

Properties:

* `decode(encode(x)) == x` (modulo meta)
* `hash(x) == hash(decode(encode(x)))`
* `normalize(normalize(x)) == normalize(x)` (idempotence)
* `normalize(x)` preserves semantics (tested with differential interpreter in small-step subset)

### 75.3 Replay determinism tests

* Run with real ports but logging.
* Replay with replay ports.
* Assert:

  * same final result
  * same sequence of semantic events (or equivalence class)
  * same scheduler decisions

This is the highest ROI test you can write for a concurrent + oracle-driven system.

### 75.4 Contract tests for ToolContracts

For each tool contract:

* validate that runtime tool implementation returns outputs conforming to schema
* validate error schema path

Pattern: **Consumer-Driven Contract Testing**.

---

# 76) Interop strategy: stop duplicating logic between TS and Lisp

The “optimal power” move is: TS hosts the kernel; Lisp expresses logic; IR connects them.

### 76.1 The FFI boundary is FlowIR, not ad-hoc callbacks

If a Lisp function is used as a binder/predicate:

* compile it to `FnDefIR`
* reference it via `VRef(kind="Fn", id=...)`

At runtime:

* TS evaluator has a `FnTable` mapping fnId → compiled closure or bytecode representation.

### 76.2 Two-phase compilation to keep macros powerful

Pipeline:

1. **Macroexpand** (LambdaLLM in Lisp space)

   * expand user macros into core forms
2. **Lower** core forms to FlowIR/PromptIR/ValueIR
3. **Normalize** + **lint** (FrameIR passes)
4. **Execute** in Omega kernel

This is a clean split:

* macros remain a *compile-time* facility
* runtime remains deterministic and auditable

Patterns:

* **Compiler Pipeline**
* **Staged Computation**
* **Partial Evaluation** (optional later)

---

# 77) Performance model: receipts, memoization, DAG sharing, and budget-aware caching

Once IR is content-addressed, performance becomes straightforward.

### 77.1 Memoization at the right granularity: “Flow subplan caching”

Because `FlowIR` is hashable, you can implement:

* `CachePort.get(hash(flowSubtree))`
* if hit, skip executing the subtree and return cached result

But you must guard with:

* effect analysis: only cache if subtree is “cacheable” under your policy (e.g., pure, or oracle calls with fixed seed and frozen tool outputs)
* provenance: cached result must carry evidence of how it was computed (your provenance library becomes essential here)

This is essentially **Common Subexpression Elimination** lifted into an effectful world via policies.

### 77.2 Receipt-backed streams prevent space blowups

Your Omega stream receipts are a direct application of the **Claim Check** pattern:

* compute stream segment
* store segment in CAS
* pass receipts through IR and logs
* hydrate on demand

This makes:

* nondet exploration feasible
* multi-agent orchestration feasible
* long-running repair loops feasible

---

# 78) Security + governance: make unsafe states unrepresentable

If you want this to scale beyond “one developer,” you need guardrails.

### 78.1 Deny-by-default capability policy

* No oracle without OracleCap
* No tool call without ToolCap and matching ToolContract
* No filesystem writes unless StoreCap allows store and policy allows mutation
* No observe unless SourceCap allowed

### 78.2 Enforce policy in two places

1. **Lint passes** (fail builds early)
2. **Runtime checks** (defense in depth)

This is the standard enterprise approach:

* static checks catch most errors
* runtime checks catch dynamic cases and malicious input

### 78.3 Provenance + evidence as a governance layer

Your LambdaRLM provenance module can become a formal “audit trail” mechanism:

* every tool result must be optionally recorded with evidence metadata
* stale evidence detection can invalidate caches
* critical outputs can require “observed/measured/derived” epistemic modes

This is where you turn “LLM outputs” into “claims with evidence.”

---

# 79) End-to-end worked example: a solver pipeline lowered into FlowIR with budgets + tool contracts

Let’s sketch a realistic “repair loop” style pipeline:

### 79.1 High-level intent (LambdaRLM-ish pseudocode)

* Generate candidate patch via `infer` with schema
* Validate patch (types, tests)
* If invalid, run repair prompt (infer again)
* Iterate up to N
* Commit result to store
* Emit progress events

### 79.2 Lowered FlowIR outline

```json
{
  "v": "frameir@1",
  "tag": "IRBundle",
  "entry": {
    "v": "frameir@1",
    "tag": "FWithBudget",
    "budget": {
      "v": "frameir@1",
      "tag": "VRecord",
      "entries": [
        { "k": { "v":"frameir@1","tag":"VKeyword","name":"llmCalls" }, "v": { "v":"frameir@1","tag":"VInt","value":"12" } },
        { "k": { "v":"frameir@1","tag":"VKeyword","name":"tokens" },   "v": { "v":"frameir@1","tag":"VInt","value":"24000" } },
        { "k": { "v":"frameir@1","tag":"VKeyword","name":"timeMs" },   "v": { "v":"frameir@1","tag":"VInt","value":"180000" } }
      ]
    },
    "flow": {
      "v": "frameir@1",
      "tag": "FLoop",
      "init": { "v":"frameir@1","tag":"VInt","value":"0" },
      "step": { "v":"frameir@1","tag":"VRef","ref":{"kind":"Fn","id":"fn:step"} },
      "until": { "v":"frameir@1","tag":"VRef","ref":{"kind":"Fn","id":"fn:until"} }
    }
  },
  "fns": {
    "fn:step": {
      "v":"frameir@1","tag":"FnDef",
      "fnId":"fn:step","params":["i"],
      "body": {
        "v":"frameir@1","tag":"FSequence",
        "flows":[
          {
            "v":"frameir@1","tag":"FEmit",
            "sink": { "v":"frameir@1","tag":"VRef","ref":{"kind":"Sink","id":"sink:progress"} },
            "item": { "v":"frameir@1","tag":"VStr","value":"iteration" }
          },
          {
            "v":"frameir@1","tag":"FBind",
            "flow": {
              "v":"frameir@1","tag":"FInfer",
              "prompt": {
                "v":"frameir@1","tag":"PromptDoc","parts":[
                  { "v":"frameir@1","tag":"PSystem","text":"You are a repair agent." },
                  { "v":"frameir@1","tag":"PAttachSchema",
                    "schema": { "v":"frameir@1","tag":"VRef","ref":{"kind":"Schema","id":"schema:patch"} },
                    "inner": { "v":"frameir@1","tag":"PromptDoc","parts":[
                      { "v":"frameir@1","tag":"PUser","text":"Produce a patch." }
                    ]}
                  }
                ]
              }
            },
            "k": { "v":"frameir@1","tag":"VRef","ref":{"kind":"Fn","id":"fn:validatePatch"} }
          }
        ]
      }
    },
    "fn:validatePatch": {
      "v":"frameir@1","tag":"FnDef",
      "fnId":"fn:validatePatch","params":["patchText"],
      "body": {
        "v":"frameir@1","tag":"FBranch",
        "pred": { "v":"frameir@1","tag":"VBool","value":true },
        "then": {
          "v":"frameir@1","tag":"FCommit",
          "store": { "v":"frameir@1","tag":"VRef","ref":{"kind":"Store","id":"store:repo"} },
          "key": { "v":"frameir@1","tag":"VStr","value":"patched" },
          "value": { "v":"frameir@1","tag":"VRef","ref":{"kind":"Global","id":"val:patch"} }
        },
        "else": {
          "v":"frameir@1","tag":"FFail",
          "reason": { "v":"frameir@1","tag":"VKeyword","name":"validation-failed" }
        }
      }
    },
    "fn:until": {
      "v":"frameir@1","tag":"FnDef",
      "fnId":"fn:until","params":["i"],
      "body": { "v":"frameir@1","tag":"VBool","value":false }
    }
  },
  "schemas": {
    "schema:patch": { "v":"frameir@1","tag":"Schema","id":"schema:patch","kind":"JsonSchema","jsonSchema":{ "type":"object" } }
  },
  "toolContracts": {}
}
```

That’s obviously abbreviated, but the important thing is the *shape*:

* budgets dominate oracle calls
* schema is explicit and referenced
* state is explicit
* “functions” are explicit and content-addressable

---

## 80) What to implement next (concrete, code-first) to unlock the rest

You now have enough spec to begin implementation without guessing.

### 80.1 Minimal kernel subset to implement first

Implement Flow nodes in this order (each one makes the system exponentially more useful):

1. `FPure`
2. `FBind` + Fn invocation (`FnDef`)
3. `FFail` + `FCatch`
4. `FSequence`
5. `FInfer` (OraclePort)
6. `FValidate` (SchemaPort or embedded validator)
7. `FToolCall` (ToolPort + ToolContract enforcement)
8. `FWithBudget` (Budget token in ExecContext)
9. `FWithTimeout` (ClockPort integration)

Only after that:

* `FAll`/`FRace`/`FAny` (fibers + scheduler)
* `FLoop` (stateful iteration)
* receipts + hydration (stream receipts, claim checks)

### 80.2 Minimal pass pipeline

* `decode/validate` (JSON Schema)
* `normalize` (prompt flattening, sequence flattening)
* `lint` (budget dominance, tool contract requirement)
* `hash` (semantic hash)
* `execute`

This is the “thin vertical slice” that gives you a real platform.

---

If you want me to continue further, the next useful continuation is to specify **the actual TS implementation skeleton** for:

1. `canonicalEncode()` + `hashNode()` (with meta exclusion rules)
2. `Registry` loader + query (`apropos`/introspection)
3. `Kernel.evalFlow(flow, ctx)` with `KFrame` definitions
4. `LoggingPort` + `ReplayPort` decorators
5. A `PassRunner` pipeline and 3 lints (budget dominance, tool contract required, schema attached for structured output)

That’s the point where you can start replacing “docs” with “enforced invariants” and the architecture becomes self-maintaining.
