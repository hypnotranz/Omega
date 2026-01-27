# OPR Integration Plan: Making OPR a First-Class Effect in OmegaLLM

## Executive Summary

**What we have (90% complete):**
- OPR Runtime with retry loop, validation, receipt chain
- 10 kernels with proper FrameIR prompts (classify, extract, plan, review, etc.)
- Working adapters for OpenAI/Anthropic (tested in REPL)
- Hash utilities, receipts, validation

**What's missing (10% wiring):**
- Bridge layer to call OPR from Lisp via `(effect "opr.step" ...)`
- `valToJson/jsonToVal` conversion functions
- Shared LLM transport (optional but clean)

**This is NOT a big refactor.** The existing OPR code is solid. We're adding a thin integration layer.

---

## Architecture: Current vs Target

### Current (Disconnected)
```
┌─────────────────────┐     ┌─────────────────────┐
│   OmegaLLM CEKS     │     │     OPR System      │
│   ┌───────────┐     │     │   ┌───────────┐     │
│   │  Oracle   │─────┼──X──┼───│  Runtime  │     │
│   │  Adapter  │     │     │   │  Adapter  │     │
│   └───────────┘     │     │   └───────────┘     │
│        ↓            │     │        ↓            │
│   OpenAI API        │     │   OpenAI API        │ ← DUPLICATE!
└─────────────────────┘     └─────────────────────┘
```

### Target (Integrated)
```
┌───────────────────────────────────────────────────┐
│                   OmegaLLM CEKS                   │
│   ┌───────────────┐     ┌───────────────────┐    │
│   │  Oracle       │     │   OPR Effect      │    │
│   │  Effects      │     │   Handler         │    │
│   └───────┬───────┘     └─────────┬─────────┘    │
│           │                       │              │
│           ↓                       ↓              │
│   ┌─────────────────────────────────────────┐    │
│   │         Shared LLMTransport Port        │    │
│   └─────────────────────────────────────────┘    │
│                       ↓                          │
│               OpenAI/Anthropic API               │
└───────────────────────────────────────────────────┘
```

---

## Phase 1: Effect Bridge (MINIMAL VIABLE INTEGRATION)

### 1.1 Create `valToJson` / `jsonToVal` Bridge

**File:** `src/core/opr/bridge.ts`

```typescript
import type { Val } from '../eval/values';

/** Convert Lisp Val to JSON-compatible value */
export function valToJson(v: Val): unknown {
  switch (v.tag) {
    case 'Unit': return null;
    case 'Num': return v.n;
    case 'Bool': return v.b;
    case 'Str': return v.s;
    case 'Sym': return { $sym: v.name };
    case 'Vector': {
      // Check if it's a cons-cell list (2-element vectors chained to Unit)
      if (v.items.length === 2 && (v.items[1].tag === 'Vector' || v.items[1].tag === 'Unit')) {
        const arr: unknown[] = [];
        let cur: Val = v;
        while (cur.tag === 'Vector' && cur.items.length === 2) {
          arr.push(valToJson(cur.items[0]));
          cur = cur.items[1];
        }
        if (cur.tag === 'Unit') return arr;
      }
      // Regular vector/tuple
      return v.items.map(valToJson);
    }
    case 'Closure':
    case 'Native':
    case 'Cont':
    case 'Continuation':
    case 'OracleProc':
      return { $type: v.tag, $opaque: true };
    default:
      return { $unknown: JSON.stringify(v) };
  }
}

/** Convert JSON value back to Lisp Val */
export function jsonToVal(j: unknown): Val {
  if (j === null || j === undefined) return { tag: 'Unit' };
  if (typeof j === 'number') return { tag: 'Num', n: j };
  if (typeof j === 'boolean') return { tag: 'Bool', b: j };
  if (typeof j === 'string') return { tag: 'Str', s: j };
  if (Array.isArray(j)) {
    // Build cons-cell list
    let result: Val = { tag: 'Unit' };
    for (let i = j.length - 1; i >= 0; i--) {
      result = { tag: 'Vector', items: [jsonToVal(j[i]), result] };
    }
    return result;
  }
  if (typeof j === 'object') {
    // Check for symbol encoding
    if ('$sym' in j) return { tag: 'Sym', name: String((j as any).$sym) };
    // Convert object to alist: ((key1 . val1) (key2 . val2) ...)
    const entries = Object.entries(j);
    let result: Val = { tag: 'Unit' };
    for (let i = entries.length - 1; i >= 0; i--) {
      const [k, v] = entries[i];
      const pair: Val = { tag: 'Vector', items: [{ tag: 'Str', s: k }, jsonToVal(v)] };
      result = { tag: 'Vector', items: [pair, result] };
    }
    return result;
  }
  return { tag: 'Str', s: String(j) };
}
```

### 1.2 Create OPR Effect Handler

**File:** `src/core/opr/effectHandler.ts`

```typescript
import type { OpCall } from '../effects/opcall';
import type { Val } from '../eval/values';
import type { State } from '../eval/machine';
import { valToJson, jsonToVal } from './bridge';
import { OprRuntime } from './runtime';
import { createReceiptStore } from './receipts';
import { KERNEL_REGISTRY } from './kernels';
import type { OprLLMAdapter } from './adapters/types';

export interface OprEffectHandlerConfig {
  adapter: OprLLMAdapter;
  defaultBudget?: { maxAttempts: number };
}

/**
 * Handle OPR effects from CEKS machine
 *
 * Recognized ops:
 * - "opr.step" -> single kernel step
 * - "opr.fixpoint" -> run kernel to completion
 */
export async function handleOprEffect(
  opcall: OpCall,
  config: OprEffectHandlerConfig
): Promise<Val> {
  const { op, args } = opcall;

  // Parse effect name: "opr.step.kernelId" or "opr.fixpoint.kernelId"
  const parts = op.split('.');
  if (parts[0] !== 'opr') {
    throw new Error(`Not an OPR effect: ${op}`);
  }

  const mode = parts[1]; // 'step' or 'fixpoint'
  const kernelId = parts.slice(2).join('.'); // e.g., 'opr.classify.v1'

  // Find kernel
  const kernel = KERNEL_REGISTRY[kernelId];
  if (!kernel) {
    throw new Error(`Unknown kernel: ${kernelId}`);
  }

  // Extract program and state from args
  // Args: [program, state?]
  const program = args.length > 0 ? valToJson(args[0]) : {};
  const state = args.length > 1 ? valToJson(args[1]) : null;

  // Create runtime
  const receipts = createReceiptStore();
  const runtime = new OprRuntime({
    kernel,
    adapter: config.adapter,
    receipts,
    budget: config.defaultBudget ?? { maxAttempts: 3 },
  });

  // Execute
  if (mode === 'step') {
    const result = await runtime.step({ program, state });
    return jsonToVal({
      ok: result.ok,
      tag: result.tag,
      output: result.tag === 'ok' ? result.output : null,
      error: result.tag !== 'ok' ? { code: result.tag } : null,
      attempts: result.attempts,
      receipts: result.receipts,
    });
  } else if (mode === 'fixpoint') {
    const result = await runtime.runToFixpoint({ program, state });
    return jsonToVal({
      ok: result.tag === 'ok',
      tag: result.tag,
      results: result.tag === 'ok' ? result.results.map(r => r.output) : null,
      finalState: result.tag === 'ok' ? result.finalState : null,
      iterations: result.iterations,
      receipts: result.receipts,
    });
  } else {
    throw new Error(`Unknown OPR mode: ${mode}`);
  }
}

/**
 * Check if an effect op is an OPR effect
 */
export function isOprEffect(op: string): boolean {
  return op.startsWith('opr.step.') || op.startsWith('opr.fixpoint.');
}
```

### 1.3 Wire into Effect Dispatch

**File:** Update `src/core/eval/run.ts` (or create `src/core/eval/effectDispatch.ts`)

The main run loop handles `StepOutcome.Op` by dispatching to effect handlers:

```typescript
import { isOprEffect, handleOprEffect } from '../opr/effectHandler';

// In the main run loop when we get an OpCall:
async function dispatchEffect(opcall: OpCall, config: RunConfig): Promise<Val> {
  if (isOprEffect(opcall.op)) {
    return handleOprEffect(opcall, {
      adapter: config.oprAdapter, // injected
    });
  }

  // ... existing oracle effects ...
  if (opcall.op === 'oracle.apply.op') {
    return handleOracleApply(opcall, config);
  }

  throw new Error(`Unknown effect: ${opcall.op}`);
}
```

---

## Phase 2: Shared LLM Transport (CLEAN UNIFICATION)

### 2.1 Define LLMTransport Port

**File:** `src/core/llm/transport.ts`

```typescript
export interface LLMRequest {
  messages: Array<{ role: 'system' | 'user' | 'assistant'; content: string }>;
  model?: string;
  temperature?: number;
  maxTokens?: number;
  metadata?: Record<string, unknown>;
}

export interface LLMResponse {
  text: string;
  usage?: {
    promptTokens: number;
    completionTokens: number;
    totalTokens: number;
    estimatedCost: number;
  };
}

export interface LLMTransport {
  complete(request: LLMRequest): Promise<LLMResponse>;
  getModel(): string;
}
```

### 2.2 Implement Transport Adapters

**File:** `src/core/llm/openaiTransport.ts`

```typescript
import type { LLMTransport, LLMRequest, LLMResponse } from './transport';

export class OpenAITransport implements LLMTransport {
  constructor(private config: { apiKey: string; model: string; baseURL?: string }) {}

  async complete(request: LLMRequest): Promise<LLMResponse> {
    // Single implementation used by both Oracle and OPR
    const response = await fetch(`${this.config.baseURL ?? 'https://api.openai.com/v1'}/chat/completions`, {
      method: 'POST',
      headers: {
        'Authorization': `Bearer ${this.config.apiKey}`,
        'Content-Type': 'application/json',
      },
      body: JSON.stringify({
        model: request.model ?? this.config.model,
        messages: request.messages,
        temperature: request.temperature,
        max_tokens: request.maxTokens,
      }),
    });

    const data = await response.json();
    return {
      text: data.choices[0]?.message?.content ?? '',
      usage: data.usage ? {
        promptTokens: data.usage.prompt_tokens,
        completionTokens: data.usage.completion_tokens,
        totalTokens: data.usage.total_tokens,
        estimatedCost: this.estimateCost(data.usage),
      } : undefined,
    };
  }

  getModel(): string { return this.config.model; }

  private estimateCost(usage: any): number {
    // Rough estimates
    return (usage.prompt_tokens * 0.001 + usage.completion_tokens * 0.002) / 1000;
  }
}
```

### 2.3 Adapt OPR to Use Transport

**File:** `src/core/opr/adapters/transportAdapter.ts`

```typescript
import type { OprLLMAdapter, OprLLMRequest } from './types';
import type { LLMTransport } from '../../llm/transport';

/**
 * OPR adapter that delegates to shared LLMTransport
 */
export class TransportOprAdapter implements OprLLMAdapter {
  constructor(private transport: LLMTransport) {}

  async complete(request: OprLLMRequest): Promise<string> {
    // Build messages from FrameIR prompt
    const messages: Array<{ role: 'system' | 'user' | 'assistant'; content: string }> = [];

    if (request.prompt?.parts) {
      for (const part of request.prompt.parts) {
        if ((part as any).tag === 'PSystem') {
          messages.push({ role: 'system', content: (part as any).text });
        }
      }
    }

    messages.push({ role: 'user', content: request.userContent });

    const response = await this.transport.complete({
      messages,
      temperature: request.temperature,
      maxTokens: request.maxTokens,
    });

    return response.text;
  }

  getModel(): string {
    return this.transport.getModel();
  }
}
```

---

## Phase 3: Working Demo

### 3.1 Lisp Surface Area

Add to stdlib or load at REPL startup:

```lisp
;; OPR convenience functions
(define (opr-classify input categories)
  (effect "opr.step.opr.classify.v1"
    (list (cons 'input input)
          (cons 'categories categories))))

(define (opr-extract input schema)
  (effect "opr.step.opr.extract.v1"
    (list (cons 'input input)
          (cons 'schema schema))))

(define (opr-plan goal constraints)
  (effect "opr.step.opr.plan.v1"
    (list (cons 'goal goal)
          (cons 'constraints constraints))))
```

### 3.2 Demo Script

**File:** `demos/opr-integration-demo.ts`

```typescript
import { compile } from '../src/core/compiler';
import { run } from '../src/core/eval/run';
import { OpenAITransport } from '../src/core/llm/openaiTransport';
import { TransportOprAdapter } from '../src/core/opr/adapters/transportAdapter';

async function demo() {
  // Shared transport
  const transport = new OpenAITransport({
    apiKey: process.env.OPENAI_API_KEY!,
    model: 'gpt-4o-mini',
  });

  // OPR adapter using shared transport
  const oprAdapter = new TransportOprAdapter(transport);

  // Compile and run Lisp code that uses OPR
  const code = `
    (begin
      ;; Classify an error message
      (define result
        (effect "opr.step.opr.classify.v1"
          '((input . "TypeError: Cannot read property 'x' of undefined")
            (categories . ("bug" "feature" "question" "docs")))))

      ;; Extract the classification
      (car (cdr (assoc 'output result))))
  `;

  const expr = compile(code);
  const result = await run(expr, {
    oprAdapter,
    // ... other config
  });

  console.log('Classification result:', result);
}

demo().catch(console.error);
```

---

## Implementation Order

1. **Today (30 min):** Create `bridge.ts` with `valToJson/jsonToVal`
2. **Today (30 min):** Create `effectHandler.ts`
3. **Today (30 min):** Wire into effect dispatch
4. **Today (15 min):** Add test proving it works
5. **Later (optional):** Phase 2 shared transport cleanup

---

## What's Already Done (No Changes Needed)

- `src/core/opr/runtime.ts` - Full runtime with retry, validation
- `src/core/opr/validate.ts` - Schema validation
- `src/core/opr/receipts.ts` - Receipt chain
- `src/core/opr/hash.ts` - Hash utilities
- `src/core/opr/retry.ts` - Counterexample feedback
- `src/core/opr/kernels/*.ts` - 10 kernels ready to use
- `src/core/opr/adapters/openai.ts` - Works (tested)
- `src/core/opr/adapters/anthropic.ts` - Works (tested)

---

## Summary

| Component | Status | Effort |
|-----------|--------|--------|
| OPR Runtime | ✅ Complete | 0 |
| Kernels (10) | ✅ Complete | 0 |
| Adapters | ✅ Complete | 0 |
| Validation | ✅ Complete | 0 |
| Receipts | ✅ Complete | 0 |
| `valToJson/jsonToVal` | 🔧 To create | 30 min |
| Effect handler | 🔧 To create | 30 min |
| Wire to dispatch | 🔧 To create | 30 min |
| Test/demo | 🔧 To create | 15 min |
| Shared transport | 📋 Optional cleanup | 1 hr |

**Total new code: ~200 lines**
**Total effort: ~2 hours**

This is NOT a rewrite. It's wiring.
