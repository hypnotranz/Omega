# 800: LLM Integration

## Status: COMPLETE ✅
- **Completed**: 2026-01-29
- **Location**: OmegaLLM-3/src/runtime/subsystems/LLMIntegration.ts (708 lines)

## Purpose
Core LLM integration providing the `infer.op` and `search.op` effects for semantic computation.

## Dependencies
- 110-events.md ✅
- 210-execution.md ✅
- 230-budget-llm-adapter.md ✅

## Source References
- ARCHITECTURE/30-EFFECTS.md
- docs/USER-MANUAL--02--Semantic-Effects.md
- docs/USER-MANUAL--04--Multiple-Worlds.md

---

## Deliverables

```
src/runtime/subsystems/
├── LLMIntegration.ts        # Main LLM integration
└── llm/
    ├── InferEffect.ts       # infer.op implementation
    ├── SearchEffect.ts      # search.op implementation
    ├── PromptBuilder.ts     # Prompt construction
    └── ResponseParser.ts    # Response parsing
```

---

## Key Types

```typescript
export interface LLMRequest {
  prompt: string;
  parameters?: LLMParameters;
  context?: LLMContext;
}

export interface LLMParameters {
  temperature?: number;      // 0-2, default 0.7
  maxTokens?: number;
  topP?: number;
  stopSequences?: string[];
  model?: string;
}

export interface LLMContext {
  systemPrompt?: string;
  examples?: { input: string; output: string }[];
  history?: { role: 'user' | 'assistant'; content: string }[];
}

export interface LLMResponse {
  content: string;
  usage?: {
    promptTokens: number;
    completionTokens: number;
    totalTokens: number;
  };
  model?: string;
  finishReason?: 'stop' | 'length' | 'content_filter';
}

export interface SearchResult {
  denotation: Val;           // Parsed value
  weight: number;            // Log probability
  raw: string;               // Raw response
}

export type Distribution = Map<Val, number>; // Value -> weight
```

---

## Key Interface

```typescript
export interface LLMIntegration {
  // ─── Effects ───

  /**
   * Perform inference (infer.op).
   * Single deterministic LLM call.
   */
  infer(prompt: string, params?: LLMParameters): Promise<Val>;

  /**
   * Perform search (search.op).
   * Multiple samples for probability distribution.
   */
  search(prompt: string, params?: LLMParameters & {
    numSamples?: number;
  }): Promise<Distribution>;

  // ─── Configuration ───

  /**
   * Set default parameters.
   */
  setDefaults(params: Partial<LLMParameters>): void;

  /**
   * Set system prompt.
   */
  setSystemPrompt(prompt: string): void;

  /**
   * Add few-shot examples.
   */
  addExamples(examples: { input: string; output: string }[]): void;

  /**
   * Clear context.
   */
  clearContext(): void;

  // ─── Low-level ───

  /**
   * Raw completion (bypasses effects).
   */
  complete(request: LLMRequest): Promise<LLMResponse>;

  /**
   * Stream completion.
   */
  stream(request: LLMRequest): AsyncIterable<string>;
}
```

---

## Effect Implementations

### infer.op

```typescript
async function inferEffect(
  prompt: string,
  params?: LLMParameters
): Promise<Val> {
  // Build full prompt
  const fullPrompt = buildPrompt(prompt, this.context);

  // Use budget adapter for tracking
  const response = await this.budgetAdapter.wrapCall(
    'infer.op',
    estimateTokens(fullPrompt),
    () => this.provider.complete({
      prompt: fullPrompt,
      parameters: { ...this.defaults, ...params }
    })
  );

  // Parse response
  const parsed = parseResponse(response.content);

  // Record provenance
  this.provenance.record({
    type: 'llm-call',
    value: parsed,
    operation: 'infer.op',
    prompt: fullPrompt,
    model: response.model
  });

  // Emit event
  this.emitter.emit('infer-complete', {
    prompt: fullPrompt,
    result: parsed,
    usage: response.usage
  });

  return parsed;
}
```

### search.op

```typescript
async function searchEffect(
  prompt: string,
  params?: LLMParameters & { numSamples?: number }
): Promise<Distribution> {
  const numSamples = params?.numSamples ?? 5;
  const distribution: Distribution = new Map();

  // Collect samples
  const samples: SearchResult[] = [];
  for (let i = 0; i < numSamples; i++) {
    const response = await this.budgetAdapter.wrapCall(
      'search.op',
      estimateTokens(prompt),
      () => this.provider.complete({
        prompt,
        parameters: {
          ...this.defaults,
          ...params,
          temperature: params?.temperature ?? 1.0 // Higher for diversity
        }
      })
    );

    samples.push({
      denotation: parseResponse(response.content),
      weight: 1.0 / numSamples, // Uniform for now
      raw: response.content
    });
  }

  // Build distribution
  for (const sample of samples) {
    const key = stringify(sample.denotation);
    const existing = distribution.get(key) ?? 0;
    distribution.set(key, existing + sample.weight);
  }

  // Emit event
  this.emitter.emit('search-complete', {
    prompt,
    samples: samples.length,
    uniqueResults: distribution.size
  });

  return distribution;
}
```

---

## Response Parsing

```typescript
/**
 * Parse LLM response into Lisp value.
 * Handles:
 * - Lisp literals: numbers, strings, symbols, lists
 * - JSON (converted to Lisp)
 * - Plain text (as string)
 */
function parseResponse(content: string): Val {
  const trimmed = content.trim();

  // Try Lisp literal
  try {
    return parseLisp(trimmed);
  } catch (e) {
    // Not valid Lisp
  }

  // Try JSON
  try {
    return jsonToLisp(JSON.parse(trimmed));
  } catch (e) {
    // Not valid JSON
  }

  // Return as string
  return trimmed;
}

function jsonToLisp(json: unknown): Val {
  if (json === null) return null;
  if (typeof json === 'boolean') return json;
  if (typeof json === 'number') return json;
  if (typeof json === 'string') return json;
  if (Array.isArray(json)) return json.map(jsonToLisp);
  if (typeof json === 'object') {
    // Convert to alist
    return Object.entries(json).map(([k, v]) => [k, jsonToLisp(v)]);
  }
  throw new Error(`Cannot convert to Lisp: ${typeof json}`);
}
```

---

## Lisp Interface

```lisp
;; Simple inference
(effect infer.op (list "What is 2 + 2?"))
; => "4"

;; With parameters
(effect infer.op
  (list "Creative story about: " topic)
  :temperature 1.2
  :max-tokens 500)

;; Search for multiple samples
(effect search.op
  (list "Translate 'hello' to French")
  :num-samples 5)
; => Distribution: {"bonjour" 0.7, "salut" 0.2, "coucou" 0.1}

;; Get most likely result
(dist-mode (effect search.op ...))
; => "bonjour"

;; Get all possibilities with weights
(dist-support (effect search.op ...))
; => (("bonjour" 0.7) ("salut" 0.2) ("coucou" 0.1))
```

---

## Test Requirements

### Unit Tests: `tests/runtime/subsystems/LLMIntegration.test.ts`
- [ ] infer() returns parsed value
- [ ] infer() uses budget adapter
- [ ] search() returns distribution
- [ ] search() samples correctly
- [ ] parseResponse() handles Lisp
- [ ] parseResponse() handles JSON
- [ ] parseResponse() handles text
- [ ] setDefaults() applies to calls
- [ ] setSystemPrompt() prepends
- [ ] Events emit correctly

### Integration Tests
- [ ] Full infer.op effect flow
- [ ] Full search.op effect flow
- [ ] Budget tracking works
- [ ] Provenance recording works
- [ ] Artifact caching works

---

## Acceptance Criteria
1. infer.op provides deterministic single response
2. search.op provides probability distribution
3. All calls go through budget adapter
4. Provenance is recorded for all calls
5. Response parsing handles common formats
6. Events enable monitoring and logging
