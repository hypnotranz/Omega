# 1200: Mock Infrastructure

## Status: COMPLETE ✅

## Purpose
Create comprehensive mock implementations for all providers, enabling isolated unit testing.

## Dependencies
- 120-providers.md ✅

## Source References
- Testing best practices
- Dependency injection patterns

---

## Deliverables

```
src/runtime/testing/
├── index.ts                 # Export all mocks
├── MockLLMProvider.ts       # Mock LLM responses
├── MockStateProvider.ts     # In-memory state
├── MockSessionProvider.ts   # In-memory sessions
├── TestFixtures.ts          # Common test data
└── TestHelpers.ts           # Testing utilities
```

---

## MockLLMProvider

```typescript
export interface MockLLMConfig {
  responses?: Map<string, string>;  // prompt pattern -> response
  defaultResponse?: string;
  latency?: number;                 // Simulated latency
  failAfter?: number;               // Fail after N calls
  tokenUsage?: { prompt: number; completion: number };
}

export class MockLLMProvider implements LLMProvider {
  private callCount = 0;
  private calls: LLMRequest[] = [];

  constructor(private config: MockLLMConfig = {}) {}

  async complete(request: LLMRequest): Promise<LLMResponse> {
    this.callCount++;
    this.calls.push(request);

    // Simulate failure
    if (this.config.failAfter && this.callCount > this.config.failAfter) {
      throw new Error('Mock LLM failure');
    }

    // Simulate latency
    if (this.config.latency) {
      await new Promise(r => setTimeout(r, this.config.latency));
    }

    // Find matching response
    const response = this.findResponse(request.prompt);

    return {
      content: response,
      usage: this.config.tokenUsage ?? { promptTokens: 10, completionTokens: 20, totalTokens: 30 },
      model: 'mock-model',
      finishReason: 'stop'
    };
  }

  private findResponse(prompt: string): string {
    if (this.config.responses) {
      for (const [pattern, response] of this.config.responses) {
        if (prompt.includes(pattern)) {
          return response;
        }
      }
    }
    return this.config.defaultResponse ?? 'Mock response';
  }

  // Test helpers
  getCalls(): LLMRequest[] {
    return [...this.calls];
  }

  getCallCount(): number {
    return this.callCount;
  }

  reset(): void {
    this.callCount = 0;
    this.calls = [];
  }

  setResponse(pattern: string, response: string): void {
    this.config.responses = this.config.responses ?? new Map();
    this.config.responses.set(pattern, response);
  }
}
```

---

## MockStateProvider

```typescript
export class MockStateProvider implements StateProvider {
  private snapshots = new Map<string, MachineSnapshot>();
  private artifacts = new Map<string, Artifact>();

  async saveSnapshot(id: string, snapshot: MachineSnapshot): Promise<void> {
    this.snapshots.set(id, structuredClone(snapshot));
  }

  async loadSnapshot(id: string): Promise<MachineSnapshot | undefined> {
    const snapshot = this.snapshots.get(id);
    return snapshot ? structuredClone(snapshot) : undefined;
  }

  async deleteSnapshot(id: string): Promise<boolean> {
    return this.snapshots.delete(id);
  }

  async listSnapshots(): Promise<string[]> {
    return Array.from(this.snapshots.keys());
  }

  async saveArtifact(key: string, artifact: Artifact): Promise<void> {
    this.artifacts.set(key, artifact);
  }

  async loadArtifact(key: string): Promise<Artifact | undefined> {
    return this.artifacts.get(key);
  }

  async deleteArtifact(key: string): Promise<boolean> {
    return this.artifacts.delete(key);
  }

  // Test helpers
  clear(): void {
    this.snapshots.clear();
    this.artifacts.clear();
  }

  getSnapshotCount(): number {
    return this.snapshots.size;
  }

  getArtifactCount(): number {
    return this.artifacts.size;
  }
}
```

---

## TestFixtures

```typescript
export const TestFixtures = {
  // Simple expressions
  expressions: {
    literal: '42',
    addition: '(+ 1 2)',
    nested: '(+ (* 2 3) (- 10 5))',
    lambda: '(lambda (x) (* x x))',
    application: '((lambda (x) (* x x)) 5)',
    ifTrue: '(if #t 1 2)',
    ifFalse: '(if #f 1 2)',
    let: '(let ((x 10)) (+ x 5))',
    define: '(define x 42)',
    recursive: '(define (fact n) (if (= n 0) 1 (* n (fact (- n 1)))))',
  },

  // AMB expressions
  amb: {
    simple: '(amb \'(1 2 3))',
    withRequire: '(let ((x (amb \'(1 2 3)))) (require (even? x)) x)',
    multiple: '(let ((x (amb \'(1 2))) (y (amb \'(a b)))) (list x y))',
  },

  // Stream expressions
  streams: {
    fromList: '(list->stream \'(1 2 3 4 5))',
    map: '(stream-map (lambda (x) (* x 2)) (list->stream \'(1 2 3)))',
    take: '(stream->list (stream-take naturals 5) 5)',
  },

  // Expected results
  results: {
    '42': 42,
    '(+ 1 2)': 3,
    '(+ (* 2 3) (- 10 5))': 11,
    '((lambda (x) (* x x)) 5)': 25,
    '(if #t 1 2)': 1,
    '(if #f 1 2)': 2,
    '(let ((x 10)) (+ x 5))': 15,
  },

  // Mock LLM responses
  llmResponses: new Map([
    ['translate hello', 'bonjour'],
    ['summarize', 'This is a summary.'],
    ['2 + 2', '4'],
  ]),
};
```

---

## TestHelpers

```typescript
export const TestHelpers = {
  /**
   * Create a runtime configured for testing.
   */
  async createTestRuntime(overrides?: Partial<RuntimeConfig>): Promise<OmegaRuntime> {
    return createRuntime({
      preset: 'testing',
      ...overrides,
      providers: {
        llmProvider: new MockLLMProvider({
          responses: TestFixtures.llmResponses
        }),
        stateProvider: new MockStateProvider(),
        sessionProvider: new MockSessionProvider(),
      }
    });
  },

  /**
   * Evaluate and expect result.
   */
  async evalExpect(
    runtime: OmegaRuntime,
    code: string,
    expected: Val
  ): Promise<void> {
    const result = await runtime.eval(code);
    expect(result.value).toEqual(expected);
  },

  /**
   * Evaluate and expect error.
   */
  async evalExpectError(
    runtime: OmegaRuntime,
    code: string,
    errorType?: string
  ): Promise<Error> {
    try {
      await runtime.eval(code);
      throw new Error('Expected error but got success');
    } catch (e) {
      if (errorType && e.name !== errorType) {
        throw new Error(`Expected ${errorType} but got ${e.name}`);
      }
      return e;
    }
  },

  /**
   * Wait for event.
   */
  waitForEvent<K extends keyof RuntimeEventMap>(
    runtime: OmegaRuntime,
    event: K,
    timeout = 5000
  ): Promise<RuntimeEventMap[K]> {
    return new Promise((resolve, reject) => {
      const timer = setTimeout(() => {
        reject(new Error(`Timeout waiting for event: ${event}`));
      }, timeout);

      runtime.on(event, (data) => {
        clearTimeout(timer);
        resolve(data);
      });
    });
  },

  /**
   * Collect all events of a type.
   */
  collectEvents<K extends keyof RuntimeEventMap>(
    runtime: OmegaRuntime,
    event: K
  ): { events: RuntimeEventMap[K][]; stop: () => void } {
    const events: RuntimeEventMap[K][] = [];
    const handler = (data: RuntimeEventMap[K]) => events.push(data);
    runtime.on(event, handler);
    return {
      events,
      stop: () => runtime.off(event, handler)
    };
  }
};
```

---

## Test Requirements

### Unit Tests
- [ ] MockLLMProvider returns configured responses
- [ ] MockLLMProvider tracks calls
- [ ] MockLLMProvider simulates failures
- [ ] MockStateProvider stores/retrieves snapshots
- [ ] MockStateProvider stores/retrieves artifacts
- [ ] TestHelpers.createTestRuntime works
- [ ] TestHelpers.evalExpect validates correctly
- [ ] TestHelpers.waitForEvent works

---

## Acceptance Criteria
1. Mocks fully implement provider interfaces
2. Mocks enable isolated testing
3. Test helpers reduce test boilerplate
4. Fixtures provide common test data
5. Mock behavior is configurable
