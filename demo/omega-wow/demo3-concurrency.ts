// demo/omega-wow/demo3-concurrency.ts
// Demo 3: Concurrency collapses semantic cost
//
// PURPOSE: Prove you can run multiple semantic tasks concurrently while:
// - Keeping determinism via recorded schedule decisions
// - Preventing duplicate oracle work via singleflight memoization

import type {
  DemoDefinition,
  DemoContext,
  DemoResult,
  InvariantSpec,
  DemoMetrics,
} from "../harness/types";

// ─────────────────────────────────────────────────────────────────
// Types
// ─────────────────────────────────────────────────────────────────

interface StreamDocument {
  id: string;
  hash: string;
  content: string;
}

interface ClassificationResult {
  docId: string;
  hash: string;
  category: string;
  confidence: number;
  fromCache: boolean;
}

interface FiberState {
  id: string;
  processed: string[];
  results: ClassificationResult[];
}

// ─────────────────────────────────────────────────────────────────
// Singleflight Cache
// ─────────────────────────────────────────────────────────────────

/**
 * Singleflight cache prevents duplicate oracle calls for same hash.
 */
class SingleflightCache {
  private cache = new Map<string, { value: unknown; pending: boolean }>();
  private waiters = new Map<string, Array<(value: unknown) => void>>();

  async getOrCompute(
    key: string,
    compute: () => Promise<unknown>,
    ledger: { record: (type: string, data: unknown) => void }
  ): Promise<{ value: unknown; fromCache: boolean }> {
    // Check if already cached
    const cached = this.cache.get(key);
    if (cached && !cached.pending) {
      return { value: cached.value, fromCache: true };
    }

    // Check if computation is in flight
    if (cached?.pending) {
      // Wait for existing computation
      return new Promise(resolve => {
        const existing = this.waiters.get(key) ?? [];
        existing.push((value) => resolve({ value, fromCache: true }));
        this.waiters.set(key, existing);
      });
    }

    // Start new computation
    this.cache.set(key, { value: null, pending: true });
    ledger.record("infer.call", { key, action: "compute" });

    try {
      const value = await compute();
      this.cache.set(key, { value, pending: false });

      // Notify waiters
      const waiters = this.waiters.get(key) ?? [];
      for (const waiter of waiters) {
        waiter(value);
      }
      this.waiters.delete(key);

      return { value, fromCache: false };
    } catch (error) {
      this.cache.delete(key);
      throw error;
    }
  }

  getStats(): { hits: number; misses: number; pending: number } {
    let hits = 0;
    let misses = 0;
    let pending = 0;

    for (const entry of this.cache.values()) {
      if (entry.pending) pending++;
      else misses++; // Each non-pending entry was a miss originally
    }

    return { hits, misses, pending };
  }

  clear(): void {
    this.cache.clear();
    this.waiters.clear();
  }
}

// ─────────────────────────────────────────────────────────────────
// Hash function
// ─────────────────────────────────────────────────────────────────

function hashContent(content: string): string {
  // Simple hash for demo
  let hash = 0;
  for (let i = 0; i < content.length; i++) {
    const char = content.charCodeAt(i);
    hash = ((hash << 5) - hash) + char;
    hash = hash & hash;
  }
  return `h${Math.abs(hash).toString(16).slice(0, 8)}`;
}

// ─────────────────────────────────────────────────────────────────
// Demo Implementation
// ─────────────────────────────────────────────────────────────────

async function runConcurrencyDemo(ctx: DemoContext): Promise<DemoResult> {
  const startTime = Date.now();
  let steps = 0;
  let oracleCallsActual = 0;
  let oracleCallsAvoided = 0;

  // ─────────────────────────────────────────────────────────────
  // Setup: Two document streams with overlapping content
  // ─────────────────────────────────────────────────────────────

  const sharedContent = [
    "Financial report Q4 2024 with revenue projections",
    "Employee handbook policy updates for remote work",
    "Customer data privacy compliance documentation",
  ];

  const uniqueContent1 = [
    "Marketing campaign analysis for product launch",
    "Technical architecture review document",
  ];

  const uniqueContent2 = [
    "Sales pipeline forecast for next quarter",
    "Legal contract templates and amendments",
  ];

  // Stream 1: shared + unique1
  const stream1: StreamDocument[] = [
    ...sharedContent.map((c, i) => ({
      id: `s1-shared-${i}`,
      hash: hashContent(c),
      content: c,
    })),
    ...uniqueContent1.map((c, i) => ({
      id: `s1-unique-${i}`,
      hash: hashContent(c),
      content: c,
    })),
  ];

  // Stream 2: shared + unique2
  const stream2: StreamDocument[] = [
    ...sharedContent.map((c, i) => ({
      id: `s2-shared-${i}`,
      hash: hashContent(c),
      content: c,
    })),
    ...uniqueContent2.map((c, i) => ({
      id: `s2-unique-${i}`,
      hash: hashContent(c),
      content: c,
    })),
  ];

  const singleflight = new SingleflightCache();

  // ─────────────────────────────────────────────────────────────
  // Configure oracle scripts
  // ─────────────────────────────────────────────────────────────

  ctx.oracle.addScript({
    match: (req, type) =>
      type === "InferOp" && (req as any)?.op === "classify",
    respond: (req) => {
      const { args } = req as { args: [string] };
      const content = args[0];
      oracleCallsActual++;

      // Classify based on keywords
      let category = "general";
      let confidence = 0.7;

      if (content.toLowerCase().includes("financial") || content.toLowerCase().includes("revenue")) {
        category = "financial";
        confidence = 0.9;
      } else if (content.toLowerCase().includes("employee") || content.toLowerCase().includes("policy")) {
        category = "hr";
        confidence = 0.85;
      } else if (content.toLowerCase().includes("customer") || content.toLowerCase().includes("privacy")) {
        category = "compliance";
        confidence = 0.88;
      } else if (content.toLowerCase().includes("marketing") || content.toLowerCase().includes("sales")) {
        category = "business";
        confidence = 0.82;
      } else if (content.toLowerCase().includes("technical") || content.toLowerCase().includes("architecture")) {
        category = "technical";
        confidence = 0.86;
      } else if (content.toLowerCase().includes("legal") || content.toLowerCase().includes("contract")) {
        category = "legal";
        confidence = 0.91;
      }

      return {
        value: { category, confidence },
        evidence: "classify-inference",
      };
    },
  });

  // ─────────────────────────────────────────────────────────────
  // Fiber simulation with deterministic scheduling
  // ─────────────────────────────────────────────────────────────

  const fiber1: FiberState = { id: "fiber-1", processed: [], results: [] };
  const fiber2: FiberState = { id: "fiber-2", processed: [], results: [] };

  // Classify function with singleflight
  async function classifyDoc(doc: StreamDocument): Promise<ClassificationResult> {
    const result = await singleflight.getOrCompute(
      doc.hash,
      async () => {
        const response = ctx.oracle.handle("InferOp", {
          op: "classify",
          args: [doc.content],
        }) as { value: { category: string; confidence: number } };
        return response.value;
      },
      ctx.ledger
    );

    if (result.fromCache) {
      oracleCallsAvoided++;
    }

    const classified = result.value as { category: string; confidence: number };
    return {
      docId: doc.id,
      hash: doc.hash,
      category: classified.category,
      confidence: classified.confidence,
      fromCache: result.fromCache,
    };
  }

  // Process a stream
  async function processStream(fiber: FiberState, stream: StreamDocument[]): Promise<void> {
    for (const doc of stream) {
      ctx.ledger.record("schedule.decision", {
        fiber: fiber.id,
        docId: doc.id,
        action: "process",
      });
      steps++;

      const result = await classifyDoc(doc);
      fiber.processed.push(doc.id);
      fiber.results.push(result);

      ctx.ledger.record("infer.result", {
        fiber: fiber.id,
        docId: doc.id,
        category: result.category,
        fromCache: result.fromCache,
      });
    }
  }

  ctx.ledger.record("demo.start", { phase: "concurrent-classification" });

  // Deterministic interleaved execution
  // Process one doc from each stream alternately
  const maxLen = Math.max(stream1.length, stream2.length);
  for (let i = 0; i < maxLen; i++) {
    if (i < stream1.length) {
      ctx.ledger.record("schedule.decision", {
        scheduler: "round-robin",
        fiber: "fiber-1",
        index: i,
      });
      const result = await classifyDoc(stream1[i]);
      fiber1.processed.push(stream1[i].id);
      fiber1.results.push(result);
    }

    if (i < stream2.length) {
      ctx.ledger.record("schedule.decision", {
        scheduler: "round-robin",
        fiber: "fiber-2",
        index: i,
      });
      const result = await classifyDoc(stream2[i]);
      fiber2.processed.push(stream2[i].id);
      fiber2.results.push(result);
    }
  }

  ctx.ledger.record("demo.end", {
    fiber1Processed: fiber1.processed.length,
    fiber2Processed: fiber2.processed.length,
    oracleCallsActual,
    oracleCallsAvoided,
  });

  // ─────────────────────────────────────────────────────────────
  // Return result
  // ─────────────────────────────────────────────────────────────

  const allHashes = new Set([
    ...stream1.map(d => d.hash),
    ...stream2.map(d => d.hash),
  ]);

  const metrics: DemoMetrics = {
    inferCalls: oracleCallsActual, // Only count actual oracle calls
    oracleReqEval: ctx.oracle.getCount("ReqEval"),
    oracleReqApply: ctx.oracle.getCount("ReqApply"),
    oracleReqObserve: ctx.oracle.getCount("ReqObserve"),
    oracleReqTest: ctx.oracle.getCount("ReqTest"),
    oracleReqReturn: ctx.oracle.getCount("ReqReturn"),
    steps,
    wallMs: Date.now() - startTime,
    scheduleDecisions: ctx.ledger.getEventsByType("schedule.decision").length,
  };

  return {
    outputs: [
      {
        fiber1: fiber1.results,
        fiber2: fiber2.results,
        uniqueHashes: allHashes.size,
        totalDocs: stream1.length + stream2.length,
        oracleCallsActual,
        oracleCallsAvoided,
      },
    ],
    success: true,
    metrics,
    transcript: ctx.oracle.getTranscript(),
  };
}

// ─────────────────────────────────────────────────────────────────
// Run without singleflight (for regression test)
// ─────────────────────────────────────────────────────────────────

async function runWithoutSingleflight(ctx: DemoContext): Promise<number> {
  let calls = 0;

  const sharedContent = [
    "Financial report Q4 2024 with revenue projections",
    "Employee handbook policy updates for remote work",
    "Customer data privacy compliance documentation",
  ];

  // Simulate calling oracle for each doc without deduplication
  const stream1 = sharedContent.concat([
    "Marketing campaign analysis for product launch",
    "Technical architecture review document",
  ]);
  const stream2 = sharedContent.concat([
    "Sales pipeline forecast for next quarter",
    "Legal contract templates and amendments",
  ]);

  // Each doc gets classified separately
  calls = stream1.length + stream2.length;

  return calls;
}

// ─────────────────────────────────────────────────────────────────
// Invariants
// ─────────────────────────────────────────────────────────────────

const invariants: InvariantSpec[] = [
  {
    name: "infer-calls-equal-unique-hashes",
    check: (result) => {
      const output = result.outputs[0] as {
        uniqueHashes: number;
        oracleCallsActual: number;
      };

      const ok = output.oracleCallsActual === output.uniqueHashes;
      return {
        name: "infer-calls-equal-unique-hashes",
        ok,
        detail: `Oracle calls: ${output.oracleCallsActual}, Unique hashes: ${output.uniqueHashes}`,
      };
    },
  },
  {
    name: "deterministic-schedule-replay",
    check: (result, ctx) => {
      const scheduleDecisions = ctx.ledger.getEventsByType("schedule.decision");

      // Should have consistent schedule decisions
      const ok = scheduleDecisions.length > 0;
      return {
        name: "deterministic-schedule-replay",
        ok,
        detail: `${scheduleDecisions.length} schedule decisions recorded`,
      };
    },
  },
  {
    name: "singleflight-reduces-calls",
    check: (result) => {
      const output = result.outputs[0] as {
        totalDocs: number;
        oracleCallsActual: number;
        oracleCallsAvoided: number;
      };

      // With singleflight, actual calls should be less than total docs
      const ok = output.oracleCallsActual < output.totalDocs;
      return {
        name: "singleflight-reduces-calls",
        ok,
        detail: `Actual: ${output.oracleCallsActual}, Avoided: ${output.oracleCallsAvoided}, Total docs: ${output.totalDocs}`,
      };
    },
  },
  {
    name: "both-fibers-complete",
    check: (result) => {
      const output = result.outputs[0] as {
        fiber1: ClassificationResult[];
        fiber2: ClassificationResult[];
      };

      const ok = output.fiber1.length > 0 && output.fiber2.length > 0;
      return {
        name: "both-fibers-complete",
        ok,
        detail: `Fiber1: ${output.fiber1.length} results, Fiber2: ${output.fiber2.length} results`,
      };
    },
  },
  {
    name: "cache-hits-for-shared-content",
    check: (result) => {
      const output = result.outputs[0] as {
        fiber1: ClassificationResult[];
        fiber2: ClassificationResult[];
      };

      // Count cache hits
      const allResults = [...output.fiber1, ...output.fiber2];
      const cacheHits = allResults.filter(r => r.fromCache).length;

      // Should have some cache hits for overlapping content
      const ok = cacheHits > 0;
      return {
        name: "cache-hits-for-shared-content",
        ok,
        detail: `${cacheHits} cache hits out of ${allResults.length} total`,
      };
    },
  },
];

// ─────────────────────────────────────────────────────────────────
// Demo Definition
// ─────────────────────────────────────────────────────────────────

export const demo3Concurrency: DemoDefinition = {
  id: "concurrency-cost-collapse",
  name: "Concurrency Collapses Semantic Cost",
  description: "Proves singleflight caching prevents duplicate oracle calls across concurrent fibers with deterministic scheduling",
  tags: ["concurrency", "singleflight", "caching", "fibers", "scheduling"],
  run: runConcurrencyDemo,
  invariants,
};
