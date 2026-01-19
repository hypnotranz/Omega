// test/oracle/parmenides.e2e.spec.ts
// E2E Test: Ask each LLM for Parmenides' academic lineage
//
// This test demonstrates the plugin system by querying multiple LLMs
// with the same philosophical question and comparing their responses.

import { describe, it, expect, beforeAll } from "vitest";
import {
  registry,
  createAdapterFromModel,
  createModelSelector,
  withModel,
  withPlugin,
} from "../../src/core/oracle/plugins";
import type { OracleAdapter } from "../../src/core/oracle/adapter";
import type { OracleResp, OracleReq } from "../../src/core/oracle/protocol";
import type { MeaningVal } from "../../src/core/oracle/meaning";
import type { Val } from "../../src/core/eval/values";

// ═══════════════════════════════════════════════════════════════════════════
// TEST CONFIGURATION
// ═══════════════════════════════════════════════════════════════════════════

const PARMENIDES_QUERY = `Who were the academic students (philosophical descendants) of Parmenides of Elea?
List the direct students and their students, going as far as you know.
Format as a structured list.`;

// Models to test (will skip unavailable ones)
const TEST_MODELS = [
  { plugin: "anthropic", model: "claude-3-haiku-20240307", envVar: "ANTHROPIC_API_KEY" },
  { plugin: "openai", model: "gpt-4o-mini", envVar: "OPENAI_API_KEY" },
  { plugin: "ollama", model: "llama3.2:1b", envVar: null }, // Ollama doesn't need key
];

// ═══════════════════════════════════════════════════════════════════════════
// HELPER: Run Parmenides query
// ═══════════════════════════════════════════════════════════════════════════

async function queryParmenides(
  adapter: OracleAdapter,
  timeoutMs: number = 120000
): Promise<{ answer: string; confidence: number; trace: string }> {
  const session = adapter.startSession({
    tag: "Infer",
    payload: { tag: "Str", s: PARMENIDES_QUERY },
    envRef: "parmenides-env",
    stateRef: "parmenides-state",
  });

  const startTime = Date.now();
  let resp: OracleResp = { tag: "RespAck" };
  let turns = 0;

  while (true) {
    // Check timeout
    if (Date.now() - startTime > timeoutMs) {
      throw new Error(`Query timed out after ${timeoutMs}ms`);
    }

    turns++;
    const step = await session.next(resp);

    if (step.done) {
      const meaning = step.value as MeaningVal;
      const answer = extractAnswer(meaning.denotation);
      return {
        answer,
        confidence: meaning.confidence ?? 0,
        trace: extractTrace(meaning.trace),
      };
    }

    // Handle oracle requests
    const req = step.value as OracleReq;
    if (req.tag === "ReqEval") {
      // For Parmenides query, we don't need real eval - just echo back
      resp = {
        tag: "RespVal",
        value: { tag: "Str", s: `[Lisp evaluated: ${req.qexpr}]` },
      };
    } else if (req.tag === "ReqObserve") {
      resp = {
        tag: "RespObs",
        data: { state: "running", query: "parmenides" },
      };
    } else {
      resp = { tag: "RespAck" };
    }

    // Safety limit
    if (turns > 30) {
      throw new Error(`Too many turns (${turns})`);
    }
  }
}

function extractAnswer(denotation: Val | undefined): string {
  if (!denotation) return "";
  if (denotation.tag === "Str") return denotation.s;
  if (denotation.tag === "Map") {
    // Look for a "value" or "answer" key
    for (const [k, v] of denotation.entries) {
      if (k.tag === "Str" && (k.s === "value" || k.s === "answer")) {
        if (v.tag === "Str") return v.s;
      }
    }
    return JSON.stringify(denotation.entries);
  }
  return JSON.stringify(denotation);
}

function extractTrace(trace: Val | undefined): string {
  if (!trace) return "";
  if (trace.tag === "Str") return trace.s;
  return JSON.stringify(trace);
}

// ═══════════════════════════════════════════════════════════════════════════
// AVAILABILITY CHECKS
// ═══════════════════════════════════════════════════════════════════════════

let ollamaAvailable = false;

beforeAll(async () => {
  // Check Ollama availability
  try {
    const resp = await fetch("http://localhost:11434/api/tags", { signal: AbortSignal.timeout(5000) });
    ollamaAvailable = resp.ok;
  } catch {
    ollamaAvailable = false;
  }
});

function isModelAvailable(testModel: typeof TEST_MODELS[0]): boolean {
  if (testModel.envVar && !process.env[testModel.envVar]) {
    return false;
  }
  if (testModel.plugin === "ollama" && !ollamaAvailable) {
    return false;
  }
  return true;
}

// ═══════════════════════════════════════════════════════════════════════════
// E2E TESTS
// ═══════════════════════════════════════════════════════════════════════════

describe("Parmenides Family Tree E2E", () => {
  // Test each model individually
  for (const testModel of TEST_MODELS) {
    describe(`${testModel.plugin}/${testModel.model}`, () => {
      it.skipIf(!isModelAvailable(testModel))(
        "answers the Parmenides query",
        async () => {
          console.log(`\n🔮 Querying ${testModel.plugin}:${testModel.model}...`);

          const adapter = createAdapterFromModel(
            `${testModel.plugin}:${testModel.model}`
          );

          const result = await queryParmenides(adapter);

          console.log(`📜 Answer from ${testModel.plugin}:`);
          console.log(result.answer.slice(0, 500) + (result.answer.length > 500 ? "..." : ""));
          console.log(`🎯 Confidence: ${result.confidence}`);
          console.log(`📍 Trace: ${result.trace}`);

          // Verify we got a meaningful response
          expect(result.answer.length).toBeGreaterThan(10);

          // Check for expected names (Parmenides' known students)
          const answerLower = result.answer.toLowerCase();
          const expectedNames = ["zeno", "melissus", "empedocles"];
          const foundNames = expectedNames.filter((name) =>
            answerLower.includes(name)
          );

          // At least one known student should be mentioned
          expect(foundNames.length).toBeGreaterThan(0);

          // Confidence should be reasonable
          expect(result.confidence).toBeGreaterThan(0);
        },
        180000 // 3 minute timeout for slow models
      );
    });
  }

  // Test model selection from Lisp
  describe("Model selection via payload", () => {
    const hasAnyKey = !!(process.env.ANTHROPIC_API_KEY || process.env.OPENAI_API_KEY);

    it.skipIf(!hasAnyKey)("routes to specified model via withModel helper", async () => {
      const selector = createModelSelector();

      // Build payload with explicit model
      const model = process.env.ANTHROPIC_API_KEY
        ? "anthropic:claude-3-haiku-20240307"
        : "openai:gpt-4o-mini";

      const payload = withModel(model, { tag: "Str", s: "Who was Zeno of Elea?" });

      const session = selector.startSession({
        tag: "Infer",
        payload,
        envRef: "test-env",
        stateRef: "test-state",
      });

      let resp: OracleResp = { tag: "RespAck" };
      let turns = 0;

      while (true) {
        turns++;
        const step = await session.next(resp);

        if (step.done) {
          const meaning = step.value as MeaningVal;
          expect(meaning.tag).toBe("Meaning");
          const answer = extractAnswer(meaning.denotation);

          // Should mention Parmenides or Elea
          const answerLower = answer.toLowerCase();
          const mentionsContext = answerLower.includes("parmenides") ||
                                  answerLower.includes("elea") ||
                                  answerLower.includes("paradox");
          expect(mentionsContext).toBe(true);
          break;
        }

        const req = step.value as OracleReq;
        if (req.tag === "ReqEval") {
          resp = { tag: "RespVal", value: { tag: "Str", s: "ok" } };
        } else {
          resp = { tag: "RespAck" };
        }

        if (turns > 30) throw new Error("Too many turns");
      }
    }, 120000);

    it.skipIf(!hasAnyKey)("routes to specified plugin via withPlugin helper", async () => {
      const selector = createModelSelector();

      const pluginId = process.env.ANTHROPIC_API_KEY ? "anthropic" : "openai";
      const model = process.env.ANTHROPIC_API_KEY
        ? "claude-3-haiku-20240307"
        : "gpt-4o-mini";

      const payload = withPlugin(pluginId, model, { tag: "Str", s: "Name one of Parmenides' students." });

      const session = selector.startSession({
        tag: "Infer",
        payload,
        envRef: "test-env",
        stateRef: "test-state",
      });

      let resp: OracleResp = { tag: "RespAck" };
      let turns = 0;

      while (true) {
        turns++;
        const step = await session.next(resp);

        if (step.done) {
          const meaning = step.value as MeaningVal;
          const answer = extractAnswer(meaning.denotation);

          // Should mention Zeno or Melissus
          const answerLower = answer.toLowerCase();
          const mentionsStudent = answerLower.includes("zeno") || answerLower.includes("melissus");
          expect(mentionsStudent).toBe(true);
          break;
        }

        const req = step.value as OracleReq;
        if (req.tag === "ReqEval") {
          resp = { tag: "RespVal", value: { tag: "Str", s: "ok" } };
        } else {
          resp = { tag: "RespAck" };
        }

        if (turns > 30) throw new Error("Too many turns");
      }
    }, 120000);
  });

  // Summary test - runs all available models and compares
  describe("Cross-model comparison", () => {
    it("compares available models (summary)", async () => {
      const availableModels = TEST_MODELS.filter(isModelAvailable);

      if (availableModels.length === 0) {
        console.log("⚠️  No models available - skipping comparison");
        return;
      }

      console.log("\n" + "=".repeat(60));
      console.log("📊 PARMENIDES QUERY - CROSS-MODEL COMPARISON");
      console.log("=".repeat(60));

      const results: Array<{
        model: string;
        answer: string;
        confidence: number;
        trace: string;
      }> = [];

      for (const testModel of availableModels) {
        try {
          const adapter = createAdapterFromModel(
            `${testModel.plugin}:${testModel.model}`
          );
          const result = await queryParmenides(adapter);
          results.push({
            model: `${testModel.plugin}:${testModel.model}`,
            ...result,
          });
        } catch (err) {
          console.log(`❌ ${testModel.plugin}:${testModel.model} failed: ${err}`);
        }
      }

      // Print comparison
      for (const result of results) {
        console.log("\n" + "-".repeat(40));
        console.log(`🤖 ${result.model}`);
        console.log(`📊 Confidence: ${result.confidence.toFixed(2)}`);
        console.log(`📍 Trace: ${result.trace}`);
        console.log("📜 Answer (first 300 chars):");
        console.log(result.answer.slice(0, 300) + (result.answer.length > 300 ? "..." : ""));
      }

      console.log("\n" + "=".repeat(60));
      console.log(`✅ Compared ${results.length} models successfully`);
      console.log("=".repeat(60) + "\n");

      // At least one model should succeed
      expect(results.length).toBeGreaterThan(0);
    }, 300000); // 5 minute timeout for all models
  });
});

// ═══════════════════════════════════════════════════════════════════════════
// REGISTRY INTROSPECTION TEST
// ═══════════════════════════════════════════════════════════════════════════

describe("Registry Introspection (what Lisp can query)", () => {
  it("lists all plugins with capabilities", () => {
    console.log("\n📋 REGISTERED PLUGINS:");
    const summary = registry.summary();

    for (const s of summary) {
      console.log(`\n  ${s.id}:`);
      console.log(`    Name: ${s.name}`);
      console.log(`    Default Model: ${s.defaultModel}`);
      console.log(`    Native Tools: ${s.hasNativeTools}`);
      console.log(`    MCP Support: ${s.hasMCP}`);
      console.log(`    Vision: ${s.hasVision}`);
      console.log(`    Max Context: ${s.maxContext}`);
      console.log(`    Score: ${s.score}`);
    }

    expect(summary.length).toBeGreaterThanOrEqual(3); // anthropic, openai, ollama
  });

  it("queries plugins by capability", () => {
    // Find plugins with native tool support
    const toolPlugins = registry.query({ tooling: { native: true } });
    console.log(`\nPlugins with native tools: ${toolPlugins.map((p) => p.id).join(", ")}`);
    expect(toolPlugins.length).toBeGreaterThanOrEqual(2); // anthropic, openai

    // Find plugins with vision
    const visionPlugins = registry.query({ io: { vision: true } });
    console.log(`Plugins with vision: ${visionPlugins.map((p) => p.id).join(", ")}`);
    expect(visionPlugins.length).toBeGreaterThanOrEqual(1); // anthropic, openai

    // Find best for Oracle
    const best = registry.findBestForOracle();
    console.log(`Best plugin for Oracle: ${best?.id}`);
    expect(best).toBeDefined();
  });

  it("validates plugin configs", () => {
    for (const plugin of registry.list()) {
      // Test with empty config
      const result = plugin.validateConfig({});
      console.log(`${plugin.id} empty config: ${result.valid ? "✅" : "❌ " + result.errors?.join(", ")}`);
    }
  });
});
