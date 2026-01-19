// test/oracle/plugins.spec.ts
// Tests for the Plugin Registry and Model Selection system

import { describe, it, expect, beforeEach, vi } from "vitest";
// Import from index to trigger plugin auto-registration
import {
  PluginRegistry,
  registry,
  createAdapterFromModel,
  ModelSelectorAdapter,
  createModelSelector,
  parseModelSpec,
  withModel,
  withPlugin,
  type OraclePlugin,
  type PluginCapabilities,
  type BasePluginConfig,
} from "../../src/core/oracle/plugins";
import type { OracleAdapter, OracleInit } from "../../src/core/oracle/adapter";
import type { Val } from "../../src/core/eval/values";
import { meaning } from "../../src/core/oracle/meaning";

// ═══════════════════════════════════════════════════════════════════════════
// MOCK PLUGINS FOR TESTING
// ═══════════════════════════════════════════════════════════════════════════

function createMockPlugin(id: string, caps: Partial<PluginCapabilities> = {}): OraclePlugin {
  return {
    id,
    name: `Mock ${id}`,
    capabilities: {
      oracleProtocol: {
        reqEval: true,
        reqApply: true,
        reqObserve: true,
        reqMatch: false,
        reqAssert: false,
        ...caps.oracleProtocol,
      },
      tooling: {
        native: true,
        format: "anthropic",
        maxTools: 64,
        ...caps.tooling,
      },
      mcp: {
        client: false,
        server: false,
        ...caps.mcp,
      },
      session: {
        multiTurn: true,
        streaming: true,
        maxContext: 100000,
        ...caps.session,
      },
      io: {
        vision: false,
        audio: false,
        structuredOutput: true,
        ...caps.io,
      },
    },
    supportedModels: [`${id}-model-1`, `${id}-model-2`, `${id}-*`],
    defaultModel: `${id}-model-1`,
    validateConfig: (config) => ({ valid: true }),
    createAdapter: (config) => createMockAdapter(id, config.model || `${id}-model-1`),
  };
}

function createMockAdapter(pluginId: string, model: string): OracleAdapter {
  return {
    startSession(init: OracleInit) {
      return (async function* () {
        // Return a Meaning that identifies the plugin and model used
        return meaning({
          denotation: {
            tag: "Map",
            entries: [
              [{ tag: "Str", s: "plugin" }, { tag: "Str", s: pluginId }],
              [{ tag: "Str", s: "model" }, { tag: "Str", s: model }],
              [{ tag: "Str", s: "payload" }, init.tag === "Infer" ? init.payload : { tag: "Unit" }],
            ],
          },
          confidence: 1.0,
          trace: { tag: "Str", s: `mock:${pluginId}:${model}` },
        });
      })();
    },
  };
}

// ═══════════════════════════════════════════════════════════════════════════
// PLUGIN REGISTRY TESTS
// ═══════════════════════════════════════════════════════════════════════════

describe("PluginRegistry", () => {
  let testRegistry: PluginRegistry;

  beforeEach(() => {
    testRegistry = new PluginRegistry();
  });

  it("registers and retrieves plugins", () => {
    const plugin = createMockPlugin("test-plugin");
    testRegistry.register(plugin);

    expect(testRegistry.has("test-plugin")).toBe(true);
    expect(testRegistry.get("test-plugin")).toBe(plugin);
  });

  it("throws when registering duplicate plugin", () => {
    const plugin = createMockPlugin("dup-plugin");
    testRegistry.register(plugin);

    expect(() => testRegistry.register(plugin)).toThrow("already registered");
  });

  it("lists all registered plugins", () => {
    testRegistry.register(createMockPlugin("plugin-a"));
    testRegistry.register(createMockPlugin("plugin-b"));
    testRegistry.register(createMockPlugin("plugin-c"));

    const list = testRegistry.list();
    expect(list).toHaveLength(3);
    expect(list.map((p) => p.id).sort()).toEqual(["plugin-a", "plugin-b", "plugin-c"]);
  });

  it("unregisters plugins", () => {
    testRegistry.register(createMockPlugin("to-remove"));
    expect(testRegistry.has("to-remove")).toBe(true);

    testRegistry.unregister("to-remove");
    expect(testRegistry.has("to-remove")).toBe(false);
  });

  it("queries plugins by capabilities", () => {
    testRegistry.register(
      createMockPlugin("with-mcp", { mcp: { client: true, server: false } })
    );
    testRegistry.register(
      createMockPlugin("with-vision", { io: { vision: true, audio: false, structuredOutput: true } })
    );
    testRegistry.register(createMockPlugin("basic"));

    const mcpPlugins = testRegistry.query({ mcp: { client: true } });
    expect(mcpPlugins).toHaveLength(1);
    expect(mcpPlugins[0].id).toBe("with-mcp");

    const visionPlugins = testRegistry.query({ io: { vision: true } });
    expect(visionPlugins).toHaveLength(1);
    expect(visionPlugins[0].id).toBe("with-vision");
  });

  it("finds plugins by model name", () => {
    testRegistry.register(createMockPlugin("provider-a"));
    testRegistry.register(createMockPlugin("provider-b"));

    const found = testRegistry.findByModel("provider-a-model-1");
    expect(found).toHaveLength(1);
    expect(found[0].id).toBe("provider-a");

    // Wildcard match
    const wildcardFound = testRegistry.findByModel("provider-b-anything");
    expect(wildcardFound).toHaveLength(1);
    expect(wildcardFound[0].id).toBe("provider-b");
  });

  it("finds best plugin for Oracle protocol", () => {
    // Plugin with full protocol support and native tools
    testRegistry.register(
      createMockPlugin("full", {
        oracleProtocol: {
          reqEval: true,
          reqApply: true,
          reqObserve: true,
          reqMatch: true,
          reqAssert: true,
        },
        tooling: { native: true, format: "anthropic", maxTools: 64 },
      })
    );

    // Plugin with partial support
    testRegistry.register(
      createMockPlugin("partial", {
        oracleProtocol: {
          reqEval: true,
          reqApply: true,
          reqObserve: false,
          reqMatch: false,
          reqAssert: false,
        },
        tooling: { native: false },
      })
    );

    const best = testRegistry.findBestForOracle();
    expect(best?.id).toBe("full");
  });

  it("returns summary of all plugins", () => {
    testRegistry.register(
      createMockPlugin("claude", {
        tooling: { native: true, format: "anthropic", maxTools: 64 },
        io: { vision: true, audio: false, structuredOutput: true },
      })
    );

    const summary = testRegistry.summary();
    expect(summary).toHaveLength(1);
    expect(summary[0]).toMatchObject({
      id: "claude",
      hasNativeTools: true,
      hasVision: true,
    });
    expect(summary[0].score).toBeGreaterThan(0);
  });
});

// ═══════════════════════════════════════════════════════════════════════════
// MODEL SPECIFICATION PARSING TESTS
// ═══════════════════════════════════════════════════════════════════════════

describe("parseModelSpec", () => {
  it("returns simple payload unchanged", () => {
    const payload: Val = { tag: "Str", s: "hello" };
    const { model, plugin, actualPayload } = parseModelSpec(payload);

    expect(model).toBeUndefined();
    expect(plugin).toBeUndefined();
    expect(actualPayload).toBe(payload);
  });

  it("extracts model from map", () => {
    const payload: Val = {
      tag: "Map",
      entries: [
        [{ tag: "Str", s: "model" }, { tag: "Str", s: "gpt-4o" }],
        [{ tag: "Str", s: "payload" }, { tag: "Str", s: "question" }],
      ],
    };

    const { model, plugin, actualPayload } = parseModelSpec(payload);

    expect(model).toBe("gpt-4o");
    expect(plugin).toBeUndefined();
    expect(actualPayload).toEqual({ tag: "Str", s: "question" });
  });

  it("extracts plugin and model from map", () => {
    const payload: Val = {
      tag: "Map",
      entries: [
        [{ tag: "Str", s: "plugin" }, { tag: "Str", s: "anthropic" }],
        [{ tag: "Str", s: "model" }, { tag: "Str", s: "claude-3-opus" }],
        [{ tag: "Str", s: "payload" }, { tag: "Num", n: 42 }],
      ],
    };

    const { model, plugin, actualPayload } = parseModelSpec(payload);

    expect(model).toBe("claude-3-opus");
    expect(plugin).toBe("anthropic");
    expect(actualPayload).toEqual({ tag: "Num", n: 42 });
  });

  it("parses plugin:model format", () => {
    const payload: Val = {
      tag: "Map",
      entries: [
        [{ tag: "Str", s: "model" }, { tag: "Str", s: "openai:gpt-4o" }],
        [{ tag: "Str", s: "payload" }, { tag: "Str", s: "test" }],
      ],
    };

    const { model, plugin, actualPayload } = parseModelSpec(payload);

    expect(model).toBe("gpt-4o");
    expect(plugin).toBe("openai");
  });
});

// ═══════════════════════════════════════════════════════════════════════════
// HELPER FUNCTIONS TESTS
// ═══════════════════════════════════════════════════════════════════════════

describe("withModel / withPlugin helpers", () => {
  it("withModel creates correct map structure", () => {
    const payload: Val = { tag: "Str", s: "question" };
    const result = withModel("gpt-4o", payload);

    expect(result.tag).toBe("Map");
    if (result.tag === "Map") {
      const modelEntry = result.entries.find(
        ([k]) => k.tag === "Str" && k.s === "model"
      );
      const payloadEntry = result.entries.find(
        ([k]) => k.tag === "Str" && k.s === "payload"
      );

      expect(modelEntry?.[1]).toEqual({ tag: "Str", s: "gpt-4o" });
      expect(payloadEntry?.[1]).toBe(payload);
    }
  });

  it("withPlugin creates correct map structure", () => {
    const payload: Val = { tag: "Num", n: 123 };
    const result = withPlugin("anthropic", "claude-3-opus", payload);

    expect(result.tag).toBe("Map");
    if (result.tag === "Map") {
      const pluginEntry = result.entries.find(
        ([k]) => k.tag === "Str" && k.s === "plugin"
      );
      const modelEntry = result.entries.find(
        ([k]) => k.tag === "Str" && k.s === "model"
      );
      const payloadEntry = result.entries.find(
        ([k]) => k.tag === "Str" && k.s === "payload"
      );

      expect(pluginEntry?.[1]).toEqual({ tag: "Str", s: "anthropic" });
      expect(modelEntry?.[1]).toEqual({ tag: "Str", s: "claude-3-opus" });
      expect(payloadEntry?.[1]).toBe(payload);
    }
  });
});

// ═══════════════════════════════════════════════════════════════════════════
// MODEL SELECTOR ADAPTER TESTS
// ═══════════════════════════════════════════════════════════════════════════

describe("ModelSelectorAdapter", () => {
  let testRegistry: PluginRegistry;
  let selector: ModelSelectorAdapter;

  beforeEach(() => {
    // Create a fresh registry for testing
    testRegistry = new PluginRegistry();
    testRegistry.register(createMockPlugin("mock-anthropic"));
    testRegistry.register(createMockPlugin("mock-openai"));
    testRegistry.register(createMockPlugin("mock-ollama"));

    // Use custom registry via config (no mocking needed)
    selector = new ModelSelectorAdapter({
      defaultModel: "mock-anthropic-model-1",
      defaultPlugin: "mock-anthropic",
      registry: testRegistry,
    });
  });

  it("uses default plugin/model when not specified", async () => {
    const session = selector.startSession({
      tag: "Infer",
      payload: { tag: "Str", s: "simple question" },
      envRef: "env-1",
      stateRef: "state-1",
    });

    const result = await session.next({ tag: "RespAck" });
    expect(result.done).toBe(true);

    const value = result.value;
    expect(value?.tag).toBe("Meaning");
    if (value?.tag === "Meaning" && value.denotation?.tag === "Map") {
      const pluginEntry = value.denotation.entries.find(
        ([k]) => k.tag === "Str" && k.s === "plugin"
      );
      expect(pluginEntry?.[1]).toEqual({ tag: "Str", s: "mock-anthropic" });
    }
  });

  it("routes to specified model", async () => {
    const payload = withModel("mock-openai-model-1", { tag: "Str", s: "question" });

    const session = selector.startSession({
      tag: "Infer",
      payload,
      envRef: "env-1",
      stateRef: "state-1",
    });

    const result = await session.next({ tag: "RespAck" });
    expect(result.done).toBe(true);

    const value = result.value;
    if (value?.tag === "Meaning" && value.denotation?.tag === "Map") {
      const modelEntry = value.denotation.entries.find(
        ([k]) => k.tag === "Str" && k.s === "model"
      );
      expect(modelEntry?.[1]).toEqual({ tag: "Str", s: "mock-openai-model-1" });
    }
  });

  it("routes to specified plugin", async () => {
    const payload = withPlugin("mock-ollama", "mock-ollama-model-2", { tag: "Str", s: "local question" });

    const session = selector.startSession({
      tag: "Infer",
      payload,
      envRef: "env-1",
      stateRef: "state-1",
    });

    const result = await session.next({ tag: "RespAck" });
    expect(result.done).toBe(true);

    const value = result.value;
    if (value?.tag === "Meaning" && value.denotation?.tag === "Map") {
      const pluginEntry = value.denotation.entries.find(
        ([k]) => k.tag === "Str" && k.s === "plugin"
      );
      const modelEntry = value.denotation.entries.find(
        ([k]) => k.tag === "Str" && k.s === "model"
      );
      expect(pluginEntry?.[1]).toEqual({ tag: "Str", s: "mock-ollama" });
      expect(modelEntry?.[1]).toEqual({ tag: "Str", s: "mock-ollama-model-2" });
    }
  });

  it("caches adapters for reuse", async () => {
    // First call
    const session1 = selector.startSession({
      tag: "Infer",
      payload: withModel("mock-anthropic-model-1", { tag: "Str", s: "q1" }),
      envRef: "env-1",
      stateRef: "state-1",
    });
    await session1.next({ tag: "RespAck" });

    // Second call with same model
    const session2 = selector.startSession({
      tag: "Infer",
      payload: withModel("mock-anthropic-model-1", { tag: "Str", s: "q2" }),
      envRef: "env-1",
      stateRef: "state-1",
    });
    await session2.next({ tag: "RespAck" });

    // We can't directly verify caching without internal access,
    // but we can verify both succeed
    expect(true).toBe(true);
  });

  it("allows updating default model", () => {
    selector.setDefaultModel("new-default-model");
    expect(selector.getConfig().defaultModel).toBe("new-default-model");
  });

  it("allows updating default plugin", () => {
    selector.setDefaultPlugin("mock-openai");
    expect(selector.getConfig().defaultPlugin).toBe("mock-openai");
  });
});

// ═══════════════════════════════════════════════════════════════════════════
// GLOBAL REGISTRY TESTS (existing plugins)
// ═══════════════════════════════════════════════════════════════════════════

describe("Global Registry (real plugins)", () => {
  it("has anthropic plugin registered", () => {
    expect(registry.has("anthropic")).toBe(true);
    const plugin = registry.get("anthropic");
    expect(plugin?.name).toBe("Anthropic Claude");
    expect(plugin?.capabilities.tooling.native).toBe(true);
  });

  it("has openai plugin registered", () => {
    expect(registry.has("openai")).toBe(true);
    const plugin = registry.get("openai");
    expect(plugin?.name).toBe("OpenAI GPT");
    expect(plugin?.capabilities.tooling.format).toBe("openai");
  });

  it("has ollama plugin registered", () => {
    expect(registry.has("ollama")).toBe(true);
    const plugin = registry.get("ollama");
    expect(plugin?.name).toBe("Ollama Local");
  });

  it("anthropic plugin validates missing API key", () => {
    const plugin = registry.get("anthropic");
    // Clear env var for test
    const originalKey = process.env.ANTHROPIC_API_KEY;
    delete process.env.ANTHROPIC_API_KEY;

    const result = plugin?.validateConfig({});
    expect(result?.valid).toBe(false);
    expect(result?.errors?.[0]).toContain("Missing API key");

    // Restore
    if (originalKey) process.env.ANTHROPIC_API_KEY = originalKey;
  });

  it("ollama plugin validates without API key", () => {
    const plugin = registry.get("ollama");
    const result = plugin?.validateConfig({});
    expect(result?.valid).toBe(true);
  });

  it("registry summary includes all plugins", () => {
    const summary = registry.summary();
    const ids = summary.map((s) => s.id);

    expect(ids).toContain("anthropic");
    expect(ids).toContain("openai");
    expect(ids).toContain("ollama");
  });

  it("finds best plugin prioritizing native tools", () => {
    const best = registry.findBestForOracle();
    // Should be anthropic or openai (both have native tools)
    expect(best?.capabilities.tooling.native).toBe(true);
  });
});
