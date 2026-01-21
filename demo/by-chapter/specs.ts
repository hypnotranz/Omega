import { buildDemo, type DemoSpec, type ScenarioResult } from "./common";
import type { DemoContext, OracleScriptEntry } from "../harness/types";
import { chapterConfigs } from "./config";
import { createChapterDemo } from "./shared";

type PromptResponse = {
  match: string;
  value: unknown;
  evidence?: string;
};

function normalizePrompt(req: unknown): string {
  const args = (req as any)?.args;
  if (Array.isArray(args) && args.length > 0) {
    return String(args[0] ?? "");
  }
  const prompt = (req as any)?.prompt ?? req;
  return String(prompt ?? "");
}

function addPromptScripts(
  ctx: DemoContext,
  prompts: PromptResponse[],
  fallback: unknown = "no-scripted-response"
): void {
  const script: OracleScriptEntry = {
    match: (_req, type) => type === "InferOp",
    respond: (req) => {
      const prompt = normalizePrompt(req).toLowerCase();
      for (const entry of prompts) {
        if (prompt.includes(entry.match.toLowerCase())) {
          return { value: entry.value, evidence: entry.evidence ?? entry.match };
        }
      }
      return { value: fallback, evidence: "fallback" };
    },
  };
  ctx.oracle.addScript(script);
}

function ask(ctx: DemoContext, prompt: string): any {
  const res = ctx.oracle.handle("InferOp", { args: [prompt] }) as any;
  return (res as any)?.value ?? res;
}

function result(outputs: unknown[], success: boolean, extra?: Partial<ScenarioResult>): ScenarioResult {
  return { outputs, success, ...extra };
}

const specs: DemoSpec[] = [
  {
    id: "ch01-getting-started",
    number: 1,
    title: "Getting Started",
    description: "Warm up the REPL with semantic strings and see deterministic oracle echoes.",
    setupOracle: (ctx) =>
      addPromptScripts(ctx, [
        { match: "welcome to omega", value: "Welcome to Omega - try semantic prompts anytime." },
        { match: "help menu", value: "Type :help to see semantic primitives and effects." },
      ]),
    runScenario: async (ctx) => {
      const greeting = ask(ctx, "welcome to omega lisp with natural language values");
      const help = ask(ctx, "show me the help menu that mentions infer.op");
      const outputs = [greeting, help];
      const success = typeof greeting === "string" && typeof help === "string";
      return result(outputs, success);
    },
  },
  {
    id: "ch02-llm-calls",
    number: 2,
    title: "LLM Calls as Functions",
    description: "Call infer.op for sentiment and translation and treat responses as values.",
    setupOracle: (ctx) =>
      addPromptScripts(ctx, [
        { match: "sentiment", value: "positive" },
        { match: "translate this greeting to french", value: "bonjour" },
      ], "unknown-analysis"),
    runScenario: async (ctx) => {
      const sentiment = ask(ctx, "sentiment of this customer note: I adore how attentive your team has been.");
      const translation = ask(ctx, "translate this greeting to french: Hello, thank you for the quick reply.");
      const outputs = [sentiment, translation];
      const success = sentiment === "positive" && translation === "bonjour";
      return result(outputs, success);
    },
  },
  {
    id: "ch03-composition",
    number: 3,
    title: "Functional Composition",
    description: "Map and filter semantic predicates that call infer.op on each element.",
    setupOracle: (ctx) =>
      addPromptScripts(ctx, [
        { match: "complaint about refund", value: "yes" },
        { match: "thankful praise", value: "no" },
        { match: "angry about outage", value: "yes" },
        { match: "neutral sizing question", value: "no" },
      ], "no"),
    runScenario: async (ctx) => {
      const messages = [
        "thankful praise about how quickly you fixed my issue",
        "complaint about refund delays for my order",
        "neutral sizing question asking about medium vs large",
        "angry about outage that interrupted a live webinar",
      ];
      const complaints = messages.filter(msg => ask(ctx, `Is this a complaint about refunds or failures? ${msg}`) === "yes");
      const tones = complaints.map(msg => ask(ctx, `Classify the tone of this complaint: ${msg}`));
      const outputs = [complaints, tones];
      return result(outputs, true);
    },
  },
  {
    id: "ch04-higher-order",
    number: 4,
    title: "Higher-Order LLM Functions",
    description: "Factory returns semantic classifiers specialized by prompt seed.",
    setupOracle: (ctx) =>
      addPromptScripts(ctx, [
        { match: "classify technology", value: "dev-tools" },
        { match: "classify cuisine", value: "mediterranean" },
      ], "other"),
    runScenario: async (ctx) => {
      const tech = ask(ctx, "classify technology description: A CLI that lints API contracts.");
      const cuisine = ask(ctx, "classify cuisine story: Olive oil tasting with grilled vegetables.");
      const outputs = [tech, cuisine];
      const success = tech === "dev-tools" && cuisine === "mediterranean";
      return result(outputs, success);
    },
  },
  {
    id: "ch05-nondeterministic",
    number: 5,
    title: "Nondeterministic Search",
    description: "Choose tone options until oracle confirms an apologetic response.",
    setupOracle: (ctx) =>
      addPromptScripts(ctx, [
        { match: "formal reply", value: "no" },
        { match: "friendly reply", value: "no" },
        { match: "apologetic reply", value: "yes" },
      ]),
    runScenario: async (ctx) => {
      const tones = ["formal", "friendly", "apologetic"];
      let chosen = "";
      let backtracks = 0;
      for (const tone of tones) {
        const toneCheck = ask(ctx, `Does this ${tone} reply sound apologetic?`);
        if (toneCheck === "yes") {
          chosen = tone;
          break;
        }
        backtracks++;
        ctx.ledger.record("amb.backtrack", { tone });
      }
      const response = `Selected ${chosen} tone for refund message.`;
      const outputs = [response, chosen];
      const success = chosen === "apologetic";
      return result(outputs, success, { backtracks, ambChoices: tones.length });
    },
  },
  {
    id: "ch06-multi-shot",
    number: 6,
    title: "Multi-Shot Sampling",
    description: "Sample multiple rewrites and view the distribution returned by search.op.",
    setupOracle: (ctx) =>
      addPromptScripts(ctx, [
        { match: "rewrite formally", value: ["We appreciate your patience.", "Thank you for your note."] },
        { match: "rewrite warmly", value: ["Hey there! We hear you.", "We're here to help you out."] },
      ], ["sample"]),
    runScenario: async (ctx) => {
      const formalSamples = ask(ctx, "rewrite formally a delayed shipping apology with empathy");
      const warmSamples = ask(ctx, "rewrite warmly a delayed shipping apology with empathy");
      const outputs = [formalSamples, warmSamples];
      const success = Array.isArray(formalSamples) && Array.isArray(warmSamples);
      return result(outputs, success);
    },
  },
  {
    id: "ch07-lazy-streams",
    number: 7,
    title: "Lazy Streams",
    description: "Generate follow-up questions lazily and only force the first two.",
    setupOracle: (ctx) =>
      addPromptScripts(ctx, [
        { match: "follow-up 1", value: "Could you share the error message you saw?" },
        { match: "follow-up 2", value: "When did the outage begin for you?" },
        { match: "follow-up 3", value: "Have you tried the safety mode toggle?" },
      ]),
    runScenario: async (ctx) => {
      const prompts = [
        "follow-up 1 for a frustrated customer about sync failures",
        "follow-up 2 for a frustrated customer about sync failures",
        "follow-up 3 for a frustrated customer about sync failures",
      ];
      const stream = prompts[Symbol.iterator]();
      const forced: string[] = [];
      for (let i = 0; i < 2; i++) {
        const next = stream.next();
        if (!next.done) {
          forced.push(ask(ctx, next.value));
        }
      }
      const outputs = [forced];
      const success = forced.length === 2 && forced.every(v => typeof v === "string");
      return result(outputs, success);
    },
  },
  {
    id: "ch08-debugger",
    number: 8,
    title: "The Debugger",
    description: "Step through a semantic check and capture oracle trace highlights.",
    setupOracle: (ctx) =>
      addPromptScripts(ctx, [
        { match: "step sentiment", value: "negative" },
        { match: "show why negative", value: "Contains frustration about unresolved outage." },
      ]),
    runScenario: async (ctx) => {
      const step1 = ask(ctx, "step sentiment analysis of: This outage is blocking my launch.");
      const step2 = ask(ctx, "show why negative sentiment was chosen");
      const outputs = [step1, step2, ctx.oracle.getTranscript().interactions.length];
      const success = step1 === "negative" && typeof step2 === "string";
      return result(outputs, success);
    },
  },
  {
    id: "ch09-agentic-repl",
    number: 9,
    title: "The Agentic REPL",
    description: "Oracle asks runtime for state (ticket counts) before crafting a reply.",
    setupOracle: (ctx) => {
      addPromptScripts(ctx, [
        { match: "compose next action", value: "Queue urgent tickets first and send apology." },
      ]);
      ctx.oracle.addScript({
        match: (_req, type) => type === "ReqEval",
        respond: () => ({ value: 4, evidence: "demo-eval" }),
      });
    },
    runScenario: async (ctx) => {
      const ticketCount = ctx.oracle.handle("ReqEval", { qexpr: "(length urgent-tickets)" }) as any;
      const plan = ask(ctx, `compose next action knowing ${ticketCount.value ?? ticketCount} urgent tickets are waiting`);
      const outputs = [ticketCount.value ?? ticketCount, plan];
      const success = (ticketCount.value ?? ticketCount) === 4 && typeof plan === "string";
      return result(outputs, success);
    },
  },
  {
    id: "ch10-api-reference",
    number: 10,
    title: "Full API Reference",
    description: "Blend infer, apply, observe, and test operations in one run.",
    setupOracle: (ctx) => {
      addPromptScripts(ctx, [
        { match: "classify ticket", value: "bug" },
      ]);
      ctx.oracle.addScript({
        match: (_req, type) => type === "ReqObserve",
        respond: () => ({ observed: { env: "demo" }, evidence: "observe" }),
      });
      ctx.oracle.addScript({
        match: (_req, type) => type === "ReqTest",
        respond: () => ({ passed: true, evidence: "test-passed" }),
      });
    },
    runScenario: async (ctx) => {
      const classification = ask(ctx, "classify ticket with stack traces and screenshots");
      const observation = ctx.oracle.handle("ReqObserve", { what: { tag: "Env" } }) as any;
      const testResult = ctx.oracle.handle("ReqTest", { spec: { tag: "Smoke", qexpr: "(+ 1 1)", envRef: "env" } }) as any;
      const outputs = [classification, observation.observed, testResult.passed];
      const success = classification === "bug" && testResult.passed === true;
      return result(outputs, success);
    },
  },
  {
    id: "ch11-semantic-procedures",
    number: 11,
    title: "Semantic Procedures as Black Boxes",
    description: "Call semantic validators without exposing their inner logic.",
    setupOracle: (ctx) =>
      addPromptScripts(ctx, [
        { match: "professional tone", value: "yes" },
        { match: "contains warm opener", value: "yes" },
        { match: "contains risky claim", value: "no" },
      ]),
    runScenario: async (ctx) => {
      const professional = ask(ctx, "professional tone check for: I appreciate your feedback and will fix this.");
      const opener = ask(ctx, "contains warm opener for: I appreciate your feedback and will fix this.");
      const risky = ask(ctx, "contains risky claim for: I appreciate your feedback and will fix this.");
      const outputs = [professional, opener, risky];
      const success = professional === "yes" && opener === "yes" && risky === "no";
      return result(outputs, success);
    },
  },
  {
    id: "ch12-inference-processes",
    number: 12,
    title: "Inference Processes",
    description: "Compare recursive vs iterative summarization with cost hints.",
    setupOracle: (ctx) =>
      addPromptScripts(ctx, [
        { match: "recursive summary", value: "Concise summary built hierarchically." },
        { match: "iterative summary", value: "Rolling summary updated per paragraph." },
      ]),
    runScenario: async (ctx) => {
      const recursive = ask(ctx, "recursive summary of a multi-section outage report");
      const iterative = ask(ctx, "iterative summary of a multi-section outage report");
      const outputs = [recursive, iterative];
      const success = typeof recursive === "string" && typeof iterative === "string";
      return result(outputs, success);
    },
  },
  {
    id: "ch13-higher-order-inference",
    number: 13,
    title: "Higher-Order Inference",
    description: "Fold stakeholder opinions into a single synthesized stance.",
    setupOracle: (ctx) =>
      addPromptScripts(ctx, [
        { match: "merge opinion security", value: "Prioritize zero-trust rollout first." },
        { match: "merge opinion product", value: "Pair secure defaults with simpler onboarding." },
        { match: "merge consensus", value: "Ship secure defaults with guided onboarding and monitoring." },
      ]),
    runScenario: async (ctx) => {
      const security = ask(ctx, "merge opinion security lead about authentication overhaul");
      const product = ask(ctx, "merge opinion product lead about authentication overhaul");
      const consensus = ask(ctx, "merge consensus from security and product leads");
      const outputs = [security, product, consensus];
      const success = typeof consensus === "string";
      return result(outputs, success);
    },
  },
  {
    id: "ch14-semantic-data",
    number: 14,
    title: "Semantic Data Abstraction",
    description: "Validators like is-haiku? and has-proper-greeting? gate downstream flow.",
    setupOracle: (ctx) =>
      addPromptScripts(ctx, [
        { match: "is this a haiku", value: "yes" },
        { match: "has a proper greeting", value: "yes" },
        { match: "needs revision", value: "no" },
      ]),
    runScenario: async (ctx) => {
      const haiku = ask(ctx, "is this a haiku about winter silence? Snow hushes the town.");
      const greeting = ask(ctx, "has a proper greeting for: Dear team, thank you for your patience.");
      const revision = ask(ctx, "needs revision for: Dear team, thank you for your patience.");
      const outputs = [haiku, greeting, revision];
      const success = haiku === "yes" && greeting === "yes" && revision === "no";
      return result(outputs, success);
    },
  },
  {
    id: "ch15-sequences",
    number: 15,
    title: "Sequences as Semantic Interfaces",
    description: "Pipeline complaints -> extract issues -> prioritize by severity.",
    setupOracle: (ctx) =>
      addPromptScripts(ctx, [
        { match: "is this severe", value: "high" },
        { match: "extract issue", value: "Database outage blocking checkouts." },
      ]),
    runScenario: async (ctx) => {
      const complaint = "Customers cannot check out during peak hours and data is missing.";
      const issue = ask(ctx, `extract issue from complaint: ${complaint}`);
      const severity = ask(ctx, `is this severe: ${issue}`);
      const outputs = [issue, severity];
      const success = typeof issue === "string" && severity === "high";
      return result(outputs, success);
    },
  },
  {
    id: "ch16-symbolic-semantic",
    number: 16,
    title: "Symbolic Semantic Data",
    description: "Reason about meaning relationships like same-meaning? implies? contradicts?.",
    setupOracle: (ctx) =>
      addPromptScripts(ctx, [
        { match: "same meaning", value: "true" },
        { match: "implies", value: "true" },
        { match: "contradicts", value: "false" },
      ]),
    runScenario: async (ctx) => {
      const same = ask(ctx, "same meaning between I am upset and I feel dissatisfied");
      const implies = ask(ctx, "implies that payment failed means order not completed");
      const contradicts = ask(ctx, "contradicts that refund issued with statement refund refused");
      const outputs = [same, implies, contradicts];
      const success = same === "true" && implies === "true" && contradicts === "false";
      return result(outputs, success);
    },
  },
  {
    id: "ch17-multiple-representations",
    number: 17,
    title: "Multiple Representations of Meaning",
    description: "Convert register from angry complaint to formal empathetic response.",
    setupOracle: (ctx) =>
      addPromptScripts(ctx, [
        { match: "convert angry to formal", value: "We apologize for the frustration and are resolving this now." },
        { match: "convert angry to empathetic", value: "I'm sorry this caused stress - I'm on it." },
      ]),
    runScenario: async (ctx) => {
      const formal = ask(ctx, "convert angry to formal tone for: This delay is unacceptable.");
      const empathetic = ask(ctx, "convert angry to empathetic tone for: This delay is unacceptable.");
      const outputs = [formal, empathetic];
      const success = typeof formal === "string" && typeof empathetic === "string";
      return result(outputs, success);
    },
  },
  {
    id: "ch18-generic-semantic",
    number: 18,
    title: "Generic Semantic Operations",
    description: "Switch behavior by domain: legal vs email summarization cues.",
    setupOracle: (ctx) =>
      addPromptScripts(ctx, [
        { match: "legal summary", value: "Summarize obligations, liabilities, and notice periods." },
        { match: "email summary", value: "Capture intent, sentiment, and promised follow-ups." },
      ]),
    runScenario: async (ctx) => {
      const legal = ask(ctx, "legal summary of the data processing addendum");
      const email = ask(ctx, "email summary of customer escalation about billing confusion");
      const outputs = [legal, email];
      const success = typeof legal === "string" && typeof email === "string";
      return result(outputs, success);
    },
  },
  {
    id: "ch19-conversational-state",
    number: 19,
    title: "Conversational State and Memory",
    description: "Track multi-turn conversation and answer with context.",
    setupOracle: (ctx) =>
      addPromptScripts(ctx, [
        { match: "remembered context", value: "You mentioned delays due to customs earlier." },
        { match: "final answer", value: "We'll expedite replacement and update you by email today." },
      ]),
    runScenario: async (ctx) => {
      const turns: string[] = [];
      turns.push("Customer: My shipment is stuck at customs, this is painful.");
      turns.push("Agent: I'm sorry; I'm checking with logistics now.");
      const reminder = ask(ctx, "remembered context from the prior turns about customs?");
      const final = ask(ctx, `final answer acknowledging context: ${reminder}`);
      const outputs = [...turns, reminder, final];
      const success = typeof final === "string" && reminder.includes("customs");
      return result(outputs, success);
    },
  },
  {
    id: "ch20-semantic-environment",
    number: 20,
    title: "Semantic Environment Model",
    description: "Meaning of 'bank' shifts with environment; oracle observes context.",
    setupOracle: (ctx) => {
      addPromptScripts(ctx, [
        { match: "bank in finance", value: "financial-institution" },
        { match: "bank near river", value: "river-edge" },
      ]);
      ctx.oracle.addScript({
        match: (_req, type) => type === "ReqObserve",
        respond: (req) => ({ observed: { context: (req as any)?.what?.label ?? "unknown" } }),
      });
    },
    runScenario: async (ctx) => {
      const financeMeaning = ask(ctx, "bank in finance conversation about wire transfers");
      const natureMeaning = ask(ctx, "bank near river hiking trail");
      const env = ctx.oracle.handle("ReqObserve", { what: { tag: "Env", label: "finance" } }) as any;
      const outputs = [financeMeaning, natureMeaning, env.observed];
      const success = financeMeaning === "financial-institution" && natureMeaning === "river-edge";
      return result(outputs, success);
    },
  },
  {
    id: "ch21-mutable-semantic",
    number: 21,
    title: "Mutable Semantic Structures",
    description: "Grow a knowledge graph of incidents with updates over time.",
    setupOracle: (ctx) =>
      addPromptScripts(ctx, [
        { match: "add node", value: "added" },
        { match: "link cause", value: "linked" },
        { match: "summarize graph", value: "Outage caused by cache misconfiguration linked to surge." },
      ]),
    runScenario: async (ctx) => {
      const add = ask(ctx, "add node: outage in us-east cache tier");
      const link = ask(ctx, "link cause: cache misconfiguration to outage");
      const summary = ask(ctx, "summarize graph with key relations");
      const outputs = [add, link, summary];
      const success = add === "added" && link === "linked" && typeof summary === "string";
      return result(outputs, success);
    },
  },
  {
    id: "ch22-concurrent-inference",
    number: 22,
    title: "Concurrent Inference",
    description: "Classify multiple tickets in parallel using Promise.all to mirror fibers.",
    setupOracle: (ctx) =>
      addPromptScripts(ctx, [
        { match: "login failure", value: "bug" },
        { match: "feature timeline", value: "feature-request" },
        { match: "data loss", value: "incident" },
        { match: "export report", value: "question" },
      ]),
    runScenario: async (ctx) => {
      const tickets = [
        "login failure after password reset",
        "feature timeline for shared dashboards",
        "data loss after sync interruption",
        "export report guidance",
      ];
      const classifications = await Promise.all(
        tickets.map(t => ask(ctx, `classify ticket: ${t}`))
      );
      const outputs = [classifications];
      const success = classifications.length === 4;
      return result(outputs, success, { scheduleDecisions: classifications.length });
    },
  },
  {
    id: "ch23-streams-of-inference",
    number: 23,
    title: "Streams of Inference",
    description: "Produce an unbounded stream of clarifications and take only the first three.",
    setupOracle: (ctx) =>
      addPromptScripts(ctx, [
        { match: "clarification 1", value: "What outcome are you optimizing for?" },
        { match: "clarification 2", value: "Which regions are impacted?" },
        { match: "clarification 3", value: "Do you have latency budgets per tier?" },
        { match: "clarification 4", value: "Should we prefer zero-downtime migrations?" },
      ]),
    runScenario: async (ctx) => {
      const clarifications: string[] = [];
      let index = 1;
      while (clarifications.length < 3) {
        const prompt = `clarification ${index} for scaling strategy`;
        clarifications.push(ask(ctx, prompt));
        index++;
      }
      const outputs = [clarifications];
      const success = clarifications.length === 3;
      return result(outputs, success);
    },
  },
  {
    id: "ch24-metacircular",
    number: 24,
    title: "Metalinguistic Abstraction",
    description: "Oracle reasons about evaluation and suggests which expression to run.",
    setupOracle: (ctx) => {
      addPromptScripts(ctx, [
        { match: "analyze expression", value: "Prefer evaluating the safety check first." },
        { match: "explain oracle protocol", value: "Use ReqEval to inspect code paths before infer." },
      ]);
      ctx.oracle.addScript({
        match: (_req, type) => type === "ReqEval",
        respond: () => ({ value: "(/ (+ 1 1) 1)", evidence: "metacircular-eval" }),
      });
    },
    runScenario: async (ctx) => {
      const evalSuggestion = ctx.oracle.handle("ReqEval", { qexpr: "(choose safety-check)" }) as any;
      const reasoning = ask(ctx, `analyze expression before infer: ${evalSuggestion.value ?? evalSuggestion}`);
      const outputs = [evalSuggestion.value ?? evalSuggestion, reasoning];
      const success = typeof reasoning === "string";
      return result(outputs, success);
    },
  },
  {
    id: "ch25-lazy-semantic",
    number: 25,
    title: "Lazy Semantic Evaluation",
    description: "Compute expensive analysis once and reuse the cached meaning.",
    setupOracle: (ctx) =>
      addPromptScripts(ctx, [
        { match: "expensive analysis", value: "Detected duplicate intents in flow." },
      ]),
    runScenario: async (ctx) => {
      let cached: string | null = null;
      const compute = () => {
        if (cached) return cached;
        cached = ask(ctx, "expensive analysis of conversation flows to reduce drift");
        return cached;
      };
      const first = compute();
      const second = compute();
      const outputs = [first, second];
      const success = first === second && cached !== null;
      return result(outputs, success);
    },
  },
  {
    id: "ch26-amb-inference",
    number: 26,
    title: "The AMB Inference Engine",
    description: "Semantic constraint satisfaction picks the first compliant tone option.",
    setupOracle: (ctx) =>
      addPromptScripts(ctx, [
        { match: "formal apology", value: "no" },
        { match: "empathetic apology", value: "yes" },
        { match: "playful apology", value: "no" },
      ]),
    runScenario: async (ctx) => {
      const tones = ["formal apology", "empathetic apology", "playful apology"];
      let choice = "";
      for (const tone of tones) {
        const ok = ask(ctx, `${tone} satisfies constraint for billing mistake?`);
        if (ok === "yes") {
          choice = tone;
          break;
        }
        ctx.ledger.record("amb.fail", { tone });
      }
      const outputs = [choice];
      const success = choice === "empathetic apology";
      return result(outputs, success, { ambChoices: tones.length });
    },
  },
  {
    id: "ch27-logic-programming",
    number: 27,
    title: "Logic Programming with Semantic Facts",
    description: "Query natural language facts to infer relationships like grandparent.",
    setupOracle: (ctx) =>
      addPromptScripts(ctx, [
        { match: "alice parent of bob", value: "fact-recorded" },
        { match: "bob parent of carol", value: "fact-recorded" },
        { match: "who is grandparent", value: "Alice is the grandparent of Carol." },
      ]),
    runScenario: async (ctx) => {
      const fact1 = ask(ctx, "alice parent of bob in natural language fact base");
      const fact2 = ask(ctx, "bob parent of carol in natural language fact base");
      const query = ask(ctx, "who is grandparent of carol using semantic facts?");
      const outputs = [fact1, fact2, query];
      const success = typeof query === "string" && query.includes("grandparent");
      return result(outputs, success);
    },
  },
];

export const chapterSpecs = specs;

export function findChapter(number: number): DemoSpec | undefined {
  return specs.find(s => s.number === number);
}

export function demoFromNumber(num: number) {
  const padded = String(num).padStart(2, "0");
  const config = Object.values(chapterConfigs).find(cfg => cfg.id.startsWith(`ch${padded}`));
  if (config) {
    return createChapterDemo(config);
  }

  const spec = findChapter(num);
  if (!spec) {
    throw new Error(`No chapter spec for ${num}`);
  }
  return buildDemo(spec);
}

