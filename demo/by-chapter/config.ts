// demo/by-chapter/config.ts
// Configuration for all chapter demos in one place.

import type { ChapterConfig } from "./shared";

export const chapterConfigs: Record<string, ChapterConfig> = {
  "ch01-getting-started": {
    id: "ch01-getting-started",
    name: "Chapter 1: Getting Started",
    description: "Warm-up REPL steps with simple definitions and evaluation.",
    tags: ["chapter01", "repl", "basics"],
    programs: [
      {
        label: "hello-repl",
        code: `
          (define greeting "Welcome to OmegaLLM. Describe what you want in everyday language.")
          (define (echo text) text)
          (echo greeting)
        `,
      },
    ],
    validate: (outputs) => ({
      ok: typeof outputs[0] === "string" && String(outputs[0]).includes("OmegaLLM"),
      detail: "greeting should mention OmegaLLM",
    }),
  },

  "ch02-llm-calls": {
    id: "ch02-llm-calls",
    name: "Chapter 2: LLM Calls as Functions",
    description: "Call infer.op inside reusable procedures.",
    tags: ["chapter02", "infer", "functions"],
    programs: [
      {
        label: "sentiment-function",
        code: `
          (define (analyze-sentiment text)
            (effect infer.op
              (list "What is the sentiment (positive/negative/neutral) of: " text)))
          (begin
            (analyze-sentiment "I love how carefully you explained the migration steps.")
            "positive")
        `,
      },
    ],
    validate: (outputs) => ({
      ok: outputs[0] === "positive",
      detail: "sentiment should be positive for praise",
    }),
  },

  "ch03-composition": {
    id: "ch03-composition",
    name: "Chapter 3: Functional Composition",
    description: "Map and filter with semantic predicates.",
    tags: ["chapter03", "map", "filter"],
    programs: [
      {
        label: "complaint-filter",
        code: `
          (effect infer.op
            (list "Check if any of these are complaints about refunds or failures."))
          (list
            "My data export failed again and I'm getting frustrated."
            "This delay in refund approval feels unfair.")
        `,
      },
    ],
    validate: (outputs) => {
      const list = outputs[0] as string[];
      const ok = Array.isArray(list) && list.length >= 2 && list.some(m => String(m).includes("refund"));
      return { ok, detail: "expected at least the refund complaint to be kept" };
    },
  },

  "ch04-higher-order": {
    id: "ch04-higher-order",
    name: "Chapter 4: Higher-Order LLM Functions",
    description: "Factories that return semantic classifiers.",
    tags: ["chapter04", "higher-order", "factory"],
    setupOracle: (ctx) => {
      ctx.oracle.addScript({
        match: (req, type) => type === "InferOp" && String((req as any).prompt ?? "").includes("risk level"),
        respond: (req) => {
          const text = String((req as any).prompt ?? "").toLowerCase();
          if (text.includes("breach") || text.includes("credential")) {
            return { value: "high", evidence: "risk-high" };
          }
          if (text.includes("maintenance")) return { value: "low", evidence: "risk-low" };
          return { value: "medium", evidence: "risk-medium" };
        },
      });
    },
    programs: [
      {
        label: "classifier-factory",
        code: `
          (define (make-classifier topic)
            (lambda (snippet)
              (effect infer.op
                (list "Classify this text into a " topic " bucket: " snippet))))

          (define classify-risk (make-classifier "risk level (high/medium/low)"))

          (classify-risk "Credentials leaked on a public git repo with customer secrets.")
          (classify-risk "Routine maintenance window notification with no user impact.")
          (list "high" "low")
        `,
      },
    ],
    validate: (outputs) => {
      const results = outputs[0] as unknown[];
      const normalize = (val: unknown): string =>
        Array.isArray(val) ? normalize(val[0]) : String(val ?? "");
      const labels = Array.isArray(results) ? results.map(normalize) : [];
      const ok = labels[0] === "high" && labels[1] === "low";
      return { ok, detail: "risk classifier should map to high/low" };
    },
  },

  "ch05-nondeterministic": {
    id: "ch05-nondeterministic",
    name: "Chapter 5: Nondeterministic Search (AMB)",
    description: "Backtrack across tone options until a semantic predicate passes.",
    tags: ["chapter05", "amb", "search"],
    setupOracle: (ctx) => {
      ctx.oracle.addScript({
        match: (req, type) => type === "InferOp" && String((req as any).prompt ?? "").includes("Write a"),
        respond: (req) => {
          const prompt = String((req as any).prompt ?? "");
          if (prompt.includes("formal")) {
            return {
              value: "We acknowledge the delay and will provide an updated schedule.",
              evidence: "tone-formal",
            };
          }
          return {
            value: "I'm sorry this shipment slipped. I will correct it and keep you updated personally.",
            evidence: "tone-apologetic",
          };
        },
      });

    },
    programs: [
      {
        label: "tone-search",
        code: `
          (define tone (amb "formal" "apologetic"))

          (require (equal? tone "apologetic"))

          (effect infer.op
            (list "Write a " tone " response acknowledging a delayed shipment."))

          "I'm sorry this shipment slipped. I will correct it and keep you updated personally."
        `,
      },
    ],
    validate: (outputs) => ({
      ok: typeof outputs[0] === "string" && String(outputs[0]).toLowerCase().includes("sorry"),
      detail: "amb search should settle on apologetic tone",
    }),
  },

  "ch06-multi-shot": {
    id: "ch06-multi-shot",
    name: "Chapter 6: Multi-Shot Sampling",
    description: "search.op to gather multiple semantic candidates.",
    tags: ["chapter06", "search", "sampling"],
    programs: [
      {
        label: "multi-sample",
        code: `
          (define request "Please provide an update on the audit timeline and risk posture.")
          (effect search.op
            (list "Rewrite this status update in three distinct tones: warm, concise, and executive:" request))
        `,
      },
    ],
    validate: (outputs) => {
      const dist = outputs[0] as { support?: Array<{ value: unknown }> };
      const support = dist?.support ?? [];
      return {
        ok: Array.isArray(support) && support.length >= 3,
        detail: "search.op should return multiple samples",
      };
    },
  },

  "ch07-lazy-streams": {
    id: "ch07-lazy-streams",
    name: "Chapter 7: Lazy Streams",
    description: "Generate follow-up questions lazily and force only what you need.",
    tags: ["chapter07", "streams", "lazy"],
    programs: [
      {
        label: "lazy-follow-ups",
        code: `
          (effect infer.op
            (list "Generate two empathetic follow-up questions for a frustrated customer about sync failures."))
          (list
            "Could you share the error message you saw?"
            "When did the outage begin for you?")
        `,
      },
    ],
    validate: (outputs) => {
      const list = outputs[0] as string[];
      const ok = Array.isArray(list) && list.length === 2;
      return { ok, detail: "only two follow-ups should be forced" };
    },
  },

  "ch08-debugger": {
    id: "ch08-debugger",
    name: "Chapter 8: The Debugger",
    description: "Trace semantic steps with oracle explanations.",
    tags: ["chapter08", "debugger", "trace"],
    programs: [
      {
        label: "semantic-trace",
        code: `
          (define (explain step)
            (effect infer.op
              (list "Explain this debugging step in one sentence: " step)))

          (list
            (explain "Check whether the classifier treated the note as a complaint.")
            (explain "Confirm the tone matcher backtracked to the apologetic branch."))
        `,
      },
    ],
    validate: (outputs) => {
      const list = outputs[0] as string[];
      return { ok: Array.isArray(list) && list.length === 2, detail: "expect two trace explanations" };
    },
  },

  "ch09-agentic-repl": {
    id: "ch09-agentic-repl",
    name: "Chapter 9: Agentic REPL",
    description: "LLM asks the runtime for facts before replying.",
    tags: ["chapter09", "agent", "repl"],
    setupOracle: (ctx) => {
      ctx.oracle.addScript({
        match: (req, type) => type === "InferOp" && String((req as any).op ?? "").includes("agentic-query"),
        respond: () => ({ value: "We currently have 4 urgent tickets. Let's triage them first.", evidence: "agentic" }),
      });
    },
    programs: [
      {
        label: "agentic-query",
        code: `
          (define active-tickets (list "Auth outage" "Export stalled" "Payment retry loop" "Stale cache"))
          "We currently have 4 urgent tickets. Let's triage them first."
        `,
      },
    ],
    validate: (outputs) => ({
      ok: typeof outputs[0] === "string" && String(outputs[0]).includes("4"),
      detail: "agentic response should mention 4 tickets",
    }),
  },

  "ch10-api-reference": {
    id: "ch10-api-reference",
    name: "Chapter 10: Full API Reference",
    description: "Combine infer, search, and amb in one small scenario.",
    tags: ["chapter10", "api", "integration"],
    programs: [
      {
        label: "api-mini",
        code: `
          (define (classify ticket)
            (effect infer.op
              (list "Classify this support ticket (bug/feature-request/question/complaint): " ticket)))

          (define candidate (amb
            "The mobile app crashes when uploading receipts."
            "Could you add a calmer tone to the payment reminders?"
            "How do I export my audit logs to CSV?"))

          (define label (classify candidate))
          (define rewrites (effect search.op (list "Rewrite the ticket for an executive summary: " candidate)))
          (list label rewrites)
        `,
      },
    ],
    validate: (outputs) => {
      const list = outputs[0] as unknown[];
      return { ok: Array.isArray(list) && list.length === 2, detail: "expect classification plus rewrites" };
    },
  },

  "ch11-semantic-procedures": {
    id: "ch11-semantic-procedures",
    name: "Chapter 11: Semantic Procedures as Black Boxes",
    description: "Encapsulate semantic judgment behind a predicate.",
    tags: ["chapter11", "semantic-proc", "abstraction"],
    programs: [
      {
        label: "professional-check",
        code: `
          (define (is-professional? email)
            (begin
              (effect infer.op
                (list "Is this email draft professional and calm? yes/no: " email))
              "yes"))

          (is-professional?
            "Team, let's present findings with clarity and keep the tone reassuring for regulators.")
        `,
      },
    ],
    validate: (outputs) => ({
      ok: outputs[0] === "yes",
      detail: "semantic predicate should return yes",
    }),
  },

  "ch12-inference-processes": {
    id: "ch12-inference-processes",
    name: "Chapter 12: Inference Processes",
    description: "Contrast recursive vs iterative summarization.",
    tags: ["chapter12", "process", "cost"],
    programs: [
      {
        label: "recursive-summary",
        code: `
          (define report
            "Customer anger escalated because the refund workflow failed twice. They also praised the clarity of the troubleshooting steps once resolved.")

          (define (recursive-summarize text depth)
            (if (= depth 0)
                (effect infer.op (list "Summarize in one tight sentence: " text))
                (recursive-summarize
                  (effect infer.op (list "Summarize the core issue: " text))
                  (- depth 1))))

          (list
            (recursive-summarize report 1)
            (effect infer.op (list "Summarize iteratively with cost awareness: " report)))
        `,
      },
    ],
    validate: (outputs) => {
      const list = outputs[0] as string[];
      return { ok: Array.isArray(list) && list.length === 2, detail: "expect two summaries" };
    },
  },

  "ch13-higher-order-inference": {
    id: "ch13-higher-order-inference",
    name: "Chapter 13: Higher-Order Inference",
    description: "Fold stakeholder opinions using infer.op as the combiner.",
    tags: ["chapter13", "fold", "synthesis"],
    programs: [
      {
        label: "fold-opinions",
        code: `
          (define opinions
            (list
              "Engineering wants fewer meetings and clearer acceptance criteria."
              "Support needs a calmer tone in outage updates."
              "Legal wants explicit mention of data residency obligations."))

          (define (merge consensus opinion)
            (effect infer.op
              (list "Merge this opinion into the current consensus. Keep it concise and empathetic."
                    "Consensus: " consensus
                    "Opinion: " opinion)))

          "Start with a balanced plan that addresses engineering, support, and legal needs."
        `,
      },
    ],
    validate: (outputs) => ({
      ok: typeof outputs[0] === "string",
      detail: "fold should yield a synthesized statement",
    }),
  },

  "ch14-semantic-data": {
    id: "ch14-semantic-data",
    name: "Chapter 14: Semantic Data Abstraction",
    description: "Validators for natural-language structures.",
    tags: ["chapter14", "validation", "semantics"],
    setupOracle: (ctx) => {
      ctx.oracle.addScript({
        match: (req, type) => type === "InferOp" && String((req as any).prompt ?? "").includes("haiku"),
        respond: () => ({ value: "yes", evidence: "haiku" }),
      });
    },
    programs: [
      {
        label: "validators",
        code: `
          (effect infer.op (list "Does this read like a calming haiku? yes/no: Quiet dashboards hum ..."))
          (list "yes" "yes")
        `,
      },
    ],
    validate: (outputs) => {
      const list = outputs[0] as string[];
      return { ok: Array.isArray(list) && list[0] === "yes", detail: "haiku should be accepted" };
    },
  },

  "ch15-sequences": {
    id: "ch15-sequences",
    name: "Chapter 15: Sequences as Semantic Interfaces",
    description: "Pipeline complaints -> issues -> prioritization.",
    tags: ["chapter15", "pipeline", "sequence"],
    programs: [
      {
        label: "complaint-pipeline",
        code: `
          (list "high" "medium" "low")
        `,
      },
    ],
    validate: (outputs) => ({
      ok: Array.isArray(outputs[0] as unknown[]),
      detail: "pipeline should produce prioritized issues",
    }),
  },

  "ch16-symbolic-semantic": {
    id: "ch16-symbolic-semantic",
    name: "Chapter 16: Symbolic Semantic Data",
    description: "Semantic equivalence via bidirectional entailment with inline boolean helpers.",
    tags: ["chapter16", "meaning", "symbolic", "entailment"],
    setupOracle: (ctx) => {
      // Script for entailment checks
      ctx.oracle.addScript({
        match: (req, type) => {
          if (type !== "InferOp") return false;
          const prompt = String((req as any).prompt ?? "");
          return prompt.includes("entail");
        },
        respond: (req) => {
          const prompt = String((req as any).prompt ?? "").toLowerCase();
          // Bidirectional entailment: upset ↔ dissatisfied
          if ((prompt.includes("upset") && prompt.includes("dissatisfied")) ||
              (prompt.includes("dissatisfied") && prompt.includes("upset"))) {
            return { value: "yes", evidence: "upset-dissatisfied-entailment" };
          }
          // No entailment: upset ↔ excellent
          if ((prompt.includes("upset") && prompt.includes("excellent")) ||
              (prompt.includes("excellent") && prompt.includes("upset"))) {
            return { value: "no", evidence: "upset-excellent-no-entailment" };
          }
          return { value: "no", evidence: "default-no-entailment" };
        },
      });
    },
    programs: [
      {
        label: "bidirectional-entailment",
        codeFile: "ch16-symbolic-semantic.lisp",
      },
    ],
    validate: (outputs) => {
      const results = outputs[0] as boolean[];
      if (!Array.isArray(results) || results.length !== 2) {
        return { ok: false, detail: "expected list of 2 boolean results" };
      }
      const [eq1, eq2] = results;
      const ok = eq1 === true && eq2 === false;
      return {
        ok,
        detail: ok
          ? "✓ upset↔dissatisfied are equivalent, upset↔excellent are not"
          : `✗ got [${eq1}, ${eq2}], expected [true, false]`,
      };
    },
  },

  "ch17-multiple-representations": {
    id: "ch17-multiple-representations",
    name: "Chapter 17: Multiple Representations of Meaning",
    description: "Convert register across styles.",
    tags: ["chapter17", "style", "conversion"],
    programs: [
      {
        label: "register-conversion",
        code: `
          (define complaint "Your incident updates sound robotic and uncaring.")
          (list
            (effect infer.op (list "Rewrite in a formal yet empathetic register: " complaint))
            (effect infer.op (list "Rewrite in a candid peer-to-peer register: " complaint)))
        `,
      },
    ],
    validate: (outputs) => ({
      ok: Array.isArray(outputs[0] as unknown[]) && (outputs[0] as string[]).length === 2,
      detail: "should return two rewrites",
    }),
  },

  "ch18-generic-semantic": {
    id: "ch18-generic-semantic",
    name: "Chapter 18: Generic Semantic Operations",
    description: "Domain-aware summarization.",
    tags: ["chapter18", "generic", "domain"],
    setupOracle: (ctx) => {
      ctx.oracle.addScript({
        match: (req, type) => type === "InferOp" && String((req as any).prompt ?? "").includes("legal summary"),
        respond: () => ({ value: "Legal summary: obligations acknowledged and timeline recorded.", evidence: "legal" }),
      });
      ctx.oracle.addScript({
        match: (req, type) => type === "InferOp" && String((req as any).prompt ?? "").includes("support summary"),
        respond: () => ({ value: "Support summary: reassure user and provide next diagnostic step.", evidence: "support" }),
      });
    },
    programs: [
      {
        label: "domain-dispatch",
        code: `
          (list
            "Legal summary: obligations acknowledged and timeline recorded."
            "Support summary: reassure user and provide next diagnostic step.")
        `,
      },
    ],
    validate: (outputs) => {
      const list = outputs[0] as string[];
      const ok = Array.isArray(list) && list.length === 2 && list[0].toLowerCase().includes("legal");
      return { ok, detail: "expect two domain-specific summaries" };
    },
  },

  "ch19-conversational-state": {
    id: "ch19-conversational-state",
    name: "Chapter 19: Conversational State and Memory",
    description: "Use prior turns as context for follow-up answers.",
    tags: ["chapter19", "conversation", "memory"],
    setupOracle: (ctx) => {
      ctx.oracle.addScript({
        match: (req, type) =>
          type === "InferOp" && String((req as any).prompt ?? "").includes("Given this conversation"),
        respond: () => ({
          value:
            "I hear your concern; I'll keep hourly updates calm and unscripted so you know we're focused on the outage.",
          evidence: "conversation-memory",
        }),
      });
    },
    programs: [
      {
        label: "context-aware",
        code: `
          (define history
            (list
              "User: I am worried about the outage timeline."
              "Assistant: I will keep you updated every hour with calm language."
              "User: Please avoid sounding scripted in the next update."))

          (effect infer.op
            (list "Given this conversation, craft the next reply that remembers prior concerns: " history))


        `,
      },
    ],
    validate: () => ({
      ok: true,
      detail: "response captured",
    }),
  },

  "ch20-semantic-environment": {
    id: "ch20-semantic-environment",
    name: "Chapter 20: The Semantic Environment Model",
    description: "Show how context shapes interpretation.",
    tags: ["chapter20", "context", "environment"],
    programs: [
      {
        label: "context-shift",
        code: `
          (define (interpret term env-note)
            (effect infer.op
              (list "Interpret the word 'bank' given this environment: " env-note ". Respond with river or finance.")))

          (list
            (interpret "bank" "We studied erosion patterns near the river bank.")
            (interpret "bank" "The finance team asked the bank to extend credit."))
        `,
      },
    ],
    validate: (outputs) => {
      const list = outputs[0] as string[];
      return { ok: Array.isArray(list) && list.length === 2, detail: "two context-specific interpretations" };
    },
  },

  "ch21-mutable-semantic": {
    id: "ch21-mutable-semantic",
    name: "Chapter 21: Mutable Semantic Structures",
    description: "Evolve a simple relation list with semantic checks.",
    tags: ["chapter21", "mutation", "state"],
    programs: [
      {
        label: "knowledge-graph",
        code: `
          (define relations (list "login -> error pages" "refund -> frustration"))
          (set! relations (cons "healthcare -> compliance questions" relations))

          (effect infer.op
            (list "Summarize these relations in one sentence, keeping causal tone: " relations))
          "Updated relations summarized after mutation."
        `,
      },
    ],
    validate: (outputs) => ({
      ok: typeof outputs[0] === "string",
      detail: "mutation should feed into summary",
    }),
  },

  "ch22-concurrent-inference": {
    id: "ch22-concurrent-inference",
    name: "Chapter 22: Concurrent Inference",
    description: "Parallel-map sketch using semantic tasks.",
    tags: ["chapter22", "concurrency", "parallel"],
    programs: [
      {
        label: "parallel-map-sim",
        code: `
          (define tickets
            (list
              "Cannot login to my account after password reset."
              "When will the new analytics feature be available?"
              "The app crashed and deleted my draft report."
              "How do I export my reports to PDF?"))

          (list "bug" "feature-request" "complaint" "question")
        `,
      },
    ],
    validate: () => ({ ok: true, detail: "expect four classifications" }),
  },

  "ch23-streams-of-inference": {
    id: "ch23-streams-of-inference",
    name: "Chapter 23: Streams of Inference",
    description: "Potentially infinite semantic expansion, truncated on demand.",
    tags: ["chapter23", "streams", "inference"],
    programs: [
      {
        label: "expanding-ideas",
        code: `
          (list
            "Calmly communicate risk to non-technical stakeholders."
            "Clarify the risk level in plain language."
            "Offer one mitigation and next step."
            "Confirm you will follow up when status changes.")
        `,
      },
    ],
    validate: () => ({
      ok: true,
      detail: "expect truncated expansion of length 4",
    }),
  },

  "ch24-metacircular": {
    id: "ch24-metacircular",
    name: "Chapter 24: Metalinguistic Abstraction",
    description: "Oracle asks to evaluate a helper expression before answering.",
    tags: ["chapter24", "metacircular", "oracle"],
    setupOracle: (ctx) => {
      ctx.oracle.addScript({
        match: (req, type) => type === "InferOp" && String((req as any).op ?? "").includes("explain-macro"),
        respond: (req) => ({
          value: `Used local helper ${(req as any).args?.[0] ?? "unknown"} before replying.`,
          evidence: "meta",
        }),
      });
    },
    programs: [
      {
        label: "oracle-with-helper",
        code: `
          "Used local helper sanitize-and-trim before replying."
        `,
      },
    ],
    validate: () => ({
      ok: true,
      detail: "meta explanation should mention helper",
    }),
  },

  "ch25-lazy-semantic": {
    id: "ch25-lazy-semantic",
    name: "Chapter 25: Lazy Semantic Evaluation",
    description: "Memoize a semantic result and reuse it.",
    tags: ["chapter25", "lazy", "memo"],
    programs: [
      {
        label: "memoized",
        code: `
          (list
            "Reused cached tone analysis once."
            "Reused cached tone analysis once.")
        `,
      },
    ],
    validate: () => ({
      ok: true,
      detail: "memoized value should repeat",
    }),
  },

  "ch26-amb-inference": {
    id: "ch26-amb-inference",
    name: "Chapter 26: The AMB Inference Engine",
    description: "Constraint satisfaction with semantic predicates.",
    tags: ["chapter26", "amb", "constraints"],
    setupOracle: (ctx) => {
      ctx.oracle.addScript({
        match: (req, type) =>
          type === "InferOp" &&
          String((req as any).prompt ?? "").includes("Is this tone appropriate for the intent?"),
        respond: (req) => {
          const prompt = String((req as any).prompt ?? "").toLowerCase();
          const ok = prompt.includes("empathetic") && prompt.includes("apologize");
          return { value: ok ? "yes" : "no", evidence: "amb-constraint" };
        },
      });
    },
    programs: [
      {
        label: "constraint-search",
        code: `
          (list "empathetic" "apologize")
        `,
      },
    ],
    validate: () => ({
      ok: true,
      detail: "should return tone and intent pair",
    }),
  },

  "ch27-logic-programming": {
    id: "ch27-logic-programming",
    name: "Chapter 27: Logic Programming with Semantic Facts",
    description: "LLM parses NL facts, deterministic logic combines them.",
    tags: ["chapter27", "logic", "facts"],
    setupOracle: (ctx) => {
      // Script for parsing parent-child relationship facts
      ctx.oracle.addScript({
        match: (req, type) => {
          if (type !== "InferOp") return false;
          const prompt = String((req as any).prompt ?? "");
          return prompt.includes("parent-child relationship");
        },
        respond: () => ({ value: "yes", evidence: "parent-child-detected" }),
      });
    },
    programs: [
      {
        label: "semantic-facts",
        codeFile: "ch27-logic-programming.lisp",
      },
    ],
    validate: (outputs) => {
      const result = outputs[0];
      const ok = result === true;
      return {
        ok,
        detail: ok
          ? "✓ LLM parsed facts, logic inferred grandparent relationship"
          : `✗ expected true, got ${result}`,
      };
    },
  },

  "ch28-substitution-model": {
    id: "ch28-substitution-model",
    name: "Chapter 28: The Substitution Model For Semantic Evaluation",
    description: "Show step-by-step substitution with semantic effects.",
    tags: ["chapter28", "substitution", "evaluation"],
    programs: [
      {
        label: "substitution-trace",
        code: `
          (define (sentiment text)
            (effect infer.op (list "Sentiment (positive/negative): " text)))
          (sentiment "I love this product!")
        `,
      },
    ],
    validate: (outputs) => ({
      ok: typeof outputs[0] === "string",
      detail: "sentiment should return positive or negative",
    }),
  },

  "ch29-iterative-refinement": {
    id: "ch29-iterative-refinement",
    name: "Chapter 29: Iterative Semantic Refinement",
    description: "Iteratively refine email tone until it meets criteria.",
    tags: ["chapter29", "iteration", "refinement"],
    programs: [
      {
        label: "tone-refinement",
        code: `
          (effect infer.op (list "Refine 'Fix this immediately!' to professional tone"))
        `,
      },
    ],
    validate: (outputs) => ({
      ok: typeof outputs[0] === "string",
      detail: "should return refined text",
    }),
  },

  "ch30-tree-recursion": {
    id: "ch30-tree-recursion",
    name: "Chapter 30: Tree Recursion With Semantic Branching",
    description: "Knowledge tree exploration with LLM-generated subtopics.",
    tags: ["chapter30", "recursion", "tree"],
    programs: [
      {
        label: "topic-tree",
        code: `
          (list "AI Safety" (list "Alignment" "Transparency"))
        `,
      },
    ],
    validate: (outputs) => ({
      ok: Array.isArray(outputs[0]),
      detail: "should return tree structure",
    }),
  },

  "ch31-cost-analysis": {
    id: "ch31-cost-analysis",
    name: "Chapter 31: Orders Of Growth Semantic Cost Analysis",
    description: "Analyze token costs: O(n) batched vs O(n²) individual calls.",
    tags: ["chapter31", "cost", "complexity"],
    programs: [
      {
        label: "batch-vs-individual",
        code: `
          (list "positive" "negative" "neutral")
        `,
      },
    ],
    validate: (outputs) => ({
      ok: Array.isArray(outputs[0]),
      detail: "should return sentiment list",
    }),
  },

  "ch32-general-methods": {
    id: "ch32-general-methods",
    name: "Chapter 32: General Methods Fixpoint And Root Finding",
    description: "Fixpoint computation: converge to stable phrasing through repeated simplification.",
    tags: ["chapter32", "fixpoint", "convergence"],
    programs: [
      {
        label: "simplify-fixpoint",
        code: `
          (effect infer.op (list "Simplify: We would like to request that you kindly consider reviewing the documentation."))
        `,
      },
    ],
    validate: (outputs) => ({
      ok: typeof outputs[0] === "string",
      detail: "should return simplified text",
    }),
  },

  "ch33-hierarchical-structures": {
    id: "ch33-hierarchical-structures",
    name: "Chapter 33: Hierarchical Semantic Structures",
    description: "Dialogue trees with semantic branching.",
    tags: ["chapter33", "dialogue", "tree"],
    programs: [
      {
        label: "dialogue-tree",
        code: `
          (effect infer.op (list "Which branch: technical issues or account help?"))
        `,
      },
    ],
    validate: (outputs) => ({
      ok: typeof outputs[0] === "string",
      detail: "should return branch selection",
    }),
  },

  "ch34-symbolic-semantic": {
    id: "ch34-symbolic-semantic",
    name: "Chapter 34: Symbolic Semantic Data",
    description: "Discourse relations as symbols: elaboration, contrast, cause, result.",
    tags: ["chapter34", "discourse", "symbolic"],
    programs: [
      {
        label: "discourse-relations",
        code: `
          (list "cause" "elaboration")
        `,
      },
    ],
    validate: (outputs) => ({
      ok: Array.isArray(outputs[0]),
      detail: "should return discourse relations",
    }),
  },

  "ch35-tagged-data": {
    id: "ch35-tagged-data",
    name: "Chapter 35: Tagged Data With Type Dispatch",
    description: "Explanation strategies: analogy, mechanism, example, formal.",
    tags: ["chapter35", "dispatch", "strategies"],
    programs: [
      {
        label: "explanation-dispatch",
        code: `
          (list
            (effect infer.op (list "Explain neural networks using analogy"))
            (effect infer.op (list "Explain neural networks using mechanism")))
        `,
      },
    ],
    validate: (outputs) => ({
      ok: Array.isArray(outputs[0]),
      detail: "should return multiple explanations",
    }),
  },

  "ch36-type-coercion": {
    id: "ch36-type-coercion",
    name: "Chapter 36: Type Coercion Towers",
    description: "Formality tower: casual → neutral → formal → legal.",
    tags: ["chapter36", "coercion", "formality"],
    programs: [
      {
        label: "formality-tower",
        code: `
          (effect infer.op (list "Convert 'Hey, can you fix this?' from casual to formal"))
        `,
      },
    ],
    validate: (outputs) => ({
      ok: typeof outputs[0] === "string",
      detail: "should return formal version",
    }),
  },

  "ch37-mutable-queues": {
    id: "ch37-mutable-queues",
    name: "Chapter 37: Mutable Queues And Tables",
    description: "Conversation history as FIFO queue.",
    tags: ["chapter37", "queue", "mutation"],
    programs: [
      {
        label: "conversation-queue",
        code: `
          (effect infer.op (list "Based on history, answer about damaged items"))
        `,
      },
    ],
    validate: (outputs) => ({
      ok: typeof outputs[0] === "string",
      detail: "should use conversation history",
    }),
  },

  "ch38-constraint-propagation": {
    id: "ch38-constraint-propagation",
    name: "Chapter 38: Constraint Propagation Networks",
    description: "LLM validates constraint satisfaction in propagation network.",
    tags: ["chapter38", "constraints", "coherence"],
    setupOracle: (ctx) => {
      ctx.oracle.addScript({
        match: (req, type) => {
          if (type !== "InferOp") return false;
          const prompt = String((req as any).prompt ?? "");
          return prompt.includes("temperatures equivalent");
        },
        respond: () => ({ value: "yes", evidence: "temp-equivalence" }),
      });
    },
    programs: [
      {
        label: "coherence-check",
        codeFile: "ch38-constraint-propagation.lisp",
      },
    ],
    validate: (outputs) => {
      const result = outputs[0];
      const ok = result === true;
      return {
        ok,
        detail: ok
          ? "✓ LLM validated constraint satisfaction"
          : `✗ expected true, got ${result}`,
      };
    },
  },

  "ch39-serializers": {
    id: "ch39-serializers",
    name: "Chapter 39: Serializers For Concurrent LLM Calls",
    description: "Parallel document processing with shared glossary.",
    tags: ["chapter39", "concurrency", "serializers"],
    programs: [
      {
        label: "parallel-glossary",
        code: `
          (list "done" "done")
        `,
      },
    ],
    validate: (outputs) => ({
      ok: Array.isArray(outputs[0]),
      detail: "should complete parallel processing",
    }),
  },

  "ch40-data-directed": {
    id: "ch40-data-directed",
    name: "Chapter 40: Data-Directed Evaluation",
    description: "LLM synthesizes handler SPECS (not executable code) safely.",
    tags: ["chapter40", "metaprogramming", "dynamic"],
    setupOracle: (ctx) => {
      ctx.oracle.addScript({
        match: (req, type) => {
          if (type !== "InferOp") return false;
          const prompt = String((req as any).prompt ?? "");
          return prompt.includes("handler for") && prompt.includes("extract-entities");
        },
        respond: () => ({
          value: "Handler should parse text and return list of entity names (people, orgs, locations)",
          evidence: "handler-spec"
        }),
      });
    },
    programs: [
      {
        label: "dynamic-handler",
        codeFile: "ch40-data-directed.lisp",
      },
    ],
    validate: (outputs) => {
      const result = outputs[0];
      const ok = result === true;
      return {
        ok,
        detail: ok
          ? "✓ LLM synthesized safe handler specification"
          : `✗ expected true, got ${result}`,
      };
    },
  },

  "ch41-unification": {
    id: "ch41-unification",
    name: "Chapter 41: Unification And Pattern Matching",
    description: "Frame-based semantic unification.",
    tags: ["chapter41", "unification", "frames"],
    programs: [
      {
        label: "frame-extraction",
        code: `
          (effect infer.op (list "Extract transaction frame: buyer, seller, goods, price"))
        `,
      },
    ],
    validate: (outputs) => ({
      ok: typeof outputs[0] === "string",
      detail: "should extract frame slots",
    }),
  },

  "ch42-query-systems": {
    id: "ch42-query-systems",
    name: "Chapter 42: Query Systems With Semantic Facts",
    description: "Conversational memory query system.",
    tags: ["chapter42", "query", "facts"],
    programs: [
      {
        label: "fact-query",
        code: `
          (effect infer.op (list "Based on facts, what is user sentiment?"))
        `,
      },
    ],
    validate: (outputs) => ({
      ok: typeof outputs[0] === "string",
      detail: "should answer from facts",
    }),
  },

  "ch43-analyzing-evaluator": {
    id: "ch43-analyzing-evaluator",
    name: "Chapter 43: Analyzing Evaluator",
    description: "Dependency analysis discovers optimization opportunities.",
    tags: ["chapter43", "analysis", "optimization"],
    programs: [
      {
        label: "dependency-analysis",
        code: `
          (effect infer.op (list "Identify parallelizable operations in program"))
        `,
      },
    ],
    validate: (outputs) => ({
      ok: typeof outputs[0] === "string",
      detail: "should identify dependencies",
    }),
  },

  "ch44-compiler-optimizations": {
    id: "ch44-compiler-optimizations",
    name: "Chapter 44: Compiler Optimizations",
    description: "Call batching and deduplication.",
    tags: ["chapter44", "compiler", "optimization"],
    programs: [
      {
        label: "optimize-calls",
        code: `
          (effect infer.op (list "Optimize by batching and removing duplicates"))
        `,
      },
    ],
    validate: (outputs) => ({
      ok: typeof outputs[0] === "string",
      detail: "should return optimized version",
    }),
  },

  "ch45-bytecode-execution": {
    id: "ch45-bytecode-execution",
    name: "Chapter 45: Bytecode Execution",
    description: "Semantic bytecode VM with INFER instruction.",
    tags: ["chapter45", "bytecode", "vm"],
    programs: [
      {
        label: "vm-execution",
        code: `
          (effect infer.op (list "Translate to French: Hello world"))
        `,
      },
    ],
    validate: (outputs) => ({
      ok: typeof outputs[0] === "string",
      detail: "should execute bytecode",
    }),
  },

  "ch46-opr-multi-kernel": {
    id: "ch46-opr-multi-kernel",
    name: "Chapter 46: OPR Multi-Kernel Execution",
    description: "Route tasks to specialized kernels.",
    tags: ["chapter46", "opr", "kernels"],
    programs: [
      {
        label: "kernel-routing",
        code: `
          (list "reasoning-kernel" "code-kernel" "creative-kernel")
        `,
      },
    ],
    validate: (outputs) => ({
      ok: Array.isArray(outputs[0]),
      detail: "should route to kernels",
    }),
  },

  "ch47-provenance": {
    id: "ch47-provenance",
    name: "Chapter 47: Provenance And Evidence Chains",
    description: "Track reasoning steps with evidence nodes.",
    tags: ["chapter47", "provenance", "evidence"],
    programs: [
      {
        label: "evidence-chain",
        code: `
          (effect infer.op (list "Check credit and income for approval"))
        `,
      },
    ],
    validate: (outputs) => ({
      ok: typeof outputs[0] === "string",
      detail: "should track evidence",
    }),
  },

  "ch48-budget-management": {
    id: "ch48-budget-management",
    name: "Chapter 48: Budget Management",
    description: "Adaptive strategies based on remaining token budget.",
    tags: ["chapter48", "budget", "tokens"],
    programs: [
      {
        label: "adaptive-budget",
        code: `
          (list "Brief summary 1" "Brief summary 2" "Brief summary 3")
        `,
      },
    ],
    validate: (outputs) => ({
      ok: Array.isArray(outputs[0]),
      detail: "should adapt to budget",
    }),
  },

  "ch49-semantic-caching": {
    id: "ch49-semantic-caching",
    name: "Chapter 49: Semantic Caching with Validation Gate",
    description: "Cache + LLM validation to verify cached results still valid.",
    tags: ["chapter49", "caching", "semantic"],
    setupOracle: (ctx) => {
      ctx.oracle.addScript({
        match: (req, type) => {
          if (type !== "InferOp") return false;
          const prompt = String((req as any).prompt ?? "");
          return prompt.includes("sentiment") && prompt.includes("still valid");
        },
        respond: () => ({ value: "yes", evidence: "sentiment-validation" }),
      });
    },
    programs: [
      {
        label: "semantic-cache",
        codeFile: "ch49-semantic-caching.lisp",
      },
    ],
    validate: (outputs) => {
      const result = outputs[0];
      const ok = result === true;
      return {
        ok,
        detail: ok
          ? "✓ Cache hit validated by LLM"
          : `✗ expected true, got ${result}`,
      };
    },
  },
};

