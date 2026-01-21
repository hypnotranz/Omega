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
          (analyze-sentiment "I love how carefully you explained the migration steps.")
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
          (define (is-complaint? text)
            (equal? "yes"
              (effect infer.op
                (list "Is this customer note a complaint? yes/no: " text))))

          (define messages
            (list
              "Your latest release fixed the crash immediately."
              "My data export failed again and I'm getting frustrated."
              "Could you clarify the compliance statement for healthcare customers?"
              "This delay in refund approval feels unfair."))

          (filter is-complaint? messages)
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

          (list
            (classify-risk "Credentials leaked on a public git repo with customer secrets.")
            (classify-risk "Routine maintenance window notification with no user impact."))
        `,
      },
    ],
    validate: (outputs) => {
      const results = outputs[0] as string[];
      const ok = Array.isArray(results) && results[0] === "high" && results[1] === "low";
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
            return { value: "We acknowledge the delay and will provide an updated schedule.", evidence: "tone-formal" };
          }
          if (prompt.includes("friendly")) {
            return { value: "Hey there! We're almost done—thanks for hanging in.", evidence: "tone-friendly" };
          }
          return {
            value: "I'm sorry this shipment slipped. I will correct it and keep you updated personally.",
            evidence: "tone-apologetic",
          };
        },
      });

      ctx.oracle.addScript({
        match: (req, type) => type === "InferOp" && String((req as any).prompt ?? "").includes("Does this reply"),
        respond: (req) => {
          const prompt = String((req as any).prompt ?? "");
          return { value: prompt.includes("sorry") ? "yes" : "no", evidence: "tone-check" };
        },
      });
    },
    programs: [
      {
        label: "tone-search",
        code: `
          (define tones (list "formal" "friendly" "apologetic"))

          (define (matches-tone? reply desired)
            (equal? "yes"
              (effect infer.op
                (list "Does this reply use a " desired " tone? yes/no: " reply))))

          (let ((tone (amb "formal" "friendly" "apologetic")))
            (let ((reply (effect infer.op
                            (list "Write a " tone " response acknowledging a delayed shipment."))))
              (require (matches-tone? reply "apologetic"))
              reply))
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
          (define notes
            (list
              "The incident response runbook feels outdated."
              "Our healthcare customers need clearer assurances about data residency."
              "The onboarding emails sound too robotic."))

          (define (follow-up note)
            (effect infer.op
              (list "Suggest one follow-up question to clarify this note. Keep it empathetic: " note)))

          (define s (list->stream notes))
          (define queued (stream-map follow-up s))
          (stream->list queued 2)
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
          (define ask-runtime (oracle-lambda (question) "agentic-query"))
          (ask-runtime "How many urgent tickets are active? Call (length active-tickets) before answering.")
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
            (equal? "yes"
              (effect infer.op
                (list "Is this email draft professional and calm? yes/no: " email))))

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

          (fold-left merge "Start with a balanced plan." opinions)
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
          (define (is-haiku? poem)
            (equal? "yes" (effect infer.op (list "Does this read like a calming haiku? yes/no: " poem))))

          (define (has-greeting? email)
            (equal? "yes" (effect infer.op (list "Does this email open with a courteous greeting? yes/no: " email))))

          (list
            (is-haiku? "Quiet dashboards hum / Incidents fall back asleep / Teams breathe evenly")
            (has-greeting? "Hello team, thank you for the latest build—can we add a changelog?"))
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
    description: "Pipeline complaints → issues → prioritization.",
    tags: ["chapter15", "pipeline", "sequence"],
    programs: [
      {
        label: "complaint-pipeline",
        code: `
          (define complaints
            (list
              "The new security banner sounds alarming to clinicians."
              "I cannot export my case notes; the button feels hidden."
              "Payment reminders sound harsh and transactional.")) 

          (define (extract-issue text)
            (effect infer.op
              (list "Extract the core issue in 6 words: " text)))

          (define (prioritize issue)
            (effect infer.op
              (list "Label this issue urgency (high/medium/low): " issue)))

          (map prioritize (map extract-issue complaints))
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
    description: "Meaning equivalence checks on emotionally worded phrases.",
    tags: ["chapter16", "meaning", "symbolic"],
    programs: [
      {
        label: "meaning-checks",
        code: `
          (define (same-meaning? a b)
            (equal? "true"
              (effect infer.op (list "Do these mean the same thing? true/false: " a " | " b))))

          (list
            (same-meaning? "I feel upset about the delay" "I am dissatisfied with how long this is taking")
            (same-meaning? "This is delightful" "This is unacceptable"))
        `,
      },
    ],
    validate: (outputs) => {
      const list = outputs[0] as string[];
      const ok = Array.isArray(list) && list[0] === "true";
      return { ok, detail: "similar phrases should be marked true" };
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
          (define issue "Customer shared medical data while requesting a refund and asked for SOC2 proof.")

          (define (summarize-legal text)
            (effect infer.op (list "Provide a legal summary highlighting duties: " text)))

          (define (summarize-support text)
            (effect infer.op (list "Provide a support summary with calming reassurance: " text)))

          (list (summarize-legal issue) (summarize-support issue))
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
    validate: (outputs) => ({
      ok: typeof outputs[0] === "string",
      detail: "response should be a string shaped by history",
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

          (define (classify ticket)
            (effect infer.op
              (list "Classify this support ticket. Return bug/feature-request/question/complaint: " ticket)))

          (define (parallel-map f xs) (map f xs)) ; sequential stand-in for demo
          (parallel-map classify tickets)
        `,
      },
    ],
    validate: (outputs) => {
      const list = outputs[0] as string[];
      return { ok: Array.isArray(list) && list.length === 4, detail: "expect four classifications" };
    },
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
          (define (expand idea)
            (effect infer.op
              (list "Generate a richer explanation building on this idea: " idea)))

          (define (iterate n seed)
            (if (= n 0)
                (list seed)
                (cons seed (iterate (- n 1) (expand seed)))))

          (iterate 3 "Calmly communicate risk to non-technical stakeholders.")
        `,
      },
    ],
    validate: (outputs) => ({
      ok: Array.isArray(outputs[0] as unknown[]) && (outputs[0] as unknown[]).length === 4,
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
          (define helper "sanitize-and-trim")
          (define explain (oracle-lambda (hint) "explain-macro"))
          (explain helper)
        `,
      },
    ],
    validate: (outputs) => ({
      ok: typeof outputs[0] === "string" && String(outputs[0]).includes("sanitize"),
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
          (define cached #f)

          (define (analyze text)
            (if cached
                cached
                (begin
                  (set! cached (effect infer.op (list "Assess emotional temperature (calm/tense): " text)))
                  cached)))

          (list
            (analyze "The outage update felt tense and robotic.")
            (analyze "Reusing the memoized tone analysis to avoid another call."))
        `,
      },
    ],
    validate: (outputs) => {
      const list = outputs[0] as string[];
      return { ok: Array.isArray(list) && list.length === 2 && list[0] === list[1], detail: "memoized value should repeat" };
    },
  },

  "ch26-amb-inference": {
    id: "ch26-amb-inference",
    name: "Chapter 26: The AMB Inference Engine",
    description: "Constraint satisfaction with semantic predicates.",
    tags: ["chapter26", "amb", "constraints"],
    programs: [
      {
        label: "constraint-search",
        code: `
          (define tones (list "formal" "empathetic" "playful"))
          (define intents (list "explain risk" "apologize" "upsell"))

          (define (fits? tone intent)
            (equal? "yes"
              (effect infer.op
                (list "Is this tone appropriate for the intent? yes/no: " tone " -> " intent))))

          (let ((tone (amb "formal" "empathetic" "playful")))
            (let ((intent (amb "explain risk" "apologize")))
              (require (fits? tone intent))
              (list tone intent)))
        `,
      },
    ],
    validate: (outputs) => {
      const pair = outputs[0] as string[];
      return { ok: Array.isArray(pair) && pair.length === 2, detail: "should return tone and intent pair" };
    },
  },

  "ch27-logic-programming": {
    id: "ch27-logic-programming",
    name: "Chapter 27: Logic Programming with Semantic Facts",
    description: "Query natural language facts with semantic matching.",
    tags: ["chapter27", "logic", "facts"],
    setupOracle: (ctx) => {
      ctx.oracle.addScript({
        match: (req, type) => type === "InferOp" && String((req as any).prompt ?? "").includes("grandparent"),
        respond: () => ({ value: "yes", evidence: "fact-check" }),
      });
    },
    programs: [
      {
        label: "semantic-facts",
        code: `
          (define facts
            (list
              "Alice is Bob's parent."
              "Bob is Carol's parent."
              "Dana mentors Erin in compliance audits."))

          (define (is-grandparent? a c)
            (equal? "yes"
              (effect infer.op
                (list "Given these facts, is " a " the grandparent of " c "? yes/no: " facts))))

          (is-grandparent? "Alice" "Carol")
        `,
      },
    ],
    validate: (outputs) => ({
      ok: outputs[0] === "yes",
      detail: "grandparent query should succeed",
    }),
  },
};
