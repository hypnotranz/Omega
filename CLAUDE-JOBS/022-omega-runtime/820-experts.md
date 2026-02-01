# 820: Expert Manager

## Status: COMPLETE ✅
- **Completed**: 2026-01-29
- **Location**: OmegaLLM-3/src/runtime/subsystems/ExpertManager.ts (500 lines)

## Purpose
Manages domain-specific expert prompts and personas for specialized inference tasks.

## Dependencies
- 100-types.md ✅
- 110-events.md ✅
- 800-llm-integration.md ✅

## Source References
- ARCHITECTURE/29-EXPERTS.md
- Chain-of-experts patterns
- Mixture of experts concepts

---

## Deliverables

```
src/runtime/subsystems/
├── ExpertsManager.ts        # Main expert manager
└── experts/
    ├── Expert.ts            # Expert type definition
    ├── ExpertLibrary.ts     # Built-in experts
    └── ExpertRouter.ts      # Expert selection
```

---

## Key Types

```typescript
export interface Expert {
  id: string;
  name: string;
  description: string;
  domain: string;            // e.g., "mathematics", "coding", "writing"

  // Prompt configuration
  systemPrompt: string;
  examples?: { input: string; output: string }[];
  constraints?: string[];

  // Parameters
  defaultTemperature?: number;
  preferredModel?: string;

  // Metadata
  version?: string;
  author?: string;
  tags?: string[];
}

export interface ExpertConsultation {
  expertId: string;
  question: string;
  response: string;
  confidence?: number;
  reasoning?: string;
  timestamp: number;
  usage?: LLMUsage;
}

export interface ExpertRoutingResult {
  selectedExpert: Expert;
  confidence: number;
  alternatives: { expert: Expert; score: number }[];
  reasoning: string;
}
```

---

## Key Interface

```typescript
export interface ExpertsManager {
  // ─── Expert Management ───

  /**
   * Register an expert.
   */
  register(expert: Expert): void;

  /**
   * Get expert by ID.
   */
  get(id: string): Expert | undefined;

  /**
   * Get all experts.
   */
  list(): Expert[];

  /**
   * Get experts by domain.
   */
  getByDomain(domain: string): Expert[];

  /**
   * Remove expert.
   */
  remove(id: string): boolean;

  // ─── Consultation ───

  /**
   * Consult a specific expert.
   */
  consult(expertId: string, question: string): Promise<ExpertConsultation>;

  /**
   * Consult with automatic expert selection.
   */
  consultAuto(question: string): Promise<ExpertConsultation>;

  /**
   * Consult multiple experts and aggregate.
   */
  consultPanel(
    expertIds: string[],
    question: string,
    aggregation?: 'vote' | 'synthesize' | 'debate'
  ): Promise<ExpertConsultation>;

  // ─── Routing ───

  /**
   * Select best expert for question.
   */
  route(question: string): Promise<ExpertRoutingResult>;

  /**
   * Set routing strategy.
   */
  setRoutingStrategy(strategy: 'keyword' | 'embedding' | 'llm'): void;

  // ─── History ───

  /**
   * Get consultation history.
   */
  getHistory(options?: { expertId?: string; limit?: number }): ExpertConsultation[];

  /**
   * Clear history.
   */
  clearHistory(): void;
}
```

---

## Built-in Experts

```typescript
const BUILTIN_EXPERTS: Expert[] = [
  {
    id: 'mathematician',
    name: 'Mathematician',
    description: 'Expert in mathematical reasoning and proofs',
    domain: 'mathematics',
    systemPrompt: `You are an expert mathematician. You approach problems systematically:
1. Identify what is being asked
2. Note relevant theorems or formulas
3. Show step-by-step reasoning
4. Verify the answer makes sense

Be precise with notation and explain your reasoning clearly.`,
    defaultTemperature: 0.3
  },

  {
    id: 'coder',
    name: 'Software Engineer',
    description: 'Expert in programming and software design',
    domain: 'coding',
    systemPrompt: `You are an expert software engineer. You write clean, efficient code:
1. Understand the requirements
2. Consider edge cases
3. Write readable, well-commented code
4. Suggest tests to verify correctness

Always explain your design choices and any trade-offs.`,
    defaultTemperature: 0.2
  },

  {
    id: 'writer',
    name: 'Creative Writer',
    description: 'Expert in creative writing and storytelling',
    domain: 'writing',
    systemPrompt: `You are an expert creative writer. You craft engaging prose:
1. Consider the tone and audience
2. Use vivid imagery and active voice
3. Vary sentence structure for rhythm
4. Show, don't tell

Let creativity flow while maintaining clarity.`,
    defaultTemperature: 1.0
  },

  {
    id: 'analyst',
    name: 'Data Analyst',
    description: 'Expert in data analysis and interpretation',
    domain: 'data',
    systemPrompt: `You are an expert data analyst. You extract insights from information:
1. Identify patterns and trends
2. Consider statistical significance
3. Present findings clearly
4. Note limitations and caveats

Be precise with numbers and honest about uncertainty.`,
    defaultTemperature: 0.4
  },

  {
    id: 'critic',
    name: 'Critical Thinker',
    description: 'Expert in logical analysis and argumentation',
    domain: 'reasoning',
    systemPrompt: `You are an expert in critical thinking. You analyze arguments:
1. Identify premises and conclusions
2. Check for logical fallacies
3. Evaluate evidence quality
4. Consider counterarguments

Be fair but rigorous in your analysis.`,
    defaultTemperature: 0.3
  }
];
```

---

## Expert Routing

```typescript
async function route(question: string): Promise<ExpertRoutingResult> {
  switch (this.routingStrategy) {
    case 'keyword':
      return this.routeByKeyword(question);
    case 'embedding':
      return this.routeByEmbedding(question);
    case 'llm':
      return this.routeByLLM(question);
  }
}

private async routeByLLM(question: string): Promise<ExpertRoutingResult> {
  const experts = this.list();
  const expertList = experts.map(e => `- ${e.id}: ${e.description}`).join('\n');

  const response = await this.llm.infer(`
Given this question:
"${question}"

And these available experts:
${expertList}

Which expert is best suited to answer? Respond with:
EXPERT: <expert-id>
CONFIDENCE: <0-1>
REASONING: <why this expert>
  `);

  // Parse response...
  return {
    selectedExpert: this.get(parsedExpertId)!,
    confidence: parsedConfidence,
    alternatives: [],
    reasoning: parsedReasoning
  };
}
```

---

## Lisp Interface

```lisp
;; Consult specific expert
(experts.consult 'mathematician "What is the derivative of x^2?")
; => "The derivative of x^2 is 2x, using the power rule..."

;; Auto-select expert
(experts.ask "Write a poem about the moon")
; => Routes to 'writer, returns poem

;; Panel consultation
(experts.panel '(mathematician critic)
               "Is this proof valid?"
               :aggregation 'debate)
; => Synthesized response from debate

;; Register custom expert
(experts.define 'my-expert
  :name "Domain Expert"
  :domain "my-domain"
  :system-prompt "You are an expert in...")

;; List available experts
(experts.list)
; => ((mathematician ...) (coder ...) ...)
```

---

## Event Emission

```typescript
// When expert is consulted
emitter.emit('expert-consulted', {
  expertId,
  question,
  response,
  usage
});

// When routing occurs
emitter.emit('expert-routed', {
  question,
  selectedExpert,
  confidence,
  alternatives
});

// When panel convenes
emitter.emit('expert-panel', {
  experts,
  question,
  responses,
  aggregatedResult
});
```

---

## Test Requirements

### Unit Tests: `tests/runtime/subsystems/ExpertsManager.test.ts`
- [ ] register() adds expert
- [ ] get() retrieves expert
- [ ] list() returns all experts
- [ ] getByDomain() filters correctly
- [ ] remove() removes expert
- [ ] consult() uses correct expert
- [ ] route() selects appropriate expert
- [ ] consultPanel() aggregates correctly
- [ ] History is tracked
- [ ] Built-in experts are available

### Integration Tests
- [ ] Full consultation flow
- [ ] Auto-routing selects correctly
- [ ] Panel debate produces synthesis
- [ ] Custom experts work
- [ ] Budget tracking for consultations

---

## Acceptance Criteria
1. Experts encapsulate domain-specific prompts
2. Routing selects appropriate expert
3. Panel consultations aggregate responses
4. Custom experts can be defined
5. Built-in library covers common domains
6. Events enable monitoring
