# 29: Experts (Intent Compilation & Expert Layers)

## The Bridge Problem

How do you go from natural language to executable code?

```
"Fix the security bug in the authentication module"
                    ↓
                    ???
                    ↓
(begin
  (let ((auth-file (world.read "src/auth.ts")))
    (let ((fixed (fix-vulnerability auth-file)))
      (world.write "src/auth.ts" fixed))))
```

**Experts solve this** through a layered system that compiles intent to Lisp.

---

## Three-Layer Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│                    Expert System Layers                         │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  Layer A: TOOL CONTRACT (stable, non-negotiable)                │
│  ┌─────────────────────────────────────────────────────────┐    │
│  │  "You are a Lisp programmer. Output one S-expression."  │    │
│  │  - Available primitives: world.*, llm.*, facts, etc.    │    │
│  │  - Output format: strict S-expression                    │    │
│  │  - No prose, no markdown, no explanation                │    │
│  └─────────────────────────────────────────────────────────┘    │
│                              ↓                                  │
│  Layer B: ROLE OVERLAY (domain expertise)                       │
│  ┌─────────────────────────────────────────────────────────┐    │
│  │  "You have PhDs in security, distributed systems..."    │    │
│  │  - Domain-specific knowledge                             │    │
│  │  - Meta-expertise (how to approach problems)            │    │
│  │  - Reasoning strategies                                  │    │
│  └─────────────────────────────────────────────────────────┘    │
│                              ↓                                  │
│  Layer C: TASK ENVELOPE (per-call context)                      │
│  ┌─────────────────────────────────────────────────────────┐    │
│  │  "Analyze this code for vulnerabilities..."              │    │
│  │  - Specific task description                             │    │
│  │  - Output mode (REPORT, PLAN, PROGRAM, ANALYSIS)        │    │
│  │  - Available context and constraints                     │    │
│  └─────────────────────────────────────────────────────────┘    │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

---

## Layer A: Tool Contract

The foundation - never changes:

```typescript
const TOOL_CONTRACT = `
You are a Lisp programmer with extensive experience. You must output exactly one valid S-expression that uses the available primitives to accomplish the task.

## Available Primitives

### World Operations
- (world.read path) - Read file content
- (world.write path content) - Write file content
- (world.list pattern) - List files matching glob
- (world.search pattern) - Search file contents
- (world.fingerprint path) - Get content hash

### LLM Operations
- (llm.complete prompt) - Get LLM completion
- (intent "description") - Compile intent to code

### Knowledge Operations
- (assert '(tag value)) - Assert a fact
- (fact? '(tag value)) - Check if fact exists
- (facts) - Get all facts

### Control Flow
- (fixpoint body :max-iters n) - Run until stable
- (subeval body) - Isolated evaluation
- (memo deps body) - Memoized computation

### Evidence
- (evidence/capture ref :lines start end) - Capture evidence
- (evidence/verify ev) - Check if evidence is fresh

## Output Rules
1. Output EXACTLY ONE S-expression
2. No prose before or after
3. No markdown formatting
4. No comments explaining your reasoning
5. The S-expression must be syntactically valid
6. Use only the primitives listed above

## Example Output
(begin
  (let ((content (world.read "src/auth.ts")))
    (assert '(file-analyzed "src/auth.ts"))
    content))
`;
```

---

## Layer B: Role Overlay

Domain expertise injected per problem type:

```typescript
interface ExpertRole {
  id: string;
  name: string;
  expertise: string[];
  metaExpertise: string;
  reasoningStyle: string;
}

const SECURITY_EXPERT: ExpertRole = {
  id: 'security',
  name: 'Security Expert',
  expertise: [
    'PhD in Computer Security',
    'Expert in OWASP Top 10 vulnerabilities',
    'Deep knowledge of authentication and authorization patterns',
    'Experience with secure coding practices in TypeScript/JavaScript',
  ],
  metaExpertise: `
    When analyzing code for security:
    1. First understand the authentication/authorization model
    2. Trace data flow from untrusted sources
    3. Check for injection points (SQL, command, XSS)
    4. Verify secrets are not hardcoded
    5. Check cryptographic usage is correct
  `,
  reasoningStyle: 'systematic-threat-modeling',
};

const REFACTORING_EXPERT: ExpertRole = {
  id: 'refactoring',
  name: 'Refactoring Expert',
  expertise: [
    'PhD in Software Engineering',
    'Expert in design patterns and SOLID principles',
    'Deep knowledge of TypeScript type system',
    'Experience with large-scale codebase migrations',
  ],
  metaExpertise: `
    When refactoring code:
    1. Understand current behavior through tests
    2. Identify code smells and coupling
    3. Plan incremental changes that preserve behavior
    4. Verify each step doesn't break tests
  `,
  reasoningStyle: 'incremental-transformation',
};

function buildRoleOverlay(role: ExpertRole): string {
  return `
## Your Expertise
${role.expertise.map(e => `- ${e}`).join('\n')}

## How to Approach Problems
${role.metaExpertise}

## Reasoning Style: ${role.reasoningStyle}
`;
}
```

---

## Layer C: Task Envelope

Per-call context and output mode:

```typescript
type OutputMode =
  | 'REPORT'    // Structured findings report
  | 'PLAN'      // Step-by-step execution plan
  | 'PROGRAM'   // Executable Lisp code
  | 'ANALYSIS'  // Reasoning without actions
  ;

interface TaskEnvelope {
  description: string;
  outputMode: OutputMode;
  context: {
    files?: string[];
    facts?: Value[];
    constraints?: string[];
  };
  expectedSchema?: string;  // For REPORT/PLAN modes
}

function buildTaskEnvelope(task: TaskEnvelope): string {
  let prompt = `
## Task
${task.description}

## Output Mode: ${task.outputMode}
`;

  switch (task.outputMode) {
    case 'REPORT':
      prompt += `
Output a (report ...) structure with findings:
(report
  :title "Report Title"
  :summary "Brief summary"
  :findings (
    (finding :description "..." :severity critical|high|medium|low :evidence (ev-id ...))
    ...
  ))
`;
      break;

    case 'PLAN':
      prompt += `
Output a (plan ...) structure with steps:
(plan
  :goal "What the plan accomplishes"
  :steps (
    (step :action "..." :rationale "..." :evidence (ev-id ...))
    ...
  ))
`;
      break;

    case 'PROGRAM':
      prompt += `
Output executable Lisp code that accomplishes the task.
The code should gather evidence, make assertions, and produce results.
`;
      break;

    case 'ANALYSIS':
      prompt += `
Output a (analysis ...) structure with reasoning:
(analysis
  :question "What was analyzed"
  :observations (...)
  :conclusions (...)
  :confidence 0.0-1.0)
`;
      break;
  }

  if (task.context.files?.length) {
    prompt += `
## Available Files
${task.context.files.map(f => `- ${f}`).join('\n')}
`;
  }

  if (task.context.constraints?.length) {
    prompt += `
## Constraints
${task.context.constraints.map(c => `- ${c}`).join('\n')}
`;
  }

  return prompt;
}
```

---

## Intent Compilation

The `intent` special form compiles natural language to Lisp:

```typescript
async function compileIntent(
  text: string,
  role: ExpertRole,
  outputMode: OutputMode,
  llm: LLMAdapter
): Promise<Value> {
  // Build full prompt
  const prompt = [
    TOOL_CONTRACT,
    buildRoleOverlay(role),
    buildTaskEnvelope({
      description: text,
      outputMode,
      context: {},
    }),
  ].join('\n\n');

  // Get LLM completion
  const response = await llm.complete(prompt);

  // Parse response as S-expression
  const parsed = read(response.trim());

  // Validate structure based on output mode
  validateOutput(parsed, outputMode);

  return parsed;
}

function validateOutput(expr: Value, mode: OutputMode): void {
  if (!Array.isArray(expr)) {
    throw new Error('Expected S-expression, got atom');
  }

  const head = expr[0];
  const expectedHead = {
    'REPORT': 'report',
    'PLAN': 'plan',
    'PROGRAM': null,  // Any valid code
    'ANALYSIS': 'analysis',
  }[mode];

  if (expectedHead && symbolName(head) !== expectedHead) {
    throw new Error(`Expected (${expectedHead} ...), got (${symbolName(head)} ...)`);
  }
}
```

---

## Lisp API

```lisp
;; Compile intent with default expert
(intent "analyze src/auth.ts for security vulnerabilities")

;; Compile with specific expert role
(intent "analyze src/auth.ts for security vulnerabilities"
  :expert 'security
  :mode 'REPORT)

;; Dynamic intent compilation
(intent* (str "analyze " target-file " for " issue-type))

;; Expert-specific helpers
(security/analyze "src/auth.ts")        ;; Security expert
(refactoring/suggest "src/utils.ts")    ;; Refactoring expert
(performance/profile "src/api.ts")      ;; Performance expert
```

---

## Parallel Expert Execution

Run multiple experts on same problem:

```typescript
interface ParallelExpertConfig {
  experts: ExpertRole[];
  task: TaskEnvelope;
  aggregation: 'first' | 'vote' | 'merge' | 'all';
}

async function runParallelExperts(
  config: ParallelExpertConfig,
  llm: LLMAdapter
): Promise<Value[]> {
  // Launch all experts in parallel
  const promises = config.experts.map(expert =>
    compileIntent(config.task.description, expert, config.task.outputMode, llm)
  );

  const results = await Promise.all(promises);

  switch (config.aggregation) {
    case 'first':
      return [results[0]];

    case 'vote':
      // Return most common result
      return [findMostCommon(results)];

    case 'merge':
      // Combine findings from all experts
      return [mergeReports(results)];

    case 'all':
      return results;
  }
}
```

```lisp
;; Run multiple experts in parallel
(experts/parallel
  (list 'security 'refactoring 'performance)
  "analyze src/api.ts"
  :mode 'REPORT
  :aggregation 'merge)
```

---

## Expert Batching

Batch multiple tasks for efficiency:

```typescript
async function batchIntents(
  intents: string[],
  role: ExpertRole,
  llm: LLMAdapter
): Promise<Value[]> {
  // Use llm.complete/batch for parallel execution
  const prompts = intents.map(intent => [
    TOOL_CONTRACT,
    buildRoleOverlay(role),
    buildTaskEnvelope({ description: intent, outputMode: 'PROGRAM', context: {} }),
  ].join('\n\n'));

  const responses = await llm.completeBatch(prompts);
  return responses.map(r => read(r.trim()));
}
```

```lisp
;; Batch multiple intents
(intent/batch
  (list
    "check file1.ts for issues"
    "check file2.ts for issues"
    "check file3.ts for issues")
  :expert 'security)
```

---

## Output Mode Examples

### REPORT Mode

```lisp
(intent "analyze src/auth.ts for security vulnerabilities"
  :mode 'REPORT)

;; Output:
(report
  :title "Security Analysis: src/auth.ts"
  :summary "Found 2 critical vulnerabilities"
  :findings (
    (finding
      :description "SQL injection in login query"
      :severity critical
      :evidence ("ev:sha256:abc123")
      :location (:file "src/auth.ts" :line 42))
    (finding
      :description "Passwords stored without hashing"
      :severity critical
      :evidence ("ev:sha256:def456")
      :location (:file "src/auth.ts" :line 78))))
```

### PLAN Mode

```lisp
(intent "fix the security vulnerabilities in src/auth.ts"
  :mode 'PLAN)

;; Output:
(plan
  :goal "Remediate security vulnerabilities"
  :steps (
    (step
      :action "Use parameterized queries for login"
      :rationale "Prevents SQL injection"
      :evidence ("ev:sha256:abc123"))
    (step
      :action "Hash passwords with bcrypt before storage"
      :rationale "Protects credentials if DB is compromised"
      :evidence ("ev:sha256:def456"))))
```

### PROGRAM Mode

```lisp
(intent "gather evidence for security issues in src/auth.ts"
  :mode 'PROGRAM)

;; Output:
(begin
  (let ((content (world.read "src/auth.ts")))
    (when (contains? content "SELECT * FROM users WHERE")
      (let ((ev (evidence/capture "src/auth.ts"
                  :lines (find-line content "SELECT"))))
        (assert '(security-issue sql-injection) :evidence ev)))
    (when (contains? content "password =")
      (let ((ev (evidence/capture "src/auth.ts"
                  :lines (find-line content "password ="))))
        (assert '(security-issue plaintext-password) :evidence ev)))
    (facts)))
```

---

## Integration with Session

Experts run within session constraints:

```typescript
class Session {
  async runExpert(
    intent: string,
    role: ExpertRole,
    mode: OutputMode
  ): Promise<Outcome> {
    // Check budget
    this.budget.check('tokens');

    // Compile intent
    const code = await compileIntent(intent, role, mode, this.llm);

    // Execute in session context
    if (mode === 'PROGRAM') {
      return this.executeTurn(print(code));
    } else {
      // REPORT/PLAN/ANALYSIS are data, return directly
      return ok(code);
    }
  }
}
```

---

## Summary

Experts provide:

1. **Three-layer architecture** - Contract, Role, Task separation
2. **Intent compilation** - Natural language → Lisp code
3. **Output modes** - REPORT, PLAN, PROGRAM, ANALYSIS
4. **Domain expertise** - Security, refactoring, performance, etc.
5. **Parallel execution** - Multiple experts on same problem
6. **Batching** - Efficient multi-task processing

Experts are the bridge that makes LLMs usable for structured reasoning.
