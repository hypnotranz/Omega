# OmegaLLM: Before and After

## The Problem

```python
# Typical agent analyzing 100 files
for file in files:
    result = agent.tool_call("analyze", file)  # 30 seconds each
    results.append(result)                      # 100 calls = 50 minutes

# Process crashes? Start over. All state lost.
```

## The Solution

```bash
npm run demo-instant   # Run this now - no API key needed
```

**Output:**
```
Files to analyze: ("auth.ts" "user.ts" "api.ts" "db.ts")
After map: ("✓ auth.ts analyzed" "✓ user.ts analyzed" "✓ api.ts analyzed" "✓ db.ts analyzed")

Finding first x > 3 from (1 2 3 4 5):
  Result: 4 (auto-backtracked past 1, 2, 3!)

Infinite fibonacci, take first 12:
  (0 1 1 2 3 5 8 13 21 34 55 89)

After filter → map → fold: 60
```

## Three Key Improvements

### 1. Parallel Operations Replace Serial Loops

**Before:** 100 sequential tool calls
```python
for file in files:
    agent.analyze(file)  # One at a time
```

**After:** Single expression
```lisp
(map analyze-file all-files)
```

### 2. Automatic Backtracking on Validation Failure

**Before:** Manual retry logic
```python
for approach in ["formal", "casual", "apologetic"]:
    result = llm.generate(approach)
    if validator.check(result):
        break
```

**After:** Declarative
```lisp
(let ((tone (amb "formal" "casual" "apologetic")))
  (require (valid? (generate tone)))  ; Backtracks automatically
  result)
```

### 3. Persistent Sessions Across Restarts

**Before:** State lost on exit
```python
# Session 1
agent.define("x", 42)

# Session 2 (new process)
agent.get("x")  # ERROR: x undefined
```

**After:** State survives
```text
# Session 1
Omega> (define x 42)
Omega> :session save mywork
Omega> :quit

# Session 2 (hours later)
Omega> :session load mywork
Omega> :session goto 3
Omega> x
=> 42  # Restored
```

## Comparison Summary

| Aspect | Traditional Agent | OmegaLLM |
|--------|------------------|----------|
| File processing | 100 sequential calls | 1 parallel expression |
| Retry logic | Manual if/else | Automatic backtracking |
| State persistence | Lost on exit | Sessions survive restarts |
| Debugging | print statements | Step-through with time travel |
| Provenance | None | Receipts for every LLM call |

## Quick Start

```bash
cd OmegaLLM
npm install && npm run build
npm run demo-instant   # Works immediately, no API key

# With LLM:
echo "OPENAI_API_KEY=sk-..." > .env
npm run omega-fast
```

```text
Omega> (map (lambda (x) (effect infer.op (list "Summarize: " x)))
            (list "cats are pets" "dogs are loyal" "fish swim"))
=> ("Cats are domestic animals" "Dogs are faithful companions" "Fish are aquatic")
```

One expression. Three parallel summaries.
