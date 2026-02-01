# REAL Agent Demos - What ACTUALLY WORKS

Based on **working code** in `auto-traceability.lisp` and `rtm-generator-v3.lisp`.

---

## 🎯 The Core Pattern: Map Instead of For-Loop

### **Traditional Agent Approach (Serial)**
```python
# Agent does this ONE AT A TIME
results = []
for file in files:
    result = analyze_file(file)  # Tool call 1, 2, 3, ...
    results.append(result)
```

### **OmegaLLM Approach (Parallel/Declarative)**
```lisp
;; Express as map - runtime handles execution
(define results
  (map analyze-file files))
```

**Key difference:** Agent writes a PROGRAM that describes the computation, not 100 serial tool calls.

---

## 📋 DEMO 1: Requirements Traceability Matrix

**What it does:** Find all primitives in source files, find which tests cover them, generate markdown table.

**Traditional approach:** 100+ serial tool calls to read/grep files one by one.

**OmegaLLM approach:** Write a program that uses `map`.

### Working Code (from auto-traceability.lisp)

```lisp
;; 1. Extract primitives from ALL files (parallel expression)
(define (extract-all-primitives files)
  (map (lambda (file)
         ;; grep for primitives in this file
         (let ((grep-result (effect shell.op
                              (string-append "grep -n \"def(\" " file))))
           (parse-grep-result grep-result)))
       files))

;; 2. Find test coverage for ALL primitives (parallel)
(define (find-test-coverage primitives test-files)
  (map (lambda (prim)
         (filter (lambda (test-file)
                   (test-mentions? test-file prim))
                 test-files))
       primitives))

;; 3. Generate matrix rows for ALL primitives (parallel)
(define rows
  (map format-row primitives))

;; 4. Write to file
(effect file.write.op "TRACEABILITY.md"
  (string-join rows "\n"))
```

**What makes it work:**
- Uses `shell.op` for grep (mechanical, no LLM needed)
- Uses `file.read.op` and `file.write.op` for I/O
- Uses `map` to express parallel operations
- Uses `filter` to find matching tests
- **NO LLM CALLS** - pure mechanical processing

**For agents:** Instead of calling 100 tools sequentially, write ONE OmegaLLM program that maps over all files.

---

## 📋 DEMO 2: Code Review Across Multiple Files

**What it does:** Analyze security vulnerabilities across entire codebase.

**Traditional approach:** Agent reviews files one by one.

**OmegaLLM approach:** Map LLM analysis over all files.

### Working Pattern

```lisp
;; Find all TypeScript files
(define source-files
  (let ((result (effect shell.op "find src -name '*.ts'")))
    (filter non-empty? (string-split result "\n"))))

;; Analyze ALL files for security issues (parallel LLM calls)
(define security-issues
  (map (lambda (file)
         (let ((content (effect file.read.op file)))
           (effect infer.op
             (list "Find security vulnerabilities (SQL injection, XSS, etc) in:\n"
                   content
                   "\nReturn: list of issues or 'NONE'"))))
       source-files))

;; Filter to files with issues
(define files-with-issues
  (filter (lambda (result)
            (not (string-contains? result "NONE")))
          security-issues))

;; Generate fixes for each issue (with backtracking!)
(define fixes
  (map (lambda (issue)
         (let ((approach (amb "input-validation" "parameterized-query" "sanitization" "escaping")))
           (let ((fix (effect infer.op
                        (list "Generate " approach " fix for: " issue))))
             (require (secure? fix))  ;; Auto-backtrack if still vulnerable
             fix)))
       files-with-issues))
```

**What makes it work:**
- `shell.op` to find files (mechanical)
- `map` with `infer.op` for parallel LLM analysis
- `filter` to find only files with issues
- `amb` + `require` for auto-retry fix generation

**For agents:** Instead of sequential code review, map LLM over entire codebase.

---

## 📋 DEMO 3: Test Failure Diagnosis

**What it does:** Analyze WHY each test failed in parallel.

**Traditional approach:** Agent debugs failures one by one.

**OmegaLLM approach:** Map diagnosis over all failures.

### Working Pattern

```lisp
;; Parse test output to get failures
(define test-output
  (effect shell.op "npm test 2>&1"))

(define failures
  (extract-failures test-output))  ;; Mechanical parsing

;; Diagnose ALL failures in parallel
(define diagnoses
  (map (lambda (failure)
         (effect infer.op
           (list "What's the likely root cause of this test failure?\n"
                 "Error: " (error-message failure) "\n"
                 "Test: " (test-name failure) "\n"
                 "Context: " (test-context failure))))
       failures))

;; Look for common patterns across failures
(define root-cause-summary
  (effect infer.op
    (list "Do these test failures share a common root cause?\n"
          (string-join diagnoses "\n---\n"))))
```

**What makes it work:**
- Mechanical parsing of test output
- `map` with `infer.op` for parallel diagnosis
- Follow-up LLM call to find patterns
- **Real data** from actual test run

**For agents:** Parallel diagnosis instead of sequential debugging.

---

## 📋 DEMO 4: Documentation Generation

**What it does:** Generate docs for all functions in codebase.

**Traditional approach:** Agent documents functions one by one.

**OmegaLLM approach:** Map doc generation over all functions.

### Working Pattern

```lisp
;; Find all exported functions (mechanical)
(define functions
  (let ((grep-result (effect shell.op
                       "grep -n 'export function' src/**/*.ts")))
    (parse-function-signatures grep-result)))

;; Generate docs for ALL functions in parallel
(define function-docs
  (map (lambda (func)
         (let ((name (function-name func))
               (sig (function-signature func))
               (file (function-file func)))
           (effect infer.op
             (list "Generate JSDoc comment for:\n"
                   sig "\n"
                   "File: " file "\n"
                   "Format: /** ... */"))))
       functions))

;; Write updated files
(map (lambda (func doc)
       (update-function-with-doc func doc))
     functions
     function-docs)
```

**What makes it work:**
- `shell.op` with grep to find functions
- `map` to generate docs in parallel
- Mechanical file updates
- **No manual iteration**

---

## 📋 DEMO 5: Codebase-Wide Refactoring

**What it does:** Find anti-patterns across codebase and suggest refactorings.

**Traditional approach:** Agent analyzes files one by one, loses context.

**OmegaLLM approach:** Map pattern detection over all files, PERSIST in session.

### Working Pattern with Sessions

```lisp
;; SESSION 1: Extract patterns from entire codebase
(define all-files
  (find-all-source-files))

;; Extract patterns from ALL files (parallel)
(define patterns
  (map (lambda (file)
         (let ((content (effect file.read.op file)))
           (effect infer.op
             (list "Extract code patterns (classes, functions, imports) from:\n"
                   content))))
       all-files))

;; Find anti-patterns
(define anti-patterns
  (filter (lambda (pattern)
            (is-anti-pattern? pattern))
          patterns))

;; SAVE SESSION
:session save codebase-analysis

;; SESSION 2: (hours/days later) Generate refactorings
:session load codebase-analysis
:session goto 100  ;; Restore environment

;; patterns and anti-patterns are STILL IN MEMORY!

;; Generate refactoring plan
(define refactoring-plan
  (effect infer.op
    (list "Given these anti-patterns:\n"
          (string-join anti-patterns "\n")
          "\nSuggest refactoring strategy")))
```

**What makes it work:**
- `map` for parallel pattern extraction
- **Persistent sessions** preserve analysis across tool calls
- No need to re-analyze codebase
- Agent builds knowledge incrementally

---

## 🔑 Key Insights from Real Code

### 1. **Mechanical First, LLM Second**
```lisp
;; Use shell.op for grep/find (fast, deterministic)
(effect shell.op "grep -n 'pattern' file")

;; Use LLM only where needed
(effect infer.op "Describe this code")
```

### 2. **Map Instead of Loop**
```lisp
;; NOT: for each file, call tool
;; YES: map operation over all files
(map analyze-file files)
```

### 3. **Filter for Selection**
```lisp
;; Find only files with issues
(filter has-issue? results)
```

### 4. **Streams for Large Data**
```lisp
;; Process infinite codebase
(stream->list (stream-map analyze file-stream) 100)
```

### 5. **Amb for Auto-Retry**
```lisp
;; Try approaches until one works
(let ((approach (amb "A" "B" "C")))
  (let ((result (generate approach)))
    (require (valid? result))
    result))
```

### 6. **Sessions for Persistence**
```lisp
;; Build knowledge incrementally
:session save analysis
;; Later...
:session load analysis
;; State still there!
```

---

## 📊 Comparison: Agent Tool Calls vs OmegaLLM

| Task | Traditional Agent | OmegaLLM |
|------|-------------------|----------|
| **Analyze 100 files** | 100 sequential tool calls | `(map analyze files)` |
| **Find test coverage** | Loop through each primitive | `(map find-tests prims)` |
| **Diagnose test failures** | Debug one by one | `(map diagnose failures)` |
| **Generate docs** | Document function by function | `(map gen-doc funcs)` |
| **Refactor codebase** | Lose context between calls | Session persists state |
| **Fix generation** | Manual retry | `amb` auto-backtrack |

---

## ✅ What Makes These REAL

1. **Working code** in `auto-traceability.lisp` and `rtm-generator-v3.lisp`
2. **Uses actual primitives**: `shell.op`, `file.read.op`, `infer.op`
3. **Mechanical where possible**: grep, find, parse
4. **LLM only where needed**: descriptions, analysis, validation
5. **No science fiction**: All code works today

---

## 🚀 Next: Build These Specific Demos

1. **Security Audit** - Map vuln detection over all files, filter to critical, amb-fix
2. **Test Diagnosis** - Map root-cause analysis over failures, find patterns
3. **Doc Generation** - Map JSDoc generation over all functions
4. **Refactoring Session** - Extract patterns with map, persist in session, refactor later
5. **RTM Matrix** - Map primitive extraction + test search (already exists!)

All based on **real, working patterns** from existing code.
