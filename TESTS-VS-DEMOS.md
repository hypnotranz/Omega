# Tests vs Demos - What's the Difference?

## DEMOS (User-Facing Examples)

**Location**: `demo/` and `MANUAL--STRUCTURE-AND-INTERPRETATION-OF-LINGUISTIC-PROGRAMS/examples/`

**Purpose**: Show users HOW TO USE OmegaLLM

**Contents**:
- `demo/lisp/ch*.lisp` - Runnable code examples for each manual chapter
- `demo/by-chapter/ch*.ts` - Test harness configs that validate chapter functionality
- `demo/omega-wow/demo*.ts` - Advanced showcase demos

**Who uses them**: Users learning OmegaLLM

**Example**: 
```lisp
; demo/lisp/ch02-llm-calls.lisp
(define (analyze-sentiment text)
  (effect infer.op 
    (list "What is the sentiment of: " text)))

(analyze-sentiment "I love this!")  ; => "positive"
```

---

## TESTS (Developer-Facing Validation)

**Location**: `test/`

**Purpose**: Verify the IMPLEMENTATION works correctly

**Contents**:
- `test/eval/` - Test the evaluator (CESK machine)
- `test/compiler/` - Test the compiler
- `test/oracle/` - Test LLM integration
- `test/opr/` - Test OPR kernels
- `test/effects/` - Test effect system
- `test/conditions/` - Test condition system
- ... (31+ subsystems)

**Who uses them**: Developers building OmegaLLM

**Example**:
```typescript
// test/eval/machine.test.ts
it('evaluates lambda application', () => {
  const state = evalExpr(parse('((lambda (x) (* x 2)) 5)'));
  expect(state.value).toBe(10);
});
```

---

## Special Case: test/demo/

This folder tests that THE DEMOS WORK:

```typescript
// test/demo/by-chapter.spec.ts
it('each chapter demo runs successfully', async () => {
  for (const demo of chapterDemos) {
    const report = await runDemo(demo, "pragmatic", 7);
    expect(report.invariants.every(i => i.ok)).toBe(true);
  }
});
```

**Purpose**: Make sure all 27 chapter demos pass their validation checks

---

## Relationship to Manual

### Goes WITH the Manual:
- ✅ `demo/lisp/ch*.lisp` - The runnable examples (MOVED to manual)
- ✅ `test/demo/by-chapter.spec.ts` - Validates demos work
- ✅ `test/docs/` - Documentation tests

### Does NOT go with Manual (tests the runtime):
- ❌ `test/eval/` - Tests evaluator internals
- ❌ `test/compiler/` - Tests compiler
- ❌ `test/oracle/` - Tests LLM integration
- ❌ `test/effects/` - Tests effect system
- ❌ ... (most of test/)

---

## Summary

| What | Where | For Whom | Purpose |
|------|-------|----------|---------|
| **Demos** | `demo/` | Users | Show how to use features |
| **Tests** | `test/` | Developers | Verify implementation works |
| **Demo Tests** | `test/demo/` | Developers | Verify demos work |

**Only `demo/lisp/` and `test/demo/` relate to the manual.**

The rest of `test/` validates the runtime implementation.
