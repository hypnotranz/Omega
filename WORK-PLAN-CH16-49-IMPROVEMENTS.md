# Work Plan: Improve Ch16-49 with File-Based Loading

## Goal
Implement improved examples for Ch16, 27, 38, 40, 49 using external .lisp files instead of embedded strings.

## Current Status
- ✅ Ch16 .lisp file created at `MANUAL--STRUCTURE-AND-INTERPRETATION-OF-LINGUISTIC-PROGRAMS/lisp/ch16-symbolic-semantic.lisp`
- 🔄 File-loading infrastructure in progress

---

## Phase 1: File-Loading Infrastructure (IN PROGRESS)

### Task 1.1: Add file-loading helper ✅ DONE
- [x] Import fs in shared.ts
- [x] Add `loadLispFile(filename)` function
- **Status**: Complete

### Task 1.2: Update ChapterConfig type
- [ ] Add optional `codeFile?: string` field to ChapterConfig
- [ ] Make `code` field optional when `codeFile` is present
- **File**: `demo/by-chapter/shared.ts` (line ~278)

### Task 1.3: Update createChapterDemo to load files
- [ ] Check if `codeFile` is specified
- [ ] If yes, load code from file using `loadLispFile()`
- [ ] If no, use embedded `code` string (backward compat)
- **File**: `demo/by-chapter/shared.ts` (line ~289, inside createChapterDemo)

### Task 1.4: Test file-loading works
- [ ] Create simple test .lisp file
- [ ] Configure demo to load from file
- [ ] Run demo and verify it executes
- [ ] Delete test file

---

## Phase 2: Convert Ch16 to File-Based

### Task 2.1: Update Ch16 .lisp file
- [x] File exists: `lisp/ch16-symbolic-semantic.lisp`
- [ ] Ensure it has working demo code (currently has educational version)
- [ ] Update with simple working example that returns `(list #t #f)`

### Task 2.2: Update Ch16 config.ts
- [ ] Remove embedded `code:` string
- [ ] Add `codeFile: "ch16-symbolic-semantic.lisp"`
- [ ] Keep setupOracle and validate as-is
- **File**: `demo/by-chapter/config.ts` (line ~429)

### Task 2.3: Rebuild and test Ch16
- [ ] Run `npm run build:tsc`
- [ ] Run test: `npx vitest run test/demo/ch16.spec.ts`
- [ ] Verify test passes
- [ ] If fails, debug and fix

---

## Phase 3: Ch27 - Logic Programming

### Task 3.1: Create Ch27 .lisp file
- [ ] Create `lisp/ch27-logic-programming.lisp`
- [ ] Implement: LLM parses NL facts → deterministic logic
- [ ] Example from recommendations document

### Task 3.2: Update Ch27 config
- [ ] Add `codeFile: "ch27-logic-programming.lisp"`
- [ ] Add setupOracle scripts
- [ ] Add validation

### Task 3.3: Test Ch27
- [ ] Build and run test
- [ ] Verify passes

---

## Phase 4: Ch38 - Constraint Propagation

### Task 4.1: Create Ch38 .lisp file
- [ ] Create `lisp/ch38-constraint-propagation.lisp`
- [ ] Implement: Real connector network with propagation
- [ ] Example from recommendations document

### Task 4.2: Update Ch38 config
- [ ] Add `codeFile: "ch38-constraint-propagation.lisp"`
- [ ] Add setupOracle scripts
- [ ] Add validation

### Task 4.3: Test Ch38
- [ ] Build and run test
- [ ] Verify passes

---

## Phase 5: Ch40 - Data-Directed Evaluation

### Task 5.1: Create Ch40 .lisp file
- [ ] Create `lisp/ch40-data-directed.lisp`
- [ ] Implement: Safe handler synthesis returning specs
- [ ] Example from recommendations document

### Task 5.2: Update Ch40 config
- [ ] Add `codeFile: "ch40-data-directed.lisp"`
- [ ] Add setupOracle scripts
- [ ] Add validation

### Task 5.3: Test Ch40
- [ ] Build and run test
- [ ] Verify passes

---

## Phase 6: Ch49 - Semantic Caching

### Task 6.1: Create Ch49 .lisp file
- [ ] Create `lisp/ch49-semantic-caching.lisp`
- [ ] Implement: Caching with validation gate
- [ ] Example from recommendations document

### Task 6.2: Update Ch49 config
- [ ] Add `codeFile: "ch49-semantic-caching.lisp"`
- [ ] Add setupOracle scripts
- [ ] Add validation

### Task 6.3: Test Ch49
- [ ] Build and run test
- [ ] Verify passes

---

## Phase 7: Integration & Documentation

### Task 7.1: Run full test suite
- [ ] Run `npm test` on all 49 chapters
- [ ] Fix any failures
- [ ] Verify zero TypeScript errors

### Task 7.2: Update DEMO-GALLERY.md
- [ ] Update Ch16 section with new code and output
- [ ] Update Ch27 section
- [ ] Update Ch38 section
- [ ] Update Ch40 section
- [ ] Update Ch49 section

### Task 7.3: Final verification
- [ ] All .lisp files are clickable in VS Code
- [ ] All tests pass
- [ ] TypeScript compiles cleanly
- [ ] Documentation is updated

---

## Notes

### File Locations
- Lisp files: `MANUAL--STRUCTURE-AND-INTERPRETATION-OF-LINGUISTIC-PROGRAMS/lisp/`
- Config: `demo/by-chapter/config.ts`
- Tests: `test/demo/`

### Testing Commands
```bash
# Build
npm run build:tsc

# Test single chapter
npx vitest run test/demo/ch16.spec.ts

# Test all
npm test
```

### Current Blockers
- Ch16 test failing - need to debug why simple `(list #t #f)` fails validation
- May be test setup issue, not code issue

### Success Criteria
- [ ] All 5 chapters (16, 27, 38, 40, 49) load from .lisp files
- [ ] All tests pass
- [ ] Code is clickable and navigable in VS Code
- [ ] No embedded code strings for these chapters
- [ ] DEMO-GALLERY.md is updated
