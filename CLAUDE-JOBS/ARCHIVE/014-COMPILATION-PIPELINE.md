# JOB-014: Compilation Pipeline

**Priority**: P1 - Core Infrastructure
**Estimated Effort**: 4-5 days
**Skills Required**: TypeScript, compiler design, macros, closures
**Status**: NOT STARTED
**Depends On**: [009-FRAMEIR-PACKAGE](./009-FRAMEIR-PACKAGE.md), [012-OUTCOME-FAILURE-DIAGNOSTIC](./012-OUTCOME-FAILURE-DIAGNOSTIC.md)

> **MUST READ**: [LOGISTICS.md](LOGISTICS.md) before starting - covers testing, file locations, and proof of completion requirements.

---

## Quick Start

```bash
# 1. Read the architecture spec
cat docs/ARCHITECTURE-LANGUAGES-4.md | grep -A 100 "§44"

# 2. Create compiler files
mkdir -p src/core/compiler test/compiler/golden
touch src/core/compiler/{types,pipeline,reader,expander,desugar,lower,normalize,sourcemap,index}.ts

# 3. Run tests as you implement
npx vitest run test/compiler/

# 4. Verify type safety
npx tsc --noEmit
```

---

## Dependencies

| Type | Job | Reason |
|------|-----|--------|
| **REQUIRES** | [009-FRAMEIR-PACKAGE](./009-FRAMEIR-PACKAGE.md) | Produces FlowIR output |
| **REQUIRES** | [012-OUTCOME-FAILURE-DIAGNOSTIC](./012-OUTCOME-FAILURE-DIAGNOSTIC.md) | Returns Outcome with Diagnostics |
| **REQUIRES** | [013-LINT-PASSES](./013-LINT-PASSES.md) | Pipeline runs lint passes |
| **BLOCKS** | [015-REPLAY-SYSTEM](./015-REPLAY-SYSTEM.md) | Replay needs compiled bundles |

---

## What Already Exists

### Current Compilation Path (partial)
```typescript
// src/core/pipeline/compileText.ts - existing surface compiler
export function compileText(source: string): AST {
  // Basic parsing only, no proper pipeline
}

// src/core/syntax/alpha.ts - alpha conversion
export function alphaConvert(ast: AST): AST {
  // Renames variables, no closure conversion
}
```

### Current Reader (partial)
```typescript
// Parsing exists but doesn't track spans properly
// No s-expression reader, just AST
```

### Gap Analysis
| Should Exist | Currently |
|--------------|-----------|
| S-expression reader with spans | AST parser without proper spans |
| Hygienic macroexpander | No macro support |
| Desugarer (and/or → if, mdo → bind) | Manual expansion in code |
| Lowerer with closure conversion | No closure conversion |
| Normalizer (flatten sequences, etc.) | No normalization passes |
| Source maps | No IR-to-source mapping |
| Diagnostic aggregation | Errors scattered across phases |

---

## Architecture Reference

From ARCHITECTURE-LANGUAGES-4.md §44:

> Your language layer becomes "optimal" when it compiles to IR in a principled way.
>
> **Phases and their responsibilities:**
>
> 1. **Reader** - parses s-expressions, tracks source spans (line/col), produces `Form` AST with metadata
> 2. **Macroexpander** - hygienic macros (syntax objects), expands `defflow`, `defagent`, `let-amb`, etc., attaches provenance info for diagnostics
> 3. **Desugarer** - converts syntactic sugar into core forms (e.g., `and`/`or` into nested branches, `mdo` into nested `bind`)
> 4. **Lowerer** - converts core forms to FrameIR nodes, resolves stdlib forms to IR builders, closure conversion if you use explicit fn defs
> 5. **Normalizer/Optimizer** - structural normalization (flatten prompt+, sequence), safe rewrites on Pure subgraphs, inserts implicit budgets/timeouts if policy demands
>
> **Key insight**: Stdlib should be mostly "IR macros", not runtime hacks.

## Type Definitions

### Phase Results

```typescript
// src/core/compiler/types.ts

import { FlowIR, ValueIR, PromptIR, IRBundle, FnDefIR } from '../frameir';
import { Span } from '../frameir/meta';
import { Diagnostic } from '../diagnostics';

// ============================================
// Reader Phase
// ============================================

export type FormTag =
  | 'Atom'
  | 'Symbol'
  | 'Keyword'
  | 'Number'
  | 'String'
  | 'List'
  | 'Vector'
  | 'Map';

export interface FormMeta {
  span: Span;
  macroExpanded?: boolean;
  originalForm?: Form;
}

export interface Form {
  tag: FormTag;
  meta: FormMeta;
  value: unknown;      // tag-specific payload
  children?: Form[];   // for composite forms
}

export interface ReadResult {
  ok: boolean;
  forms: Form[];
  diagnostics: Diagnostic[];
}

// ============================================
// Macroexpander Phase
// ============================================

export interface MacroEnv {
  macros: Map<string, MacroDefinition>;
  gensymCounter: number;
  hygieneContext: HygieneContext;
}

export interface MacroDefinition {
  name: string;
  transformer: (args: Form[], env: MacroEnv) => Form;
  patterns?: PatternRule[];  // for pattern-based macros
}

export interface PatternRule {
  pattern: Form;
  template: Form;
  guards?: (bindings: Map<string, Form>) => boolean;
}

export interface HygieneContext {
  scope: number;
  marks: Set<number>;
  renames: Map<string, string>;
}

export interface ExpandResult {
  ok: boolean;
  form: Form;
  diagnostics: Diagnostic[];
  macrosUsed: string[];
}

// ============================================
// Desugarer Phase
// ============================================

export type CoreFormTag =
  | 'quote'
  | 'if'
  | 'lambda'
  | 'let'
  | 'letrec'
  | 'begin'
  | 'set!'
  | 'define'
  // Flow core forms
  | 'pure'
  | 'bind'
  | 'fail'
  | 'catch'
  | 'with-budget'
  | 'with-timeout'
  | 'infer'
  | 'tool-call'
  | 'validate'
  | 'commit'
  | 'emit'
  | 'observe'
  | 'all'
  | 'race'
  | 'any'
  | 'sequence'
  | 'branch'
  | 'loop';

export interface CoreForm {
  tag: CoreFormTag;
  meta: FormMeta;
  args: (CoreForm | ValueLiteral)[];
}

export interface ValueLiteral {
  tag: 'literal';
  meta: FormMeta;
  value: unknown;
}

export interface DesugarResult {
  ok: boolean;
  coreForm: CoreForm;
  diagnostics: Diagnostic[];
}

// ============================================
// Lowerer Phase
// ============================================

export interface LowerEnv {
  fnDefs: Map<string, FnDefIR>;
  schemas: Map<string, unknown>;
  toolContracts: Map<string, unknown>;
  globals: Map<string, ValueIR>;
  currentFn?: string;
  capturedVars: Set<string>;
}

export interface LowerResult {
  ok: boolean;
  ir: FlowIR | ValueIR | PromptIR;
  fnDefs: FnDefIR[];
  diagnostics: Diagnostic[];
}

// ============================================
// Normalizer Phase
// ============================================

export interface NormalizeConfig {
  flattenSequences: boolean;
  flattenPrompts: boolean;
  insertImplicitBudgets: boolean;
  insertImplicitTimeouts: boolean;
  defaultBudget?: {
    llmCalls: number;
    tokens: number;
    timeMs: number;
  };
  defaultTimeoutMs?: number;
}

export interface NormalizeResult {
  ok: boolean;
  ir: FlowIR;
  rewrites: RewriteRecord[];
  diagnostics: Diagnostic[];
}

export interface RewriteRecord {
  kind: string;
  location: Span;
  before: string;  // hash of before
  after: string;   // hash of after
}

// ============================================
// Full Pipeline
// ============================================

export interface CompileConfig {
  normalize: NormalizeConfig;
  lint: boolean;
  sourceMap: boolean;
  debug: boolean;
}

export interface CompileResult {
  ok: boolean;
  bundle?: IRBundle;
  sourceMap?: SourceMap;
  diagnostics: Diagnostic[];
  phases: {
    read?: ReadResult;
    expand?: ExpandResult;
    desugar?: DesugarResult;
    lower?: LowerResult;
    normalize?: NormalizeResult;
  };
}

export interface SourceMap {
  version: 3;
  file: string;
  sourceRoot: string;
  sources: string[];
  names: string[];
  mappings: string;
  // IR node hash -> source span
  irToSource: Map<string, Span>;
}
```

### Pipeline Interface

```typescript
// src/core/compiler/pipeline.ts

import { Form, CoreForm, CompileConfig, CompileResult, ReadResult,
         ExpandResult, DesugarResult, LowerResult, NormalizeResult,
         MacroEnv, LowerEnv, NormalizeConfig } from './types';
import { FlowIR, IRBundle } from '../frameir';
import { Diagnostic } from '../diagnostics';

/**
 * Individual phase interfaces
 */
export interface Reader {
  read(source: string, filename?: string): ReadResult;
}

export interface Macroexpander {
  expand(form: Form, env: MacroEnv): ExpandResult;
  expandModule(forms: Form[], env: MacroEnv): ExpandResult[];
}

export interface Desugarer {
  desugar(form: Form): DesugarResult;
  desugarCoreForm(form: Form): CoreForm;
}

export interface Lowerer {
  lower(coreForm: CoreForm, env: LowerEnv): LowerResult;
  lowerModule(coreForms: CoreForm[], env: LowerEnv): LowerResult;
}

export interface Normalizer {
  normalize(ir: FlowIR, config: NormalizeConfig): NormalizeResult;
}

/**
 * Full compilation pipeline
 */
export interface CompilePipeline {
  compile(source: string, config: CompileConfig): CompileResult;
  compileModule(sources: { filename: string; source: string }[], config: CompileConfig): CompileResult;

  // Individual phases for debugging/testing
  read(source: string, filename?: string): ReadResult;
  expand(form: Form): ExpandResult;
  desugar(form: Form): DesugarResult;
  lower(coreForm: CoreForm): LowerResult;
  normalize(ir: FlowIR): NormalizeResult;
}

/**
 * Create default pipeline with standard configuration
 */
export function createPipeline(
  reader: Reader,
  expander: Macroexpander,
  desugarer: Desugarer,
  lowerer: Lowerer,
  normalizer: Normalizer
): CompilePipeline {
  return new DefaultPipeline(reader, expander, desugarer, lowerer, normalizer);
}

class DefaultPipeline implements CompilePipeline {
  constructor(
    private reader: Reader,
    private expander: Macroexpander,
    private desugarer: Desugarer,
    private lowerer: Lowerer,
    private normalizer: Normalizer
  ) {}

  compile(source: string, config: CompileConfig): CompileResult {
    const diagnostics: Diagnostic[] = [];
    const phases: CompileResult['phases'] = {};

    // Phase 1: Read
    const readResult = this.reader.read(source);
    phases.read = readResult;
    diagnostics.push(...readResult.diagnostics);
    if (!readResult.ok) {
      return { ok: false, diagnostics, phases };
    }

    // Phase 2: Macroexpand
    const macroEnv = this.createMacroEnv();
    const expandResults = this.expander.expandModule(readResult.forms, macroEnv);
    const expandedForms = expandResults.map(r => r.form);
    expandResults.forEach(r => diagnostics.push(...r.diagnostics));
    if (expandResults.some(r => !r.ok)) {
      return { ok: false, diagnostics, phases };
    }

    // Phase 3: Desugar
    const desugarResults = expandedForms.map(f => this.desugarer.desugar(f));
    desugarResults.forEach(r => diagnostics.push(...r.diagnostics));
    if (desugarResults.some(r => !r.ok)) {
      return { ok: false, diagnostics, phases };
    }
    const coreForms = desugarResults.map(r => r.coreForm);

    // Phase 4: Lower
    const lowerEnv = this.createLowerEnv();
    const lowerResult = this.lowerer.lowerModule(coreForms, lowerEnv);
    phases.lower = lowerResult;
    diagnostics.push(...lowerResult.diagnostics);
    if (!lowerResult.ok) {
      return { ok: false, diagnostics, phases };
    }

    // Phase 5: Normalize (if IR is Flow)
    if (lowerResult.ir.tag.startsWith('F')) {
      const normalizeResult = this.normalizer.normalize(
        lowerResult.ir as FlowIR,
        config.normalize
      );
      phases.normalize = normalizeResult;
      diagnostics.push(...normalizeResult.diagnostics);
      if (!normalizeResult.ok) {
        return { ok: false, diagnostics, phases };
      }
    }

    // Build bundle
    const bundle = this.buildBundle(lowerResult, phases.normalize);

    return {
      ok: true,
      bundle,
      diagnostics,
      phases
    };
  }

  // ... implement remaining methods
}
```

### Desugaring Rules

```typescript
// src/core/compiler/desugar.ts

import { Form, CoreForm, DesugarResult, CoreFormTag } from './types';
import { Diagnostic } from '../diagnostics';

/**
 * Desugaring rules - convert surface syntax to core forms
 *
 * Reference: ARCHITECTURE-LANGUAGES-4.md §44.3
 * "Desugarer converts syntactic sugar into core forms, e.g.:
 *  - and/or into nested branches
 *  - mdo into nested bind
 *  - let* into nested let"
 */

export const desugarRules: Record<string, (form: Form) => CoreForm> = {

  // (and a b c) => (if a (if b c #f) #f)
  'and': (form: Form): CoreForm => {
    const args = getArgs(form);
    if (args.length === 0) return literal(true, form);
    if (args.length === 1) return desugar(args[0]);

    return args.reduceRight(
      (acc, arg) => branch(desugar(arg), acc, literal(false, form), form),
      desugar(args[args.length - 1])
    );
  },

  // (or a b c) => (if a a (if b b c))
  'or': (form: Form): CoreForm => {
    const args = getArgs(form);
    if (args.length === 0) return literal(false, form);
    if (args.length === 1) return desugar(args[0]);

    return args.reduceRight(
      (acc, arg) => {
        const temp = gensym('or-temp');
        return letForm(
          [[temp, desugar(arg)]],
          branch(ref(temp), ref(temp), acc, form),
          form
        );
      },
      desugar(args[args.length - 1])
    );
  },

  // (mdo [x <- m1] [y <- m2] body) => (bind m1 (lambda (x) (bind m2 (lambda (y) body))))
  'mdo': (form: Form): CoreForm => {
    const bindings = getBindings(form);
    const body = getBody(form);

    return bindings.reduceRight(
      (acc, [name, expr]) => ({
        tag: 'bind' as CoreFormTag,
        meta: form.meta,
        args: [
          desugar(expr),
          lambda([name], acc, form)
        ]
      }),
      desugar(body)
    );
  },

  // (let* ([x 1] [y x]) body) => (let ([x 1]) (let ([y x]) body))
  'let*': (form: Form): CoreForm => {
    const bindings = getBindings(form);
    const body = getBody(form);

    return bindings.reduceRight(
      (acc, binding) => letForm([binding], acc, form),
      desugar(body)
    );
  },

  // (cond [test1 expr1] [test2 expr2] [else expr3])
  // => (if test1 expr1 (if test2 expr2 expr3))
  'cond': (form: Form): CoreForm => {
    const clauses = getClauses(form);

    return clauses.reduceRight(
      (acc, [test, expr]) => {
        if (isElse(test)) return desugar(expr);
        return branch(desugar(test), desugar(expr), acc, form);
      },
      { tag: 'fail' as CoreFormTag, meta: form.meta, args: [literal('no-matching-clause', form)] }
    );
  },

  // (when test body ...) => (if test (begin body ...) #<void>)
  'when': (form: Form): CoreForm => {
    const [test, ...body] = getArgs(form);
    return branch(
      desugar(test),
      begin(body.map(desugar), form),
      literal(null, form),
      form
    );
  },

  // (unless test body ...) => (if test #<void> (begin body ...))
  'unless': (form: Form): CoreForm => {
    const [test, ...body] = getArgs(form);
    return branch(
      desugar(test),
      literal(null, form),
      begin(body.map(desugar), form),
      form
    );
  },

  // (-> x f1 f2 f3) => (f3 (f2 (f1 x)))
  '->': (form: Form): CoreForm => {
    const [init, ...fns] = getArgs(form);
    return fns.reduce(
      (acc, fn) => apply(desugar(fn), [acc], form),
      desugar(init)
    );
  },

  // (->> x f1 f2 f3) => (f3 (f2 (f1 x))) but with x as last arg
  '->>': (form: Form): CoreForm => {
    const [init, ...fns] = getArgs(form);
    return fns.reduce(
      (acc, fn) => {
        if (isList(fn)) {
          const [head, ...tail] = getChildren(fn);
          return apply(desugar(head), [...tail.map(desugar), acc], form);
        }
        return apply(desugar(fn), [acc], form);
      },
      desugar(init)
    );
  },

  // (for-each fn list) => (map-flow (lambda (x) (begin (fn x) (pure nil))) list)
  // Then discard results
  'for-each': (form: Form): CoreForm => {
    const [fn, list] = getArgs(form);
    const x = gensym('x');
    return {
      tag: 'bind' as CoreFormTag,
      meta: form.meta,
      args: [
        apply(ref('map-flow'), [
          lambda([x], begin([apply(desugar(fn), [ref(x)], form), pure(literal(null, form), form)], form), form),
          desugar(list)
        ], form),
        lambda([gensym('_')], pure(literal(null, form), form), form)
      ]
    };
  }
};

// Helper constructors
function branch(test: CoreForm, then_: CoreForm, else_: CoreForm, source: Form): CoreForm {
  return { tag: 'branch', meta: source.meta, args: [test, then_, else_] };
}

function lambda(params: string[], body: CoreForm, source: Form): CoreForm {
  return { tag: 'lambda', meta: source.meta, args: [{ tag: 'literal', meta: source.meta, value: params }, body] };
}

function letForm(bindings: [string, CoreForm][], body: CoreForm, source: Form): CoreForm {
  return { tag: 'let', meta: source.meta, args: [{ tag: 'literal', meta: source.meta, value: bindings }, body] };
}

function begin(forms: CoreForm[], source: Form): CoreForm {
  return { tag: 'begin', meta: source.meta, args: forms };
}

function pure(value: CoreForm, source: Form): CoreForm {
  return { tag: 'pure', meta: source.meta, args: [value] };
}

function apply(fn: CoreForm, args: CoreForm[], source: Form): CoreForm {
  return { tag: 'apply' as CoreFormTag, meta: source.meta, args: [fn, ...args] };
}

function ref(name: string): CoreForm {
  return { tag: 'quote' as CoreFormTag, meta: { span: { file: '', startLine: 0, startCol: 0, endLine: 0, endCol: 0 } }, args: [{ tag: 'literal', meta: { span: { file: '', startLine: 0, startCol: 0, endLine: 0, endCol: 0 } }, value: name }] };
}

function literal(value: unknown, source: Form): CoreForm {
  return { tag: 'quote' as CoreFormTag, meta: source.meta, args: [{ tag: 'literal', meta: source.meta, value }] };
}

let gensymCounter = 0;
function gensym(prefix: string): string {
  return `${prefix}$${gensymCounter++}`;
}
```

### Lowering to IR

```typescript
// src/core/compiler/lower.ts

import { CoreForm, LowerEnv, LowerResult } from './types';
import { FlowIR, ValueIR, PromptIR, FnDefIR, VRef, VRecord, VList } from '../frameir';
import { Diagnostic, DiagnosticCode } from '../diagnostics';

/**
 * Lower core forms to FrameIR
 *
 * Reference: ARCHITECTURE-LANGUAGES-4.md §44.4
 * "Stdlib should be mostly 'IR macros', not runtime hacks."
 */

export function lowerCoreForm(form: CoreForm, env: LowerEnv): LowerResult {
  const diagnostics: Diagnostic[] = [];

  switch (form.tag) {
    case 'pure':
      return lowerPure(form, env, diagnostics);

    case 'bind':
      return lowerBind(form, env, diagnostics);

    case 'fail':
      return lowerFail(form, env, diagnostics);

    case 'catch':
      return lowerCatch(form, env, diagnostics);

    case 'with-budget':
      return lowerWithBudget(form, env, diagnostics);

    case 'with-timeout':
      return lowerWithTimeout(form, env, diagnostics);

    case 'infer':
      return lowerInfer(form, env, diagnostics);

    case 'tool-call':
      return lowerToolCall(form, env, diagnostics);

    case 'validate':
      return lowerValidate(form, env, diagnostics);

    case 'commit':
      return lowerCommit(form, env, diagnostics);

    case 'emit':
      return lowerEmit(form, env, diagnostics);

    case 'observe':
      return lowerObserve(form, env, diagnostics);

    case 'all':
      return lowerAll(form, env, diagnostics);

    case 'race':
      return lowerRace(form, env, diagnostics);

    case 'any':
      return lowerAny(form, env, diagnostics);

    case 'sequence':
      return lowerSequence(form, env, diagnostics);

    case 'branch':
      return lowerBranch(form, env, diagnostics);

    case 'loop':
      return lowerLoop(form, env, diagnostics);

    case 'lambda':
      return lowerLambda(form, env, diagnostics);

    case 'let':
    case 'letrec':
      return lowerLet(form, env, diagnostics);

    case 'if':
      // Pure if becomes EIf
      return lowerIf(form, env, diagnostics);

    case 'quote':
      return lowerQuote(form, env, diagnostics);

    default:
      diagnostics.push({
        code: DiagnosticCode.E0001,
        severity: 'error',
        message: `Unknown core form: ${form.tag}`,
        span: form.meta.span
      });
      return { ok: false, ir: null as any, fnDefs: [], diagnostics };
  }
}

function lowerPure(form: CoreForm, env: LowerEnv, diagnostics: Diagnostic[]): LowerResult {
  const [valueForm] = form.args;
  const valueResult = lowerCoreForm(valueForm as CoreForm, env);
  diagnostics.push(...valueResult.diagnostics);

  if (!valueResult.ok) {
    return { ok: false, ir: null as any, fnDefs: valueResult.fnDefs, diagnostics };
  }

  const ir: FlowIR = {
    v: 'frameir@1',
    tag: 'FPure',
    value: valueResult.ir as ValueIR,
    meta: { span: form.meta.span }
  };

  return { ok: true, ir, fnDefs: valueResult.fnDefs, diagnostics };
}

function lowerBind(form: CoreForm, env: LowerEnv, diagnostics: Diagnostic[]): LowerResult {
  const [flowForm, kForm] = form.args;

  const flowResult = lowerCoreForm(flowForm as CoreForm, env);
  diagnostics.push(...flowResult.diagnostics);

  const kResult = lowerCoreForm(kForm as CoreForm, env);
  diagnostics.push(...kResult.diagnostics);

  if (!flowResult.ok || !kResult.ok) {
    return { ok: false, ir: null as any, fnDefs: [...flowResult.fnDefs, ...kResult.fnDefs], diagnostics };
  }

  // k should be a lambda that we closure-convert
  const fnId = generateFnId();
  const fnDef = closureConvert(kResult.ir, fnId, env);

  const ir: FlowIR = {
    v: 'frameir@1',
    tag: 'FBind',
    flow: flowResult.ir as FlowIR,
    k: { v: 'frameir@1', tag: 'VRef', ref: { kind: 'Fn', id: fnId } },
    meta: { span: form.meta.span }
  };

  return { ok: true, ir, fnDefs: [...flowResult.fnDefs, ...kResult.fnDefs, fnDef], diagnostics };
}

function lowerInfer(form: CoreForm, env: LowerEnv, diagnostics: Diagnostic[]): LowerResult {
  const [promptForm, optionsForm] = form.args;

  const promptResult = lowerPrompt(promptForm as CoreForm, env, diagnostics);

  let options: ValueIR | undefined;
  if (optionsForm) {
    const optResult = lowerCoreForm(optionsForm as CoreForm, env);
    diagnostics.push(...optResult.diagnostics);
    options = optResult.ir as ValueIR;
  }

  const ir: FlowIR = {
    v: 'frameir@1',
    tag: 'FInfer',
    prompt: promptResult.ir as PromptIR,
    options,
    meta: { span: form.meta.span }
  };

  return { ok: true, ir, fnDefs: promptResult.fnDefs, diagnostics };
}

function lowerToolCall(form: CoreForm, env: LowerEnv, diagnostics: Diagnostic[]): LowerResult {
  const [toolForm, argsForm, contractForm] = form.args;

  const toolResult = lowerCoreForm(toolForm as CoreForm, env);
  const argsResult = lowerCoreForm(argsForm as CoreForm, env);
  diagnostics.push(...toolResult.diagnostics, ...argsResult.diagnostics);

  let contract: VRef | undefined;
  if (contractForm) {
    const contractName = extractSymbol(contractForm);
    const contractDef = env.toolContracts.get(contractName);
    if (!contractDef) {
      diagnostics.push({
        code: DiagnosticCode.E0303,
        severity: 'error',
        message: `Unknown tool contract: ${contractName}`,
        span: form.meta.span
      });
    } else {
      contract = {
        v: 'frameir@1',
        tag: 'VRef',
        ref: { kind: 'ToolContract', id: `contract:${contractName}` }
      };
    }
  }

  const ir: FlowIR = {
    v: 'frameir@1',
    tag: 'FToolCall',
    tool: toolResult.ir as ValueIR,
    args: argsResult.ir as ValueIR,
    contract,
    meta: { span: form.meta.span }
  };

  return { ok: true, ir, fnDefs: [...toolResult.fnDefs, ...argsResult.fnDefs], diagnostics };
}

function lowerWithBudget(form: CoreForm, env: LowerEnv, diagnostics: Diagnostic[]): LowerResult {
  const [budgetForm, flowForm] = form.args;

  const budgetResult = lowerCoreForm(budgetForm as CoreForm, env);
  const flowResult = lowerCoreForm(flowForm as CoreForm, env);
  diagnostics.push(...budgetResult.diagnostics, ...flowResult.diagnostics);

  const ir: FlowIR = {
    v: 'frameir@1',
    tag: 'FWithBudget',
    budget: budgetResult.ir as ValueIR,
    flow: flowResult.ir as FlowIR,
    meta: { span: form.meta.span }
  };

  return { ok: true, ir, fnDefs: [...budgetResult.fnDefs, ...flowResult.fnDefs], diagnostics };
}

// ... implement remaining lowering functions

/**
 * Closure conversion: turn lambda into FnDef with explicit captures
 *
 * Reference: ARCHITECTURE-LANGUAGES-4.md §42.3
 * "Option C: Closure conversion at lower time - turn lambdas into named
 * functions with explicit environments"
 */
function closureConvert(lambdaIR: any, fnId: string, env: LowerEnv): FnDefIR {
  // Extract free variables
  const freeVars = findFreeVariables(lambdaIR.body, new Set(lambdaIR.params));

  // Build captures record
  const captures: VRecord = {
    v: 'frameir@1',
    tag: 'VRecord',
    entries: Array.from(freeVars).map(name => ({
      k: { v: 'frameir@1', tag: 'VSymbol', name },
      v: env.globals.get(name) || { v: 'frameir@1', tag: 'VRef', ref: { kind: 'Global', id: name } }
    }))
  };

  return {
    v: 'frameir@1',
    tag: 'FnDef',
    fnId,
    params: lambdaIR.params,
    body: lambdaIR.body,
    captures
  };
}

function findFreeVariables(ir: any, bound: Set<string>): Set<string> {
  const free = new Set<string>();

  function walk(node: any, localBound: Set<string>) {
    if (!node || typeof node !== 'object') return;

    if (node.tag === 'VSymbol' && !localBound.has(node.name)) {
      free.add(node.name);
    }

    // Handle binding forms
    if (node.tag === 'FnDef') {
      const newBound = new Set([...localBound, ...node.params]);
      walk(node.body, newBound);
      return;
    }

    // Recurse into children
    for (const key of Object.keys(node)) {
      if (key === 'meta') continue;
      walk(node[key], localBound);
    }
  }

  walk(ir, bound);
  return free;
}

let fnIdCounter = 0;
function generateFnId(): string {
  return `fn:gen$${fnIdCounter++}`;
}

function extractSymbol(form: CoreForm): string {
  if (form.tag === 'quote' && form.args[0]) {
    const lit = form.args[0] as any;
    return lit.value as string;
  }
  return '';
}
```

## Output Files

```
src/core/compiler/
  types.ts          # Phase result types
  pipeline.ts       # Pipeline interface and default impl
  reader.ts         # S-expression reader (if not already exists)
  expander.ts       # Macroexpander
  desugar.ts        # Desugaring rules
  lower.ts          # Core form to IR lowering
  normalize.ts      # IR normalization passes
  sourcemap.ts      # Source map generation
  index.ts          # Public exports

test/compiler/
  reader.spec.ts
  expander.spec.ts
  desugar.spec.ts
  lower.spec.ts
  normalize.spec.ts
  pipeline.spec.ts
  golden/           # Golden test fixtures
```

## Tasks

### Phase 1: Types and Infrastructure
- [ ] Create `src/core/compiler/types.ts` with all phase result types
- [ ] Create `src/core/compiler/pipeline.ts` with pipeline interface
- [ ] Create `src/core/compiler/sourcemap.ts` for source mapping

### Phase 2: Individual Phases
- [ ] Implement/verify `reader.ts` handles spans correctly
- [ ] Implement `expander.ts` with hygiene support
- [ ] Implement `desugar.ts` with all desugaring rules
- [ ] Implement `lower.ts` with closure conversion
- [ ] Implement `normalize.ts` with structural normalization

### Phase 3: Integration
- [ ] Wire phases together in `DefaultPipeline`
- [ ] Add source map propagation through all phases
- [ ] Add diagnostic aggregation with phase attribution

### Phase 4: Testing
- [ ] Create golden tests for each desugaring rule
- [ ] Create golden tests for lowering each core form
- [ ] Create integration tests for full pipeline
- [ ] Property tests: `lower(desugar(parse(x)))` is well-formed IR

## Verification Steps

```bash
# Run compiler tests
npm run test -- test/compiler/

# Run golden tests
npm run test -- test/compiler/golden/

# Test round-trip
npm run test -- --grep "pipeline round-trip"

# Verify source maps
npm run test -- --grep "source map"
```

## Checklist

- [ ] All phase types defined in `types.ts`
- [ ] Reader produces Forms with accurate spans
- [ ] Macroexpander supports hygienic macros
- [ ] All desugaring rules implemented
- [ ] All core forms lower to correct IR
- [ ] Closure conversion produces explicit captures
- [ ] Source maps track IR nodes to source spans
- [ ] Golden tests pass for all transformations
- [ ] Pipeline produces valid IRBundle
- [ ] Diagnostics include source locations

## References

- ARCHITECTURE-LANGUAGES-4.md §44 (Compilation Pipeline)
- ARCHITECTURE-LANGUAGES-4.md §42.3 (Closure Conversion)
- ARCHITECTURE-LANGUAGES-5.md §56-60 (IR Type Definitions)
- Job 009 (FrameIR Package)

---

## Test Plan

### Happy Path Tests

| ID | Test Case | Input | Expected Output |
|----|-----------|-------|-----------------|
| HP-1 | Reader parses atoms | `42` | Form { tag: 'Number', value: 42 } |
| HP-2 | Reader parses list | `(+ 1 2)` | Form { tag: 'List', children: [...] } |
| HP-3 | Reader tracks spans | `(foo bar)` | Accurate line/col spans |
| HP-4 | Desugar and → if | `(and a b c)` | Nested if forms |
| HP-5 | Desugar mdo → bind | `(mdo [x <- m] x)` | FBind with lambda |
| HP-6 | Desugar let* → let | `(let* ([x 1] [y x]) y)` | Nested let forms |
| HP-7 | Lower pure | `(pure 42)` | FPure { value: VNumber } |
| HP-8 | Lower bind | `(bind m k)` | FBind with FnDef |
| HP-9 | Lower infer | `(infer prompt)` | FInfer with PromptIR |
| HP-10 | Closure conversion | Lambda with free vars | FnDef with captures |

### Edge Case Tests

| ID | Test Case | Input | Expected Output |
|----|-----------|-------|-----------------|
| EC-1 | Empty source | `` | Empty forms list |
| EC-2 | Deeply nested | 50 levels deep | Handles without stack overflow |
| EC-3 | Complex macro | Custom defflow macro | Correct expansion |
| EC-4 | Shadowed variable | Lambda shadows outer var | Correct scoping |
| EC-5 | No free variables | Closed lambda | Empty captures |
| EC-6 | Multiple free vars | 10 free variables | All captured |
| EC-7 | Recursive function | letrec with self-ref | Correct FnDef |
| EC-8 | Mutual recursion | letrec with mutual refs | Multiple FnDefs |

### Error Cases

| ID | Test Case | Input | Expected Error |
|----|-----------|-------|----------------|
| ERR-1 | Unbalanced parens | `(foo bar` | E0002: Unbalanced parentheses |
| ERR-2 | Invalid string | `"unterminated` | E0003: Invalid string literal |
| ERR-3 | Unknown core form | `(unknown-form x)` | E0001: Unknown core form |
| ERR-4 | Unknown tool contract | `(tool-call x {} :unknown)` | E0303: Unknown tool contract |
| ERR-5 | Type mismatch | `(infer 42)` not prompt | E0100: Type mismatch |
| ERR-6 | Undefined variable | Reference to unbound var | E0101: Undefined variable |
| ERR-7 | Wrong arity | `(bind x)` missing k | E0102: Wrong number of arguments |

### Integration Tests

| ID | Test Case | Description |
|----|-----------|-------------|
| INT-1 | Full pipeline | Source → IRBundle, verify structure |
| INT-2 | Source maps work | IR node → source span lookup |
| INT-3 | Diagnostics aggregate | Errors from all phases collected |
| INT-4 | Golden tests | Compare output to known-good fixtures |
| INT-5 | Round-trip property | parse(lower(desugar(expand(read(s))))) is valid IR |

---

## Notes

### Why Principled Compilation

From ARCHITECTURE-LANGUAGES-4.md §44:
> "Stdlib should be mostly 'IR macros', not runtime hacks."

A principled pipeline enables:
1. **Static analysis**: Lint passes can check IR before execution
2. **Optimization**: Pure subgraphs can be rewritten
3. **Source maps**: Errors point to source locations
4. **Extensibility**: New macros/desugaring without runtime changes

### Phase Responsibilities

| Phase | Input | Output | Responsibility |
|-------|-------|--------|----------------|
| Reader | String | Form[] | Parse, track spans |
| Macroexpander | Form | Form | Expand macros, hygiene |
| Desugarer | Form | CoreForm | Eliminate sugar |
| Lowerer | CoreForm | FlowIR | Produce IR, closure convert |
| Normalizer | FlowIR | FlowIR | Flatten, insert implicit guards |

### Closure Conversion Strategy

Option C from §42.3: Convert lambdas to named FnDefs with explicit captures:
```typescript
// Before: (lambda (x) (+ x y))  where y is free
// After: FnDef { params: ["x"], body: ..., captures: { y: VRef(...) } }
```

### Hygienic Macros

Macro expansion preserves hygiene via:
1. **Scope numbers**: Each macro invocation gets a fresh scope
2. **Marks**: Identifiers track which scopes they belong to
3. **Renames**: Collisions resolved by renaming

---

## Proof of Completion

When this job is complete:

1. **File Structure Verified**
   ```bash
   ls src/core/compiler/*.ts  # 8+ files
   ls test/compiler/*.ts      # 6+ test files
   ls test/compiler/golden/   # Golden fixtures
   ```

2. **All Phases Implemented**
   ```bash
   grep "export function" src/core/compiler/*.ts | wc -l  # Should be ≥ 15
   ```

3. **Golden Tests Pass**
   ```bash
   npx vitest run test/compiler/golden/
   ```

4. **Full Pipeline Works**
   ```typescript
   const result = pipeline.compile(`
     (defflow my-flow
       (with-budget {:llm-calls 10}
         (infer "What is 2+2?")))
   `, { lint: true });
   assert(result.ok);
   assert(result.bundle?.entry.tag === "FWithBudget");
   ```

5. **Source Maps Work**
   ```typescript
   const irNode = result.bundle.entry;
   const span = result.sourceMap.irToSource.get(hash(irNode));
   assert(span.startLine === 3);  // infer is on line 3
   ```

---

## Footer

| Field | Value |
|-------|-------|
| Created | 2025-01-20 |
| Last Updated | 2025-01-20 |
| Author | Claude |
| Related Docs | [ARCHITECTURE-LANGUAGES-4.md §44](../docs/ARCHITECTURE-LANGUAGES-4.md), [ARCHITECTURE-LANGUAGES-4.md §42.3](../docs/ARCHITECTURE-LANGUAGES-4.md) |
| Predecessors | [009-FRAMEIR-PACKAGE](./009-FRAMEIR-PACKAGE.md), [012-OUTCOME-FAILURE-DIAGNOSTIC](./012-OUTCOME-FAILURE-DIAGNOSTIC.md), [013-LINT-PASSES](./013-LINT-PASSES.md) |
| Successor | [015-REPLAY-SYSTEM](./015-REPLAY-SYSTEM.md) |
