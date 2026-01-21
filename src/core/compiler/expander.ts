import type { Diagnostic } from "../../outcome/diagnostic";
import { errorDiag } from "../../outcome/diagnostic";
import type { ExpandResult, Form, MacroDefinition, MacroEnv, PatternRule } from "./types";

const ERR_NO_PATTERN = "E1000";

export function createMacroEnv(): MacroEnv {
  return {
    macros: new Map(),
    gensymCounter: 0,
    hygieneContext: { scope: 0, marks: new Set(), renames: new Map() },
  };
}

export function defineMacro(env: MacroEnv, name: string, def: Omit<MacroDefinition, "name">): void {
  env.macros.set(name, { name, ...def });
}

export function gensym(env: MacroEnv, prefix = "g"): string {
  const id = `${prefix}$${env.gensymCounter++}`;
  return id;
}

export function expandModule(forms: Form[], env: MacroEnv): ExpandResult[] {
  return forms.map(f => expand(f, env));
}

export function expand(form: Form, env: MacroEnv): ExpandResult {
  const diagnostics: Diagnostic[] = [];

  if (form.tag !== "List" || !form.children || form.children.length === 0) {
    return { ok: true, form, diagnostics, macrosUsed: [] };
  }

  const head = form.children[0];
  const macroName = head.tag === "Symbol" ? String(head.value) : null;

  if (macroName && env.macros.has(macroName)) {
    const macro = env.macros.get(macroName)!;
    const args = form.children.slice(1);
    const expanded = applyMacro(macro, macroName, args, env, diagnostics);
    if (!expanded) {
      return { ok: false, form, diagnostics, macrosUsed: [macroName] };
    }
    const marked = markExpanded(expanded, form);
    return { ok: diagnostics.length === 0, form: marked, diagnostics, macrosUsed: [macroName] };
  }

  // No macro: expand children recursively
  const newChildren = form.children.map(child => expand(child, env).form);
  return {
    ok: true,
    macrosUsed: [],
    diagnostics,
    form: { ...form, children: newChildren },
  };
}

function applyMacro(
  macro: MacroDefinition,
  macroName: string,
  args: Form[],
  env: MacroEnv,
  diagnostics: Diagnostic[]
): Form | null {
  if (macro.transformer) {
    return macro.transformer(args, env);
  }

  if (!macro.patterns || macro.patterns.length === 0) {
    diagnostics.push(errorDiag(ERR_NO_PATTERN, `Macro ${macroName} has no patterns`));
    return null;
  }

  for (const rule of macro.patterns) {
    const bindings = matchPattern(rule.pattern, args, macroName);
    if (!bindings) continue;
    if (rule.guards && !rule.guards(bindings)) continue;
    return instantiateTemplate(rule.template, bindings);
  }

  diagnostics.push(errorDiag(ERR_NO_PATTERN, `No matching pattern for macro ${macroName}`));
  return null;
}

function matchPattern(pattern: Form, args: Form[], macroName: string): Map<string, Form> | null {
  // Expect pattern to be a list with the macro name as head
  if (pattern.tag !== "List" || !pattern.children) return null;
  if (pattern.children.length === 0) return null;
  const [head, ...patArgs] = pattern.children;
  if (head.tag !== "Symbol" || head.value !== macroName) return null;
  if (patArgs.length !== args.length) return null;

  const bindings = new Map<string, Form>();
  for (let i = 0; i < patArgs.length; i++) {
    if (!matchForm(patArgs[i], args[i], bindings)) return null;
  }
  return bindings;
}

function matchForm(pattern: Form, value: Form, bindings: Map<string, Form>): boolean {
  if (pattern.tag === "Symbol") {
    const name = String(pattern.value);
    bindings.set(name, value);
    return true;
  }

  if (pattern.tag !== value.tag) return false;

  if ((pattern.tag === "List" || pattern.tag === "Vector" || pattern.tag === "Map") && pattern.children) {
    if (!value.children || pattern.children.length !== value.children.length) return false;
    for (let i = 0; i < pattern.children.length; i++) {
      if (!matchForm(pattern.children[i], value.children[i], bindings)) return false;
    }
    return true;
  }

  return pattern.value === value.value;
}

function instantiateTemplate(template: Form, bindings: Map<string, Form>): Form {
  if (template.tag === "Symbol") {
    const name = String(template.value);
    if (bindings.has(name)) {
      const bound = bindings.get(name)!;
      return { ...bound, meta: { ...bound.meta, macroExpanded: true } };
    }
  }

  if (template.children) {
    const children = template.children.map(child => instantiateTemplate(child, bindings));
    return { ...template, children, meta: { ...template.meta, macroExpanded: true } };
  }

  return { ...template, meta: { ...template.meta, macroExpanded: true } };
}

function markExpanded(expanded: Form, original: Form): Form {
  const mark = (f: Form): Form => {
    const meta = { ...f.meta, macroExpanded: true, originalForm: original };
    const children = f.children?.map(mark);
    return { ...f, meta, children };
  };
  return mark(expanded);
}
