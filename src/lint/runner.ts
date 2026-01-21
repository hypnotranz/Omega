import type { IRBundle } from "../frameir/bundle";
import type { Diagnostic } from "../outcome/diagnostic";
import { defaultRegistry, PrimitiveRegistry } from "../registry";
import { budgetDominatorPass } from "./passes/budgetDominator";
import { timeoutGuardPass } from "./passes/timeoutGuard";
import { toolContractPass } from "./passes/toolContract";
import type { LintConfig, Pass, PassConfig, PassPhase, PassResult } from "./types";

const DEFAULT_CONFIG: LintConfig = { passes: {} };
const PHASE_ORDER: PassPhase[] = ["parsing", "lowering", "normalize", "lint", "optimize"];

export class LintRunner {
  private passes: Map<string, Pass> = new Map();
  private config: LintConfig;
  private registry: PrimitiveRegistry;

  constructor(config: Partial<LintConfig> = DEFAULT_CONFIG, registry: PrimitiveRegistry = defaultRegistry) {
    this.config = { passes: config?.passes ?? {} };
    this.registry = registry;
  }

  register(pass: Pass): void {
    this.passes.set(pass.id, pass);
  }

  run(bundle: IRBundle): { bundle: IRBundle; diagnostics: Diagnostic[]; passResults: Map<string, PassResult> } {
    let currentBundle = bundle;
    const diagnostics: Diagnostic[] = [];
    const passResults = new Map<string, PassResult>();
    const sorted = this.resolvePassOrder();

    for (const pass of sorted) {
      const config = this.getPassConfig(pass.id);
      const result = pass.run(currentBundle, this.registry);
      passResults.set(pass.id, result);

      diagnostics.push(...applySeverityOverride(result.diagnostics, config.severityOverride));

      if (result.transformed) {
        currentBundle = result.transformed;
      }
    }

    return { bundle: currentBundle, diagnostics, passResults };
  }

  hasErrors(diags: Diagnostic[]): boolean {
    return diags.some(d => d.severity === "error");
  }

  private resolvePassOrder(): Pass[] {
    const enabledPasses = Array.from(this.passes.values()).filter(p => this.isPassEnabled(p.id));
    const passLookup = new Map(enabledPasses.map(p => [p.id, p]));
    const ordered: Pass[] = [];
    const executed = new Set<string>();

    for (const pass of enabledPasses) {
      for (const dep of this.enabledDependencies(pass)) {
        if (!passLookup.has(dep)) {
          throw new Error(`Pass dependency not registered: ${dep}`);
        }
      }
    }

    for (const phase of PHASE_ORDER) {
      const phasePasses = enabledPasses.filter(p => p.phase === phase);
      if (phasePasses.length === 0) continue;

      const indegree = new Map<string, number>();
      const edges = new Map<string, Set<string>>();

      for (const pass of phasePasses) {
        indegree.set(pass.id, 0);
        edges.set(pass.id, new Set());
      }

      for (const pass of phasePasses) {
        for (const dep of this.enabledDependencies(pass)) {
          const depPass = passLookup.get(dep);
          if (!depPass) {
            continue;
          }

          const depPhaseIndex = this.phaseIndex(depPass.phase);
          const passPhaseIndex = this.phaseIndex(pass.phase);

          if (depPhaseIndex > passPhaseIndex) {
            throw new Error(`Pass ${pass.id} depends on ${dep} in later phase ${depPass.phase}`);
          }

          if (depPhaseIndex < passPhaseIndex) {
            if (!executed.has(dep)) {
              throw new Error(`Pass dependency has not run: ${dep} (required by ${pass.id})`);
            }
            continue;
          }

          edges.get(dep)!.add(pass.id);
          indegree.set(pass.id, (indegree.get(pass.id) ?? 0) + 1);
        }
      }

      const ready = phasePasses
        .filter(p => (indegree.get(p.id) ?? 0) === 0)
        .sort((a, b) => a.id.localeCompare(b.id));

      let processed = 0;
      while (ready.length > 0) {
        const next = ready.shift()!;
        ordered.push(next);
        executed.add(next.id);
        processed++;

        for (const target of edges.get(next.id) ?? []) {
          const updated = (indegree.get(target) ?? 0) - 1;
          indegree.set(target, updated);
          if (updated === 0) {
            ready.push(passLookup.get(target)!);
            ready.sort((a, b) => a.id.localeCompare(b.id));
          }
        }
      }

      if (processed !== phasePasses.length) {
        throw new Error(`Pass dependency cycle detected in phase ${phase}`);
      }
    }

    return ordered;
  }

  private getPassConfig(passId: string): PassConfig {
    return this.config.passes[passId] ?? { enabled: true };
  }

  private isPassEnabled(passId: string): boolean {
    const config = this.getPassConfig(passId);
    return config.enabled !== false && config.severityOverride !== "off";
  }

  private enabledDependencies(pass: Pass): string[] {
    return (pass.dependencies ?? []).filter(dep => this.isPassEnabled(dep));
  }

  private phaseIndex(phase: PassPhase): number {
    const idx = PHASE_ORDER.indexOf(phase);
    if (idx === -1) {
      throw new Error(`Unknown pass phase: ${phase}`);
    }
    return idx;
  }
}

function applySeverityOverride(
  diagnostics: Diagnostic[],
  override?: PassConfigSeverity
): Diagnostic[] {
  if (!override || override === "off") {
    return diagnostics;
  }
  return diagnostics.map(d => ({ ...d, severity: override }));
}

type PassConfigSeverity = "error" | "warning" | "info" | "off" | undefined;

export function createDefaultRunner(
  config?: Partial<LintConfig>,
  registry: PrimitiveRegistry = defaultRegistry
): LintRunner {
  const runner = new LintRunner(config ?? DEFAULT_CONFIG, registry);
  runner.register(budgetDominatorPass);
  runner.register(timeoutGuardPass);
  runner.register(toolContractPass);
  return runner;
}
