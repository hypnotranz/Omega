import type { IRBundle } from "../frameir/bundle";
import type { Diagnostic } from "../outcome/diagnostic";
import { defaultRegistry, PrimitiveRegistry } from "../registry";
import { budgetDominatorPass } from "./passes/budgetDominator";
import { timeoutGuardPass } from "./passes/timeoutGuard";
import { toolContractPass } from "./passes/toolContract";
import type { LintConfig, Pass, PassPhase, PassResult } from "./types";

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
    const sorted = this.sortPasses();

    for (const pass of sorted) {
      const config = this.config.passes[pass.id] ?? { enabled: true };
      const enabled = config.enabled !== false && config.severityOverride !== "off";
      if (!enabled) {
        continue;
      }

      this.assertDependenciesSatisfied(pass, passResults);

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

  private sortPasses(): Pass[] {
    return Array.from(this.passes.values()).sort((a, b) => {
      const phaseA = PHASE_ORDER.indexOf(a.phase);
      const phaseB = PHASE_ORDER.indexOf(b.phase);
      if (phaseA !== phaseB) return phaseA - phaseB;
      return a.id.localeCompare(b.id);
    });
  }

  private assertDependenciesSatisfied(pass: Pass, results: Map<string, PassResult>): void {
    if (!pass.dependencies || pass.dependencies.length === 0) {
      return;
    }

    for (const dep of pass.dependencies) {
      const depPass = this.passes.get(dep);
      if (!depPass) {
        throw new Error(`Pass dependency not registered: ${dep}`);
      }

      const depConfig = this.config.passes[dep] ?? { enabled: true };
      const depEnabled = depConfig.enabled !== false && depConfig.severityOverride !== "off";
      if (!depEnabled) {
        continue;
      }

      if (!results.has(dep)) {
        throw new Error(`Pass dependency has not run: ${dep} (required by ${pass.id})`);
      }
    }
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
