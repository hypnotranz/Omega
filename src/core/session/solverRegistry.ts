import type { Store } from "../eval/store";
import type { Val } from "../eval/values";

export type SolverRegistry = Map<string, Val>;

export function buildSolverRegistry(store: Store): SolverRegistry {
  const registry: SolverRegistry = new Map();

  for (let addr = 0; addr < store.next; addr++) {
    try {
      const val = store.read(addr);
      if (val && (val as any).tag === "Solver") {
        registry.set((val as any).name, val);
      }
    } catch {
      // Skip invalid addresses
    }
  }

  return registry;
}

export function registerSolver(registry: SolverRegistry, name: string, solver: Val): void {
  if ((solver as any).tag !== "Solver") {
    throw new Error(`Not a Solver: ${name}`);
  }
  registry.set(name, solver);
}
