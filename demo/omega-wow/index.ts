// demo/omega-wow/index.ts
// Ω Wow Pack - All demo exports

export { demo1OracleRepl } from "./demo1-oracle-repl";
export { demo2Backtracking } from "./demo2-backtracking";
export { demo3Concurrency } from "./demo3-concurrency";
export { demo4GenericSynthesis } from "./demo4-generic-synthesis";
export { demo5ConstraintRepair } from "./demo5-constraint-repair";
export { demo6SemanticMacros } from "./demo6-semantic-macros";
export { demo7Compilation } from "./demo7-compilation";
export { demo8MetaCircular } from "./demo8-meta-circular";

import type { DemoDefinition } from "../harness/types";
import { demo1OracleRepl } from "./demo1-oracle-repl";
import { demo2Backtracking } from "./demo2-backtracking";
import { demo3Concurrency } from "./demo3-concurrency";
import { demo4GenericSynthesis } from "./demo4-generic-synthesis";
import { demo5ConstraintRepair } from "./demo5-constraint-repair";
import { demo6SemanticMacros } from "./demo6-semantic-macros";
import { demo7Compilation } from "./demo7-compilation";
import { demo8MetaCircular } from "./demo8-meta-circular";

/**
 * All available demos.
 */
export const allDemos: DemoDefinition[] = [
  demo1OracleRepl,
  demo2Backtracking,
  demo3Concurrency,
  demo4GenericSynthesis,
  demo5ConstraintRepair,
  demo6SemanticMacros,
  demo7Compilation,
  demo8MetaCircular,
];

/**
 * Get demo by ID.
 */
export function getDemoById(id: string): DemoDefinition | undefined {
  return allDemos.find(d => d.id === id);
}

/**
 * Get demos by tag.
 */
export function getDemosByTag(tag: string): DemoDefinition[] {
  return allDemos.filter(d => d.tags.includes(tag));
}

/**
 * List all demo IDs.
 */
export function listDemoIds(): string[] {
  return allDemos.map(d => d.id);
}

/**
 * Demo summary for listing.
 */
export interface DemoSummary {
  id: string;
  name: string;
  description: string;
  tags: string[];
}

/**
 * Get all demo summaries.
 */
export function listDemos(): DemoSummary[] {
  return allDemos.map(d => ({
    id: d.id,
    name: d.name,
    description: d.description,
    tags: d.tags,
  }));
}
