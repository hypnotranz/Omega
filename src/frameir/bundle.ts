import type { NodeBase } from "./meta";
import type { IRVersion } from "./version";
import type { ValueIR } from "./value";
import type { FlowIR } from "./flow";
import type { SchemaIR } from "./schema";
import type { ToolContractIR } from "./contract";

// Closure-converted function definition
export interface FnDefIR extends NodeBase {
  tag: "FnDef";
  fnId: string;
  params: string[];
  body: FlowIR | ValueIR;
  captures?: ValueIR;
}

// Complete IR bundle (the unit of compilation/caching)
export interface IRBundle {
  v: IRVersion;
  entry: FlowIR;
  fns: Record<string, FnDefIR>;
  schemas: Record<string, SchemaIR>;
  toolContracts: Record<string, ToolContractIR>;
  modules?: Record<string, string[]>;
  docs?: Record<string, string>;
}
