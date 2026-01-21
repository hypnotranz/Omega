import type { FlowCompileConfig } from "./types";

export const defaultFlowCompileConfig: FlowCompileConfig = {
  normalize: {
    flattenSequences: true,
    flattenPrompts: true,
    insertImplicitBudgets: false,
    insertImplicitTimeouts: false,
  },
  lint: false,
  sourceMap: true,
  debug: false,
};
