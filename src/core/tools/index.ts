// src/core/tools/index.ts
// Tool system exports

export {
  ToolRegistry,
  ToolResult,
  ToolDef,
  createShellTool,
  createReadFileTool,
  createWriteFileTool,
  createGrepTool,
  getDefaultRegistry,
  setDefaultRegistry,
} from "./registry";
