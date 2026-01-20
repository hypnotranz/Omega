import type { NodeBase } from "./meta";

// Atomic schema nodes
export interface SAny extends NodeBase { tag: "SAny" }
export interface SString extends NodeBase { tag: "SString" }
export interface SNumber extends NodeBase { tag: "SNumber" }
export interface SInt extends NodeBase { tag: "SInt" }
export interface SBool extends NodeBase { tag: "SBool" }
export interface SNil extends NodeBase { tag: "SNil" }

// Compound schema nodes
export interface SList extends NodeBase { tag: "SList"; item: FrameSchemaNode }
export interface SRecord extends NodeBase {
  tag: "SRecord";
  fields: Array<{ key: string; schema: FrameSchemaNode; optional?: boolean }>;
  closed?: boolean;
}
export interface SUnion extends NodeBase { tag: "SUnion"; options: FrameSchemaNode[] }
export interface SRef extends NodeBase { tag: "SRef"; schemaId: string }

export type FrameSchemaNode =
  | SAny | SString | SNumber | SInt | SBool | SNil
  | SList | SRecord
  | SUnion
  | SRef;

// Top-level schema (supports both FrameSchema and JSON Schema)
export interface SchemaIR extends NodeBase {
  tag: "Schema";
  id: string;
  kind: "JsonSchema" | "FrameSchema";
  jsonSchema?: unknown;
  frameSchema?: FrameSchemaNode;
}
