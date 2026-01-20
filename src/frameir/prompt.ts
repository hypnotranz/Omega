import type { NodeBase } from "./meta";
import type { ValueIR, VRef } from "./value";

// Role-based segments
export interface PSystem extends NodeBase { tag: "PSystem"; text: string }
export interface PUser extends NodeBase { tag: "PUser"; text: string }
export interface PAssistant extends NodeBase { tag: "PAssistant"; text: string }

// Few-shot examples
export interface PFewShot extends NodeBase {
  tag: "PFewShot";
  examples: Array<{ user: string; assistant: string }>;
}

// Data embedding
export interface PData extends NodeBase { tag: "PData"; value: ValueIR }

// Structure wrappers
export interface PXml extends NodeBase { tag: "PXml"; tagName: string; inner: PromptIR }
export interface PCodeBlock extends NodeBase { tag: "PCodeBlock"; lang: string; code: string }
export interface PNumbered extends NodeBase { tag: "PNumbered"; items: PromptIR[] }

// Attachments (for tools/schemas)
export interface PAttachTools extends NodeBase {
  tag: "PAttachTools";
  tools: VRef[];
  inner: PromptIR;
}

export interface PAttachSchema extends NodeBase {
  tag: "PAttachSchema";
  schema: VRef;
  inner: PromptIR;
}

export interface PAttachFormat extends NodeBase {
  tag: "PAttachFormat";
  format: { kind: "json" | "xml" | "text"; details?: ValueIR };
  inner: PromptIR;
}

// Transforms
export interface PTransform extends NodeBase {
  tag: "PTransform";
  transform: string;
  inner: PromptIR;
}

export type PromptPart =
  | PSystem | PUser | PAssistant
  | PFewShot
  | PData
  | PXml | PCodeBlock | PNumbered
  | PAttachTools | PAttachSchema | PAttachFormat
  | PTransform;

// Top-level prompt document
export interface PromptDoc extends NodeBase {
  tag: "PromptDoc";
  parts: PromptPart[];
}

export type PromptIR = PromptDoc;
