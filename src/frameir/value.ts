import type { NodeBase } from "./meta";
import type { VExpr } from "./expr";

// Primitive values
export interface VNil extends NodeBase { tag: "VNil" }
export interface VBool extends NodeBase { tag: "VBool"; value: boolean }
export interface VInt extends NodeBase { tag: "VInt"; value: string }
export interface VFloat extends NodeBase { tag: "VFloat"; value: string }
export interface VStr extends NodeBase { tag: "VStr"; value: string }
export interface VSymbol extends NodeBase { tag: "VSymbol"; name: string }
export interface VKeyword extends NodeBase { tag: "VKeyword"; name: string }

// Compound values
export interface VList extends NodeBase { tag: "VList"; items: ValueIR[] }
export interface VRecord extends NodeBase {
  tag: "VRecord";
  entries: Array<{ k: ValueIR; v: ValueIR }>;
}

// References (into bundle or external resources)
export type RefKind = "Global" | "Fn" | "ToolContract" | "Schema" | "Store" | "Sink" | "Source";

export interface VRef extends NodeBase {
  tag: "VRef";
  ref: {
    kind: RefKind;
    id: string;
    name?: string;
  };
}

// Union of all value types
export type ValueIR =
  | VNil | VBool | VInt | VFloat | VStr | VSymbol | VKeyword
  | VList | VRecord
  | VRef
  | VExpr;
