import type { NodeBase } from "./meta";
import type { ValueIR } from "./value";

// Boolean ops
export interface EIf extends NodeBase { tag: "EIf"; cond: ValueIR; then: ValueIR; else: ValueIR }
export interface EEq extends NodeBase { tag: "EEq"; a: ValueIR; b: ValueIR }
export interface ENot extends NodeBase { tag: "ENot"; x: ValueIR }
export interface EAnd extends NodeBase { tag: "EAnd"; xs: ValueIR[] }
export interface EOr extends NodeBase { tag: "EOr"; xs: ValueIR[] }

// Arithmetic ops
export interface EAdd extends NodeBase { tag: "EAdd"; xs: ValueIR[] }
export interface ESub extends NodeBase { tag: "ESub"; xs: ValueIR[] }
export interface EMul extends NodeBase { tag: "EMul"; xs: ValueIR[] }
export interface EDiv extends NodeBase { tag: "EDiv"; a: ValueIR; b: ValueIR }
export interface EMod extends NodeBase { tag: "EMod"; a: ValueIR; b: ValueIR }

// Map ops
export interface EGet extends NodeBase { tag: "EGet"; map: ValueIR; key: ValueIR; default?: ValueIR }
export interface EAssoc extends NodeBase { tag: "EAssoc"; map: ValueIR; key: ValueIR; val: ValueIR }

// Primitive call
export interface ECallPrim extends NodeBase {
  tag: "ECallPrim";
  prim: string;
  args: ValueIR[];
}

export type VExpr =
  | EIf | EEq | ENot | EAnd | EOr
  | EAdd | ESub | EMul | EDiv | EMod
  | EGet | EAssoc
  | ECallPrim;
