import type { IRVersion } from "./version";

export interface Span {
  file?: string;
  startLine?: number;
  startCol?: number;
  endLine?: number;
  endCol?: number;
}

export interface Meta {
  span?: Span;
  doc?: string;
  attrs?: Record<string, unknown>;
}

export interface NodeBase {
  v: IRVersion;
  tag: string;
  meta?: Meta;
}
