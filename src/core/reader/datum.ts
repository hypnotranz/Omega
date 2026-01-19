// SOURCE: ARCHITECTURE/32-LANGUAGE-OFFICIAL-13-IMPLEMENTATION-5.md
// AUTO-EXTRACTED - Do not edit directly. Edit the source document.

export type Sym = { sym: string };

export type Datum =
  | number
  | string
  | boolean
  | null
  | Sym
  | Datum[];

export const sym = (s: string): Sym => ({ sym: s });
export const isSym = (d: Datum): d is Sym => typeof d === "object" && d !== null && !Array.isArray(d) && "sym" in d;