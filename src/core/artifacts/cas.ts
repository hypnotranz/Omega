// SOURCE: ARCHITECTURE/32-LANGUAGE-OFFICIAL-13-IMPLEMENTATION-7.md
// AUTO-EXTRACTED - Do not edit directly. Edit the source document.

export type Hash = string;

export interface CAS {
  putJSON(x: unknown): Hash;
  getJSON(h: Hash): unknown;
  putText(s: string): Hash;
  getText(h: Hash): string;
}