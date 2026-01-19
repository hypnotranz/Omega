// SOURCE: ARCHITECTURE/32-LANGUAGE-OFFICIAL-13-IMPLEMENTATION-11.md
// AUTO-EXTRACTED - Do not edit directly. Edit the source document.

import type { Hash } from "./hash";

export type RegistryEntry = {
  name: string;
  time: number; // unix ms
};

export type Registry = {
  candidates: Record<Hash, RegistryEntry>;
  trusted: Record<Hash, RegistryEntry>;
  pointers: {
    defaultExpander?: Hash;
    defaultPolicy?: Hash;
  };
};

export function emptyRegistry(): Registry {
  return { candidates: {}, trusted: {}, pointers: {} };
}