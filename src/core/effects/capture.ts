// SOURCE: ARCHITECTURE/32-LANGUAGE-OFFICIAL-13-IMPLEMENTATION-4.md
// AUTO-EXTRACTED - Do not edit directly. Edit the source document.

import type { Val } from "../eval/values";
import type { State } from "../eval/machine";
import type { Resumption } from "./opcall";

function uuid(): string {
  return Math.random().toString(16).slice(2) + "-" + Date.now().toString(16);
}

export function captureValueResumption(state: State): Resumption {
  const rid = uuid();
  const base: State = {
    ...state,
    store: state.store.snapshot(),
    // IMPORTANT: env and kont and handlers must be treated as immutable; do not mutate in place.
  };

  return {
    rid,
    base,
    invoke: (v: Val) => ({
      ...base,
      control: { tag: "Val", v },
      // base.store is already a snapshot; if you use mutable stores, snapshot again here.
    }),
    digest: () =>
      JSON.stringify({
        rid,
        store: base.store.digest(),
        kontDepth: base.kont.length,
        handlersDepth: base.handlers.length,
      }),
  };
}