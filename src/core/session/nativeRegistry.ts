import type { Val } from "../eval/values";
import type { Store } from "../eval/store";

/**
 * Build a registry of Native functions by scanning the primed store.
 * Used during deserialization to restore Native.fn from Native.name.
 */
export function buildNativeRegistry(store: Store): Map<string, Val> {
  const registry = new Map<string, Val>();

  for (let addr = 0; addr < store.next; addr++) {
    try {
      const val = store.read(addr);
      if (val && (val as any).tag === "Native") {
        registry.set((val as any).name, val);
      }
    } catch {
      // Skip invalid addresses
    }
  }

  return registry;
}
