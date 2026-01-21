import { PrimitiveRegistry } from "./registry";
import type { Effect, PrimitiveDescriptor } from "./types";

export function findPrimitive(registry: PrimitiveRegistry, id: string): PrimitiveDescriptor | undefined {
  return registry.get(id);
}

export function apropos(registry: PrimitiveRegistry, query: string): PrimitiveDescriptor[] {
  return registry.search(query);
}

export function filterByLayer(registry: PrimitiveRegistry, layer: string): PrimitiveDescriptor[] {
  return registry.getByLayer(layer);
}

export function filterByEffect(registry: PrimitiveRegistry, effect: Effect): PrimitiveDescriptor[] {
  return registry.getByEffect(effect);
}
