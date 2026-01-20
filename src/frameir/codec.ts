import type { NodeBase } from "./meta";

/**
 * Encode an IR node to a canonical JSON string.
 * - Object keys are sorted lexicographically
 * - Optional meta is excluded unless explicitly requested
 * - Arrays preserve order
 */
export function encodeCanonical(node: NodeBase, options?: { includeMeta?: boolean }): string {
  return JSON.stringify(node, (_key, value) => {
    if (!options?.includeMeta && _key === "meta") {
      return undefined;
    }

    if (value && typeof value === "object" && !Array.isArray(value)) {
      const sorted: Record<string, unknown> = {};
      for (const k of Object.keys(value).sort()) {
        sorted[k] = (value as Record<string, unknown>)[k];
      }
      return sorted;
    }

    return value;
  });
}

/**
 * Decode a canonical JSON string into an IR node.
 * Validates that the required discriminator fields exist.
 */
export function decode<T extends NodeBase>(json: string): T {
  const node = JSON.parse(json);
  if (!node || typeof node !== "object") {
    throw new Error("Invalid IR node: expected object");
  }
  if (!("v" in node)) {
    throw new Error("Invalid IR node: missing v");
  }
  if (!("tag" in node)) {
    throw new Error("Invalid IR node: missing tag");
  }
  return node as T;
}
