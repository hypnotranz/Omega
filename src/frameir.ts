import { createHash } from "crypto";

// FrameIR base value type: deterministic JSON-compatible structure
export type ScalarIR = string | number | boolean | null;
export type ValueIR = ScalarIR | ValueIR[] | { [key: string]: ValueIR };

export type MessageIR = {
  role: string;
  content: ValueIR | string;
  name?: string;
  metadata?: ValueIR;
};

export type PromptIR = {
  kind: "prompt";
  messages: MessageIR[];
  metadata?: ValueIR;
};

export type FlowStepIR = {
  id: string;
  prompt: PromptIR;
  input?: ValueIR;
  metadata?: ValueIR;
};

export type FlowIR = {
  kind: "flow";
  steps: FlowStepIR[];
  metadata?: ValueIR;
};

export type FrameValue = ValueIR | PromptIR | FlowIR;

export type MerkleObjectEntry = { key: string; node: MerkleNode };
export type MerkleObjectNode = { type: "object"; entries: MerkleObjectEntry[]; hash: string };
export type MerkleArrayNode = { type: "array"; items: MerkleNode[]; hash: string };
export type MerkleValueNode = { type: "value"; value: ScalarIR; hash: string };
export type MerkleNode = MerkleObjectNode | MerkleArrayNode | MerkleValueNode;

type CanonicalValue = ScalarIR | CanonicalValue[] | { [key: string]: CanonicalValue };

/**
 * Encode FrameIR into canonical JSON (sorted object keys, rejects non-JSON inputs).
 */
export function canonicalJson(value: FrameValue): string {
  const canonical = canonicalize(value);
  return JSON.stringify(canonical);
}

/**
 * Compute a merkle hash (sha256 hex) of the canonical JSON encoding.
 */
export function merkleHash(value: FrameValue): string {
  const canonical = canonicalize(value);
  return sha256Hex(JSON.stringify(canonical));
}

/**
 * Build a merkle tree for the given FrameIR value.
 */
export function merkleize(value: FrameValue): MerkleNode {
  const canonical = canonicalize(value);
  return buildMerkleNode(canonical);
}

function canonicalize(value: unknown): CanonicalValue {
  if (value === null) return null;

  const t = typeof value;

  if (t === "string" || t === "boolean") return value as CanonicalValue;

  if (t === "number") {
    if (Number.isNaN(value)) throw new Error("canonicalJson: NaN is not supported");
    if (!Number.isFinite(value)) throw new Error("canonicalJson: expected finite number");
    return value as CanonicalValue;
  }

  if (Array.isArray(value)) {
    return value.map(canonicalize);
  }

  if (t === "object") {
    const proto = Object.getPrototypeOf(value as object);
    if (proto !== Object.prototype && proto !== null) {
      throw new Error("Unsupported type for canonicalJson: object prototype not supported");
    }

    const source = value as Record<string, unknown>;
    const keys = Object.keys(source).sort();
    const out: Record<string, CanonicalValue> = {};

    for (const key of keys) {
      const v = source[key];
      if (typeof v === "undefined") throw new Error("canonicalJson: undefined values are not supported");
      out[key] = canonicalize(v);
    }
    return out;
  }

  throw new Error(`Unsupported type for canonicalJson: ${t}`);
}

function buildMerkleNode(value: CanonicalValue): MerkleNode {
  if (Array.isArray(value)) {
    const items = value.map(buildMerkleNode);
    return {
      type: "array",
      items,
      hash: sha256Hex(JSON.stringify(value)),
    };
  }

  if (value !== null && typeof value === "object") {
    const entries = Object.keys(value)
      .sort()
      .map(key => ({ key, node: buildMerkleNode((value as Record<string, CanonicalValue>)[key]) }));
    return {
      type: "object",
      entries,
      hash: sha256Hex(JSON.stringify(value)),
    };
  }

  return {
    type: "value",
    value: value as ScalarIR,
    hash: sha256Hex(JSON.stringify(value)),
  };
}

function sha256Hex(s: string): string {
  return createHash("sha256").update(s).digest("hex");
}
