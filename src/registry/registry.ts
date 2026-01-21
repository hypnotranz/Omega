import { type Effect, type PrimitiveDescriptor } from "./types";

/**
 * Registry of primitive descriptors used for analysis and documentation.
 */
export class PrimitiveRegistry {
  private descriptors: Map<string, PrimitiveDescriptor> = new Map();
  private byLayer: Map<PrimitiveDescriptor["layer"], Set<string>> = new Map();
  private byEffect: Map<Effect, Set<string>> = new Map();
  private byIrTag: Map<string, Set<string>> = new Map();

  /**
   * Register a descriptor. Throws on duplicate ids.
   */
  register(descriptor: PrimitiveDescriptor): void {
    if (this.descriptors.has(descriptor.id)) {
      throw new Error(`Primitive already registered: ${descriptor.id}`);
    }

    this.descriptors.set(descriptor.id, descriptor);
    this.indexByLayer(descriptor);
    this.indexByEffect(descriptor);
    this.indexByIrTag(descriptor);
  }

  /**
   * Retrieve by canonical id.
   */
  get(id: string): PrimitiveDescriptor | undefined {
    return this.descriptors.get(id);
  }

  /**
   * List all descriptors.
   */
  getAll(): PrimitiveDescriptor[] {
    return Array.from(this.descriptors.values());
  }

  /**
   * Query by layer.
   */
  getByLayer(layer: PrimitiveDescriptor["layer"]): PrimitiveDescriptor[] {
    const ids = this.byLayer.get(layer);
    if (!ids) return [];
    return Array.from(ids).map(id => this.descriptors.get(id)!);
  }

  /**
   * Query by required effect.
   */
  getByEffect(effect: Effect): PrimitiveDescriptor[] {
    const ids = this.byEffect.get(effect);
    if (!ids) return [];
    return Array.from(ids).map(id => this.descriptors.get(id)!);
  }

  /**
   * Find descriptor by IR tag (first match).
   */
  getByIrTag(irTag: string): PrimitiveDescriptor | undefined {
    const ids = this.byIrTag.get(irTag);
    if (!ids || ids.size === 0) return undefined;
    const [first] = ids;
    return this.descriptors.get(first);
  }

  /**
   * Simple text search (id, summary, detail).
   */
  search(query: string): PrimitiveDescriptor[] {
    const q = query.toLowerCase();
    return this.getAll().filter(d =>
      d.id.toLowerCase().includes(q) ||
      d.doc.summary.toLowerCase().includes(q) ||
      (d.doc.detail?.toLowerCase().includes(q) ?? false)
    );
  }

  /**
   * Validate descriptors for required fields and consistency.
   */
  validate(): { valid: boolean; errors: string[] } {
    const errors: string[] = [];

    for (const desc of this.descriptors.values()) {
      if (!desc.signature) {
        errors.push(`${desc.id}: missing signature`);
      }
      if (!desc.doc?.summary) {
        errors.push(`${desc.id}: missing doc.summary`);
      }
      if (!desc.version) {
        errors.push(`${desc.id}: missing version`);
      }

      if (desc.effects.includes("Oracle") && !desc.constraints?.mustBeDominatedByBudget) {
        errors.push(`${desc.id}: Oracle effects should require budget dominance`);
      }
      if (desc.effects.includes("Tool") && !desc.constraints?.requiresToolContract) {
        errors.push(`${desc.id}: Tool effects should require tool contracts`);
      }
    }

    return { valid: errors.length === 0, errors };
  }

  /**
   * Serialize registry to JSON-compatible object.
   */
  toJSON(): Record<string, PrimitiveDescriptor> {
    return Object.fromEntries(this.descriptors);
  }

  /**
   * Hydrate registry from JSON descriptors.
   */
  static fromJSON(data: Record<string, PrimitiveDescriptor>): PrimitiveRegistry {
    const registry = new PrimitiveRegistry();
    for (const desc of Object.values(data)) {
      registry.register(desc);
    }
    return registry;
  }

  private indexByLayer(descriptor: PrimitiveDescriptor): void {
    if (!this.byLayer.has(descriptor.layer)) {
      this.byLayer.set(descriptor.layer, new Set());
    }
    this.byLayer.get(descriptor.layer)!.add(descriptor.id);
  }

  private indexByEffect(descriptor: PrimitiveDescriptor): void {
    for (const effect of descriptor.effects) {
      if (!this.byEffect.has(effect)) {
        this.byEffect.set(effect, new Set());
      }
      this.byEffect.get(effect)!.add(descriptor.id);
    }
  }

  private indexByIrTag(descriptor: PrimitiveDescriptor): void {
    const irTag = descriptor.lowering?.irTag;
    if (!irTag) return;

    if (!this.byIrTag.has(irTag)) {
      this.byIrTag.set(irTag, new Set());
    }
    this.byIrTag.get(irTag)!.add(descriptor.id);
  }
}

export const defaultRegistry = new PrimitiveRegistry();

const DEFAULT_DESCRIPTORS: PrimitiveDescriptor[] = [
  {
    id: "framelisp/infer",
    layer: "FrameLisp",
    kind: "SpecialForm",
    signature: { params: [{ name: "prompt", type: "Prompt" }], returns: "Str" },
    effects: ["Oracle"],
    doc: { summary: "Core inference primitive." },
    version: "1.0.0",
    lowering: { kind: "Intrinsic", irTag: "FInfer" },
    constraints: { mustBeDominatedByBudget: true, mustBeDominatedByTimeout: true },
  },
  {
    id: "framelisp/call-tool",
    layer: "FrameLisp",
    kind: "SpecialForm",
    signature: { params: [{ name: "tool", type: "Tool" }], returns: "Any" },
    effects: ["Tool"],
    doc: { summary: "Call an external tool." },
    version: "1.0.0",
    lowering: { kind: "Intrinsic", irTag: "FToolCall" },
    constraints: { requiresToolContract: true, mustBeDominatedByBudget: true },
  },
  {
    id: "framelisp/with-budget",
    layer: "FrameLisp",
    kind: "SpecialForm",
    signature: { params: [{ name: "budget", type: "Budget" }], returns: "Flow" },
    effects: ["Control"],
    doc: { summary: "Budget guard." },
    version: "1.0.0",
    lowering: { kind: "Intrinsic", irTag: "FWithBudget" },
  },
  {
    id: "framelisp/with-timeout",
    layer: "FrameLisp",
    kind: "SpecialForm",
    signature: { params: [{ name: "timeoutMs", type: "Int" }], returns: "Flow" },
    effects: ["Control"],
    doc: { summary: "Timeout guard." },
    version: "1.0.0",
    lowering: { kind: "Intrinsic", irTag: "FWithTimeout" },
  },
];

for (const desc of DEFAULT_DESCRIPTORS) {
  try {
    defaultRegistry.register(desc);
  } catch {
    // Ignore duplicate registration if executed more than once.
  }
}
