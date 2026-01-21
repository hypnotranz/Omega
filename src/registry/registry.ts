import { type Effect, type PrimitiveDescriptor } from "./types";

export class PrimitiveRegistry {
  private descriptors: Map<string, PrimitiveDescriptor> = new Map();
  private byLayer: Map<string, Set<string>> = new Map();
  private byEffect: Map<Effect, Set<string>> = new Map();

  /**
   * Register a primitive descriptor.
   * @throws if id already registered
   */
  register(descriptor: PrimitiveDescriptor): void {
    if (this.descriptors.has(descriptor.id)) {
      throw new Error(`Primitive already registered: ${descriptor.id}`);
    }

    this.descriptors.set(descriptor.id, descriptor);

    if (!this.byLayer.has(descriptor.layer)) {
      this.byLayer.set(descriptor.layer, new Set());
    }
    this.byLayer.get(descriptor.layer)!.add(descriptor.id);

    for (const effect of descriptor.effects) {
      if (!this.byEffect.has(effect)) {
        this.byEffect.set(effect, new Set());
      }
      this.byEffect.get(effect)!.add(descriptor.id);
    }
  }

  /**
   * Get descriptor by ID.
   */
  get(id: string): PrimitiveDescriptor | undefined {
    return this.descriptors.get(id);
  }

  /**
   * Get all descriptors.
   */
  getAll(): PrimitiveDescriptor[] {
    return Array.from(this.descriptors.values());
  }

  /**
   * Get all descriptors for a layer.
   */
  getByLayer(layer: string): PrimitiveDescriptor[] {
    const ids = this.byLayer.get(layer);
    if (!ids) return [];
    return Array.from(ids).map(id => this.descriptors.get(id)!).filter(Boolean);
  }

  /**
   * Get all descriptors with a specific effect.
   */
  getByEffect(effect: Effect): PrimitiveDescriptor[] {
    const ids = this.byEffect.get(effect);
    if (!ids) return [];
    return Array.from(ids).map(id => this.descriptors.get(id)!).filter(Boolean);
  }

  /**
   * Search descriptors by text (apropos).
   * Matches against id, summary, and detail.
   */
  search(query: string): PrimitiveDescriptor[] {
    const q = query.toLowerCase();
    if (!q) return [];
    return this.getAll().filter(d =>
      d.id.toLowerCase().includes(q) ||
      d.doc.summary.toLowerCase().includes(q) ||
      (d.doc.detail?.toLowerCase().includes(q) ?? false)
    );
  }

  /**
   * Get lowering rule for an IR tag.
   */
  getByIrTag(irTag: string): PrimitiveDescriptor | undefined {
    return this.getAll().find(d => d.lowering?.irTag === irTag);
  }

  /**
   * Validate registry completeness.
   */
  validate(): { valid: boolean; errors: string[] } {
    const errors: string[] = [];

    for (const [id, desc] of this.descriptors) {
      if (!desc.signature) {
        errors.push(`${id}: missing signature`);
      }
      if (!desc.doc?.summary) {
        errors.push(`${id}: missing doc.summary`);
      }
      if (!desc.version) {
        errors.push(`${id}: missing version`);
      }
      if (!desc.effects || desc.effects.length === 0) {
        errors.push(`${id}: missing effects`);
      }

      if (desc.effects.includes("Oracle") && !desc.constraints?.mustBeDominatedByBudget) {
        errors.push(`${id}: Oracle effect should require budget dominance`);
      }
      if (desc.effects.includes("Tool") && !desc.constraints?.requiresToolContract) {
        errors.push(`${id}: Tool effect should require tool contract`);
      }
    }

    return { valid: errors.length === 0, errors };
  }

  /**
   * Export registry as JSON for tooling.
   */
  toJSON(): Record<string, PrimitiveDescriptor> {
    return Object.fromEntries(this.descriptors);
  }

  /**
   * Load registry from JSON.
   */
  static fromJSON(data: Record<string, PrimitiveDescriptor>): PrimitiveRegistry {
    const registry = new PrimitiveRegistry();
    for (const desc of Object.values(data)) {
      registry.register(desc);
    }
    return registry;
  }
}

/**
 * Global default registry instance.
 */
export const defaultRegistry = new PrimitiveRegistry();
