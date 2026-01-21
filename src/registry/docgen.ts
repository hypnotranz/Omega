import { PrimitiveRegistry } from "./registry";
import type { PrimitiveDescriptor } from "./types";

/**
 * Generate markdown documentation from registry.
 */
export function generateMarkdown(registry: PrimitiveRegistry): string {
  const lines: string[] = [];

  lines.push("# Generated Primitive Reference\n");
  lines.push(`Generated: ${new Date().toISOString()}\n`);
  lines.push(`Total primitives: ${registry.getAll().length}\n`);
  lines.push("---\n");

  const layers = ["FrameLisp", "LambdaLLM", "OmegaLLM", "LambdaRLM"];

  for (const layer of layers) {
    const descriptors = registry.getByLayer(layer);
    if (descriptors.length === 0) continue;

    lines.push(`## ${layer}\n`);

    const byKind = new Map<string, PrimitiveDescriptor[]>();
    for (const d of descriptors) {
      if (!byKind.has(d.kind)) byKind.set(d.kind, []);
      byKind.get(d.kind)!.push(d);
    }

    for (const [kind, descs] of byKind) {
      lines.push(`### ${kind}s\n`);
      lines.push("| Function | Signature | Effects | Description |");
      lines.push("|----------|-----------|---------|-------------|");

      for (const d of descs.sort((a, b) => a.id.localeCompare(b.id))) {
        const name = d.id.split("/")[1];
        const sig = formatSignature(d.signature);
        const effects = d.effects.join(", ") || "Pure";
        const desc = d.doc.summary;
        lines.push(`| \`${name}\` | \`${sig}\` | ${effects} | ${desc} |`);
      }

      lines.push("");
    }
  }

  return lines.join("\n");
}

function formatSignature(sig: { params: Array<{ name: string; type: string; optional?: boolean }>; returns: string }): string {
  const params = sig.params
    .map(p => p.optional ? `${p.name}?` : p.name)
    .join(", ");
  return `(${params}) -> ${sig.returns}`;
}

/**
 * Generate JSON API reference.
 */
export function generateJSON(registry: PrimitiveRegistry): string {
  return JSON.stringify(registry.toJSON(), null, 2);
}
