// SOURCE: ARCHITECTURE/32-LANGUAGE-OFFICIAL-13-IMPLEMENTATION-8.md
// AUTO-EXTRACTED - Do not edit directly. Edit the source document.

export type DepGraph = Map<string, string[]>; // module -> deps

export function topoOrScc(graph: DepGraph): { tag: "Topo"; order: string[] } | { tag: "Cycle"; scc: string[] } {
  const index = new Map<string, number>();
  const lowlink = new Map<string, number>();
  const onStack = new Set<string>();
  const stack: string[] = [];
  let idx = 0;
  const sccs: string[][] = [];

  function strongconnect(v: string) {
    index.set(v, idx);
    lowlink.set(v, idx);
    idx++;
    stack.push(v);
    onStack.add(v);

    for (const w of graph.get(v) ?? []) {
      if (!index.has(w)) {
        strongconnect(w);
        lowlink.set(v, Math.min(lowlink.get(v)!, lowlink.get(w)!));
      } else if (onStack.has(w)) {
        lowlink.set(v, Math.min(lowlink.get(v)!, index.get(w)!));
      }
    }

    if (lowlink.get(v) === index.get(v)) {
      const scc: string[] = [];
      while (true) {
        const w = stack.pop()!;
        onStack.delete(w);
        scc.push(w);
        if (w === v) break;
      }
      sccs.push(scc);
    }
  }

  for (const v of graph.keys()) {
    if (!index.has(v)) strongconnect(v);
  }

  // if any SCC has size > 1 (or self-loop), it's a cycle
  for (const scc of sccs) {
    if (scc.length > 1) return { tag: "Cycle", scc };
    const v = scc[0];
    if ((graph.get(v) ?? []).includes(v)) return { tag: "Cycle", scc };
  }

  // If no cycles, a topo order can be obtained by reverse postorder via SCC list; simplest:
  // Here sccs is already in reverse topo of SCC DAG in Tarjan; flatten reversed:
  const order = sccs.flat().reverse();
  return { tag: "Topo", order };
}