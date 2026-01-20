import { ProvenanceGraph, type SourceChecker } from "./graph";

let currentGraph: ProvenanceGraph = new ProvenanceGraph();
let currentChecker: SourceChecker = { getSourceHash: () => undefined };

export function getProvenanceGraph(): ProvenanceGraph {
  return currentGraph;
}

export function setProvenanceGraph(graph: ProvenanceGraph): void {
  currentGraph = graph;
}

export function getSourceChecker(): SourceChecker {
  return currentChecker;
}

export function setSourceChecker(checker: SourceChecker): void {
  currentChecker = checker;
}
