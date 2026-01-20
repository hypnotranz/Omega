import type { ProvenanceGraphData } from "../graph";

export type ReceiptFilter = {
  before?: number;
  after?: number;
  idPrefix?: string;
};

export type StoredReceipt = {
  id: string;
  timestamp: number;
  request: unknown;
  response: unknown;
  sourceHash?: string;
};

export interface ProvenanceStore {
  storeReceipt(receipt: StoredReceipt): Promise<void>;
  getReceipt(id: string): Promise<StoredReceipt | undefined>;
  queryReceipts(filter?: ReceiptFilter): Promise<StoredReceipt[]>;

  storeGraph(graph: ProvenanceGraphData): Promise<void>;
  loadGraph(): Promise<ProvenanceGraphData | undefined>;

  pruneOlderThan(timestamp: number): Promise<number>;
}

export function matchesFilter(r: StoredReceipt, filter?: ReceiptFilter): boolean {
  if (!filter) return true;
  const ts = r.timestamp ?? 0;
  if (filter.after !== undefined && ts <= filter.after) return false;
  if (filter.before !== undefined && ts >= filter.before) return false;
  if (filter.idPrefix && !r.id.startsWith(filter.idPrefix)) return false;
  return true;
}
