import * as fs from "node:fs/promises";
import * as path from "node:path";
import type { ProvenanceGraphData } from "../graph";
import { matchesFilter, type ProvenanceStore, type ReceiptFilter, type StoredReceipt } from "./interface";

async function ensureDir(dir: string): Promise<void> {
  await fs.mkdir(dir, { recursive: true });
}

export class FileProvenanceStore implements ProvenanceStore {
  private receiptsDir: string;
  private graphFile: string;

  constructor(private basePath: string) {
    this.receiptsDir = path.join(basePath, "receipts");
    this.graphFile = path.join(basePath, "graph.json");
  }

  async storeReceipt(receipt: StoredReceipt): Promise<void> {
    await ensureDir(this.receiptsDir);
    const file = path.join(this.receiptsDir, `${receipt.id}.json`);
    await fs.writeFile(file, JSON.stringify(receipt, null, 2), "utf8");
  }

  async getReceipt(id: string): Promise<StoredReceipt | undefined> {
    const file = path.join(this.receiptsDir, `${id}.json`);
    try {
      const data = await fs.readFile(file, "utf8");
      return JSON.parse(data) as StoredReceipt;
    } catch (e: any) {
      if (e?.code === "ENOENT") return undefined;
      throw e;
    }
  }

  async queryReceipts(filter?: ReceiptFilter): Promise<StoredReceipt[]> {
    try {
      const entries = await fs.readdir(this.receiptsDir);
      const receipts: StoredReceipt[] = [];
      for (const entry of entries) {
        if (!entry.endsWith(".json")) continue;
        const id = entry.replace(/\.json$/, "");
        const r = await this.getReceipt(id);
        if (r && matchesFilter(r, filter)) {
          receipts.push(r);
        }
      }
      return receipts;
    } catch (e: any) {
      if (e?.code === "ENOENT") return [];
      throw e;
    }
  }

  async storeGraph(graph: ProvenanceGraphData): Promise<void> {
    await ensureDir(this.basePath);
    await fs.writeFile(this.graphFile, JSON.stringify(graph, null, 2), "utf8");
  }

  async loadGraph(): Promise<ProvenanceGraphData | undefined> {
    try {
      const data = await fs.readFile(this.graphFile, "utf8");
      return JSON.parse(data) as ProvenanceGraphData;
    } catch (e: any) {
      if (e?.code === "ENOENT") return undefined;
      throw e;
    }
  }

  async pruneOlderThan(timestamp: number): Promise<number> {
    const receipts = await this.queryReceipts({ before: timestamp });
    let removed = 0;
    for (const r of receipts) {
      const file = path.join(this.receiptsDir, `${r.id}.json`);
      await fs.rm(file, { force: true });
      removed += 1;
    }
    return removed;
  }
}
