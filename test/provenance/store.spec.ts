import { describe, it, expect, afterEach } from "vitest";
import * as fs from "node:fs/promises";
import * as path from "node:path";
import * as os from "node:os";
import { FileProvenanceStore } from "../../src/core/provenance/store/file";
import { ProvenanceGraph } from "../../src/core/provenance/graph";
import { evidenceId, type OracleEvidence } from "../../src/core/provenance/evidence";
import type { StoredReceipt } from "../../src/core/provenance/store/interface";

const tempDirs: string[] = [];

async function makeTempDir(): Promise<string> {
  const dir = await fs.mkdtemp(path.join(os.tmpdir(), "prov-store-"));
  tempDirs.push(dir);
  return dir;
}

afterEach(async () => {
  while (tempDirs.length > 0) {
    const dir = tempDirs.pop();
    if (dir) {
      await fs.rm(dir, { recursive: true, force: true });
    }
  }
});

function receipt(id: string, timestamp: number): StoredReceipt {
  return {
    id,
    timestamp,
    request: { tag: "ReqEval", qexpr: id, envRef: `env-${id}` },
    response: { tag: "RespVal", value: { tag: "Str", s: id } },
  };
}

function oracleEvidence(receiptId: string, sourceHash: string, timestamp = 0): OracleEvidence {
  return { tag: "OracleEvidence", receiptId, sourceHash, timestamp };
}

describe("FileProvenanceStore", () => {
  it("stores and retrieves receipts", async () => {
    const dir = await makeTempDir();
    const store = new FileProvenanceStore(dir);
    const r = receipt("rx-1", 1000);
    await store.storeReceipt(r);
    const loaded = await store.getReceipt("rx-1");
    expect(loaded).toEqual(r);
  });

  it("returns undefined for missing receipts", async () => {
    const dir = await makeTempDir();
    const store = new FileProvenanceStore(dir);
    const loaded = await store.getReceipt("missing");
    expect(loaded).toBeUndefined();
  });

  it("stores and loads provenance graph data", async () => {
    const dir = await makeTempDir();
    const store = new FileProvenanceStore(dir);
    const graph = new ProvenanceGraph();
    const ev = oracleEvidence("rx-1", "sh-1", 1);
    graph.addNode(ev);

    await store.storeGraph(graph.toJSON());
    const loaded = await store.loadGraph();
    expect(loaded).toBeTruthy();
    if (loaded) {
      expect(loaded.nodes).toHaveLength(1);
      expect(loaded.nodes[0].id).toBe(evidenceId(ev));
    }
  });

  it("returns undefined when no graph is stored", async () => {
    const dir = await makeTempDir();
    const store = new FileProvenanceStore(dir);
    const loaded = await store.loadGraph();
    expect(loaded).toBeUndefined();
  });

  it("queries receipts by timestamp window", async () => {
    const dir = await makeTempDir();
    const store = new FileProvenanceStore(dir);
    await store.storeReceipt(receipt("rx-1", 1000));
    await store.storeReceipt(receipt("rx-2", 2000));
    await store.storeReceipt(receipt("rx-3", 3000));

    const results = await store.queryReceipts({ after: 1500 });
    const ids = results.map(r => r.id).sort();
    expect(ids).toEqual(["rx-2", "rx-3"]);

    const none = await store.queryReceipts({ after: 9999 });
    expect(none).toEqual([]);
  });

  it("prunes receipts older than a threshold", async () => {
    const dir = await makeTempDir();
    const store = new FileProvenanceStore(dir);
    await store.storeReceipt(receipt("rx-old", 1000));
    await store.storeReceipt(receipt("rx-new", 6000));

    const removed = await store.pruneOlderThan(5000);
    expect(removed).toBe(1);
    expect(await store.getReceipt("rx-old")).toBeUndefined();
    expect(await store.getReceipt("rx-new")).toBeTruthy();
  });
});
