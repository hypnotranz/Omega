import type { FactStoreVal } from "./types";
import type { Val } from "../eval/values";

function valEquals(a: Val, b: Val): boolean {
  if (a.tag !== b.tag) return false;
  switch (a.tag) {
    case "Num": return (b as any).n === a.n;
    case "Str": return (b as any).s === a.s;
    case "Bool": return (b as any).b === a.b;
    case "Sym": return (b as any).name === a.name;
    case "Unit": return true;
    case "Vector": {
      const bb = b as any;
      if (bb.items.length !== a.items.length) return false;
      for (let i = 0; i < a.items.length; i++) {
        if (!valEquals(a.items[i], bb.items[i])) return false;
      }
      return true;
    }
    default:
      return JSON.stringify(a) === JSON.stringify(b);
  }
}

export function createFactStore(): FactStoreVal {
  return { tag: "FactStore", facts: new Map() };
}

export function asFactStore(v: any): FactStoreVal {
  if (v && v.tag === "FactStore") return v as FactStoreVal;
  throw new Error("expected FactStore");
}

export function assertFact(store: FactStoreVal, key: string, value: Val): FactStoreVal {
  const facts = store.facts as Map<string, Val>;
  if (facts.has(key)) {
    const existing = facts.get(key)!;
    if (!valEquals(existing, value)) {
      throw new Error(`Fact ${key} already exists with different value`);
    }
    return store;
  }
  const newFacts = new Map(facts);
  newFacts.set(key, value);
  return { tag: "FactStore", facts: newFacts };
}

export function queryFact(store: FactStoreVal, key: string): Val | undefined {
  const facts = store.facts as Map<string, Val>;
  return facts.get(key);
}

export function queryFactsByPattern(store: FactStoreVal, pattern: string): Array<[string, Val]> {
  let regex: RegExp;
  try {
    regex = new RegExp(pattern);
  } catch (e) {
    throw new Error(`Invalid regex pattern: ${pattern}`);
  }
  const results: Array<[string, Val]> = [];
  const facts = store.facts as Map<string, Val>;
  for (const [k, v] of facts.entries()) {
    if (regex.test(k)) {
      results.push([k, v]);
    }
  }
  return results;
}
