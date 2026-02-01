// test/demo/by-chapter.spec.ts
// Validates the Quick Reference and per-chapter demos

import { describe, it, expect, vi } from "vitest";
import { existsSync, readFileSync, readdirSync } from "fs";
import path from "path";
import { fileURLToPath } from "url";
import type { DemoDefinition } from "../../demo/harness";
import { runDemo } from "../../demo/harness";

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const rootDir = path.resolve(__dirname, "..", "..");
const docsDir = path.join(rootDir, "docs");
const quickRefPath = path.join(docsDir, "USER-MANUAL--00--Quick-Reference.md");
const demoDir = path.join(rootDir, "demo", "by-chapter");

type ChapterMeta = {
  num: number;
  id: string;
  file: string;
  exportName: string;
};

const chapters: ChapterMeta[] = [
  { num: 1, id: "ch01-getting-started", file: "ch01-getting-started.ts", exportName: "ch01Demo" },
  { num: 2, id: "ch02-llm-calls", file: "ch02-llm-calls.ts", exportName: "ch02Demo" },
  { num: 3, id: "ch03-composition", file: "ch03-composition.ts", exportName: "ch03Demo" },
  { num: 4, id: "ch04-higher-order", file: "ch04-higher-order.ts", exportName: "ch04Demo" },
  { num: 5, id: "ch05-nondeterministic", file: "ch05-nondeterministic.ts", exportName: "ch05Demo" },
  { num: 6, id: "ch06-multi-shot", file: "ch06-multi-shot.ts", exportName: "ch06Demo" },
  { num: 7, id: "ch07-lazy-streams", file: "ch07-lazy-streams.ts", exportName: "ch07Demo" },
  { num: 8, id: "ch08-debugger", file: "ch08-debugger.ts", exportName: "ch08Demo" },
  { num: 9, id: "ch09-agentic-repl", file: "ch09-agentic-repl.ts", exportName: "ch09Demo" },
  { num: 10, id: "ch10-api-reference", file: "ch10-api-reference.ts", exportName: "ch10Demo" },
  { num: 11, id: "ch11-semantic-procedures", file: "ch11-semantic-procedures.ts", exportName: "ch11Demo" },
  { num: 12, id: "ch12-inference-processes", file: "ch12-inference-processes.ts", exportName: "ch12Demo" },
  { num: 13, id: "ch13-higher-order-inference", file: "ch13-higher-order-inference.ts", exportName: "ch13Demo" },
  { num: 14, id: "ch14-semantic-data", file: "ch14-semantic-data.ts", exportName: "ch14Demo" },
  { num: 15, id: "ch15-sequences", file: "ch15-sequences.ts", exportName: "ch15Demo" },
  { num: 16, id: "ch16-symbolic-semantic", file: "ch16-symbolic-semantic.ts", exportName: "ch16Demo" },
  { num: 17, id: "ch17-multiple-representations", file: "ch17-multiple-representations.ts", exportName: "ch17Demo" },
  { num: 18, id: "ch18-generic-semantic", file: "ch18-generic-semantic.ts", exportName: "ch18Demo" },
  { num: 19, id: "ch19-conversational-state", file: "ch19-conversational-state.ts", exportName: "ch19Demo" },
  { num: 20, id: "ch20-semantic-environment", file: "ch20-semantic-environment.ts", exportName: "ch20Demo" },
  { num: 21, id: "ch21-mutable-semantic", file: "ch21-mutable-semantic.ts", exportName: "ch21Demo" },
  { num: 22, id: "ch22-concurrent-inference", file: "ch22-concurrent-inference.ts", exportName: "ch22Demo" },
  { num: 23, id: "ch23-streams-of-inference", file: "ch23-streams-of-inference.ts", exportName: "ch23Demo" },
  { num: 24, id: "ch24-metacircular", file: "ch24-metacircular.ts", exportName: "ch24Demo" },
  { num: 25, id: "ch25-lazy-semantic", file: "ch25-lazy-semantic.ts", exportName: "ch25Demo" },
  { num: 26, id: "ch26-amb-inference", file: "ch26-amb-inference.ts", exportName: "ch26Demo" },
  { num: 27, id: "ch27-logic-programming", file: "ch27-logic-programming.ts", exportName: "ch27Demo" },
];

function docFileForChapter(num: number): string | undefined {
  const prefix = `USER-MANUAL--${String(num).padStart(2, "0")}--`;
  return readdirSync(docsDir).find(name => name.startsWith(prefix));
}

describe("Quick Reference", () => {
  it("exists and lists all 27 chapters with links to docs and demos", () => {
    expect(existsSync(quickRefPath)).toBe(true);

    const content = readFileSync(quickRefPath, "utf8");
    const headings = Array.from(content.matchAll(/^##\s+(\d+)\./gm)).map(m => Number(m[1]));
    expect(headings).toHaveLength(chapters.length);
    expect(new Set(headings).size).toBe(chapters.length);

    for (const ch of chapters) {
      expect(content).toMatch(new RegExp(`^##\\s+${ch.num}\\.`, "m"));

      const docFile = docFileForChapter(ch.num);
      expect(docFile, `Missing manual file for chapter ${ch.num}`).toBeDefined();
      if (docFile) {
        expect(content).toContain(`./${docFile}`);
      }

      expect(content).toContain(`../demo/by-chapter/${ch.file}`);
    }
  });
});

describe("Chapter demos", () => {
  it("have files for each chapter", () => {
    expect(existsSync(demoDir)).toBe(true);
    for (const ch of chapters) {
      expect(existsSync(path.join(demoDir, ch.file)), `Missing demo file ${ch.file}`).toBe(true);
    }
  });

  it("are exported via the barrel and include an entry per chapter", async () => {
    const mod = await import("../../demo/by-chapter/index");

    const exportedDemos: DemoDefinition[] = (mod as any).chapterDemos;
    expect(exportedDemos, "chapterDemos export is required").toBeDefined();
    expect(exportedDemos).toHaveLength(chapters.length);

    const ids = exportedDemos.map(d => d.id);
    expect(ids).toEqual(chapters.map(ch => ch.id));

    for (const ch of chapters) {
      expect((mod as any)[ch.exportName], `Missing export ${ch.exportName}`).toBeDefined();
    }
  });

  it("each run successfully through the harness", async () => {
    const mod = await import("../../demo/by-chapter/index");
    const exportedDemos: DemoDefinition[] = (mod as any).chapterDemos;

    for (const demo of exportedDemos) {
      const report = await runDemo(demo, "pragmatic", 7, { verbose: false });
      const allPassed = report.invariants.every(i => i.ok);
      expect(allPassed).toBe(true);
    }
  }, 180_000);
});
