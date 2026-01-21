import { describe, it, expect } from "vitest";
import fs from "fs";
import path from "path";
import { fileURLToPath, pathToFileURL } from "url";
import type { DemoDefinition } from "../demo/harness/types";
import { runDemo } from "../demo/harness/runner";

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);
const rootDir = path.resolve(__dirname, "..");

interface ChapterSpec {
  number: number;
  slug: string;
  manualLink: string;
  demoLink: string;
  demoFile: string;
  exportName: string;
}

const chapters: ChapterSpec[] = [
  {
    number: 1,
    slug: "getting-started",
    manualLink: "./USER-MANUAL--01--Getting-Started.md",
    demoLink: "../demo/by-chapter/ch01-getting-started.ts",
    demoFile: "ch01-getting-started.ts",
    exportName: "ch01Demo",
  },
  {
    number: 2,
    slug: "llm-calls-as-functions",
    manualLink: "./USER-MANUAL--02--Llm-Calls-As-Functions.md",
    demoLink: "../demo/by-chapter/ch02-llm-calls.ts",
    demoFile: "ch02-llm-calls.ts",
    exportName: "ch02Demo",
  },
  {
    number: 3,
    slug: "functional-composition",
    manualLink: "./USER-MANUAL--03--Functional-Composition.md",
    demoLink: "../demo/by-chapter/ch03-composition.ts",
    demoFile: "ch03-composition.ts",
    exportName: "ch03Demo",
  },
  {
    number: 4,
    slug: "higher-order-llm-functions",
    manualLink: "./USER-MANUAL--04--Higher-Order-Llm-Functions.md",
    demoLink: "../demo/by-chapter/ch04-higher-order.ts",
    demoFile: "ch04-higher-order.ts",
    exportName: "ch04Demo",
  },
  {
    number: 5,
    slug: "nondeterministic-search",
    manualLink: "./USER-MANUAL--05--Nondeterministic-Search.md",
    demoLink: "../demo/by-chapter/ch05-nondeterministic.ts",
    demoFile: "ch05-nondeterministic.ts",
    exportName: "ch05Demo",
  },
  {
    number: 6,
    slug: "multi-shot-sampling",
    manualLink: "./USER-MANUAL--06--Multi-Shot-Sampling.md",
    demoLink: "../demo/by-chapter/ch06-multi-shot.ts",
    demoFile: "ch06-multi-shot.ts",
    exportName: "ch06Demo",
  },
  {
    number: 7,
    slug: "lazy-streams",
    manualLink: "./USER-MANUAL--07--Lazy-Streams.md",
    demoLink: "../demo/by-chapter/ch07-lazy-streams.ts",
    demoFile: "ch07-lazy-streams.ts",
    exportName: "ch07Demo",
  },
  {
    number: 8,
    slug: "the-debugger",
    manualLink: "./USER-MANUAL--08--The-Debugger.md",
    demoLink: "../demo/by-chapter/ch08-debugger.ts",
    demoFile: "ch08-debugger.ts",
    exportName: "ch08Demo",
  },
  {
    number: 9,
    slug: "the-agentic-repl",
    manualLink: "./USER-MANUAL--09--The-Agentic-Repl.md",
    demoLink: "../demo/by-chapter/ch09-agentic-repl.ts",
    demoFile: "ch09-agentic-repl.ts",
    exportName: "ch09Demo",
  },
  {
    number: 10,
    slug: "full-api-reference",
    manualLink: "./USER-MANUAL--10--Full-Api-Reference.md",
    demoLink: "../demo/by-chapter/ch10-api-reference.ts",
    demoFile: "ch10-api-reference.ts",
    exportName: "ch10Demo",
  },
  {
    number: 11,
    slug: "semantic-procedures-as-black-boxes",
    manualLink: "./USER-MANUAL--11--Semantic-Procedures-As-Black-Boxes.md",
    demoLink: "../demo/by-chapter/ch11-semantic-procedures.ts",
    demoFile: "ch11-semantic-procedures.ts",
    exportName: "ch11Demo",
  },
  {
    number: 12,
    slug: "inference-processes",
    manualLink:
      "./USER-MANUAL--12--Inference-Processes-Recursion-And-Iteration-In-Semantic-Space.md",
    demoLink: "../demo/by-chapter/ch12-inference-processes.ts",
    demoFile: "ch12-inference-processes.ts",
    exportName: "ch12Demo",
  },
  {
    number: 13,
    slug: "higher-order-inference",
    manualLink: "./USER-MANUAL--13--Higher-Order-Inference.md",
    demoLink: "../demo/by-chapter/ch13-higher-order-inference.ts",
    demoFile: "ch13-higher-order-inference.ts",
    exportName: "ch13Demo",
  },
  {
    number: 14,
    slug: "semantic-data-abstraction",
    manualLink: "./USER-MANUAL--14--Semantic-Data-Abstraction.md",
    demoLink: "../demo/by-chapter/ch14-semantic-data.ts",
    demoFile: "ch14-semantic-data.ts",
    exportName: "ch14Demo",
  },
  {
    number: 15,
    slug: "sequences-as-semantic-interfaces",
    manualLink: "./USER-MANUAL--15--Sequences-As-Semantic-Interfaces.md",
    demoLink: "../demo/by-chapter/ch15-sequences.ts",
    demoFile: "ch15-sequences.ts",
    exportName: "ch15Demo",
  },
  {
    number: 16,
    slug: "symbolic-semantic-data",
    manualLink: "./USER-MANUAL--16--Symbolic-Semantic-Data.md",
    demoLink: "../demo/by-chapter/ch16-symbolic-semantic.ts",
    demoFile: "ch16-symbolic-semantic.ts",
    exportName: "ch16Demo",
  },
  {
    number: 17,
    slug: "multiple-representations-of-meaning",
    manualLink: "./USER-MANUAL--17--Multiple-Representations-Of-Meaning.md",
    demoLink: "../demo/by-chapter/ch17-multiple-representations.ts",
    demoFile: "ch17-multiple-representations.ts",
    exportName: "ch17Demo",
  },
  {
    number: 18,
    slug: "generic-semantic-operations",
    manualLink: "./USER-MANUAL--18--Generic-Semantic-Operations.md",
    demoLink: "../demo/by-chapter/ch18-generic-semantic.ts",
    demoFile: "ch18-generic-semantic.ts",
    exportName: "ch18Demo",
  },
  {
    number: 19,
    slug: "conversational-state-and-memory",
    manualLink: "./USER-MANUAL--19--Conversational-State-And-Memory.md",
    demoLink: "../demo/by-chapter/ch19-conversational-state.ts",
    demoFile: "ch19-conversational-state.ts",
    exportName: "ch19Demo",
  },
  {
    number: 20,
    slug: "semantic-environment-model",
    manualLink: "./USER-MANUAL--20--The-Semantic-Environment-Model.md",
    demoLink: "../demo/by-chapter/ch20-semantic-environment.ts",
    demoFile: "ch20-semantic-environment.ts",
    exportName: "ch20Demo",
  },
  {
    number: 21,
    slug: "mutable-semantic-structures",
    manualLink: "./USER-MANUAL--21--Mutable-Semantic-Structures.md",
    demoLink: "../demo/by-chapter/ch21-mutable-semantic.ts",
    demoFile: "ch21-mutable-semantic.ts",
    exportName: "ch21Demo",
  },
  {
    number: 22,
    slug: "concurrent-inference",
    manualLink: "./USER-MANUAL--22--Concurrent-Inference.md",
    demoLink: "../demo/by-chapter/ch22-concurrent-inference.ts",
    demoFile: "ch22-concurrent-inference.ts",
    exportName: "ch22Demo",
  },
  {
    number: 23,
    slug: "streams-of-inference",
    manualLink: "./USER-MANUAL--23--Streams-Of-Inference.md",
    demoLink: "../demo/by-chapter/ch23-streams-of-inference.ts",
    demoFile: "ch23-streams-of-inference.ts",
    exportName: "ch23Demo",
  },
  {
    number: 24,
    slug: "metalinguistic-abstraction",
    manualLink: "./USER-MANUAL--24--Metalinguistic-Abstraction-The-Oracle-In-The-Evaluator.md",
    demoLink: "../demo/by-chapter/ch24-metacircular.ts",
    demoFile: "ch24-metacircular.ts",
    exportName: "ch24Demo",
  },
  {
    number: 25,
    slug: "lazy-semantic-evaluation",
    manualLink: "./USER-MANUAL--25--Lazy-Semantic-Evaluation.md",
    demoLink: "../demo/by-chapter/ch25-lazy-semantic.ts",
    demoFile: "ch25-lazy-semantic.ts",
    exportName: "ch25Demo",
  },
  {
    number: 26,
    slug: "amb-inference-engine",
    manualLink: "./USER-MANUAL--26--The-Amb-Inference-Engine.md",
    demoLink: "../demo/by-chapter/ch26-amb-inference.ts",
    demoFile: "ch26-amb-inference.ts",
    exportName: "ch26Demo",
  },
  {
    number: 27,
    slug: "logic-programming-with-semantic-facts",
    manualLink: "./USER-MANUAL--27--Logic-Programming-With-Semantic-Facts.md",
    demoLink: "../demo/by-chapter/ch27-logic-programming.ts",
    demoFile: "ch27-logic-programming.ts",
    exportName: "ch27Demo",
  },
];

describe("Quick Reference manual", () => {
  it("exists and includes all chapter sections with links", () => {
    const quickRefPath = path.join(
      rootDir,
      "docs",
      "USER-MANUAL--00--Quick-Reference.md"
    );
    expect(fs.existsSync(quickRefPath)).toBe(true);

    const content = fs.readFileSync(quickRefPath, "utf8");
    const headings = content.match(/^## \d+\./gm) ?? [];
    expect(headings.length).toBe(chapters.length);

    for (const chapter of chapters) {
      expect(content).toMatch(new RegExp(`^## ${chapter.number}\\.`,"m"));
      expect(content).toContain(chapter.manualLink);
      expect(content).toContain(chapter.demoLink);
    }
  });
});

describe("Chapter demo barrel", () => {
  it("exports every chapter demo from the index", async () => {
    const indexPath = path.join(rootDir, "demo", "by-chapter", "index.ts");
    expect(fs.existsSync(indexPath)).toBe(true);

    const indexMod = await import(pathToFileURL(indexPath).href);
    for (const chapter of chapters) {
      expect(indexMod[chapter.exportName], `Index missing ${chapter.exportName}`).toBeDefined();
    }
  });
});

describe("Chapter demos", () => {
  it(
    "exist, export demo definitions, and run via harness",
    async () => {
      for (const chapter of chapters) {
        const demoPath = path.join(rootDir, "demo", "by-chapter", chapter.demoFile);
        expect(fs.existsSync(demoPath), `Missing demo file ${chapter.demoFile}`).toBe(true);

        const mod = await import(pathToFileURL(demoPath).href);
        const demoDef: DemoDefinition | undefined = mod[chapter.exportName];
        expect(demoDef, `Missing export ${chapter.exportName}`).toBeDefined();
        expect(typeof demoDef?.run).toBe("function");

        const report = await runDemo(demoDef as DemoDefinition, "pragmatic", 7);
        const invariantsOk = report.invariants.every(i => i.ok);
        if (!invariantsOk) {
          const failures = report.invariants
            .filter(i => !i.ok)
            .map(i => `${i.name}:${i.detail ?? ""}`)
            .join("; ");
          throw new Error(`Demo ${chapter.demoFile} invariants failed: ${failures}`);
        }
      }
    },
    240000
  );
});
