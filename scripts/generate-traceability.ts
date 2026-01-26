#!/usr/bin/env npx tsx
/**
 * generate-traceability.ts
 *
 * Automatically generates TRACEABILITY-MATRIX.md from:
 * 1. Primitives defined in src/core/prims.ts
 * 2. Test files in test/
 * 3. Requirements in docs/LAMBDA-LLM--OMEGA-LLM-FEATURES.md
 *
 * Run: npx tsx scripts/generate-traceability.ts
 */

import * as fs from "fs";
import * as path from "path";

interface Primitive {
  name: string;
  line: number;
  file: string;
  category: string;
}

interface TestFile {
  path: string;
  primitives: string[];
}

// Extract primitives from prims.ts
function extractPrimitives(filePath: string): Primitive[] {
  const content = fs.readFileSync(filePath, "utf-8");
  const lines = content.split("\n");
  const primitives: Primitive[] = [];

  let currentCategory = "Unknown";

  for (let i = 0; i < lines.length; i++) {
    const line = lines[i];

    // Track category from comments
    if (line.includes("// ") && line.includes("primitives")) {
      currentCategory = line.replace(/\/\/\s*/, "").replace(/primitives.*/, "").trim();
    }

    // Find def("name", patterns
    const match = line.match(/def\("([^"]+)"/);
    if (match) {
      primitives.push({
        name: match[1],
        line: i + 1,
        file: filePath,
        category: currentCategory,
      });
    }
  }

  return primitives;
}

// Find all test files
function findTestFiles(testDir: string): TestFile[] {
  const testFiles: TestFile[] = [];

  function walk(dir: string) {
    const files = fs.readdirSync(dir);
    for (const file of files) {
      const fullPath = path.join(dir, file);
      const stat = fs.statSync(fullPath);
      if (stat.isDirectory()) {
        walk(fullPath);
      } else if (file.endsWith(".spec.ts")) {
        const content = fs.readFileSync(fullPath, "utf-8");
        // Extract primitive names mentioned in test
        const primitives: string[] = [];
        const defMatches = content.matchAll(/["'`]([a-z][a-z0-9?!*+-><=/]*)["`']/gi);
        for (const match of defMatches) {
          if (match[1].includes("-") || match[1].includes("?") || match[1].includes("!")) {
            primitives.push(match[1]);
          }
        }
        testFiles.push({ path: fullPath, primitives: [...new Set(primitives)] });
      }
    }
  }

  walk(testDir);
  return testFiles;
}

// Find test files that mention a primitive
function findTestsForPrimitive(primitive: string, testFiles: TestFile[]): string[] {
  return testFiles
    .filter(tf => tf.primitives.includes(primitive))
    .map(tf => tf.path.replace(/.*test\//, "test/"));
}

// Categorize primitives
function categorizePrimitives(primitives: Primitive[]): Map<string, Primitive[]> {
  const categories = new Map<string, Primitive[]>();

  for (const prim of primitives) {
    const cat = inferCategory(prim.name);
    if (!categories.has(cat)) {
      categories.set(cat, []);
    }
    categories.get(cat)!.push(prim);
  }

  return categories;
}

function inferCategory(name: string): string {
  if (["+", "-", "*", "/", "=", "<", ">", "<=", ">=", "modulo", "even?"].includes(name)) {
    return "Arithmetic";
  }
  if (["not", "or", "and"].includes(name)) {
    return "Logic";
  }
  if (["unit", "mzero", "mplus", "bind"].includes(name)) {
    return "Monadic";
  }
  if (name.startsWith("call") || name.includes("prompt")) {
    return "Continuations";
  }
  if (["cons", "car", "cdr", "null?", "pair?", "list", "append", "eq?", "equal?", "length", "list-ref", "reverse", "cadr", "caddr", "map", "filter", "fold", "foldr"].includes(name)) {
    return "Lists";
  }
  if (name.startsWith("string")) {
    return "Strings";
  }
  if (name.endsWith("?") && !name.startsWith("stream") && !name.startsWith("machine") && !name.startsWith("dist")) {
    return "Type Predicates";
  }
  if (["compose", "pipe", "partial", "apply", "identity", "constantly", "andmap", "ormap"].includes(name)) {
    return "Higher-Order";
  }
  if (name.includes("promise") || name === "force") {
    return "Promises";
  }
  if (name.startsWith("stream") || name === "the-empty-stream" || name.includes("->stream")) {
    return "Streams";
  }
  if (name.includes("op-table") || name.includes("tag") || name === "contents" || name.includes("generic")) {
    return "Generic Dispatch";
  }
  if (name.includes("coercion") || name === "coerce-value") {
    return "Coercion";
  }
  if (name.includes("rule") || name.includes("rewrite") || name.includes("match-pattern") || name.includes("substitute")) {
    return "Term Rewriting";
  }
  if (name.includes("evidence")) {
    return "Evidence";
  }
  if (name.startsWith("machine")) {
    return "Machine Introspection";
  }
  if (name.startsWith("dist")) {
    return "Distributions";
  }
  return "Other";
}

// Generate markdown
function generateMarkdown(
  primitives: Primitive[],
  testFiles: TestFile[]
): string {
  const categories = categorizePrimitives(primitives);

  let md = `# OmegaLLM Traceability Matrix (Auto-Generated)

> Generated: ${new Date().toISOString()}
> Source: scripts/generate-traceability.ts

## Summary

| Category | Count | Has Tests |
|----------|-------|-----------|
`;

  for (const [cat, prims] of categories) {
    const withTests = prims.filter(p => findTestsForPrimitive(p.name, testFiles).length > 0).length;
    md += `| ${cat} | ${prims.length} | ${withTests}/${prims.length} |\n`;
  }

  md += `\n---\n\n`;

  for (const [cat, prims] of categories) {
    md += `## ${cat} (${prims.length})\n\n`;
    md += `| Primitive | Line | Tests |\n`;
    md += `|-----------|------|-------|\n`;

    for (const prim of prims) {
      const tests = findTestsForPrimitive(prim.name, testFiles);
      const testStr = tests.length > 0 ? tests.slice(0, 2).join(", ") : "❌";
      md += `| \`${prim.name}\` | ${prim.line} | ${testStr} |\n`;
    }

    md += `\n`;
  }

  return md;
}

// Main
function main() {
  console.log("Extracting primitives from prims.ts...");
  const primitives = extractPrimitives("src/core/prims.ts");
  console.log(`Found ${primitives.length} primitives`);

  console.log("Finding test files...");
  const testFiles = findTestFiles("test");
  console.log(`Found ${testFiles.length} test files`);

  console.log("Generating markdown...");
  const markdown = generateMarkdown(primitives, testFiles);

  const outputPath = "docs/TRACEABILITY-MATRIX-GENERATED.md";
  fs.writeFileSync(outputPath, markdown);
  console.log(`Written to ${outputPath}`);
}

main();
