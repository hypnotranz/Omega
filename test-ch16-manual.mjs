#!/usr/bin/env node
// Manual test for Ch16
import { runDemo } from "./dist/demo/harness/runner.js";
import { chapterConfigs } from "./dist/demo/by-chapter/config.js";
import { createChapterDemo } from "./dist/demo/by-chapter/shared.js";

const config = chapterConfigs["ch16-symbolic-semantic"];
console.log("Config:", config ? "found" : "not found");

const demo = createChapterDemo(config);
console.log("\nRunning demo...");

try {
  const report = await runDemo(demo, "pragmatic", 42);
  console.log("\nReport:", JSON.stringify(report, null, 2));
} catch (error) {
  console.error("\nError:", error.message);
  console.error(error.stack);
}
