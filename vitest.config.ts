// vitest.config.ts
// Configuration for vitest test runner

import { defineConfig } from "vitest/config";
import { loadEnv } from "vite";
import type { Reporter, File, TaskResultPack } from "vitest";

// Custom minimal reporter - outputs only "All tests passed" or list of failures
class MinimalReporter implements Reporter {
  private failures: string[] = [];

  onTaskUpdate(packs: TaskResultPack[]) {
    for (const pack of packs) {
      const [id, result] = pack;
      if (result?.state === "fail" && result.errors) {
        for (const error of result.errors) {
          this.failures.push(error.message || "Unknown error");
        }
      }
    }
  }

  onFinished(files?: File[]) {
    if (this.failures.length === 0) {
      console.log("\n✓ All tests passed\n");
    } else {
      console.log("\n✗ Test failures:\n");
      for (const failure of this.failures) {
        console.log(`  - ${failure.split("\n")[0]}`);
      }
      console.log("");
    }
  }
}

export default defineConfig(({ mode }) => {
  // Load .env files - this makes them available to tests
  const env = loadEnv(mode, process.cwd(), "");

  return {
    test: {
      // Make environment variables available
      env,
      // Increase timeout for live API tests
      testTimeout: 120_000,
      // Run tests in parallel by default
      pool: "threads",
      // Include all test files (legacy .spec and new .test under tests/)
      include: ["test/**/*.spec.ts", "tests/**/*.test.ts"],
      // Use our minimal reporter by default
      reporters: [new MinimalReporter()],
    },
  };
});
