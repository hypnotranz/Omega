// vitest.config.ts
// Configuration for vitest test runner

import { defineConfig } from "vitest/config";
import { loadEnv } from "vite";

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
      // Include all test files
      include: ["test/**/*.spec.ts"],
    },
  };
});
