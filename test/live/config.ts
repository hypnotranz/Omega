// test/live/config.ts
// Configuration for live LLM tests
//
// Usage:
//   npm test                           # skips live tests (default)
//   RUN_LIVE_TESTS=true npm test       # runs live tests with real LLM
//   npm run test:live                  # convenience script for live tests

import * as fs from "fs";
import * as path from "path";

function loadApiKey(): string | undefined {
  try {
    const configPath = path.join(__dirname, "../../../LambdaRLM/config.yaml");
    const content = fs.readFileSync(configPath, "utf8");
    const match = content.match(/api_key:\s*(\S+)/);
    return match?.[1];
  } catch {
    return process.env.OPENAI_API_KEY;
  }
}

export const OPENAI_API_KEY = loadApiKey();
export const hasApiKey = !!OPENAI_API_KEY;

// Live tests only run when BOTH conditions are met:
// 1. RUN_LIVE_TESTS=true is set
// 2. API key is available
export const runLive = process.env.RUN_LIVE_TESTS === "true" && hasApiKey;

// For manual single-test runs
export const runLiveManual = hasApiKey;
