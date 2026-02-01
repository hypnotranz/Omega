#!/usr/bin/env node
// Simple test runner for individual chapter demos
import { createRequire } from 'module';
import { fileURLToPath } from 'url';
import { dirname, join } from 'path';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);
const require = createRequire(import.meta.url);

const chapterNum = process.argv[2];
if (!chapterNum) {
  console.error('Usage: node test-chapter.mjs <chapter-number>');
  console.error('Example: node test-chapter.mjs 16');
  process.exit(1);
}

console.log(`Testing Chapter ${chapterNum}...`);

// Import demo runner from dist
const { runDemo } = await import('./dist/demo/harness/runner.js');
const { demoConfigs } = await import('./dist/demo/by-chapter/config.js');

// Find matching demo
const chId = `ch${chapterNum}` || `ch${String(chapterNum).padStart(2, '0')}`;
const demoId = Object.keys(demoConfigs).find(k => k.startsWith(chId));

if (!demoId) {
  console.error(`No demo found for chapter ${chapterNum}`);
  console.error(`Available demos: ${Object.keys(demoConfigs).join(', ')}`);
  process.exit(1);
}

const config = demoConfigs[demoId];
console.log(`Running: ${config.name}`);
console.log(`Description: ${config.description}`);

try {
  const result = await runDemo(config, { seed: 42, profile: 'pragmatic' });
  console.log('\n✓ Demo completed successfully');
  console.log('Result:', JSON.stringify(result, null, 2));
  process.exit(0);
} catch (error) {
  console.error('\n✗ Demo failed:', error.message);
  console.error(error.stack);
  process.exit(1);
}
