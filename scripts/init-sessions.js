#!/usr/bin/env node
/**
 * Initialize example sessions on first build
 * Creates .omega-session/sessions/ directory and sample session
 */

import { mkdirSync, writeFileSync, existsSync } from 'fs';
import { join, dirname } from 'path';
import { fileURLToPath } from 'url';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);
const projectRoot = join(__dirname, '..');

const sessionsDir = join(projectRoot, '.omega-session', 'sessions');
const exampleSessionPath = join(sessionsDir, 'getting-started.jsonl');

// Create sessions directory if it doesn't exist
if (!existsSync(sessionsDir)) {
  mkdirSync(sessionsDir, { recursive: true });
  console.log('✓ Created .omega-session/sessions/ directory');
}

// Create example session if it doesn't exist
if (!existsSync(exampleSessionPath)) {
  const exampleSession = `{"type":"session","version":1,"id":"getting-started","created":"2026-01-31T00:00:00.000Z"}
{"seq":0,"ts":1738368000000,"type":"input","code":"(define greeting \\"Hello, OmegaLLM!\\")"}
{"seq":1,"ts":1738368000001,"type":"step","d":0,"ctrl":"(define greeting \\"Hello, OmegaLLM!\\")"}
{"seq":2,"ts":1738368000020,"type":"result","value":"greeting"}
{"seq":3,"ts":1738368000027,"type":"checkpoint","d":0,"reason":"manual","stateId":"state-3"}
{"seq":4,"ts":1738368000100,"type":"input","code":"(define (double x) (* x 2))"}
{"seq":5,"ts":1738368000101,"type":"step","d":0,"ctrl":"(define (double x) (* x 2))"}
{"seq":6,"ts":1738368000120,"type":"result","value":"double"}
{"seq":7,"ts":1738368000127,"type":"checkpoint","d":0,"reason":"manual","stateId":"state-7"}
{"seq":8,"ts":1738368000200,"type":"input","code":"(double 21)"}
{"seq":9,"ts":1738368000201,"type":"step","d":0,"ctrl":"(double 21)"}
{"seq":10,"ts":1738368000220,"type":"result","value":"42"}
{"seq":11,"ts":1738368000227,"type":"checkpoint","d":0,"reason":"manual","stateId":"state-11"}
`;

  writeFileSync(exampleSessionPath, exampleSession);
  console.log('✓ Created example session: getting-started.jsonl');
  console.log('\n  Try it:');
  console.log('    npm run omega-fast');
  console.log('    Omega> :session load getting-started');
  console.log('    Omega> :session goto 11\n');
}
