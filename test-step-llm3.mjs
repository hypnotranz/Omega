#!/usr/bin/env node
/**
 * Test script to verify Step button calls LLM correctly
 * With race condition fix
 */

import puppeteer from 'puppeteer';
import { spawn } from 'child_process';
import { setTimeout } from 'timers/promises';

const PORT = 3470; // Fresh port

async function startServer() {
  console.log('🚀 Starting debug server on port', PORT);

  const server = spawn('npx', ['tsx', 'start-test-server.ts'], {
    cwd: process.cwd(),
    env: { ...process.env, PORT: String(PORT) },
    stdio: ['pipe', 'pipe', 'pipe'],
    shell: true
  });

  let serverOutput = '';

  server.stdout.on('data', (data) => {
    const text = data.toString();
    serverOutput += text;
    // Log key events
    if (text.includes('EFFECT') || text.includes('LLM') || text.includes('Lock') || text.includes('BLOCKED')) {
      console.log('[SERVER]', text.trim());
    }
  });

  server.stderr.on('data', (data) => {
    console.log('[SERVER ERR]', data.toString().trim());
  });

  // Wait for server to be ready
  for (let i = 0; i < 30; i++) {
    await setTimeout(1000);
    if (serverOutput.includes('Server started') || serverOutput.includes('running at')) {
      break;
    }
  }

  console.log('✅ Server is ready');
  return server;
}

async function runTest() {
  let server;
  let browser;

  try {
    server = await startServer();
    await setTimeout(2000);

    console.log('\n🌐 Launching browser...');
    browser = await puppeteer.launch({
      headless: true,
      args: ['--no-sandbox']
    });

    const page = await browser.newPage();
    await page.goto(`http://localhost:${PORT}`, { waitUntil: 'networkidle0' });
    await setTimeout(1000);

    // Click Translate example
    console.log('🔘 Clicking Translate example...');
    await page.click('[data-example="translate"]');
    await setTimeout(2000);

    // Run steps - wait for each step to complete properly
    console.log('\n▶️ Stepping through code (max 60 steps)...');

    for (let i = 0; i < 60; i++) {
      // Check if step button is disabled (LLM in progress)
      const btnDisabled = await page.$eval('#btn-step', btn => btn.disabled);
      if (btnDisabled) {
        console.log(`  Button disabled (LLM in progress), waiting...`);
        // Wait for button to be re-enabled
        for (let j = 0; j < 60; j++) {
          await setTimeout(500);
          const stillDisabled = await page.$eval('#btn-step', btn => btn.disabled);
          if (!stillDisabled) {
            console.log(`  Button re-enabled after ${j*0.5}s`);
            break;
          }
        }
      }

      // Click step and wait for response
      await page.click('#btn-step');
      await setTimeout(400);

      const step = await page.$eval('#step-count', el => el.textContent);
      const status = await page.$eval('#status-badge', el => el.textContent);

      if (i % 5 === 0 || status !== 'paused') {
        console.log(`Step ${step}: status=${status}`);
      }

      if (status === 'effect') {
        console.log('\n🔶 HIT EFFECT! Button should be disabled while LLM runs...');
        // Wait for LLM to complete (button will be re-enabled)
        for (let j = 0; j < 60; j++) {
          await setTimeout(500);
          const stillDisabled = await page.$eval('#btn-step', btn => btn.disabled);
          const newStatus = await page.$eval('#status-badge', el => el.textContent);
          if (!stillDisabled && newStatus !== 'effect') {
            console.log(`LLM complete after ${j*0.5}s, status now: ${newStatus}`);
            break;
          }
        }
        // Continue stepping to reach the final result
        continue;
      }

      if (status === 'done') {
        console.log('\n🏁 DONE!');
        const items = await page.$$eval('.exec-final-value', els => els.map(el => el.textContent));
        console.log('Final values:', items);

        // Check for LLM results in execution flow
        const llmResults = await page.$$eval('.exec-llm-result', els => els.map(el => el.textContent));
        if (llmResults.length > 0) {
          console.log('LLM results:', llmResults);
        }
        break;
      }
    }

    await setTimeout(3000);
    console.log('\n✅ Test complete');

  } catch (error) {
    console.error('❌ Test failed:', error);
  } finally {
    if (browser) await browser.close();
    if (server) server.kill();
  }
}

runTest();
