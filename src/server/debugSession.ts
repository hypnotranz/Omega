/**
 * Debug Session - Wrapper around CESK machine for web debugging
 *
 * Manages a single debug session with:
 * - Code loading and compilation
 * - Step execution with breakpoint checking
 * - State inspection
 * - History recording for time-travel
 */

import type {
  SessionConfig,
  MachineSnapshot,
  StepResult,
  Breakpoint,
  SerializedValue,
  SerializedFrame,
  SerializedStoreEntry,
} from './debugService';
import { serializeState, serializeValue, serializeFrame, extractBindings } from './stateSerializer';
import { COWStore } from '../core/eval/store';
import { stepOnce } from '../core/eval/machineStep';
import { installPrims } from '../core/prims';
import { compileTextToExpr } from '../core/pipeline/compileText';
import type { State, StepOutcome, Frame } from '../core/eval/machine';
import type { Val } from '../core/eval/values';
import { envGet } from '../core/eval/env';

// OPR Effect handling
import { isOprEffect, handleOprEffect } from '../core/opr/effectHandler';
import { valToJson, jsonToVal } from '../core/opr/bridge';
import type { OprLLMAdapter } from '../core/opr/adapters/types';
import { getKernel } from '../core/opr/kernels';

// ============================================================
// LLM EFFECT DETECTION
// ============================================================

/**
 * Check if an effect is an LLM-related effect.
 *
 * Two main patterns:
 * 1. Direct inference: (effect infer.op (list "prompt..." data))
 *    - infer.op, int.op, rewrite.op, search.op
 *    - The prompt is embedded directly in the Lisp code
 *
 * 2. OPR kernels: (effect "opr.step.opr.classify.v1" data)
 *    - Structured kernels with predefined prompts
 *    - Used for classification, planning, etc.
 */
function isLLMEffect(op: string): boolean {
  return (
    // Direct inference effects - the main LLM call pattern
    // (effect infer.op (list "What is the sentiment of: " text))
    op === 'infer.op' ||
    op === 'int.op' ||
    op === 'rewrite.op' ||
    op === 'search.op' ||
    op === 'oracle.apply.op' ||
    // OPR kernel effects
    isOprEffect(op)
  );
}

/**
 * Check if this is a direct inference effect (prompt in code)
 */
function isDirectInferenceEffect(op: string): boolean {
  return op === 'infer.op' || op === 'int.op' || op === 'rewrite.op' || op === 'search.op';
}

// LLM Adapters
import { OpenAIOprAdapter } from '../core/opr/adapters/openai';
import { AnthropicOprAdapter } from '../core/opr/adapters/anthropic';
import { ScriptedOprAdapter } from '../core/opr/adapters/scripted';

// Config loading (same as tests)
import * as fs from 'fs';
import * as path from 'path';
import { fileURLToPath } from 'url';

// ESM __dirname equivalent
const __filename_local = fileURLToPath(import.meta.url);
const __dirname_local = path.dirname(__filename_local);

function loadApiKey(): string | undefined {
  console.log('\n🔑 LOADING API KEY...');
  console.log(`   Current working directory: ${process.cwd()}`);
  console.log(`   __dirname_local: ${__dirname_local}`);

  // Try multiple possible config file locations
  const possiblePaths = [
    // Absolute fallback - most reliable on Windows
    'c:/Users/Richa/parmenides-dev/agent-harness/LambdaRLM/config.yaml',
    'C:\\Users\\Richa\\parmenides-dev\\agent-harness\\LambdaRLM\\config.yaml',
    // From src/server (development)
    path.join(__dirname_local, '../../../LambdaRLM/config.yaml'),
    // From dist/server (compiled)
    path.join(__dirname_local, '../../../../LambdaRLM/config.yaml'),
    // From OmegaLLM root
    path.join(process.cwd(), '../LambdaRLM/config.yaml'),
    // Relative to cwd
    path.join(process.cwd(), '../../LambdaRLM/config.yaml'),
  ];

  for (const configPath of possiblePaths) {
    try {
      console.log(`   Trying: ${configPath}`);
      if (fs.existsSync(configPath)) {
        const content = fs.readFileSync(configPath, 'utf8');
        console.log(`   ✓ File exists, content length: ${content.length}`);
        const match = content.match(/api_key:\s*(\S+)/);
        if (match?.[1]) {
          const key = match[1];
          console.log(`   ✓ Found API key: ${key.slice(0, 10)}...${key.slice(-4)}`);
          return key;
        } else {
          console.log(`   ✗ No api_key found in file`);
        }
      }
    } catch (e) {
      console.log(`   ✗ Error: ${(e as Error).message}`);
    }
  }

  console.log('   ✗ Could not find config file in any location');

  // Try environment variable
  if (process.env.OPENAI_API_KEY) {
    console.log('   ✓ Using API key from OPENAI_API_KEY env var');
    return process.env.OPENAI_API_KEY;
  }

  if (process.env.ANTHROPIC_API_KEY) {
    console.log('   ✓ Using API key from ANTHROPIC_API_KEY env var');
    return process.env.ANTHROPIC_API_KEY;
  }

  console.log('   ❌ NO API KEY FOUND! LLM features will be disabled.');
  return undefined;
}

// ============================================================
// LLM INSPECTOR TYPES
// ============================================================

export interface PendingLLMRequest {
  /** The effect operation name */
  op: string;
  /** The kernel ID being called */
  kernelId: string;
  /** The model being used */
  model: string;
  /** System prompt (if any) */
  systemPrompt: string | null;
  /** User content being sent */
  userContent: string;
  /** Raw program data from Lisp */
  program: unknown;
  /** Current state (if any) */
  state: unknown | null;
  /** The raw args from the effect call */
  rawArgs: unknown[];
  /** Timestamp when the request was created */
  timestamp: string;
}

export interface LLMResponse {
  /** Raw response text from LLM */
  rawResponse: string;
  /** Parsed JSON result */
  parsedResult: unknown;
  /** Token usage */
  usage?: { promptTokens: number; completionTokens: number; totalTokens: number };
  /** Duration in ms */
  duration: number;
}

// ============================================================
// HISTORY RECORD
// ============================================================

interface HistoryRecord {
  step: number;
  state: State;
  controlSummary: string;
  timestamp: string;
}

// ============================================================
// DEBUG SESSION
// ============================================================

let sessionCounter = 0;

export class DebugSession {
  readonly id: string;
  readonly config: SessionConfig;

  private state: State | null = null;
  private baseStore: COWStore;
  private baseEnv: any;

  private _stepCount = 0;
  private _status: 'idle' | 'running' | 'paused' | 'done' | 'error' | 'effect' = 'idle';
  private _lastOutcome: StepOutcome | null = null;
  private _error: Error | null = null;

  // Breakpoints
  private breakpoints: Breakpoint[] = [];
  private nextBreakpointId = 1;

  // History for time-travel
  private history: HistoryRecord[] = [];
  private maxHistory = 1000;

  // Execution control
  private paused = false;

  // LLM Adapter for auto-executing effects
  private llmAdapter: OprLLMAdapter | null = null;

  // Effect execution log (for UI display)
  private effectLog: Array<{ step: number; op: string; result: any; duration: number }> = [];

  // LLM Inspector mode - pause before LLM calls to show the request
  private _llmInspectMode = false;
  private _pendingLLMRequest: PendingLLMRequest | null = null;

  // Lock to prevent concurrent stepAsync calls (race condition fix)
  private _stepAsyncInProgress = false;

  constructor(config: SessionConfig = {}) {
    this.id = `session_${++sessionCounter}_${Date.now().toString(36)}`;
    this.config = config;

    console.log(`\n🟢🟢🟢 CREATING NEW DEBUG SESSION: ${this.id} 🟢🟢🟢`);

    // Initialize base environment with primitives
    this.baseStore = new COWStore();
    const prim = installPrims(this.baseStore);
    this.baseEnv = prim.env;
    this.baseStore = prim.store as COWStore;

    // Initialize LLM adapter if configured
    this.initializeLLMAdapter();

    console.log(`🟢 Session ${this.id} created. LLM adapter: ${this.llmAdapter ? 'YES ✓' : 'NO ✗'}`);
  }

  private initializeLLMAdapter(): void {
    console.log('\n🤖 INITIALIZING LLM ADAPTER...');
    const { llm } = this.config;

    // If explicit config provided, use that
    if (llm) {
      console.log('   Using explicit config:', llm.provider);
      const apiKey = llm.apiKey || loadApiKey() || '';

      if (!apiKey) {
        console.log('   ❌ No API key provided in config or found in files!');
        return;
      }

      switch (llm.provider) {
        case 'openai':
          this.llmAdapter = new OpenAIOprAdapter({
            apiKey,
            model: llm.model || 'gpt-4o-mini',
          });
          console.log(`   ✓ OpenAI adapter created: ${llm.model || 'gpt-4o-mini'}`);
          break;

        case 'anthropic':
          this.llmAdapter = new AnthropicOprAdapter({
            apiKey,
            model: llm.model || 'claude-3-haiku-20240307',
          });
          console.log(`   ✓ Anthropic adapter created: ${llm.model || 'claude-3-haiku'}`);
          break;

        case 'scripted':
          this.llmAdapter = new ScriptedOprAdapter({
            responses: llm.scriptedResponses || [],
          });
          console.log('   ✓ Scripted adapter created');
          break;
      }
      return;
    }

    // No explicit config - try to auto-detect from config file or env
    console.log('   No explicit config, auto-detecting...');
    const apiKey = loadApiKey();
    if (apiKey) {
      // Default to OpenAI
      this.llmAdapter = new OpenAIOprAdapter({
        apiKey,
        model: 'gpt-4o-mini',
      });
      console.log('   ✓ Auto-configured OpenAI adapter (gpt-4o-mini)');
      console.log(`   ✓ LLM ADAPTER IS READY!`);
    } else {
      console.log('   ❌ NO API KEY FOUND - LLM FEATURES DISABLED!');
      console.log('   ❌ Steps will NOT call LLM. Effects will return () instead.');
    }
  }

  /** Check if LLM is available for auto-executing effects */
  hasLLM(): boolean {
    return this.llmAdapter !== null;
  }

  /** Get the effect execution log */
  getEffectLog(): typeof this.effectLog {
    return [...this.effectLog];
  }

  // ─────────────────────────────────────────────────────────────
  // LLM INSPECTOR MODE
  // ─────────────────────────────────────────────────────────────

  /** Enable/disable LLM inspect mode - pauses before LLM calls to show the request */
  setLLMInspectMode(enabled: boolean): void {
    this._llmInspectMode = enabled;
    console.log(`[DebugSession] LLM inspect mode: ${enabled ? 'enabled' : 'disabled'}`);
  }

  /** Check if LLM inspect mode is enabled */
  isLLMInspectMode(): boolean {
    return this._llmInspectMode;
  }

  /** Get the pending LLM request (if in inspect mode and paused on effect) */
  getPendingLLMRequest(): PendingLLMRequest | null {
    return this._pendingLLMRequest;
  }

  /** Clear the pending LLM request */
  clearPendingLLMRequest(): void {
    this._pendingLLMRequest = null;
  }

  /** Get the current LLM model name */
  getLLMModel(): string {
    if (!this.llmAdapter) return 'none';
    return this.llmAdapter.getModel();
  }

  get stepCount(): number {
    return this._stepCount;
  }

  get status(): string {
    return this._status;
  }

  // ─────────────────────────────────────────────────────────────
  // CODE LOADING
  // ─────────────────────────────────────────────────────────────

  loadCode(code: string): { success: boolean; error?: string } {
    try {
      // Ensure there's a newline before the closing paren so trailing comments
      // don't swallow it (comments extend to end of line)
      const wrapped = `(begin ${code}\n)`;
      const expr = compileTextToExpr(wrapped);

      this.state = {
        control: { tag: 'Expr', e: expr },
        env: this.baseEnv,
        store: this.baseStore.snapshot(),
        kont: [],
        handlers: [],
      };

      this._stepCount = 0;
      this._status = 'paused';
      this._lastOutcome = null;
      this._error = null;
      this.history = [];

      // Record initial state
      this.recordHistory();

      return { success: true };
    } catch (e) {
      this._status = 'error';
      this._error = e as Error;
      return { success: false, error: (e as Error).message };
    }
  }

  // ─────────────────────────────────────────────────────────────
  // STEPPING
  // ─────────────────────────────────────────────────────────────

  step(): StepResult {
    if (!this.state) {
      return {
        snapshot: this.getSnapshot(),
        outcome: 'error',
      };
    }

    if (this._status === 'done' || this._status === 'error') {
      return {
        snapshot: this.getSnapshot(),
        outcome: this._status === 'done' ? 'done' : 'error',
      };
    }

    try {
      const outcome = stepOnce(this.state);
      this._lastOutcome = outcome;
      this._stepCount++;

      switch (outcome.tag) {
        case 'State':
          this.state = outcome.state;
          this._status = 'paused';
          this.recordHistory();
          return {
            snapshot: this.getSnapshot(),
            outcome: this.checkBreakpoints() ?? 'stepped',
            breakpointId: this.getHitBreakpoint(),
          };

        case 'Done':
          this.state = outcome.state;
          this._status = 'done';
          this.recordHistory();
          return {
            snapshot: this.getSnapshot(),
            outcome: 'done',
          };

        case 'Op':
          this.state = outcome.state;
          this._status = 'effect';
          this.recordHistory();
          return {
            snapshot: this.getSnapshot(),
            outcome: 'effect',
          };
      }
    } catch (e) {
      this._status = 'error';
      this._error = e as Error;
      return {
        snapshot: this.getSnapshot(),
        outcome: 'error',
      };
    }
  }

  /**
   * Build PendingLLMRequest from an LLM effect.
   *
   * Handles two patterns:
   * 1. Direct inference (infer.op, int.op, etc.):
   *    The prompt is in the Lisp code: (effect infer.op (list "What is..." text))
   *
   * 2. OPR kernels (opr.step.*, opr.fixpoint.*):
   *    Uses predefined kernel prompts from the kernel registry
   */
  private buildPendingLLMRequest(op: string, args: Val[]): PendingLLMRequest {
    // Convert args to JSON for display
    const rawArgs = args.map(a => valToJson(a));
    const payload = args.length > 0 ? valToJson(args[0]) : null;

    // ─────────────────────────────────────────────────────────────
    // PATTERN 1: Direct inference (infer.op, int.op, etc.)
    // The prompt is embedded directly in the Lisp code
    // ─────────────────────────────────────────────────────────────
    if (isDirectInferenceEffect(op)) {
      // For infer.op, the argument IS the prompt
      // e.g., (effect infer.op (list "What is the sentiment of: " text))
      // The payload could be a string, list, or other structure

      let promptText: string;

      if (typeof payload === 'string') {
        promptText = payload;
      } else if (Array.isArray(payload)) {
        // Join list elements into readable prompt
        promptText = payload.map(p => {
          if (typeof p === 'string') return p;
          if (p && typeof p === 'object') return JSON.stringify(p);
          return String(p);
        }).join('');
      } else if (payload && typeof payload === 'object') {
        promptText = JSON.stringify(payload, null, 2);
      } else {
        promptText = String(payload ?? '(empty)');
      }

      return {
        op,
        kernelId: op,  // For infer.op, the "kernel" is just the op name
        model: this.llmAdapter?.getModel() || 'unknown',
        systemPrompt: null,  // Direct inference typically has no system prompt
        userContent: promptText,  // The prompt from the Lisp code
        program: payload,
        state: null,
        rawArgs,
        timestamp: new Date().toISOString(),
      };
    }

    // ─────────────────────────────────────────────────────────────
    // PATTERN 2: OPR kernels (opr.step.*, opr.fixpoint.*)
    // Structured kernels with predefined prompts
    // ─────────────────────────────────────────────────────────────
    const match = op.match(/^opr\.(step|fixpoint)\.(.+)$/);
    const kernelId = match?.[2] || op;

    // Extract program and state from args
    let program: unknown = {};
    let state: unknown = null;

    if (args.length > 0) {
      program = valToJson(args[0]);
    }
    if (args.length > 1) {
      state = valToJson(args[1]);
    }

    // Get the ACTUAL kernel prompt
    const kernel = getKernel(kernelId);
    let systemPrompt: string | null = null;

    if (kernel?.prompt?.parts) {
      for (const part of kernel.prompt.parts) {
        if ((part as any).tag === 'PSystem' && (part as any).text) {
          systemPrompt = (part as any).text;
          break;
        }
      }
    }

    if (!systemPrompt) {
      systemPrompt = `[Kernel: ${kernelId}]\nKernel prompt not found.`;
    }

    return {
      op,
      kernelId,
      model: this.llmAdapter?.getModel() || 'unknown',
      systemPrompt,
      userContent: JSON.stringify(program, null, 2),
      program,
      state,
      rawArgs,
      timestamp: new Date().toISOString(),
    };
  }

  /**
   * Approve and execute the pending LLM request
   * Works whether _pendingLLMRequest is set (inspect mode) or not (regular stepping)
   */
  async approvePendingLLM(): Promise<StepResult & { llmResponse?: LLMResponse }> {
    // We need: an LLM adapter and to be paused at an Op (effect)
    if (!this.llmAdapter || !this._lastOutcome || this._lastOutcome.tag !== 'Op') {
      console.log(`[DebugSession] approvePendingLLM: cannot proceed - adapter=${!!this.llmAdapter}, lastOutcome=${this._lastOutcome?.tag}`);
      return {
        snapshot: this.getSnapshot(),
        outcome: 'error',
      };
    }

    // Verify this is an LLM effect
    const opcall = this._lastOutcome.opcall;
    const op = opcall.op;
    if (!isLLMEffect(op)) {
      console.log(`[DebugSession] approvePendingLLM: not an LLM effect: ${op}`);
      return {
        snapshot: this.getSnapshot(),
        outcome: 'error',
      };
    }

    const startTime = Date.now();

    console.log(`[DebugSession] Executing approved LLM request: ${op}`);

    try {
      let jsonResult: unknown;

      // Check if this is a direct inference effect (infer.op, int.op, etc.)
      if (isDirectInferenceEffect(op)) {
        // Handle direct inference - prompt is in the args
        const args = opcall.args;
        let prompt = '';
        if (args.length > 0) {
          const payload = valToJson(args[0]);
          if (typeof payload === 'string') {
            prompt = payload;
          } else if (Array.isArray(payload)) {
            prompt = payload.map(p => typeof p === 'string' ? p : JSON.stringify(p)).join(' ');
          } else {
            prompt = JSON.stringify(payload);
          }
        }

        console.log(`[DebugSession] Direct inference with prompt: ${prompt.slice(0, 100)}...`);

        // Call the LLM adapter directly
        const llmResult = await this.llmAdapter!.complete({
          kernelId: 'direct-inference',
          prompt: { tag: 'PromptDoc', v: 'frameir@1', parts: [] } as any,
          userContent: prompt,
        });

        jsonResult = llmResult;
      } else {
        // OPR effect - use handleOprEffect
        const resultVal = await handleOprEffect(opcall, { adapter: this.llmAdapter });
        jsonResult = valToJson(resultVal);
      }

      const duration = Date.now() - startTime;

      this.effectLog.push({
        step: this._stepCount,
        op,
        result: jsonResult,
        duration,
      });

      console.log(`[DebugSession] LLM call completed in ${duration}ms`);

      // Clear pending request
      this._pendingLLMRequest = null;

      // Resume the continuation with the result
      const stepResult = this.resumeWithValue(jsonResult);

      return {
        ...stepResult,
        llmResponse: {
          rawResponse: JSON.stringify(jsonResult, null, 2),
          parsedResult: jsonResult,
          duration,
        },
      };
    } catch (e) {
      console.error(`[DebugSession] LLM call failed:`, e);
      this._pendingLLMRequest = null;
      return {
        snapshot: this.getSnapshot(),
        outcome: 'error',
      };
    }
  }

  /**
   * Async step that auto-handles OPR effects when LLM is available.
   * This is the "magic" mode that actually calls the LLM!
   *
   * If llmInspectMode is enabled, pauses before LLM calls to allow inspection.
   */
  async stepAsync(): Promise<StepResult> {
    console.log(`\n========== STEP ASYNC [${this._stepCount}] ==========`);
    console.log(`[stepAsync] HAS LLM ADAPTER: ${this.llmAdapter ? 'YES ✓' : 'NO ✗ (THIS IS THE PROBLEM!)'}`);

    // Prevent concurrent stepAsync calls - race condition fix
    // If an LLM call is in progress, block new steps
    if (this._stepAsyncInProgress) {
      console.log(`[stepAsync] ⏳ BLOCKED - another stepAsync is in progress (LLM call pending)`);
      return {
        snapshot: this.getSnapshot(),
        outcome: 'effect', // Return effect to signal UI should wait
      };
    }

    // First, do a normal step
    const result = this.step();

    console.log(`[stepAsync] outcome=${result.outcome}, lastOutcome.tag=${this._lastOutcome?.tag}`);

    // Check if we hit an LLM effect
    const isEffect = result.outcome === 'effect';
    const hasAdapter = !!this.llmAdapter;
    const isOp = this._lastOutcome?.tag === 'Op';

    if (isEffect && isOp) {
      const op = (this._lastOutcome as any).opcall?.op;
      const isLLM = isLLMEffect(op);
      console.log(`🔶 EFFECT HIT: op="${op}", isLLMEffect=${isLLM}, hasAdapter=${hasAdapter}`);

      if (!hasAdapter) {
        console.log(`❌ NO LLM ADAPTER! Cannot call LLM. Effect will return () instead of LLM result.`);
      }
    }

    if (result.outcome === 'effect' && this.llmAdapter && this._lastOutcome?.tag === 'Op') {
      const opcall = this._lastOutcome.opcall;
      const op = opcall.op;
      console.log(`[stepAsync] ALL CONDITIONS MET - entering LLM handling for op: "${op}"`);

      console.log(`[stepAsync] Effect fired: op=${op}, isLLMEffect=${isLLMEffect(op)}, isOprEffect=${isOprEffect(op)}, isDirectInference=${isDirectInferenceEffect(op)}`);

      // Check if this is an LLM effect (either direct inference or OPR kernel)
      console.log(`[stepAsync] Checking isLLMEffect: ${isLLMEffect(op)}`);
      if (isLLMEffect(op)) {
        console.log(`[stepAsync] Inside isLLMEffect block, inspectMode=${this._llmInspectMode}`);
        // If in inspect mode, build the pending request and return without executing
        // This works for BOTH infer.op (prompt in code) and opr.step.* (kernel prompts)
        if (this._llmInspectMode) {
          console.log(`[DebugSession] LLM inspect mode - pausing for approval: ${op}`);
          this._pendingLLMRequest = this.buildPendingLLMRequest(op, opcall.args);

          // Return with effect outcome - UI should check for pending request
          return {
            snapshot: this.getSnapshot(),
            outcome: 'effect',
          };
        }

        // Auto-execute LLM effects
        // Set lock BEFORE making the async LLM call
        this._stepAsyncInProgress = true;
        console.log(`[stepAsync] 🔒 Lock acquired - starting LLM call`);

        console.log(`[stepAsync] Checking isOprEffect=${isOprEffect(op)}, isDirectInference=${isDirectInferenceEffect(op)}`);
        if (isOprEffect(op)) {
          // OPR kernel effects
          console.log(`[DebugSession] Auto-handling OPR effect: ${op}`);
          const startTime = Date.now();

          try {
            const resultVal = await handleOprEffect(opcall, { adapter: this.llmAdapter });
            const jsonResult = valToJson(resultVal);
            this.effectLog.push({
              step: this._stepCount,
              op,
              result: jsonResult,
              duration: Date.now() - startTime,
            });
            console.log(`[DebugSession] OPR effect completed in ${Date.now() - startTime}ms`);
            this._stepAsyncInProgress = false;
            console.log(`[stepAsync] 🔓 Lock released`);
            return this.resumeWithValue(jsonResult);
          } catch (e) {
            console.log(`[DebugSession] OPR effect failed: ${e}`);
            this._stepAsyncInProgress = false;
            console.log(`[stepAsync] 🔓 Lock released (error)`);
            return result;
          }
        } else if (isDirectInferenceEffect(op)) {
          // Direct inference effects (infer.op, etc.)
          console.log(`[DebugSession] Auto-handling direct inference: ${op}`);
          const startTime = Date.now();

          try {
            // Build prompt from args
            const args = opcall.args;
            let prompt = '';
            console.log(`[stepAsync] Building prompt from ${args.length} args`);
            if (args.length > 0) {
              const payload = valToJson(args[0]);
              console.log(`[stepAsync] Payload type: ${typeof payload}, value: ${JSON.stringify(payload).slice(0, 100)}`);
              if (typeof payload === 'string') {
                prompt = payload;
              } else if (Array.isArray(payload)) {
                prompt = payload.map(p => typeof p === 'string' ? p : JSON.stringify(p)).join(' ');
              } else {
                prompt = JSON.stringify(payload);
              }
            }

            console.log(`[DebugSession] Calling LLM with prompt: ${prompt.slice(0, 100)}...`);

            // Call the LLM adapter directly
            // Use userContent for the prompt (OprLLMRequest interface)
            const llmResult = await this.llmAdapter!.complete({
              kernelId: 'direct-inference',
              prompt: { tag: 'PromptDoc', v: 'frameir@1', parts: [] } as any,
              userContent: prompt,
            });

            console.log(`[DebugSession] LLM response: ${(llmResult || '').slice(0, 100)}...`);

            this.effectLog.push({
              step: this._stepCount,
              op,
              result: llmResult,
              duration: Date.now() - startTime,
            });

            this._stepAsyncInProgress = false;
            console.log(`[stepAsync] 🔓 Lock released`);
            return this.resumeWithValue(llmResult);
          } catch (e) {
            console.log(`[DebugSession] Direct inference failed: ${e}`);
            this._stepAsyncInProgress = false;
            console.log(`[stepAsync] 🔓 Lock released (error)`);
            return result;
          }
        } else {
          console.log(`[stepAsync] No handler matched for op: ${op}`);
          this._stepAsyncInProgress = false;
          console.log(`[stepAsync] 🔓 Lock released (no handler)`);
        }
      }
    }

    console.log(`[stepAsync] Returning original result with outcome: ${result.outcome}`);
    return result;
  }

  /**
   * Async continue that auto-handles effects
   */
  async continueAsync(): Promise<StepResult> {
    let result: StepResult = { snapshot: this.getSnapshot(), outcome: 'stepped' };
    const maxSteps = this.config.maxSteps ?? 10000;

    for (let i = 0; i < maxSteps; i++) {
      result = await this.stepAsync();

      // Stop on done, error, or unhandled effect
      if (result.outcome === 'done' || result.outcome === 'error') break;
      if (result.outcome === 'effect') break; // Couldn't auto-handle
      if (result.outcome === 'breakpoint') break;
      if (this.paused) break;
    }

    return result;
  }

  /**
   * Async run to completion (auto-handles effects)
   */
  async runAsync(): Promise<StepResult> {
    return this.continueAsync();
  }

  stepN(n: number): StepResult {
    let result: StepResult = { snapshot: this.getSnapshot(), outcome: 'stepped' };

    for (let i = 0; i < n; i++) {
      result = this.step();
      if (result.outcome !== 'stepped') break;
      if (this.paused) break;
    }

    return result;
  }

  continue(): StepResult {
    let result: StepResult = { snapshot: this.getSnapshot(), outcome: 'stepped' };
    const maxSteps = this.config.maxSteps ?? 10000;

    for (let i = 0; i < maxSteps; i++) {
      result = this.step();
      if (result.outcome !== 'stepped') break;
      if (this.paused) break;
    }

    return result;
  }

  run(maxSteps?: number): StepResult {
    return this.continue();
  }

  pause(): void {
    this.paused = true;
  }

  resumeWithValue(value: unknown): StepResult {
    if (this._status !== 'effect' || !this._lastOutcome || this._lastOutcome.tag !== 'Op') {
      return {
        snapshot: this.getSnapshot(),
        outcome: 'error',
      };
    }

    // Convert JSON value to Val and resume
    const val = jsonToVal(value);

    // Resume the continuation with the provided value
    const opcall = this._lastOutcome.opcall;
    if (opcall.resumption) {
      const newState = opcall.resumption.invoke(val);
      this.state = newState;
      this._status = 'paused';
      return this.step();
    }

    return {
      snapshot: this.getSnapshot(),
      outcome: 'error',
    };
  }

  // ─────────────────────────────────────────────────────────────
  // BREAKPOINTS
  // ─────────────────────────────────────────────────────────────

  addBreakpoint(bp: Omit<Breakpoint, 'id'>): string {
    const id = `bp_${this.nextBreakpointId++}`;
    this.breakpoints.push({ ...bp, id });
    return id;
  }

  removeBreakpoint(breakpointId: string): void {
    this.breakpoints = this.breakpoints.filter(bp => bp.id !== breakpointId);
  }

  listBreakpoints(): Breakpoint[] {
    return [...this.breakpoints];
  }

  toggleBreakpoint(breakpointId: string, enabled: boolean): void {
    const bp = this.breakpoints.find(b => b.id === breakpointId);
    if (bp) bp.enabled = enabled;
  }

  private checkBreakpoints(): 'breakpoint' | null {
    if (!this.state) return null;

    for (const bp of this.breakpoints) {
      if (!bp.enabled) continue;

      switch (bp.type) {
        case 'step':
          // Not applicable for step-based (would be checked externally)
          break;

        case 'expression':
          if (this.state.control.tag === 'Expr' && this.state.control.e.tag === bp.exprType) {
            return 'breakpoint';
          }
          break;

        case 'effect':
          if (this._lastOutcome?.tag === 'Op' && this._lastOutcome.opcall.op === bp.effectOp) {
            return 'breakpoint';
          }
          break;

        case 'binding':
          // Check if a specific variable was just defined
          if (this.state.control.tag === 'Val') {
            const frame = this.state.kont[this.state.kont.length - 1];
            if (frame?.tag === 'KDefine' && frame.name === bp.bindingName) {
              return 'breakpoint';
            }
          }
          break;
      }
    }

    return null;
  }

  private getHitBreakpoint(): string | undefined {
    if (!this.state) return undefined;

    for (const bp of this.breakpoints) {
      if (!bp.enabled) continue;

      switch (bp.type) {
        case 'expression':
          if (this.state.control.tag === 'Expr' && this.state.control.e.tag === bp.exprType) {
            return bp.id;
          }
          break;

        case 'effect':
          if (this._lastOutcome?.tag === 'Op' && this._lastOutcome.opcall.op === bp.effectOp) {
            return bp.id;
          }
          break;

        case 'binding':
          if (this.state.control.tag === 'Val') {
            const frame = this.state.kont[this.state.kont.length - 1];
            if (frame?.tag === 'KDefine' && frame.name === bp.bindingName) {
              return bp.id;
            }
          }
          break;
      }
    }

    return undefined;
  }

  // ─────────────────────────────────────────────────────────────
  // STATE INSPECTION
  // ─────────────────────────────────────────────────────────────

  getSnapshot(): MachineSnapshot {
    if (!this.state) {
      return {
        snapshotId: `empty_${Date.now()}`,
        step: 0,
        timestamp: new Date().toISOString(),
        control: { tag: 'Val', valType: 'Unit', valSummary: '()' },
        environment: [],
        callStack: [],
        handlers: [],
        status: this._status === 'idle' ? 'paused' : this._status,
        error: this._error ? { message: this._error.message, stack: this._error.stack } : undefined,
      };
    }

    const options: any = {};

    if (this._status === 'effect' && this._lastOutcome?.tag === 'Op') {
      options.pendingEffect = {
        op: this._lastOutcome.opcall.op,
        args: this._lastOutcome.opcall.args,
      };
    }

    if (this._status === 'done' && this._lastOutcome?.tag === 'Done') {
      options.result = this._lastOutcome.value;
    }

    if (this._status === 'error' && this._error) {
      options.error = this._error;
    }

    const status = this._status === 'idle' ? 'paused' : this._status;
    return serializeState(this.state, this._stepCount, status, options);
  }

  getBinding(name: string): SerializedValue | null {
    if (!this.state) return null;

    try {
      const addr = envGet(this.state.env, name);
      if (addr !== undefined) {
        const val = this.state.store.read(addr);
        return serializeValue(val, 4);
      }
    } catch {
      // Binding not found
    }

    return null;
  }

  getStoreEntry(address: number): SerializedStoreEntry | null {
    if (!this.state) return null;

    try {
      const val = this.state.store.read(address);
      if (val) {
        return {
          address,
          value: serializeValue(val, 4),
        };
      }
    } catch {
      // Address not found
    }

    return null;
  }

  evaluate(expr: string): { value: SerializedValue } | { error: string } {
    if (!this.state) {
      return { error: 'No active session' };
    }

    try {
      // Compile the expression
      const compiled = compileTextToExpr(expr);

      // Create a temporary state to evaluate in current environment
      const tempState: State = {
        control: { tag: 'Expr', e: compiled },
        env: this.state.env,
        store: this.state.store.snapshot(),
        kont: [],
        handlers: [],
      };

      // Run to completion (with step limit)
      let current = tempState;
      for (let i = 0; i < 1000; i++) {
        const outcome = stepOnce(current);
        if (outcome.tag === 'Done') {
          return { value: serializeValue(outcome.value, 4) };
        } else if (outcome.tag === 'Op') {
          return { error: `Effect encountered: ${outcome.opcall.op}` };
        }
        current = outcome.state;
      }

      return { error: 'Evaluation exceeded step limit' };
    } catch (e) {
      return { error: (e as Error).message };
    }
  }

  getCallStack(): SerializedFrame[] {
    if (!this.state) return [];
    return this.state.kont.map((frame, i) => serializeFrame(frame, i));
  }

  // ─────────────────────────────────────────────────────────────
  // TIME TRAVEL
  // ─────────────────────────────────────────────────────────────

  private recordHistory(): void {
    if (!this.state) return;

    const record: HistoryRecord = {
      step: this._stepCount,
      state: this.cloneState(this.state),
      controlSummary: this.controlSummary(),
      timestamp: new Date().toISOString(),
    };

    this.history.push(record);

    // Trim history if too long
    if (this.history.length > this.maxHistory) {
      this.history = this.history.slice(-this.maxHistory);
    }
  }

  private cloneState(state: State): State {
    // Deep clone the state for history
    return {
      control: state.control,
      env: state.env,
      store: (state.store as COWStore).snapshot(),
      kont: [...state.kont],
      handlers: [...state.handlers],
      profile: state.profile,
      budget: state.budget ? { ...state.budget } : undefined,
      sec: state.sec,
    };
  }

  private controlSummary(): string {
    if (!this.state) return '';

    const ctrl = this.state.control;
    if (ctrl.tag === 'Val') {
      return `Val: ${ctrl.v.tag}`;
    } else {
      return `Expr: ${ctrl.e.tag}`;
    }
  }

  jumpToStep(step: number): MachineSnapshot {
    const record = this.history.find(h => h.step === step);
    if (!record) {
      // If not in history, we can't jump
      return this.getSnapshot();
    }

    // Restore state from history
    this.state = this.cloneState(record.state);
    this._stepCount = record.step;
    this._status = 'paused';
    this._lastOutcome = null;
    this._error = null;

    return this.getSnapshot();
  }

  getHistory(): Array<{ step: number; control: string; timestamp: string }> {
    return this.history.map(h => ({
      step: h.step,
      control: h.controlSummary,
      timestamp: h.timestamp,
    }));
  }
}
