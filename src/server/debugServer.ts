/**
 * Debug Server - HTTP + WebSocket server for CESK machine debugging
 *
 * Exposes the CESK machine state through:
 * - REST API for session management, stepping, inspection
 * - WebSocket for real-time state updates during execution
 */

import express, { Request, Response, NextFunction } from 'express';
import { WebSocketServer, WebSocket } from 'ws';
import { createServer } from 'http';
import type {
  IDebugService,
  SessionConfig,
  MachineSnapshot,
  StepResult,
  Breakpoint,
  SerializedValue,
  SerializedFrame,
  ServerEvent,
  ClientCommand,
} from './debugService';
import { DebugSession } from './debugSession';
import { listKernels, getKernel } from '../core/opr/kernels';

// ============================================================
// DEBUG SERVER IMPLEMENTATION
// ============================================================

interface SessionInfo {
  session: DebugSession;
  clients: Set<WebSocket>;
}

export class DebugServer implements IDebugService {
  private app: express.Application;
  private server: ReturnType<typeof createServer>;
  private wss: WebSocketServer;
  private sessions = new Map<string, SessionInfo>();
  private port: number;

  constructor(port = 3456) {
    this.port = port;
    this.app = express();
    this.app.use(express.json());
    this.app.use(this.corsMiddleware);

    this.server = createServer(this.app);
    this.wss = new WebSocketServer({ server: this.server, path: '/ws' });

    this.setupRoutes();
    this.setupWebSocket();
  }

  // ─────────────────────────────────────────────────────────────
  // MIDDLEWARE
  // ─────────────────────────────────────────────────────────────

  private corsMiddleware = (_req: Request, res: Response, next: NextFunction) => {
    res.header('Access-Control-Allow-Origin', '*');
    res.header('Access-Control-Allow-Methods', 'GET, POST, PUT, DELETE, OPTIONS');
    res.header('Access-Control-Allow-Headers', 'Content-Type');
    if (_req.method === 'OPTIONS') {
      res.sendStatus(200);
      return;
    }
    next();
  };

  // ─────────────────────────────────────────────────────────────
  // HTTP ROUTES
  // ─────────────────────────────────────────────────────────────

  private setupRoutes() {
    const app = this.app;

    // Health check
    app.get('/health', (_req, res) => {
      res.json({ status: 'ok', sessions: this.sessions.size });
    });

    // ─── Session Management ───
    app.post('/session', async (req, res) => {
      try {
        const config: SessionConfig = req.body ?? {};
        const id = await this.createSession(config);
        res.json({ id });
      } catch (e) {
        res.status(500).json({ error: (e as Error).message });
      }
    });

    app.get('/sessions', async (_req, res) => {
      const sessions = await this.listSessions();
      res.json(sessions);
    });

    app.get('/session/:id', async (req, res) => {
      try {
        const info = await this.getSession(req.params.id);
        res.json(info);
      } catch (e) {
        res.status(404).json({ error: (e as Error).message });
      }
    });

    app.delete('/session/:id', async (req, res) => {
      try {
        await this.closeSession(req.params.id);
        res.json({ success: true });
      } catch (e) {
        res.status(404).json({ error: (e as Error).message });
      }
    });

    // ─── Code Execution ───
    app.post('/session/:id/load', async (req, res) => {
      try {
        const { code } = req.body;
        const result = await this.loadCode(req.params.id, code);
        res.json(result);
      } catch (e) {
        res.status(400).json({ error: (e as Error).message });
      }
    });

    app.post('/session/:id/step', async (req, res) => {
      try {
        const result = await this.step(req.params.id);
        res.json(result);
        this.broadcastToSession(req.params.id, { type: 'snapshot', snapshot: result.snapshot });
      } catch (e) {
        res.status(400).json({ error: (e as Error).message });
      }
    });

    app.post('/session/:id/step/:n', async (req, res) => {
      try {
        const n = parseInt(req.params.n, 10);
        const result = await this.stepN(req.params.id, n);
        res.json(result);
        this.broadcastToSession(req.params.id, { type: 'snapshot', snapshot: result.snapshot });
      } catch (e) {
        res.status(400).json({ error: (e as Error).message });
      }
    });

    app.post('/session/:id/continue', async (req, res) => {
      try {
        const result = await this.continue(req.params.id);
        res.json(result);
        this.broadcastToSession(req.params.id, { type: 'snapshot', snapshot: result.snapshot });
      } catch (e) {
        res.status(400).json({ error: (e as Error).message });
      }
    });

    app.post('/session/:id/run', async (req, res) => {
      try {
        const { maxSteps } = req.body ?? {};
        const result = await this.run(req.params.id, maxSteps);
        res.json(result);
        this.broadcastToSession(req.params.id, { type: 'snapshot', snapshot: result.snapshot });
      } catch (e) {
        res.status(400).json({ error: (e as Error).message });
      }
    });

    app.post('/session/:id/resume', async (req, res) => {
      try {
        const { value } = req.body;
        const result = await this.resumeWithValue(req.params.id, value);
        res.json(result);
        this.broadcastToSession(req.params.id, { type: 'snapshot', snapshot: result.snapshot });
      } catch (e) {
        res.status(400).json({ error: (e as Error).message });
      }
    });

    // ─── State Inspection ───
    app.get('/session/:id/snapshot', async (req, res) => {
      try {
        const snapshot = await this.getSnapshot(req.params.id);
        res.json(snapshot);
      } catch (e) {
        res.status(404).json({ error: (e as Error).message });
      }
    });

    app.get('/session/:id/stack', async (req, res) => {
      try {
        const stack = await this.getCallStack(req.params.id);
        res.json(stack);
      } catch (e) {
        res.status(404).json({ error: (e as Error).message });
      }
    });

    app.get('/session/:id/binding/:name', async (req, res) => {
      try {
        const value = await this.getBinding(req.params.id, req.params.name);
        res.json({ value });
      } catch (e) {
        res.status(404).json({ error: (e as Error).message });
      }
    });

    app.post('/session/:id/eval', async (req, res) => {
      try {
        const { expr } = req.body;
        const result = await this.evaluate(req.params.id, expr);
        res.json(result);
      } catch (e) {
        res.status(400).json({ error: (e as Error).message });
      }
    });

    // ─── Breakpoints ───
    app.post('/session/:id/breakpoint', async (req, res) => {
      try {
        const bp = req.body;
        const id = await this.addBreakpoint(req.params.id, bp);
        res.json({ id });
      } catch (e) {
        res.status(400).json({ error: (e as Error).message });
      }
    });

    app.delete('/session/:id/breakpoint/:bpId', async (req, res) => {
      try {
        await this.removeBreakpoint(req.params.id, req.params.bpId);
        res.json({ success: true });
      } catch (e) {
        res.status(404).json({ error: (e as Error).message });
      }
    });

    app.get('/session/:id/breakpoints', async (req, res) => {
      try {
        const bps = await this.listBreakpoints(req.params.id);
        res.json(bps);
      } catch (e) {
        res.status(404).json({ error: (e as Error).message });
      }
    });

    // ─── Time Travel ───
    app.post('/session/:id/jump/:step', async (req, res) => {
      try {
        const step = parseInt(req.params.step, 10);
        const snapshot = await this.jumpToStep(req.params.id, step);
        res.json(snapshot);
        this.broadcastToSession(req.params.id, { type: 'snapshot', snapshot });
      } catch (e) {
        res.status(400).json({ error: (e as Error).message });
      }
    });

    app.get('/session/:id/history', async (req, res) => {
      try {
        const history = await this.getHistory(req.params.id);
        res.json(history);
      } catch (e) {
        res.status(404).json({ error: (e as Error).message });
      }
    });

    // ─── OPR ───
    app.get('/kernels', async (_req, res) => {
      const kernels = await this.listKernels();
      res.json(kernels);
    });

    app.post('/kernel/:id/execute', async (req, res) => {
      try {
        const { program } = req.body;
        const result = await this.executeKernel(req.params.id, program);
        res.json(result);
      } catch (e) {
        res.status(400).json({ error: (e as Error).message });
      }
    });

    // ─── Static Files (Web UI) ───
    app.use(express.static('public'));
  }

  // ─────────────────────────────────────────────────────────────
  // WEBSOCKET
  // ─────────────────────────────────────────────────────────────

  private setupWebSocket() {
    this.wss.on('connection', (ws, req) => {
      // Parse session ID from query string: /ws?session=xxx
      const url = new URL(req.url ?? '', `http://${req.headers.host}`);
      const sessionId = url.searchParams.get('session');

      if (!sessionId || !this.sessions.has(sessionId)) {
        ws.close(4404, 'Session not found');
        return;
      }

      const info = this.sessions.get(sessionId)!;
      info.clients.add(ws);

      // Send initial snapshot
      const snapshot = info.session.getSnapshot();
      ws.send(JSON.stringify({ type: 'snapshot', snapshot }));

      // Handle commands
      ws.on('message', async (data) => {
        try {
          const cmd: ClientCommand = JSON.parse(data.toString());
          await this.handleWebSocketCommand(sessionId, cmd, ws);
        } catch (e) {
          ws.send(JSON.stringify({ type: 'error', error: { message: (e as Error).message } }));
        }
      });

      ws.on('close', () => {
        info.clients.delete(ws);
      });
    });
  }

  private async handleWebSocketCommand(sessionId: string, cmd: ClientCommand, ws: WebSocket) {
    let result: StepResult;

    switch (cmd.type) {
      case 'step':
        result = await this.step(sessionId);
        break;
      case 'stepN':
        result = await this.stepN(sessionId, cmd.n);
        break;
      case 'continue':
        result = await this.continue(sessionId);
        break;
      case 'run':
        result = await this.run(sessionId, cmd.maxSteps);
        break;
      case 'resume':
        result = await this.resumeWithValue(sessionId, cmd.value);
        break;
      case 'evaluate':
        const evalResult = await this.evaluate(sessionId, cmd.expr);
        ws.send(JSON.stringify({ type: 'evalResult', ...evalResult }));
        return;
      case 'addBreakpoint':
        const bpId = await this.addBreakpoint(sessionId, cmd.breakpoint);
        ws.send(JSON.stringify({ type: 'breakpointAdded', id: bpId }));
        return;
      case 'removeBreakpoint':
        await this.removeBreakpoint(sessionId, cmd.breakpointId);
        ws.send(JSON.stringify({ type: 'breakpointRemoved', id: cmd.breakpointId }));
        return;
      case 'jumpToStep':
        const snapshot = await this.jumpToStep(sessionId, cmd.step);
        this.broadcastToSession(sessionId, { type: 'snapshot', snapshot });
        return;
      case 'pause':
        // Pause is handled by the session
        const info = this.sessions.get(sessionId);
        if (info) {
          info.session.pause();
        }
        return;
      default:
        throw new Error(`Unknown command: ${(cmd as any).type}`);
    }

    // Broadcast result to all clients
    this.broadcastToSession(sessionId, { type: 'snapshot', snapshot: result.snapshot });

    // Send specific events based on outcome
    if (result.outcome === 'breakpoint') {
      this.broadcastToSession(sessionId, {
        type: 'breakpointHit',
        breakpointId: result.breakpointId!,
        snapshot: result.snapshot,
      });
    } else if (result.outcome === 'effect') {
      this.broadcastToSession(sessionId, {
        type: 'effectPending',
        effect: result.snapshot.pendingEffect!,
        snapshot: result.snapshot,
      });
    } else if (result.outcome === 'done') {
      this.broadcastToSession(sessionId, {
        type: 'done',
        result: result.snapshot.result!,
        snapshot: result.snapshot,
      });
    }
  }

  private broadcastToSession(sessionId: string, event: ServerEvent) {
    const info = this.sessions.get(sessionId);
    if (!info) return;

    const msg = JSON.stringify(event);
    for (const client of info.clients) {
      if (client.readyState === WebSocket.OPEN) {
        client.send(msg);
      }
    }
  }

  // ─────────────────────────────────────────────────────────────
  // IDebugService IMPLEMENTATION
  // ─────────────────────────────────────────────────────────────

  async createSession(config?: SessionConfig): Promise<string> {
    const session = new DebugSession(config);
    const id = session.id;
    this.sessions.set(id, { session, clients: new Set() });
    return id;
  }

  async listSessions(): Promise<Array<{ id: string; name?: string; step: number; status: string }>> {
    return Array.from(this.sessions.entries()).map(([id, info]) => ({
      id,
      name: info.session.config.name,
      step: info.session.stepCount,
      status: info.session.status,
    }));
  }

  async getSession(sessionId: string): Promise<{ id: string; config: SessionConfig; snapshot: MachineSnapshot }> {
    const info = this.sessions.get(sessionId);
    if (!info) throw new Error(`Session not found: ${sessionId}`);
    return {
      id: sessionId,
      config: info.session.config,
      snapshot: info.session.getSnapshot(),
    };
  }

  async closeSession(sessionId: string): Promise<void> {
    const info = this.sessions.get(sessionId);
    if (!info) throw new Error(`Session not found: ${sessionId}`);

    // Close all WebSocket connections
    for (const client of info.clients) {
      client.close(1000, 'Session closed');
    }

    this.sessions.delete(sessionId);
  }

  async loadCode(sessionId: string, code: string): Promise<{ success: boolean; error?: string }> {
    const info = this.sessions.get(sessionId);
    if (!info) throw new Error(`Session not found: ${sessionId}`);
    return info.session.loadCode(code);
  }

  async step(sessionId: string): Promise<StepResult> {
    const info = this.sessions.get(sessionId);
    if (!info) throw new Error(`Session not found: ${sessionId}`);
    return info.session.step();
  }

  async stepN(sessionId: string, n: number): Promise<StepResult> {
    const info = this.sessions.get(sessionId);
    if (!info) throw new Error(`Session not found: ${sessionId}`);
    return info.session.stepN(n);
  }

  async continue(sessionId: string): Promise<StepResult> {
    const info = this.sessions.get(sessionId);
    if (!info) throw new Error(`Session not found: ${sessionId}`);
    return info.session.continue();
  }

  async run(sessionId: string, maxSteps?: number): Promise<StepResult> {
    const info = this.sessions.get(sessionId);
    if (!info) throw new Error(`Session not found: ${sessionId}`);
    return info.session.run(maxSteps);
  }

  async resumeWithValue(sessionId: string, value: unknown): Promise<StepResult> {
    const info = this.sessions.get(sessionId);
    if (!info) throw new Error(`Session not found: ${sessionId}`);
    return info.session.resumeWithValue(value);
  }

  async getSnapshot(sessionId: string): Promise<MachineSnapshot> {
    const info = this.sessions.get(sessionId);
    if (!info) throw new Error(`Session not found: ${sessionId}`);
    return info.session.getSnapshot();
  }

  async getBinding(sessionId: string, name: string): Promise<SerializedValue | null> {
    const info = this.sessions.get(sessionId);
    if (!info) throw new Error(`Session not found: ${sessionId}`);
    return info.session.getBinding(name);
  }

  async getStoreEntry(sessionId: string, address: number): Promise<any> {
    const info = this.sessions.get(sessionId);
    if (!info) throw new Error(`Session not found: ${sessionId}`);
    return info.session.getStoreEntry(address);
  }

  async evaluate(sessionId: string, expr: string): Promise<{ value: SerializedValue } | { error: string }> {
    const info = this.sessions.get(sessionId);
    if (!info) throw new Error(`Session not found: ${sessionId}`);
    return info.session.evaluate(expr);
  }

  async getCallStack(sessionId: string): Promise<SerializedFrame[]> {
    const info = this.sessions.get(sessionId);
    if (!info) throw new Error(`Session not found: ${sessionId}`);
    return info.session.getCallStack();
  }

  async addBreakpoint(sessionId: string, bp: Omit<Breakpoint, 'id'>): Promise<string> {
    const info = this.sessions.get(sessionId);
    if (!info) throw new Error(`Session not found: ${sessionId}`);
    return info.session.addBreakpoint(bp);
  }

  async removeBreakpoint(sessionId: string, breakpointId: string): Promise<void> {
    const info = this.sessions.get(sessionId);
    if (!info) throw new Error(`Session not found: ${sessionId}`);
    return info.session.removeBreakpoint(breakpointId);
  }

  async listBreakpoints(sessionId: string): Promise<Breakpoint[]> {
    const info = this.sessions.get(sessionId);
    if (!info) throw new Error(`Session not found: ${sessionId}`);
    return info.session.listBreakpoints();
  }

  async toggleBreakpoint(sessionId: string, breakpointId: string, enabled: boolean): Promise<void> {
    const info = this.sessions.get(sessionId);
    if (!info) throw new Error(`Session not found: ${sessionId}`);
    return info.session.toggleBreakpoint(breakpointId, enabled);
  }

  async jumpToStep(sessionId: string, step: number): Promise<MachineSnapshot> {
    const info = this.sessions.get(sessionId);
    if (!info) throw new Error(`Session not found: ${sessionId}`);
    return info.session.jumpToStep(step);
  }

  async getHistory(sessionId: string): Promise<Array<{ step: number; control: string; timestamp: string }>> {
    const info = this.sessions.get(sessionId);
    if (!info) throw new Error(`Session not found: ${sessionId}`);
    return info.session.getHistory();
  }

  async listKernels(): Promise<Array<{ id: string; op: string; description?: string }>> {
    const ids = listKernels();
    return ids.map(id => {
      const kernel = getKernel(id);
      return { id, op: kernel?.op ?? 'step' };
    });
  }

  async executeKernel(kernelId: string, program: unknown): Promise<{ ok: boolean; result?: unknown; error?: unknown }> {
    // Delegate to OPR runtime
    const { handleOprEffect } = await import('../core/opr/effectHandler');
    const { OpenAIOprAdapter } = await import('../core/opr/adapters/openai');
    const { jsonToVal, valToJson } = await import('../core/opr/bridge');

    const apiKey = process.env.OPENAI_API_KEY;
    if (!apiKey) {
      return { ok: false, error: 'OPENAI_API_KEY not set' };
    }

    const adapter = new OpenAIOprAdapter({ apiKey, model: 'gpt-4o-mini' });
    const programVal = jsonToVal(program);

    const result = await handleOprEffect(
      { op: `opr.step.${kernelId}`, args: [programVal], ctxDigest: 'api', resumption: null as any },
      { adapter }
    );

    const json = valToJson(result) as any;
    return { ok: json.ok === true, result: json.output, error: json.error };
  }

  // ─────────────────────────────────────────────────────────────
  // SERVER LIFECYCLE
  // ─────────────────────────────────────────────────────────────

  start(): Promise<void> {
    return new Promise((resolve) => {
      this.server.listen(this.port, () => {
        console.log(`\n🔍 OmegaLLM Debug Server running at http://localhost:${this.port}`);
        console.log(`   WebSocket: ws://localhost:${this.port}/ws?session=<id>`);
        console.log(`   API: http://localhost:${this.port}/sessions`);
        console.log(`   Health: http://localhost:${this.port}/health\n`);
        resolve();
      });
    });
  }

  stop(): Promise<void> {
    return new Promise((resolve) => {
      this.wss.close();
      this.server.close(() => resolve());
    });
  }
}

// ─────────────────────────────────────────────────────────────
// CLI ENTRY POINT
// ─────────────────────────────────────────────────────────────

if (require.main === module) {
  const port = parseInt(process.env.DEBUG_PORT ?? '3456', 10);
  const server = new DebugServer(port);
  server.start();

  process.on('SIGINT', async () => {
    console.log('\nShutting down...');
    await server.stop();
    process.exit(0);
  });
}
