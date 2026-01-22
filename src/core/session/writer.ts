import * as fs from "fs";
import * as path from "path";
import { serializeState } from "./serializer";
import type { SessionEvent, SessionIndex, CheckpointIndex } from "./types";
import type { State } from "../eval/machine";
import { sha256JSON } from "../artifacts/hash";

const PREVIEW_LIMIT = 200;

export class SessionWriter {
  private sessionId: string;
  private sessionDir: string;
  private eventFile: string;
  private indexFile: string;

  private seq = 0;
  private depth = 0;
  private checkpoints: CheckpointIndex[] = [];
  private states: Record<string, any> = {};
  private receipts: Record<string, any> = {};
  private byteOffset = 0;

  constructor(sessionDir: string, sessionId?: string) {
    this.sessionDir = sessionDir;
    this.sessionId = sessionId || `session-${Date.now().toString(36)}`;

    // Ensure directories exist before writing any files.
    fs.mkdirSync(path.join(sessionDir, "sessions"), { recursive: true });
    fs.mkdirSync(path.join(sessionDir, "receipts"), { recursive: true });

    this.eventFile = path.join(sessionDir, "sessions", `${this.sessionId}.jsonl`);
    this.indexFile = path.join(sessionDir, "sessions", `${this.sessionId}.index.json`);

    // Start a fresh event log for this session.
    fs.writeFileSync(this.eventFile, "");

    // Write session header as the first event.
    this.writeEvent({
      type: "session",
      version: 1,
      id: this.sessionId,
      created: new Date().toISOString(),
    });
  }

  private writeEvent(event: SessionEvent): void {
    const line = `${JSON.stringify(event)}\n`;
    fs.appendFileSync(this.eventFile, line);
    this.byteOffset += Buffer.byteLength(line, "utf8");
    if ("seq" in event) {
      this.seq = event.seq + 1;
    }
  }

  private saveIndex(): void {
    const index: SessionIndex = {
      sessionId: this.sessionId,
      eventCount: this.seq,
      checkpoints: this.checkpoints,
      states: this.states,
      receipts: this.receipts,
    };
    fs.writeFileSync(this.indexFile, JSON.stringify(index, null, 2));
  }

  getSessionId(): string {
    return this.sessionId;
  }

  getSeq(): number {
    return this.seq;
  }

  pushDepth(): void {
    this.depth++;
  }

  popDepth(): void {
    this.depth = Math.max(0, this.depth - 1);
  }

  input(code: string): void {
    this.writeEvent({
      seq: this.seq,
      ts: Date.now(),
      type: "input",
      code,
    });
  }

  step(controlSummary: string): void {
    this.writeEvent({
      seq: this.seq,
      ts: Date.now(),
      type: "step",
      d: this.depth,
      ctrl: controlSummary,
    });
  }

  checkpoint(state: State, reason: "llm_boundary" | "periodic" | "manual" | "effect"): void {
    const stateId = `state-${this.seq}`;
    const serialized = serializeState(state);

    this.states[stateId] = serialized;

    this.checkpoints.push({
      seq: this.seq,
      byteOffset: this.byteOffset,
      stateId,
      reason,
    });

    this.writeEvent({
      seq: this.seq,
      ts: Date.now(),
      type: "checkpoint",
      d: this.depth,
      reason,
      stateId,
    });

    this.saveIndex();
  }

  effect(op: string, args: any[]): void {
    this.writeEvent({
      seq: this.seq,
      ts: Date.now(),
      type: "effect",
      d: this.depth,
      op,
      argsPreview: this.preview(args),
    });
  }

  llmRequest(model: string, prompt: string, fullRequest: any): string {
    const receiptKey = sha256JSON(fullRequest);

    this.writeEvent({
      seq: this.seq,
      ts: Date.now(),
      type: "llm_req",
      d: this.depth,
      model,
      promptPreview: this.preview(prompt),
      receiptKey,
    });

    return receiptKey;
  }

  llmResponse(receiptKey: string, value: string, fullResponse: any, durationMs: number, tokens?: number): void {
    this.receipts[receiptKey] = {
      key: receiptKey,
      request: fullResponse?.request,
      response: fullResponse?.response,
      timestamp: Date.now(),
      durationMs,
      tokens,
    };

    this.writeEvent({
      seq: this.seq,
      ts: Date.now(),
      type: "llm_resp",
      d: this.depth,
      valuePreview: this.preview(value),
      tokens,
      durationMs,
      receiptKey,
    });

    this.saveIndex();
  }

  resume(value: string): void {
    this.writeEvent({
      seq: this.seq,
      ts: Date.now(),
      type: "resume",
      d: this.depth,
      valuePreview: this.preview(value),
    });
  }

  result(value: string): void {
    this.writeEvent({
      seq: this.seq,
      ts: Date.now(),
      type: "result",
      value,
    });
    this.saveIndex();
  }

  error(message: string, stack?: string): void {
    this.writeEvent({
      seq: this.seq,
      ts: Date.now(),
      type: "error",
      d: this.depth,
      message,
      stack,
    });
    this.saveIndex();
  }

  close(): void {
    this.saveIndex();
  }

  private preview(value: any): string {
    if (typeof value === "string") {
      return value.slice(0, PREVIEW_LIMIT);
    }
    try {
      return JSON.stringify(value).slice(0, PREVIEW_LIMIT);
    } catch {
      return String(value).slice(0, PREVIEW_LIMIT);
    }
  }
}
