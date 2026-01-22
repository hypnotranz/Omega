import type { SessionEvent } from "./types";

export type RenderOptions = {
  showTime?: boolean;
  showSeq?: boolean;
  maxValueWidth?: number;
  colors?: boolean;
};

const DEFAULT_OPTIONS: Required<RenderOptions> = {
  showTime: false,
  showSeq: true,
  maxValueWidth: 60,
  colors: false,
};

const SEQ_WIDTH = 3;
const SOURCE_WIDTH = 4;
const INDENT_UNIT = "  ";
const LLM_METADATA_BUFFER = 20;
const ELLIPSIS = "...";

export function renderEvent(event: SessionEvent, opts: RenderOptions = {}): string {
  const options = { ...DEFAULT_OPTIONS, ...opts };

  if (event.type === "session") {
    return `=== Session ${event.id} (${event.created}) ===`;
  }

  if (!("seq" in event)) {
    return "";
  }

  const depth = "d" in event && typeof event.d === "number" ? Math.max(0, event.d) : 0;
  const indent = INDENT_UNIT.repeat(depth);
  const maxWidth = Math.max(0, options.maxValueWidth ?? DEFAULT_OPTIONS.maxValueWidth);
  const seqPart = options.showSeq ? `[${String(event.seq).padStart(SEQ_WIDTH, "0")}] ` : "";
  const timePart = options.showTime && "ts" in event ? `${new Date(event.ts).toISOString()} ` : "";

  const parts = renderParts(event, maxWidth);
  if (!parts) {
    return "";
  }

  const { source, symbol, content } = parts;

  return `${seqPart}${timePart}${indent}${source} ${symbol} ${content}`;
}

export function renderTrace(events: SessionEvent[], opts: RenderOptions = {}): string {
  return events
    .map((event) => renderEvent(event, opts))
    .filter((line) => line.length > 0)
    .join("\n");
}

type RenderParts = {
  source: string;
  symbol: string;
  content: string;
};

function renderParts(event: SessionEvent, maxWidth: number): RenderParts | undefined {
  switch (event.type) {
    case "input":
      return {
        source: formatTag("REPL"),
        symbol: ">",
        content: truncate(event.code, maxWidth),
      };

    case "step":
      return {
        source: formatTag("EVAL"),
        symbol: "~",
        content: truncate(event.ctrl, maxWidth),
      };

    case "checkpoint":
      return {
        source: formatTag("SAVE"),
        symbol: "*",
        content: truncate(`checkpoint (${event.reason})`, maxWidth),
      };

    case "effect": {
      const args = event.argsPreview ? ` ${event.argsPreview}` : "";
      return {
        source: formatTag("EFCT"),
        symbol: "!",
        content: truncate(`${event.op}${args}`, maxWidth),
      };
    }

    case "llm_req": {
      const promptWidth = Math.max(0, maxWidth - LLM_METADATA_BUFFER);
      return {
        source: formatTag("LLM"),
        symbol: "->",
        content: `${event.model}: ${truncate(event.promptPreview, promptWidth)}`,
      };
    }

    case "llm_resp": {
      const valueWidth = Math.max(0, maxWidth - LLM_METADATA_BUFFER);
      const duration = `${event.durationMs}ms`;
      return {
        source: formatTag("LLM"),
        symbol: "<-",
        content: `${truncate(event.valuePreview, valueWidth)} (${duration})`,
      };
    }

    case "resume":
      return {
        source: formatTag("RSME"),
        symbol: "<~",
        content: truncate(event.valuePreview, maxWidth),
      };

    case "result":
      return {
        source: formatTag("OUT"),
        symbol: "=>",
        content: truncate(event.value, maxWidth),
      };

    case "error":
      return {
        source: formatTag("ERR"),
        symbol: "!!",
        content: truncate(event.message, maxWidth),
      };

    default:
      return undefined;
  }
}

function formatTag(tag: string): string {
  return tag.padEnd(SOURCE_WIDTH, " ").slice(0, SOURCE_WIDTH);
}

function truncate(value: string, max: number): string {
  if (max <= ELLIPSIS.length) return value.slice(0, max);
  if (value.length <= max) return value;
  return `${value.slice(0, max - ELLIPSIS.length)}${ELLIPSIS}`;
}
