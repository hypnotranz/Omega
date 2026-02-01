// demo/by-chapter/index.ts
// Barrel export for all chapter demos
import type { DemoDefinition } from "../harness";
import { ch01Demo } from "./ch01-getting-started";
import { ch02Demo } from "./ch02-llm-calls";
import { ch03Demo } from "./ch03-composition";
import { ch04Demo } from "./ch04-higher-order";
import { ch05Demo } from "./ch05-nondeterministic";
import { ch06Demo } from "./ch06-multi-shot";
import { ch07Demo } from "./ch07-lazy-streams";
import { ch08Demo } from "./ch08-debugger";
import { ch09Demo } from "./ch09-agentic-repl";
import { ch10Demo } from "./ch10-api-reference";
import { ch11Demo } from "./ch11-semantic-procedures";
import { ch12Demo } from "./ch12-inference-processes";
import { ch13Demo } from "./ch13-higher-order-inference";
import { ch14Demo } from "./ch14-semantic-data";
import { ch15Demo } from "./ch15-sequences";
import { ch16Demo } from "./ch16-symbolic-semantic";
import { ch17Demo } from "./ch17-multiple-representations";
import { ch18Demo } from "./ch18-generic-semantic";
import { ch19Demo } from "./ch19-conversational-state";
import { ch20Demo } from "./ch20-semantic-environment";
import { ch21Demo } from "./ch21-mutable-semantic";
import { ch22Demo } from "./ch22-concurrent-inference";
import { ch23Demo } from "./ch23-streams-of-inference";
import { ch24Demo } from "./ch24-metacircular";
import { ch25Demo } from "./ch25-lazy-semantic";
import { ch26Demo } from "./ch26-amb-inference";
import { ch27Demo } from "./ch27-logic-programming";

export { ch01Demo, ch02Demo, ch03Demo, ch04Demo, ch05Demo, ch06Demo, ch07Demo, ch08Demo, ch09Demo, ch10Demo, ch11Demo, ch12Demo, ch13Demo, ch14Demo, ch15Demo, ch16Demo, ch17Demo, ch18Demo, ch19Demo, ch20Demo, ch21Demo, ch22Demo, ch23Demo, ch24Demo, ch25Demo, ch26Demo, ch27Demo };

export const chapterDemos: DemoDefinition[] = [ch01Demo, ch02Demo, ch03Demo, ch04Demo, ch05Demo, ch06Demo, ch07Demo, ch08Demo, ch09Demo, ch10Demo, ch11Demo, ch12Demo, ch13Demo, ch14Demo, ch15Demo, ch16Demo, ch17Demo, ch18Demo, ch19Demo, ch20Demo, ch21Demo, ch22Demo, ch23Demo, ch24Demo, ch25Demo, ch26Demo, ch27Demo];
