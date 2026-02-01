/**
 * =========================================================================
 * CHAPTER 2: LLM Calls as Functions
 * =========================================================================
 *
 * Quick Reference: docs/USER-MANUAL--00--Quick-Reference.md#2-llm-calls-as-functions
 * Full Chapter:    docs/USER-MANUAL--02--Llm-Calls-As-Functions.md
 *
 * DEMONSTRATES:
 *   Call infer.op inside reusable procedures.
 * =========================================================================
 */

import { demoFromNumber } from "./specs";
import type { DemoDefinition } from "../harness/types";

export const ch02Demo: DemoDefinition = demoFromNumber(2);
