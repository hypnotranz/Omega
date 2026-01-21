/**
 * =========================================================================
 * CHAPTER 4: Higher-Order LLM Functions
 * =========================================================================
 *
 * Quick Reference: docs/USER-MANUAL--00--Quick-Reference.md#4-higher-order-llm-functions
 * Full Chapter:    docs/USER-MANUAL--04--Higher-Order-Llm-Functions.md
 *
 * DEMONSTRATES:
 *   Factories returning LLM-backed functions.
 * =========================================================================
 */

import { demoFromNumber } from "./specs";
import type { DemoDefinition } from "../harness/types";

export const ch04Demo: DemoDefinition = demoFromNumber(4);
