/**
 * =========================================================================
 * CHAPTER 1: Getting Started
 * =========================================================================
 *
 * Quick Reference: docs/USER-MANUAL--00--Quick-Reference.md#1-getting-started
 * Full Chapter:    docs/USER-MANUAL--01--Getting-Started.md
 *
 * DEMONSTRATES:
 *   Warm-up REPL steps with simple definitions and evaluation.
 * =========================================================================
 */

import { demoFromNumber } from "./specs";
import type { DemoDefinition } from "../harness/types";

export const ch01Demo: DemoDefinition = demoFromNumber(1);
