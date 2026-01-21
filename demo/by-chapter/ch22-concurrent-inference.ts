/**
 * =========================================================================
 * CHAPTER 22: Concurrent Inference
 * =========================================================================
 *
 * Quick Reference: docs/USER-MANUAL--00--Quick-Reference.md#22-concurrent-inference
 * Full Chapter:    docs/USER-MANUAL--22--Concurrent-Inference.md
 *
 * DEMONSTRATES:
 *   Parallel infer.op with fibers.
 * =========================================================================
 */

import { demoFromNumber } from "./specs";
import type { DemoDefinition } from "../harness/types";

export const ch22Demo: DemoDefinition = demoFromNumber(22);
