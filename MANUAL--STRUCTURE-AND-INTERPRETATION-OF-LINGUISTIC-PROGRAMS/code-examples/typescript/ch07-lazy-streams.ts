/**
 * =========================================================================
 * CHAPTER 7: Lazy Streams
 * =========================================================================
 *
 * Quick Reference: docs/USER-MANUAL--00--Quick-Reference.md#7-lazy-streams
 * Full Chapter:    docs/USER-MANUAL--07--Lazy-Streams.md
 *
 * DEMONSTRATES:
 *   stream-map/filter for on-demand LLM calls.
 * =========================================================================
 */

import { demoFromNumber } from "./specs";
import type { DemoDefinition } from "../harness/types";

export const ch07Demo: DemoDefinition = demoFromNumber(7);
