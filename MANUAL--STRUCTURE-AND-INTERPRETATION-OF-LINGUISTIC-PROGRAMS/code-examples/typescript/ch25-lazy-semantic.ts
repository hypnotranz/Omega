/**
 * =========================================================================
 * CHAPTER 25: Lazy Semantic Evaluation
 * =========================================================================
 *
 * Quick Reference: docs/USER-MANUAL--00--Quick-Reference.md#25-lazy-semantic-evaluation
 * Full Chapter:    docs/USER-MANUAL--25--Lazy-Semantic-Evaluation.md
 *
 * DEMONSTRATES:
 *   Memoized thunks and reuse.
 * =========================================================================
 */

import { demoFromNumber } from "./specs";
import type { DemoDefinition } from "../harness/types";

export const ch25Demo: DemoDefinition = demoFromNumber(25);
