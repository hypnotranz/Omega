/**
 * =========================================================================
 * CHAPTER 29: Iterative Semantic Refinement
 * =========================================================================
 *
 * Quick Reference: docs/USER-MANUAL--00--Quick-Reference.md#29-iterative-refinement
 * Full Chapter:    docs/USER-MANUAL--29--Iterative-Semantic-Refinement.md
 *
 * DEMONSTRATES:
 *   Newton's method adapted to semantic space - iteratively refine responses
 *   until they meet quality criteria judged by LLM.
 * =========================================================================
 */

import { demoFromNumber } from "./specs";
import type { DemoDefinition } from "../harness/types";

export const ch29Demo: DemoDefinition = demoFromNumber(29);
