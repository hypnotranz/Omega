/**
 * =========================================================================
 * CHAPTER 30: Tree Recursion with Semantic Branching
 * =========================================================================
 *
 * Quick Reference: docs/USER-MANUAL--00--Quick-Reference.md#30-tree-recursion
 * Full Chapter:    docs/USER-MANUAL--30--Tree-Recursion-With-Semantic-Branching.md
 *
 * DEMONSTRATES:
 *   Tree recursion where the LLM decides branching structure and content.
 *   Exponential exploration with semantic control.
 * =========================================================================
 */

import { demoFromNumber } from "./specs";
import type { DemoDefinition } from "../harness/types";

export const ch30Demo: DemoDefinition = demoFromNumber(30);
