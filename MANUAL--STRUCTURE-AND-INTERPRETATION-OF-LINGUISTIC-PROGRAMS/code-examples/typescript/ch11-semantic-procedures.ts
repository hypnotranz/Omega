/**
 * =========================================================================
 * CHAPTER 11: Semantic Procedures as Black Boxes
 * =========================================================================
 *
 * Quick Reference: docs/USER-MANUAL--00--Quick-Reference.md#11-semantic-procedures-as-black-boxes
 * Full Chapter:    docs/USER-MANUAL--11--Semantic-Procedures-As-Black-Boxes.md
 *
 * DEMONSTRATES:
 *   LLM predicates hide inner judgment.
 * =========================================================================
 */

import { demoFromNumber } from "./specs";
import type { DemoDefinition } from "../harness/types";

export const ch11Demo: DemoDefinition = demoFromNumber(11);
