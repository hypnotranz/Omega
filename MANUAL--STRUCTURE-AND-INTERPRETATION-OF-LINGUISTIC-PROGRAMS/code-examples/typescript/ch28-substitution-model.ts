/**
 * =========================================================================
 * CHAPTER 28: The Substitution Model for Semantic Evaluation
 * =========================================================================
 *
 * Quick Reference: docs/USER-MANUAL--00--Quick-Reference.md#28-substitution-model
 * Full Chapter:    docs/USER-MANUAL--28--The-Substitution-Model-For-Semantic-Evaluation.md
 *
 * DEMONSTRATES:
 *   How to trace semantic expression evaluation step-by-step using substitution.
 *   Shows how prompts are built inside-out through function composition.
 * =========================================================================
 */

import { demoFromNumber } from "./specs";
import type { DemoDefinition } from "../harness/types";

export const ch28Demo: DemoDefinition = demoFromNumber(28);
