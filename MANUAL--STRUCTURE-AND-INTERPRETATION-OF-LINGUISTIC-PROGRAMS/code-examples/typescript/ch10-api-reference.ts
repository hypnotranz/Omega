/**
 * =========================================================================
 * CHAPTER 10: Full API Reference
 * =========================================================================
 *
 * Quick Reference: docs/USER-MANUAL--00--Quick-Reference.md#10-full-api-reference
 * Full Chapter:    docs/USER-MANUAL--10--Full-Api-Reference.md
 *
 * DEMONSTRATES:
 *   Mixed use of infer, observe, apply, test.
 * =========================================================================
 */

import { demoFromNumber } from "./specs";
import type { DemoDefinition } from "../harness/types";

export const ch10Demo: DemoDefinition = demoFromNumber(10);
