/**
 * =========================================================================
 * CHAPTER 35: Tagged Data With Type Dispatch
 * =========================================================================
 *
 * Quick Reference: docs/USER-MANUAL--00--Quick-Reference.md#35-tagged-data-with-type-dispatch
 * Full Chapter:    docs/USER-MANUAL--35--Tagged-Data-With-Type-Dispatch.md
 *
 * DEMONSTRATES:
 *   Response strategies with tagged dispatch flow.
 * =========================================================================
 */

import { demoFromNumber } from "./specs";
import type { DemoDefinition } from "../harness/types";

export const ch35Demo: DemoDefinition = demoFromNumber(35);
