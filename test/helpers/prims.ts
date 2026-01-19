// test/helpers/prims.ts
// Re-export from production primitives
// This ensures tests use the exact same primitives as production
//
// The original implementation was moved to src/core/prims.ts
// See CLAUDE-JOBS/001-FIX-PRODUCTION-PRIMITIVES.md for details

export { installPrims } from "../../src/core/prims";
