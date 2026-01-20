// src/core/eval/env.ts
// SOURCE: ARCHITECTURE/32-LANGUAGE-OFFICIAL-13-IMPLEMENTATION-17.md
// Env is Ctx.

import type { StoreAddr } from "./store";
import type { Ctx } from "../ctx/ctx";
import { ctxLookup, ctxDefine, ctxExtend, ctxRootFromProfile } from "../ctx/ctx";
import { DEFAULT_PROFILE } from "../governance/profile";

/**
 * Env *is* Ctx in runtime, but we allow any here for compatibility with tests and stubs.
 */
export type Env = any;

export function envEmpty(): Env {
  return ctxRootFromProfile(DEFAULT_PROFILE);
}

export function envSet(env: Env, name: string, addr: StoreAddr): Env {
  return ctxDefine(env, name, addr);
}

export function envGet(env: Env, name: string): StoreAddr | undefined {
  return ctxLookup(env, name);
}

export function envExtend(env: Env, binds: Array<[string, StoreAddr]>): Env {
  return ctxExtend(env, binds);
}
