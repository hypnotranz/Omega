// src/core/meta/env.ts
// SOURCE: ARCHITECTURE/32-LANGUAGE-OFFICIAL-13-IMPLEMENTATION-18.md
// Prompt 18: Meta-circular evaluator - Environment operations

import type {
  Omega0Val,
  Omega0Env,
  Omega0Frame,
} from "./types";
import { makeFrame, emptyEnv } from "./types";

// Re-export emptyEnv for convenience
export { emptyEnv } from "./types";

// ─────────────────────────────────────────────────────────────────
// Environment Operations
// ─────────────────────────────────────────────────────────────────

/**
 * Look up a variable in the environment.
 *
 * Searches from innermost to outermost frame.
 * Throws if variable is not found.
 */
export function lookup(env: Omega0Env, name: string): Omega0Val {
  for (const frame of env) {
    if (frame.has(name)) {
      return frame.get(name)!;
    }
  }
  throw new Error(`Unbound variable: ${name}`);
}

/**
 * Try to look up a variable, returning undefined if not found.
 */
export function tryLookup(env: Omega0Env, name: string): Omega0Val | undefined {
  for (const frame of env) {
    if (frame.has(name)) {
      return frame.get(name);
    }
  }
  return undefined;
}

/**
 * Check if a variable is bound in the environment.
 */
export function isBound(env: Omega0Env, name: string): boolean {
  for (const frame of env) {
    if (frame.has(name)) {
      return true;
    }
  }
  return false;
}

/**
 * Extend the environment with a new frame containing the given bindings.
 *
 * @param vars - Variable names
 * @param vals - Corresponding values
 * @param baseEnv - Environment to extend
 */
export function extendEnv(
  vars: string[],
  vals: Omega0Val[],
  baseEnv: Omega0Env
): Omega0Env {
  const frame = makeFrame();

  // Pair up variables with values
  for (let i = 0; i < vars.length; i++) {
    frame.set(vars[i], vals[i] ?? null);
  }

  // Return new environment with this frame at the front
  return [frame, ...baseEnv];
}

/**
 * Define a variable in the topmost frame of the environment.
 *
 * If the variable already exists in the top frame, it's updated.
 * If the environment is empty, a new frame is created.
 */
export function defineVar(
  env: Omega0Env,
  name: string,
  value: Omega0Val
): Omega0Env {
  if (env.length === 0) {
    // Create a new frame
    const frame = makeFrame();
    frame.set(name, value);
    return [frame];
  }

  // Define in the topmost frame (mutating)
  env[0].set(name, value);
  return env;
}

/**
 * Set a variable's value in the environment.
 *
 * The variable must already exist somewhere in the environment.
 * Throws if the variable is not found.
 */
export function setVar(
  env: Omega0Env,
  name: string,
  value: Omega0Val
): void {
  for (const frame of env) {
    if (frame.has(name)) {
      frame.set(name, value);
      return;
    }
  }
  throw new Error(`Cannot set! unbound variable: ${name}`);
}

/**
 * Create a copy of the environment (shallow copy of frames).
 *
 * This creates new frame objects but shares the values.
 */
export function copyEnv(env: Omega0Env): Omega0Env {
  return env.map(frame => new Map(frame));
}

/**
 * Get all variable names bound in the environment.
 */
export function envNames(env: Omega0Env): string[] {
  const names = new Set<string>();
  for (const frame of env) {
    for (const name of frame.keys()) {
      names.add(name);
    }
  }
  return Array.from(names);
}

/**
 * Get the bindings in the topmost frame.
 */
export function topFrameBindings(env: Omega0Env): Map<string, Omega0Val> {
  if (env.length === 0) {
    return new Map();
  }
  return new Map(env[0]);
}

/**
 * Create an environment snapshot for debugging/REPL.
 */
export function envSnapshot(env: Omega0Env): object {
  return {
    frameCount: env.length,
    bindings: env.map((frame, i) => ({
      frame: i,
      vars: Array.from(frame.keys()),
    })),
  };
}

// ─────────────────────────────────────────────────────────────────
// Environment Reference (for oracle protocol)
// ─────────────────────────────────────────────────────────────────

/** Global store for environment references */
const envStore = new Map<string, Omega0Env>();
let envCounter = 0;

/**
 * Store an environment and return a reference ID.
 */
export function storeEnv(env: Omega0Env): string {
  const id = `env-${envCounter++}`;
  envStore.set(id, env);
  return id;
}

/**
 * Retrieve an environment by reference ID.
 */
export function getEnv(id: string): Omega0Env | undefined {
  return envStore.get(id);
}

/**
 * Clear the environment store (for testing).
 */
export function clearEnvStore(): void {
  envStore.clear();
  envCounter = 0;
}
