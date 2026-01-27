/**
 * OPR Hash Utilities
 *
 * Deterministic hashing for content-addressed receipts and audit trail integrity.
 */

import { createHash, randomBytes } from 'crypto';
import type { Hash } from './types';

/**
 * Compute SHA-256 hash of content
 * Returns branded Hash type: `sha256:${hex}`
 */
export function sha256Of(content: unknown): Hash {
  const canonical = canonicalJson(content);
  const hash = createHash('sha256').update(canonical).digest('hex');
  return `sha256:${hash}` as Hash;
}

/**
 * Generate a new unique ID (for receipt IDs, etc.)
 */
export function newId(): string {
  return randomBytes(16).toString('hex');
}

/**
 * Canonical JSON serialization for deterministic hashing
 * - Sorts object keys alphabetically
 * - No extra whitespace
 * - Handles undefined by omitting keys
 */
export function canonicalJson(value: unknown): string {
  return JSON.stringify(value, (_, v) => {
    if (v && typeof v === 'object' && !Array.isArray(v)) {
      // Sort object keys
      const sorted: Record<string, unknown> = {};
      for (const key of Object.keys(v).sort()) {
        if (v[key] !== undefined) {
          sorted[key] = v[key];
        }
      }
      return sorted;
    }
    return v;
  });
}

/**
 * Verify a hash matches content
 */
export function verifyHash(content: unknown, expectedHash: Hash): boolean {
  const computed = sha256Of(content);
  return computed === expectedHash;
}

/**
 * Extract the hex portion from a Hash
 */
export function hashToHex(hash: Hash): string {
  return hash.slice(7); // Remove "sha256:" prefix
}

/**
 * Create a Hash from a hex string
 */
export function hexToHash(hex: string): Hash {
  return `sha256:${hex}` as Hash;
}
