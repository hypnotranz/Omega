# 021a4-hash: Hash Utilities

> **Scope**: Implement sha256Of(), canonicalJson(), content-addressed hashing
> **Architecture Reference**: [021-OPR-RUNTIME.md](021-OPR-RUNTIME.md)
> **Depends on**: 021-types

## Overview

Deterministic hashing for content-addressed receipts and audit trail integrity.

## File to Create

`src/core/opr/hash.ts`

## Implementation

```typescript
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
```

## Exports

```typescript
export {
  sha256Of,
  newId,
  canonicalJson,
  verifyHash,
  hashToHex,
  hexToHash,
};
```

## Acceptance Criteria

1. [ ] `sha256Of` returns branded Hash type
2. [ ] `canonicalJson` sorts object keys alphabetically
3. [ ] Same content always produces same hash
4. [ ] `verifyHash` validates content against expected hash
5. [ ] `newId` generates unique identifiers
6. [ ] Unit tests pass

## Test Cases

- H1: sha256Of returns sha256-prefixed string
- H2: canonicalJson sorts keys: {b:1, a:2} → {"a":2,"b":1}
- H3: same object with different key order produces same hash
- H4: verifyHash returns true for matching content
- H5: verifyHash returns false for tampered content
- H6: newId generates different IDs on each call
