import { describe, it, expect } from 'vitest';
import { sha256Of, canonicalJson, verifyHash, newId } from '../../../src/core/opr/hash';

describe('sha256Of', () => {
  it('H1: returns sha256-prefixed string', () => {
    const hash = sha256Of({ test: 'data' });
    expect(hash).toMatch(/^sha256:[a-f0-9]{64}$/);
  });

  it('H2: same content produces same hash', () => {
    const hash1 = sha256Of({ a: 1, b: 2 });
    const hash2 = sha256Of({ a: 1, b: 2 });
    expect(hash1).toBe(hash2);
  });

  it('H3: different content produces different hash', () => {
    const hash1 = sha256Of({ a: 1 });
    const hash2 = sha256Of({ a: 2 });
    expect(hash1).not.toBe(hash2);
  });
});

describe('canonicalJson', () => {
  it('H4: sorts keys alphabetically', () => {
    const result = canonicalJson({ z: 1, a: 2, m: 3 });
    expect(result).toBe('{"a":2,"m":3,"z":1}');
  });

  it('H5: handles nested objects', () => {
    const result = canonicalJson({ b: { z: 1, a: 2 }, a: 1 });
    expect(result).toBe('{"a":1,"b":{"a":2,"z":1}}');
  });

  it('H6: omits undefined values', () => {
    const result = canonicalJson({ a: 1, b: undefined });
    expect(result).toBe('{"a":1}');
  });
});

describe('verifyHash', () => {
  it('H7: returns true for matching content', () => {
    const content = { test: 'data' };
    const hash = sha256Of(content);
    expect(verifyHash(content, hash)).toBe(true);
  });

  it('H8: returns false for tampered content', () => {
    const hash = sha256Of({ test: 'original' });
    expect(verifyHash({ test: 'tampered' }, hash)).toBe(false);
  });
});

describe('newId', () => {
  it('H9: generates unique IDs', () => {
    const ids = new Set<string>();
    for (let i = 0; i < 100; i++) {
      ids.add(newId());
    }
    expect(ids.size).toBe(100);
  });

  it('H10: generates hex strings', () => {
    const id = newId();
    expect(id).toMatch(/^[a-f0-9]+$/);
  });
});
