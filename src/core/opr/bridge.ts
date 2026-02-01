/**
 * OPR Bridge: Val <-> JSON conversion for integrating OPR with CEKS machine
 *
 * This is the thin layer that connects Lisp values to OPR's JSON-based
 * kernel interface. The key insight: OPR kernels work with JSON objects,
 * while CEKS works with Val. These functions bridge the gap.
 */

import type { Val } from '../eval/values';

/**
 * Convert a Lisp Val to a JSON-compatible value.
 *
 * This is used to pass Lisp data to OPR kernels.
 *
 * Mapping:
 * - Unit -> null
 * - Num -> number
 * - Bool -> boolean
 * - Str -> string
 * - Sym -> { $sym: name }
 * - Vector (cons-cell list) -> array
 * - Vector (flat) -> array
 * - Closure/Native/etc -> { $type, $opaque: true }
 */
export function valToJson(v: Val): unknown {
  switch (v.tag) {
    case 'Unit':
    case 'Uninit':
      return null;

    case 'Num':
      return v.n;

    case 'Int':
      // BigInt -> number (may lose precision for very large values)
      return Number(v.value);

    case 'Bool':
      return v.b;

    case 'Str':
      return v.s;

    case 'Sym':
      return { $sym: v.name };

    case 'Pair':
      // Lisp pair -> 2-element array
      return [valToJson(v.car), valToJson(v.cdr)];

    case 'List':
      return v.elements.map(valToJson);

    case 'Vector': {
      // Check if it's a cons-cell list: (a . (b . (c . ())))
      // Cons-cells are 2-element vectors where items[1] is another cons or Unit
      if (v.items.length === 2) {
        const second = v.items[1];
        if (second.tag === 'Unit' || (second.tag === 'Vector' && second.items.length === 2)) {
          // Likely a cons-cell list - flatten it
          const arr: unknown[] = [];
          let cur: Val = v;
          while (cur.tag === 'Vector' && cur.items.length === 2) {
            arr.push(valToJson(cur.items[0]));
            cur = cur.items[1];
          }
          // Only return as array if properly terminated with Unit
          if (cur.tag === 'Unit') {
            return arr;
          }
          // Otherwise treat as dotted pair
          arr.push(valToJson(cur));
          return { $dotted: arr };
        }
      }
      // Regular vector/tuple -> array
      return v.items.map(valToJson);
    }

    case 'Map': {
      // Map -> object (keys must be strings)
      const obj: Record<string, unknown> = {};
      for (const [k, val] of v.entries) {
        const key = k.tag === 'Str' ? k.s : k.tag === 'Sym' ? k.name : JSON.stringify(valToJson(k));
        obj[key] = valToJson(val);
      }
      return obj;
    }

    case 'Syntax':
      return { $syntax: JSON.stringify(v.stx) };

    // Opaque values - can't serialize meaningfully
    case 'Closure':
      return { $type: 'Closure', $opaque: true };
    case 'Native':
      return { $type: 'Native', name: v.name, $opaque: true };
    case 'Cont':
      return { $type: 'Cont', $opaque: true };
    case 'Continuation':
      return { $type: 'Continuation', $opaque: true };
    case 'OracleProc':
      return { $type: 'OracleProc', $opaque: true };
    case 'Machine':
      return { $type: 'Machine', machineId: v.machineId, $opaque: true };
    case 'Profile':
      return { $type: 'Profile', profileId: v.profileId, $opaque: true };
    case 'Fiber':
      return { $type: 'Fiber', id: v.id, $opaque: true };
    case 'Promise':
      return { $type: 'Promise', id: v.id, $opaque: true };
    case 'Solver':
      return { $type: 'Solver', name: v.name, $opaque: true };

    // Constraint network values
    case 'ConnRef':
      return { $type: 'ConnRef', id: v.id, netId: v.netId };
    case 'NetRef':
      return { $type: 'NetRef', id: v.id };

    // Explanation and contradiction
    case 'Explanation':
      return { $type: 'Explanation', kind: v.kind };
    case 'Contradiction':
      return { $type: 'Contradiction', constraintId: v.constraintId };

    // Tagged values
    case 'Tagged':
      return { $tagged: v.typeTag, $value: valToJson(v.payload) };

    // Error value
    case 'Err':
      return { $error: v.message ?? 'error' };

    // Condition values
    case 'Condition':
      return { $type: 'Condition', conditionType: String((v as any).type?.description ?? 'condition'), message: (v as any).message };

    // Budget/Result/CostEstimate
    case 'Budget':
      return { $type: 'Budget', tokens: v.tokens, calls: v.calls, time: v.time };
    case 'Result':
      return { $type: 'Result', kind: v.kind, cost: v.cost, reason: v.reason };
    case 'CostEstimate':
      return { $type: 'CostEstimate', minCost: v.minCost, maxCost: v.maxCost };

    // Stream
    case 'Stream':
      return { $type: 'Stream', isEmpty: v.isEmpty };

    // IR
    case 'IR':
      return { $type: 'IR', form: v.form, digest: v.digest };

    // Distribution and Meaning (from oracle layer)
    case 'Dist':
      return { $type: 'Dist' };
    case 'Meaning':
      return { $type: 'Meaning' };

    // Various references
    case 'Ctx':
      return { $type: 'Ctx' };
    case 'Module':
      return { $type: 'Module', moduleId: v.moduleId };
    case 'ReceiptRef':
      return { $type: 'ReceiptRef', rid: v.rid, kind: v.kind };
    case 'Mutex':
      return { $type: 'Mutex', id: v.id };
    case 'IVar':
      return { $type: 'IVar', id: v.id };
    case 'Channel':
      return { $type: 'Channel', id: v.id };
    case 'Actor':
      return { $type: 'Actor', id: v.id };
    case 'GenericRegistry':
      return { $type: 'GenericRegistry', id: v.id };
    case 'GenericMiss':
      return { $type: 'GenericMiss', op: v.op, signature: v.signature };
    case 'FactStore':
      return { $type: 'FactStore' };
    case 'Evaluator':
      return { $type: 'Evaluator', $opaque: true };
    case 'MacroTransformer':
      return { $type: 'MacroTransformer', name: v.name, $opaque: true };

    default: {
      // Exhaustive check - if we get here, we have an unhandled tag
      const _exhaustive: never = v;
      return { $unknown: JSON.stringify(v) };
    }
  }
}

/**
 * Convert a JSON value back to a Lisp Val.
 *
 * This is used to pass OPR results back to Lisp.
 *
 * Mapping:
 * - null/undefined -> Unit
 * - number -> Num
 * - boolean -> Bool
 * - string -> Str
 * - array -> cons-cell list
 * - { $sym: name } -> Sym
 * - object -> alist ((key . val) ...)
 */
export function jsonToVal(j: unknown): Val {
  // Null/undefined -> Unit
  if (j === null || j === undefined) {
    return { tag: 'Unit' };
  }

  // Primitives
  if (typeof j === 'number') {
    return { tag: 'Num', n: j };
  }
  if (typeof j === 'boolean') {
    return { tag: 'Bool', b: j };
  }
  if (typeof j === 'string') {
    return { tag: 'Str', s: j };
  }

  // Arrays -> cons-cell lists
  if (Array.isArray(j)) {
    // Build proper cons-cell list: [a, b, c] -> (a . (b . (c . ())))
    let result: Val = { tag: 'Unit' };
    for (let i = j.length - 1; i >= 0; i--) {
      result = { tag: 'Vector', items: [jsonToVal(j[i]), result] };
    }
    return result;
  }

  // Objects
  if (typeof j === 'object') {
    const obj = j as Record<string, unknown>;

    // Check for special encodings
    if ('$sym' in obj && typeof obj.$sym === 'string') {
      return { tag: 'Sym', name: obj.$sym };
    }

    if ('$error' in obj) {
      return { tag: 'Err', message: String(obj.$error) };
    }

    if ('$tagged' in obj && typeof obj.$tagged === 'string') {
      return {
        tag: 'Tagged',
        typeTag: obj.$tagged,
        payload: jsonToVal(obj.$value),
      };
    }

    if ('$dotted' in obj && Array.isArray(obj.$dotted)) {
      // Dotted pair: reconstruct as improper list
      const items = obj.$dotted;
      if (items.length < 2) {
        return { tag: 'Unit' };
      }
      let result = jsonToVal(items[items.length - 1]);
      for (let i = items.length - 2; i >= 0; i--) {
        result = { tag: 'Vector', items: [jsonToVal(items[i]), result] };
      }
      return result;
    }

    // Check for opaque type markers - just return as-is wrapped in tagged
    if ('$type' in obj && '$opaque' in obj && obj.$opaque === true) {
      return {
        tag: 'Tagged',
        typeTag: String(obj.$type),
        payload: { tag: 'Str', s: JSON.stringify(obj) },
      };
    }

    // Regular object -> alist: ((key1 . val1) (key2 . val2) ...)
    const entries = Object.entries(obj);
    let result: Val = { tag: 'Unit' };
    for (let i = entries.length - 1; i >= 0; i--) {
      const [k, v] = entries[i];
      // Each entry is a pair (key . value) represented as 2-element vector
      const pair: Val = {
        tag: 'Vector',
        items: [{ tag: 'Str', s: k }, jsonToVal(v)],
      };
      result = { tag: 'Vector', items: [pair, result] };
    }
    return result;
  }

  // Fallback
  return { tag: 'Str', s: String(j) };
}

/**
 * Extract a value from an alist by key.
 * Useful for working with JSON-converted objects in Lisp.
 */
export function alistGet(alist: Val, key: string): Val | null {
  let cur = alist;
  while (cur.tag === 'Vector' && cur.items.length === 2) {
    const pair = cur.items[0];
    if (pair.tag === 'Vector' && pair.items.length === 2) {
      const k = pair.items[0];
      if ((k.tag === 'Str' && k.s === key) || (k.tag === 'Sym' && k.name === key)) {
        return pair.items[1];
      }
    }
    cur = cur.items[1];
  }
  return null;
}

/**
 * Build an alist from key-value pairs.
 * Useful for constructing OPR program arguments from Lisp.
 */
export function makeAlist(pairs: Array<[string, Val]>): Val {
  let result: Val = { tag: 'Unit' };
  for (let i = pairs.length - 1; i >= 0; i--) {
    const [k, v] = pairs[i];
    const pair: Val = {
      tag: 'Vector',
      items: [{ tag: 'Str', s: k }, v],
    };
    result = { tag: 'Vector', items: [pair, result] };
  }
  return result;
}
