# OmegaLLM Traceability Matrix (Auto-Generated)

> Generated: 2026-01-24T03:36:42.288Z
> Source: scripts/generate-traceability.ts

## Summary

| Category | Count | Has Tests |
|----------|-------|-----------|
| Other | 2 | 0/2 |
| Arithmetic | 11 | 0/11 |
| Logic | 3 | 0/3 |
| Monadic | 4 | 0/4 |
| Continuations | 3 | 0/3 |
| Lists | 18 | 1/18 |
| Distributions | 10 | 0/10 |
| Strings | 11 | 1/11 |
| Type Predicates | 11 | 0/11 |
| Higher-Order | 8 | 0/8 |
| Promises | 2 | 0/2 |
| Streams | 11 | 3/11 |
| Generic Dispatch | 8 | 1/8 |
| Coercion | 6 | 0/6 |
| Term Rewriting | 8 | 0/8 |
| Evidence | 2 | 0/2 |
| Machine Introspection | 13 | 0/13 |

---

## Other (2)

| Primitive | Line | Tests |
|-----------|------|-------|
| `*uninit*` | 99 | ❌ |
| `substring` | 629 | ❌ |

## Arithmetic (11)

| Primitive | Line | Tests |
|-----------|------|-------|
| `+` | 104 | ❌ |
| `-` | 105 | ❌ |
| `*` | 111 | ❌ |
| `=` | 116 | ❌ |
| `<` | 121 | ❌ |
| `>` | 126 | ❌ |
| `<=` | 131 | ❌ |
| `>=` | 136 | ❌ |
| `/` | 141 | ❌ |
| `modulo` | 146 | ❌ |
| `even?` | 151 | ❌ |

## Logic (3)

| Primitive | Line | Tests |
|-----------|------|-------|
| `not` | 156 | ❌ |
| `or` | 162 | ❌ |
| `and` | 173 | ❌ |

## Monadic (4)

| Primitive | Line | Tests |
|-----------|------|-------|
| `unit` | 194 | ❌ |
| `mzero` | 196 | ❌ |
| `mplus` | 198 | ❌ |
| `bind` | 202 | ❌ |

## Continuations (3)

| Primitive | Line | Tests |
|-----------|------|-------|
| `call/cc` | 222 | ❌ |
| `call-with-prompt` | 240 | ❌ |
| `abort-to-prompt` | 260 | ❌ |

## Lists (18)

| Primitive | Line | Tests |
|-----------|------|-------|
| `cons` | 302 | ❌ |
| `car` | 308 | ❌ |
| `cdr` | 317 | ❌ |
| `null?` | 326 | ❌ |
| `pair?` | 333 | ❌ |
| `list` | 340 | ❌ |
| `append` | 350 | ❌ |
| `eq?` | 373 | ❌ |
| `length` | 672 | ❌ |
| `list-ref` | 683 | ❌ |
| `reverse` | 696 | ❌ |
| `cadr` | 712 | ❌ |
| `caddr` | 720 | ❌ |
| `equal?` | 731 | test\repl\file-loader.spec.ts |
| `map` | 772 | ❌ |
| `filter` | 804 | ❌ |
| `fold` | 841 | ❌ |
| `foldr` | 862 | ❌ |

## Distributions (10)

| Primitive | Line | Tests |
|-----------|------|-------|
| `dist` | 398 | ❌ |
| `dist?` | 405 | ❌ |
| `dist-count` | 412 | ❌ |
| `dist-value-at` | 420 | ❌ |
| `dist-weight-at` | 429 | ❌ |
| `dist-normalize` | 438 | ❌ |
| `dist-sample` | 452 | ❌ |
| `dist-topk` | 485 | ❌ |
| `dist-from-list` | 497 | ❌ |
| `dist-to-list` | 518 | ❌ |

## Strings (11)

| Primitive | Line | Tests |
|-----------|------|-------|
| `string=?` | 538 | ❌ |
| `string-contains?` | 545 | ❌ |
| `string-replace-all` | 552 | ❌ |
| `string-split` | 563 | ❌ |
| `string-join` | 576 | ❌ |
| `string-trim` | 591 | ❌ |
| `string-downcase` | 598 | ❌ |
| `string-upcase` | 605 | ❌ |
| `string-length` | 612 | ❌ |
| `string-append` | 619 | test\prompt18-meta\meta.spec.ts |
| `string?` | 646 | ❌ |

## Type Predicates (11)

| Primitive | Line | Tests |
|-----------|------|-------|
| `symbol?` | 641 | ❌ |
| `number?` | 651 | ❌ |
| `boolean?` | 656 | ❌ |
| `procedure?` | 661 | ❌ |
| `promise?` | 1161 | ❌ |
| `promise-forced?` | 1167 | ❌ |
| `op-table?` | 1670 | ❌ |
| `tagged?` | 1721 | ❌ |
| `coercion-table?` | 1912 | ❌ |
| `rule?` | 2130 | ❌ |
| `evidence-stale?` | 2383 | ❌ |

## Higher-Order (8)

| Primitive | Line | Tests |
|-----------|------|-------|
| `compose` | 890 | ❌ |
| `pipe` | 918 | ❌ |
| `partial` | 946 | ❌ |
| `apply` | 970 | ❌ |
| `identity` | 1033 | ❌ |
| `constantly` | 1038 | ❌ |
| `andmap` | 1052 | ❌ |
| `ormap` | 1072 | ❌ |

## Promises (2)

| Primitive | Line | Tests |
|-----------|------|-------|
| `make-promise` | 1098 | ❌ |
| `force` | 1115 | ❌ |

## Streams (11)

| Primitive | Line | Tests |
|-----------|------|-------|
| `the-empty-stream` | 1214 | test\oracle\observe.spec.ts, test\repl\debugger.spec.ts |
| `stream-null?` | 1217 | ❌ |
| `stream-car` | 1224 | ❌ |
| `stream-cdr` | 1233 | ❌ |
| `list->stream` | 1274 | ❌ |
| `stream->list` | 1283 | ❌ |
| `stream-take` | 1330 | ❌ |
| `stream-interleave` | 1409 | test\streams\stream-interleave.spec.ts |
| `stream-interleave-lazy` | 1416 | ❌ |
| `stream-map` | 1433 | test\docs\quick-reference.spec.ts, test\semantic-rewriting\semantic-rewriting.spec.ts |
| `stream-filter` | 1532 | ❌ |

## Generic Dispatch (8)

| Primitive | Line | Tests |
|-----------|------|-------|
| `make-op-table` | 1600 | ❌ |
| `op-table-put` | 1610 | ❌ |
| `op-table-get` | 1639 | ❌ |
| `attach-tag` | 1681 | ❌ |
| `type-tag` | 1699 | ❌ |
| `contents` | 1710 | ❌ |
| `apply-generic` | 1733 | test\semantic-rewriting\semantic-rewriting.spec.ts |
| `apply-generic-coerced` | 1774 | ❌ |

## Coercion (6)

| Primitive | Line | Tests |
|-----------|------|-------|
| `make-coercion-table` | 1860 | ❌ |
| `put-coercion` | 1870 | ❌ |
| `get-coercion` | 1896 | ❌ |
| `find-coercion-path` | 1919 | ❌ |
| `find-all-coercion-paths` | 1968 | ❌ |
| `coerce-value` | 2015 | ❌ |

## Term Rewriting (8)

| Primitive | Line | Tests |
|-----------|------|-------|
| `make-rule` | 2084 | ❌ |
| `make-rule-where` | 2107 | ❌ |
| `rewrite-once` | 2180 | ❌ |
| `rewrite-fixpoint` | 2212 | ❌ |
| `rewrite-trace` | 2242 | ❌ |
| `rewrite-conflicts` | 2277 | ❌ |
| `match-pattern` | 2304 | ❌ |
| `substitute-template` | 2324 | ❌ |

## Evidence (2)

| Primitive | Line | Tests |
|-----------|------|-------|
| `evidence-id` | 2368 | ❌ |
| `verify-evidence` | 2377 | ❌ |

## Machine Introspection (13)

| Primitive | Line | Tests |
|-----------|------|-------|
| `machine-new` | 2414 | ❌ |
| `machine-step` | 2446 | ❌ |
| `machine-run` | 2474 | ❌ |
| `machine-stack` | 2520 | ❌ |
| `machine-control` | 2538 | ❌ |
| `machine-done?` | 2552 | ❌ |
| `machine-value` | 2560 | ❌ |
| `machine-fork` | 2577 | ❌ |
| `machine-resume` | 2603 | ❌ |
| `machine-add-breakpoint` | 2623 | ❌ |
| `machine-step-count` | 2645 | ❌ |
| `machine-last-op` | 2653 | ❌ |
| `machine?` | 2665 | ❌ |

