# 14: Standard Library Reference

## Organization

```
lambdallm.core      ; Auto-imported, fundamental operations
lambdallm.list      ; List manipulation
lambdallm.string    ; String operations
lambdallm.map       ; Hash maps
lambdallm.set       ; Sets
lambdallm.math      ; Mathematical functions
lambdallm.io        ; File and console I/O
lambdallm.llm       ; LLM operations
lambdallm.test      ; Testing framework
lambdallm.debug     ; Debugging utilities
```

---

## lambdallm.core

### Arithmetic

| Function | Signature | Description |
|----------|-----------|-------------|
| `+` | `(+ n...)` | Sum of numbers |
| `-` | `(- n)` or `(- n m...)` | Negation or subtraction |
| `*` | `(* n...)` | Product of numbers |
| `/` | `(/ n m)` | Division |
| `mod` | `(mod n m)` | Modulo |
| `inc` | `(inc n)` | Add 1 |
| `dec` | `(dec n)` | Subtract 1 |
| `abs` | `(abs n)` | Absolute value |
| `min` | `(min n...)` | Minimum |
| `max` | `(max n...)` | Maximum |

### Comparison

| Function | Signature | Description |
|----------|-----------|-------------|
| `=` | `(= a b)` | Equality (value) |
| `eq?` | `(eq? a b)` | Identity (reference) |
| `<` | `(< a b)` | Less than |
| `>` | `(> a b)` | Greater than |
| `<=` | `(<= a b)` | Less or equal |
| `>=` | `(>= a b)` | Greater or equal |
| `zero?` | `(zero? n)` | Is zero? |
| `positive?` | `(positive? n)` | Is positive? |
| `negative?` | `(negative? n)` | Is negative? |

### Logic

| Function | Signature | Description |
|----------|-----------|-------------|
| `not` | `(not x)` | Logical negation |
| `and` | `(and x...)` | Short-circuit and (special form) |
| `or` | `(or x...)` | Short-circuit or (special form) |

### Type Predicates

| Function | Signature | Description |
|----------|-----------|-------------|
| `number?` | `(number? x)` | Is number? |
| `string?` | `(string? x)` | Is string? |
| `symbol?` | `(symbol? x)` | Is symbol? |
| `keyword?` | `(keyword? x)` | Is keyword? |
| `list?` | `(list? x)` | Is list? |
| `null?` | `(null? x)` | Is null/nil? |
| `procedure?` | `(procedure? x)` | Is procedure? |
| `boolean?` | `(boolean? x)` | Is boolean? |

### Identity and Equality

| Function | Signature | Description |
|----------|-----------|-------------|
| `identical?` | `(identical? a b)` | Same object? |
| `equal?` | `(equal? a b)` | Deep equality? |
| `hash` | `(hash x)` | Hash code |

---

## lambdallm.list

### Construction

| Function | Signature | Description |
|----------|-----------|-------------|
| `list` | `(list x...)` | Create list |
| `cons` | `(cons h t)` | Prepend to list |
| `list*` | `(list* x... tail)` | Create with tail |

### Access

| Function | Signature | Description |
|----------|-----------|-------------|
| `car` | `(car l)` | First element |
| `cdr` | `(cdr l)` | Rest of list |
| `first` | `(first l)` | Alias for car |
| `rest` | `(rest l)` | Alias for cdr |
| `second` | `(second l)` | Second element |
| `third` | `(third l)` | Third element |
| `nth` | `(nth l n)` | Element at index |
| `last` | `(last l)` | Last element |
| `butlast` | `(butlast l)` | All but last |

### Query

| Function | Signature | Description |
|----------|-----------|-------------|
| `length` | `(length l)` | Number of elements |
| `empty?` | `(empty? l)` | Is empty? |
| `member` | `(member x l)` | Find element |
| `index-of` | `(index-of x l)` | Find index |
| `contains?` | `(contains? l x)` | Element exists? |

### Transformation

| Function | Signature | Description |
|----------|-----------|-------------|
| `map` | `(map f l)` | Apply f to each |
| `filter` | `(filter p l)` | Keep matching |
| `remove` | `(remove p l)` | Remove matching |
| `reduce` | `(reduce f init l)` | Fold left |
| `reduce-right` | `(reduce-right f init l)` | Fold right |
| `reverse` | `(reverse l)` | Reverse list |
| `append` | `(append l...)` | Concatenate |
| `flatten` | `(flatten l)` | Flatten nested |
| `take` | `(take n l)` | First n elements |
| `drop` | `(drop n l)` | Skip n elements |
| `take-while` | `(take-while p l)` | While predicate |
| `drop-while` | `(drop-while p l)` | Skip while predicate |
| `partition` | `(partition p l)` | Split by predicate |
| `sort` | `(sort l cmp?)` | Sort list |
| `unique` | `(unique l)` | Remove duplicates |

### Combination

| Function | Signature | Description |
|----------|-----------|-------------|
| `zip` | `(zip l1 l2)` | Pair elements |
| `interleave` | `(interleave l1 l2)` | Alternate elements |
| `cartesian` | `(cartesian l1 l2)` | All pairs |

---

## lambdallm.string

### Construction

| Function | Signature | Description |
|----------|-----------|-------------|
| `str` | `(str x...)` | Concatenate to string |
| `string` | `(string c...)` | From characters |
| `format` | `(format fmt args...)` | Format string |

### Query

| Function | Signature | Description |
|----------|-----------|-------------|
| `string-length` | `(string-length s)` | Length |
| `string-empty?` | `(string-empty? s)` | Is empty? |
| `string-contains?` | `(string-contains? s sub)` | Contains substring? |
| `string-starts-with?` | `(string-starts-with? s prefix)` | Starts with? |
| `string-ends-with?` | `(string-ends-with? s suffix)` | Ends with? |
| `string-index-of` | `(string-index-of s sub)` | Find substring |

### Transformation

| Function | Signature | Description |
|----------|-----------|-------------|
| `substring` | `(substring s start end?)` | Extract portion |
| `string-append` | `(string-append s...)` | Concatenate |
| `string-join` | `(string-join l sep)` | Join with separator |
| `string-split` | `(string-split s sep)` | Split on separator |
| `string-trim` | `(string-trim s)` | Remove whitespace |
| `string-upcase` | `(string-upcase s)` | Uppercase |
| `string-downcase` | `(string-downcase s)` | Lowercase |
| `string-replace` | `(string-replace s old new)` | Replace all |

### Regex

| Function | Signature | Description |
|----------|-----------|-------------|
| `regex-match` | `(regex-match pattern s)` | Match regex |
| `regex-find` | `(regex-find pattern s)` | Find all matches |
| `regex-replace` | `(regex-replace pattern s replacement)` | Replace matches |

---

## lambdallm.map

| Function | Signature | Description |
|----------|-----------|-------------|
| `hash-map` | `(hash-map k1 v1...)` | Create map |
| `get` | `(get m k default?)` | Get value |
| `assoc` | `(assoc m k v)` | Add/update key |
| `dissoc` | `(dissoc m k)` | Remove key |
| `contains-key?` | `(contains-key? m k)` | Key exists? |
| `keys` | `(keys m)` | Get all keys |
| `vals` | `(vals m)` | Get all values |
| `entries` | `(entries m)` | Get key-value pairs |
| `merge` | `(merge m1 m2...)` | Merge maps |
| `select-keys` | `(select-keys m ks)` | Subset of keys |
| `map-vals` | `(map-vals f m)` | Map over values |
| `map-keys` | `(map-keys f m)` | Map over keys |

---

## lambdallm.io

### Console

| Function | Signature | Description |
|----------|-----------|-------------|
| `print` | `(print x...)` | Print without newline |
| `println` | `(println x...)` | Print with newline |
| `read-line` | `(read-line)` | Read line from stdin |

### Files (FFI wrappers)

| Function | Signature | Description |
|----------|-----------|-------------|
| `read-file` | `(read-file path)` | Read entire file |
| `write-file` | `(write-file path content)` | Write file |
| `append-file` | `(append-file path content)` | Append to file |
| `file-exists?` | `(file-exists? path)` | File exists? |
| `list-files` | `(list-files pattern)` | Glob files |
| `delete-file` | `(delete-file path)` | Delete file |

---

## lambdallm.llm

| Function | Signature | Description |
|----------|-----------|-------------|
| `complete` | `(complete prompt system?)` | Get completion |
| `chat` | `(chat messages tools?)` | Multi-turn chat |
| `embed` | `(embed text)` | Get embedding |
| `intent` | `(intent description)` | Compile intent to code |

---

## lambdallm.test

| Function | Signature | Description |
|----------|-----------|-------------|
| `deftest` | `(deftest name body...)` | Define test |
| `is` | `(is expr)` | Assert truthy |
| `is=` | `(is= expected actual)` | Assert equal |
| `is-not` | `(is-not expr)` | Assert falsy |
| `throws?` | `(throws? type body)` | Assert throws |
| `run-tests` | `(run-tests ns?)` | Run all tests |

```lisp
(deftest addition-works
  (is= 4 (+ 2 2))
  (is= 0 (+ -1 1)))

(run-tests)
```

---

## lambdallm.debug

| Function | Signature | Description |
|----------|-----------|-------------|
| `trace` | `(trace expr)` | Print and return |
| `spy` | `(spy label expr)` | Labeled trace |
| `time` | `(time expr)` | Measure time |
| `break` | `(break)` | Enter debugger |
| `apropos` | `(apropos pattern)` | Find matching symbols |
| `doc` | `(doc symbol)` | Show documentation |
| `source` | `(source symbol)` | Show source |
| `macroexpand` | `(macroexpand form)` | Expand macros |

---

## Implementation Notes

### Lisp-Defined Primitives

Many stdlib functions are defined in Lisp:

```lisp
;; stdlib/list.lisp
(define (map f l)
  (if (null? l)
      '()
      (cons (f (car l))
            (map f (cdr l)))))

(define (filter p l)
  (cond
    ((null? l) '())
    ((p (car l)) (cons (car l) (filter p (cdr l))))
    (else (filter p (cdr l)))))

(define (reduce f init l)
  (if (null? l)
      init
      (reduce f (f init (car l)) (cdr l))))
```

### FFI-Based Primitives

I/O operations are FFI wrappers:

```typescript
// Registered in TypeScript
ffi.register('read-file', (path: string) => world.read(path));
ffi.register('write-file', (path: string, content: string) => world.write(path, content));
```
