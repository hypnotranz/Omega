# Prompt 14: Generic Operations

This module implements SICP-style generic operations with data-directed programming and coercion towers, adapted for semantic pipelines with inference-based method synthesis.

## Overview

Generic operations in Ω extend SICP's data-directed programming to support:
- **Semantic Type Tags**: Documents, text formats, policies (not just arithmetic types)
- **Coercion Towers**: Type conversions with governance (lossy transforms are explicit)
- **Method Synthesis**: Missing methods trigger inference/search for candidates
- **Commit Barriers**: Synthesized methods require obligations before installation

## Architecture

```
omega.generic/
  types.ts      - Core types (TaggedVal, MethodEntry, CoercionEntry, etc.)
  registry.ts   - Method and coercion table management
  coercion.ts   - Coercion graph and path finding
  dispatch.ts   - apply-generic implementation
  synthesis.ts  - Method synthesis via inference + commit barriers
```

## Key Concepts

### Tagged Values

Tagged values implement SICP's data abstraction barriers:

```scheme
(attach-tag "Doc/Email" email-data)    ; Create tagged value
(type-tag v)                            ; Get type tag
(contents v)                            ; Unwrap payload
```

Type tags use hierarchical naming:
- `Text/Plain`, `Text/Markdown`, `Text/Raw`
- `Doc/Email`, `Doc/Ticket`, `Doc/Chat`
- `Policy/Strict`, `Policy/Permissive`

### Generic Operations

Define and install methods:

```scheme
(defgeneric 'sanitize :arity 1)
(defmethod 'sanitize '(Doc/Email) sanitize-email-proc)
(defmethod 'sanitize '(Text/Plain) sanitize-text-proc)
```

Dispatch to the appropriate method:

```scheme
(apply-generic 'sanitize tagged-doc)
```

### Coercion Towers

Register type conversions:

```scheme
(defcoercion "Doc/Email" -> "Text/Plain" extract-body-proc)
(defcoercion "Text/Markdown" -> "Text/Plain" strip-markdown-proc)
```

When a method isn't found, the system searches for coercion paths:

```
Doc/Email -> Text/Plain -> sanitize(Text/Plain)
```

### Method Synthesis

When no method or coercion path exists, `apply-generic` emits a `generic.miss`:

```typescript
type GenericMissPayload = {
  op: string;
  signature: string[];
  argsPreview: Val[];
  registryId: string;
  profileName?: string;
};
```

The miss handler can:
1. Use inference to propose candidate methods
2. Search over adapter/coercion sequences
3. Request tests for validation
4. Commit successful candidates

### Commit Barriers

Synthesized methods require obligations:

```scheme
(commit/method 'sanitize '(Doc/ChatTranscript) proc
  :requires (test-obligations)
  :profile 'pragmatic)
```

Profile determines what's allowed:
- `explore`: Can synthesize, cannot commit
- `pragmatic`: Can commit with passing tests
- `strict`: Requires heavy obligations
- `airgap`: No synthesis or commits

## Flow: Missing Method Resolution

1. `apply-generic` called with `(op, args)`
2. Extract signature from args' type tags
3. Look up exact method → if found, apply
4. Search coercion paths → if found, apply coercions then method
5. Emit `generic.miss` for synthesis
6. Miss handler:
   - Generate candidates via inference
   - Test candidates against obligations
   - Return best passing candidate
7. Optionally commit to registry for future use

## Concurrency Safety

Method synthesis uses singleflight:
- Key = `(op, signature, registryId)`
- Concurrent misses wait on same IVar
- Only one oracle session per signature

## Types

### TaggedVal

```typescript
type TaggedVal = {
  tag: "Tagged";
  typeTag: string;    // Semantic type
  payload: Val;       // Wrapped data
};
```

### GenericRegistryVal

```typescript
type GenericRegistryVal = {
  tag: "GenericRegistry";
  id: string;
  name?: string;
};
```

### MethodEntry

```typescript
type MethodEntry = {
  op: string;
  signature: TypeSignature;
  proc: Val;
  synthesized: boolean;
  commitHash?: Hash;
  createdAt: number;
};
```

### CoercionEntry

```typescript
type CoercionEntry = {
  fromTag: TypeTag;
  toTag: TypeTag;
  proc: Val;
  lossy: boolean;
  synthesized: boolean;
  cost: number;
};
```

## Integration with Other Prompts

- **Prompt 9 (Profiles)**: Commit barriers respect profile constraints
- **Prompt 11 (Search)**: Method synthesis uses `amb` for candidate exploration
- **Prompt 12 (Constraints)**: Obligations as constraints on synthesis
- **Prompt 13 (Concurrency)**: Singleflight for synthesis deduplication

## Example: Semantic Sanitization Pipeline

```scheme
;; Define generic sanitize operation
(defgeneric 'sanitize :arity 1)

;; Register methods for known types
(defmethod 'sanitize '(Doc/Email)
  (lambda (doc) (sanitize-email-body doc)))

;; Register coercion
(defcoercion "Doc/Ticket" -> "Text/Plain"
  (lambda (ticket) (ticket-description ticket)))

;; Dispatch works via coercion
(apply-generic 'sanitize ticket-doc)
;; -> Coerces ticket to Text/Plain, then sanitizes

;; Unknown type triggers synthesis
(apply-generic 'sanitize chat-doc)
;; -> Emits generic.miss
;; -> Inference proposes method
;; -> Tests validate
;; -> Method committed for future use
```
