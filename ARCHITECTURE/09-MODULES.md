# 09: Module System

## Purpose

The module system organizes code into reusable, isolated units with explicit dependencies.

## Design Goals

1. **Isolation**: Modules have private internals by default
2. **Explicit dependencies**: All imports declared at top
3. **No circular deps**: Dependency graph is acyclic
4. **Hot reload**: Modules can be reloaded in REPL
5. **Serializable**: Module state can be persisted

---

## Namespace Declaration

Every file begins with a namespace declaration:

```lisp
(ns myapp.core
  (:require [myapp.utils :as u]
            [myapp.data :refer [parse serialize]])
  (:import [js/fetch]))
```

### Syntax

```ebnf
<ns-form> ::= "(ns" <namespace-name> <ns-clause>* ")"

<namespace-name> ::= <symbol>  ; e.g., myapp.core

<ns-clause> ::= <require-clause>
              | <import-clause>
              | <export-clause>

<require-clause> ::= "(:require" <require-spec>* ")"
<require-spec>   ::= <namespace-name>
                   | "[" <namespace-name> ":as" <alias> "]"
                   | "[" <namespace-name> ":refer" "[" <symbol>* "]" "]"
                   | "[" <namespace-name> ":refer" ":all" "]"

<import-clause> ::= "(:import" <import-spec>* ")"
<import-spec>   ::= "[" <ffi-module> "]"

<export-clause> ::= "(:export" <symbol>* ")"
```

---

## File Structure

```
project/
├── src/
│   ├── myapp/
│   │   ├── core.lisp        ; ns myapp.core
│   │   ├── utils.lisp       ; ns myapp.utils
│   │   └── data/
│   │       ├── parse.lisp   ; ns myapp.data.parse
│   │       └── serialize.lisp
│   └── main.lisp            ; ns main
├── test/
│   └── myapp/
│       └── core_test.lisp   ; ns myapp.core-test
└── project.yaml             ; Project configuration
```

### Mapping Rules

- File path `src/myapp/core.lisp` → namespace `myapp.core`
- Dots in namespace → directories in filesystem
- Hyphens in names OK (`my-app.core` → `src/my_app/core.lisp`)

---

## Resolution Algorithm

When loading `(require '[myapp.utils :as u])`:

1. **Check cache**: If already loaded, return cached
2. **Find file**: Search load-path for `myapp/utils.lisp`
3. **Parse**: Read source file
4. **Create namespace**: Empty namespace `myapp.utils`
5. **Evaluate**: Evaluate all forms in new namespace
6. **Cache**: Store in namespace registry
7. **Return**: Return namespace object

---

## Namespace Object

```typescript
interface Namespace {
  name: string;                           // "myapp.core"
  bindings: Map<string, Value>;           // Public + private defs
  exports: Set<string>;                   // Explicitly exported names
  imports: Map<string, Namespace>;        // Aliased imports
  refers: Map<string, [Namespace, string]>; // Direct imports
  meta: NamespaceMeta;                    // Metadata
}

interface NamespaceMeta {
  file?: string;                          // Source file path
  loadedAt: number;                       // Timestamp
  dependencies: string[];                 // Required namespaces
  doc?: string;                           // Namespace docstring
}
```

---

## Visibility

### Default: Public

All `define`d names are public by default:

```lisp
(ns myapp.core)

(define (public-fn x) ...)   ; Accessible from other namespaces
```

### Private Definitions

Use `define-` prefix or `^:private` metadata:

```lisp
(define- (private-helper x) ...)         ; Private (convention)
(define ^:private (also-private x) ...)  ; Private (metadata)
```

### Explicit Exports

If `(:export ...)` is present, only listed names are public:

```lisp
(ns myapp.core
  (:export public-fn other-fn))

(define (public-fn x) ...)     ; Exported
(define (other-fn x) ...)      ; Exported
(define (internal-fn x) ...)   ; NOT exported (private)
```

---

## Qualified Access

```lisp
;; With alias
(require '[myapp.utils :as u])
(u/helper-fn arg)              ; Call myapp.utils/helper-fn

;; Full qualification
myapp.utils/helper-fn

;; Direct refer
(require '[myapp.utils :refer [helper-fn]])
(helper-fn arg)                ; No prefix needed
```

---

## Circular Dependencies

Circular dependencies are **not allowed**.

```
myapp.a requires myapp.b
myapp.b requires myapp.a   ; ERROR: Circular dependency
```

The loader detects cycles and signals a condition:

```lisp
;; Condition signaled:
{:type circular-dependency
 :cycle ["myapp.a" "myapp.b" "myapp.a"]
 :restarts [(abort "Abort loading")]}
```

---

## Hot Reload

In REPL, namespaces can be reloaded:

```lisp
(require '[myapp.core :reload])           ; Reload single
(require '[myapp.core :reload-all])       ; Reload + dependencies
```

### Reload Semantics

1. Re-read source file
2. Clear namespace bindings
3. Re-evaluate all forms
4. Update dependent namespaces (for `:reload-all`)

**Caveat**: Existing closures retain old definitions.

---

## Standard Namespaces

| Namespace | Description |
|-----------|-------------|
| `lambdallm.core` | Core primitives (auto-imported) |
| `lambdallm.list` | List operations |
| `lambdallm.string` | String operations |
| `lambdallm.io` | I/O operations (FFI wrappers) |
| `lambdallm.llm` | LLM operations |
| `lambdallm.debug` | Debugging utilities |
| `lambdallm.test` | Testing framework |

---

## Implementation

```typescript
// modules.ts

class ModuleLoader {
  private registry: Map<string, Namespace> = new Map();
  private loading: Set<string> = new Set();  // Cycle detection

  async load(name: string): Promise<Namespace> {
    // Check cache
    if (this.registry.has(name)) {
      return this.registry.get(name)!;
    }

    // Check for cycle
    if (this.loading.has(name)) {
      throw new CircularDependencyError(name, [...this.loading]);
    }

    this.loading.add(name);

    try {
      // Find and read file
      const path = this.resolvePath(name);
      const source = await this.readFile(path);

      // Create namespace
      const ns = createNamespace(name);

      // Parse and evaluate
      const forms = read(source, path);
      for (const form of forms) {
        await this.evalInNamespace(form, ns);
      }

      // Cache and return
      this.registry.set(name, ns);
      return ns;

    } finally {
      this.loading.delete(name);
    }
  }

  private resolvePath(name: string): string {
    // myapp.core → src/myapp/core.lisp
    const parts = name.split('.');
    return `src/${parts.join('/')}.lisp`;
  }
}
```

---

## Project Configuration

```yaml
# project.yaml
name: myapp
version: 0.1.0
main: main

paths:
  src: src
  test: test
  resources: resources

dependencies:
  lambdallm: "^1.0.0"
  some-library: "^2.0.0"

dev-dependencies:
  test-runner: "^1.0.0"
```
