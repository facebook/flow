# Environment Builder (`env_builder`)

The Rust environment analysis is split between this crate and
`flow_env_builder_resolver`. Together they map variable reads to writes and establish
the dependency graph needed for type resolution.

## Overview

The environment builder operates in three main phases:

```
                 ┌─────────────────────────────────────────┐
                 │       AST + scope/SSA prepasses         │
                 └───────────────────┬─────────────────────┘
                                     │
                                     ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│                            name_resolver                                    │
│  Builds SSA form with refinements. Maps each read to the writes/refinements │
│  that reach it at that program point.                                       │
│                                                                             │
│  Output:                                                                    │
│  - env_values: read_loc → reaching writes & refinements                     │
│  - env_entries: write_loc → binding info                                    │
│  - refinement_of_id: refinement_id → refinement structure                  │
└─────────────────────────────────────────────────────────────────────────────┘
                                     │
                                     ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│                                name_def                                     │
│  Analyzes each write to determine how it can be synthesized. Describes how  │
│  each write depends on reads.                                               │
│                                                                             │
│  Output:                                                                    │
│  - EnvEntriesMap: all possible definitions                                  │
│  - HintMap: type hints for synthesis                                        │
└─────────────────────────────────────────────────────────────────────────────┘
                                     │
                                     ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│                            name_def_ordering                                │
│  Builds write→write dependency graph using the read→write mapping from      │
│  name_resolver and the write→read dependencies from name_def.               │
│                                                                             │
│  Output:                                                                    │
│  - Vec<OrderingResult> in dependency-first order                            │
│  - Singleton, ResolvableSCC, and IllegalSCC components                      │
└─────────────────────────────────────────────────────────────────────────────┘
```

## Core Modules

### `flow_env_builder_resolver::name_resolver` - Read-to-Write Analysis

**Purpose:** Maps variable reads to the writes and refinements that reach those reads.

This module is based on the SSA builder but with divergent behavior for Flow's type
inference needs. It maintains a mapping from read locations to SSA values, tracking
which writes can reach each read point in the control flow.

#### Key Concepts

**SSA Values:** Each value tracks its "write state" - whether it's uninitialized,
undeclared, a concrete write at a location, a merge of multiple writes (PHI node),
or a refinement wrapping another value. PHI nodes are critical for representing
control flow merge points where multiple values can reach the same program point.

**Refinement Chains:** Refinements form a DAG structure. Complex refinements (like
`x != null && typeof x === 'string'`) are composed from simpler ones using AND, OR,
and NOT operations. Each refinement gets a unique ID for tracking.

**Environment Values:** For each variable in scope, the environment tracks the current
SSA value, a "havoc" value (for resetting after invalidation), the definition location,
heap refinements (for property accesses like `x.y`), and the binding kind (const, let, var, etc.).

#### Environment Model

The environment uses a two-level hierarchy:

1. **Function scope stack** - Separated by function boundaries. Values below
   function scope boundaries are **frozen** to prevent side effects from leaking.

2. **Local shadowing stack** - Within each function scope, tracks lexical shadowing.

```
-----------------------
{"foo" => [v3; v2]}      ← Current function scope (inner scopes first)
-----------------------
{"foo" => [v1]}          ← Outer function scope (frozen)
-----------------------
```

**Critical Invariant:** Values below function scope boundaries cannot be mutated
directly. When a closure accesses an outer variable, a copy is lazily created in
a captured variable map.

#### Refinements as Writes

**Key Insight:** Refinements are treated like writes in the SSA graph.

When you write:
```javascript
if (x.y) {
  // x.y is refined to truthy here
  use(x.y);
}
```

The refinement creates a new SSA value that wraps the original value. This refined
value is what the read of `x.y` inside the block will see.

#### Partial Environment Snapshots

At control flow merge points (if/else, loops, etc.), the system needs to merge
environments from multiple paths. Partial snapshots store only the values
that might change in the current scope, with a fallback mechanism to outer scopes.

**Invariant:** The current environment's key set is a superset of any snapshot's
key set, since the captured value map grows monotonically.

### `flow_env_builder::name_def` - Write Synthesis Analysis

**Purpose:** Describes how each write can be synthesized and what reads it depends on.

#### Definition Types

The system categorizes all possible definition kinds:
- Variable/import bindings
- Expression writes (like `x.y = expr`)
- Member assignments to object properties
- Operator assignments (`+=`, etc.)
- Increment/decrement operations
- Function declarations/expressions
- Class declarations
- Type aliases, opaque types, interfaces
- And many more...

#### Synthesizability

Synthesizability records whether functions and objects have enough annotations to be
resolved without first checking all of their bodies or member values. It is one input
to deciding whether a definition can participate in a legal recursive cycle.

**Why It Matters:** The dependency graph between definitions may contain cycles. A
multi-element cycle is resolvable only if every definition in it is "recursively
resolvable" - meaning it can be created as a placeholder type variable before being
fully resolved. `recursively_resolvable` is broader than synthesizability: it also
accepts classes, records, components, type definitions, annotation-backed bindings,
and a limited set of simple expressions whose dependencies are themselves resolvable.

### `flow_env_builder_resolver::name_def_ordering` - Write Dependency Graph

**Purpose:** Builds the write→write dependency graph and determines resolution order.

#### The Core Question

Given a definition, what other definitions must be resolved first?

```javascript
var x = 42;        // Write to x
type T = typeof x; // Depends on resolving x first
```

The dependency finder traverses each definition's AST structure to find which
variables are referenced, then maps those references back to their writes using
the environment from name_resolver.

**Critical:** Refinement dependencies are also extracted. If a refined value is
read, the refinement itself becomes a dependency.

#### SCC Detection with Tarjan's Algorithm

The dependency graph may contain cycles. The module uses Tarjan's algorithm to
find strongly connected components and classify them:

**Resolvable cycles (`ResolvableSCC`) can include:**
- Synthesizable mutually recursive functions and objects
- Classes, records, components, and declared definitions
- Type aliases, opaque types, type parameters, and interfaces
- Annotation-backed bindings and a limited set of simple expression definitions

**Illegal cycles (`IllegalSCC`) include:**
- Recursive bindings, functions, or objects that are not recursively resolvable
- Cycles through update, member-assignment, operator-assignment, and other
  non-resolvable definitions

A definition can participate in a legal cycle if it's "recursively resolvable" -
meaning it can be created as a placeholder before being fully resolved. This
includes annotation-backed bindings, synthesizable objects and functions, classes,
records, components, type aliases, opaque types, type parameters, and interfaces.

## Key Invariants

### 1. Dependency Correspondence Between `name_resolver` and `name_def`

The `env_entries` map produced by `name_resolver` determines which writes may receive
definitions in the `EnvEntriesMap` produced by `name_def`:

**`name_resolver` produces `env_entries`:** A map from `(def_loc_type, location)` to one of:

- `AssigningWrite reason` - A write that actually assigns a value
- `GlobalWrite reason` - A write to a global variable
- `NonAssigningWrite` - A syntactic write that doesn't actually assign (dead code)

**`name_def` only adds assigning writes:** Before adding a definition to its map,
`name_def` checks `has_assigning_write`, which returns true only for `AssigningWrite`
and `GlobalWrite` entries. `NonAssigningWrite` entries are skipped.

**Invariant:** Every dependency retained by `name_def_ordering` must have a
corresponding definition in `EnvEntriesMap`. If ordering tries to look up a definition
that does not exist, it raises `NameDefGraphMismatch`; it separately reports
`NameDefOrderingFailure` when the dependency graph contains roots absent from the
definition graph.

**Why this matters:** The dependency graph is built by iterating over `name_def`'s entries.
If there's a mismatch, either:

- A dependency points to a write for which `name_def` did not produce a definition
- `name_def` tried to add a definition for a location that `name_resolver` did not mark
  as an assigning write

### 2. NonAssigningWrite and Dead Code Detection

Not every syntactic assignment updates its target. The `NonAssigningWrite` marker is
used for invalid writes and writes in unreachable code. This prevents spurious type
errors about updates that cannot happen at runtime.

**Some Cases that produce NonAssigningWrite (non-exhaustive):**

1. **Const reassignment:** Assigning to a `const` variable after its initialization.
   The runtime will throw, so the right-hand type is not checked against the `const`.

2. **Invalid rebinding:** Invalid duplicate bindings and most other assignment errors
   are marked as non-assigning. Duplicate class bindings are a special case kept as
   assigning writes so their `this` and `super` entries can be installed.

3. **Reference in its own annotation:** Referencing a binding while its annotation is
   being traversed marks the declaration write as non-assigning and reports an
   `EReferenceInAnnotation` error.

4. **Dead writes in unreachable code:** Writes that occur after control flow has
   definitively exited (return, throw, etc.) are marked via `mark_dead_write`.

**How name_def uses this:** The `add_binding` method checks `has_assigning_write` before
adding a definition for the write. If it is non-assigning, no definition is created for
that write and `resolve_env_entry` does not record a type for it. Expressions within the
assignment are still traversed and type-checked as applicable.

## Downstream Consumers

The output is consumed by the Rust typing crates that perform actual type resolution.
Understanding this flow is essential for debugging type inference issues.

### `loc_env` - Location-Based Type Environment

Maps write locations to their resolved types during type checking. The locations come
from `env_builder` analysis.

### `type_env` - Type Environment Operations

**Purpose:** High-level interface for type lookups and environment manipulation during
type checking.

**Key Operations:**

- `get_var`: Reads a variable at a location, following the SSA graph from env_builder
  to find which writes reach that read, then looks up their types in loc_env

- `resolve_env_entry`: Writes a resolved type to loc_env for a definition. This is
  called after type checking a definition to record its type.

- `ref_entry_exn`: Reads a type from the environment, applying refinements from the
  SSA graph to narrow the type

- `provider_type_for_def_loc`: Computes the provider type for a variable by looking
  up all provider locations and unioning or intersecting their types, as requested by
  the caller

**NonAssigningWrite Handling:** When `resolve_env_entry` encounters a `NonAssigningWrite`,
it does nothing - no type is recorded because the write doesn't actually occur.

### `env_resolution` - Definition Resolution

**Purpose:** Resolves each definition in the dependency order to produce its type.

The caller invokes `resolve_component` once for each ordered component. It:

1. Marks the component's environment entries as under resolution
2. Initializes recursive type parameters and placeholders as needed
3. Resolves each definition by dispatching to the appropriate handler
4. Records resolved types via `type_env::resolve_env_entry`

**Synthesizability at Work:** For synthesizable definitions (annotated functions,
synthesizable objects), the type can be determined from annotations alone. For
non-synthesizable definitions, the full expression/body must be type-checked.

**Illegal Cycles:** For `IllegalSCC` components, all definitions in the cycle are
resolved to `any` since their types cannot be determined.

### Data Flow Summary

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                    Rust environment analysis                               │
│                                                                             │
│  resolver::name_resolver → builder::name_def → resolver::name_def_ordering  │
│       ↓              ↓              ↓                                       │
│  EnvInfo       EnvEntriesMap     Vec<OrderingResult>                        │
└─────────────────────────────────────────────────────────────────────────────┘
                                     │
                                     ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│                            typing/ modules                                  │
│                                                                             │
│  LocEnv::with_info(...)          -- Initialize loc_env with analysis       │
│       ↓                              output                                 │
│  env_resolution::resolve_component -- Resolve definitions in order         │
│       ↓                                                                     │
│  type_env::resolve_env_entry     -- Record resolved types in loc_env       │
│       ↓                                                                     │
│  type_env::get_var / ref_entry   -- Read types during expression checking  │
└─────────────────────────────────────────────────────────────────────────────┘
```

## File Reference

| File | Purpose |
|------|---------|
| `flow_env_builder_resolver/src/name_resolver.rs` | Read→write analysis, SSA construction |
| `flow_env_builder/src/name_def.rs` | Write synthesis and read dependencies |
| `flow_env_builder_resolver/src/name_def_ordering.rs` | Write→write graph, resolution ordering |
| `flow_env_builder/src/name_def_types.rs` | Core definition data types |
| `flow_env_builder/src/ssa_val.rs` | SSA value representation |
| `flow_env_builder/src/env_api.rs` | Public environment API and data types |
| `flow_env_builder/src/selector.rs` | Property/element selectors for destructuring |
| `flow_env_builder/src/find_providers.rs` | Provider analysis for variables |
| `flow_env_builder/src/provider_api.rs` | Higher-level provider API types |
| `flow_env_builder_resolver/src/dependency_sigs.rs` | Context and typing dependency traits |
| `flow_env_builder/src/invalidation_api.rs` | Refinement invalidation tracking |
| `flow_env_builder/src/refinement_key.rs` | Keys for heap-based refinements |
| `flow_env_builder/src/eq_test.rs` | Equality testing utilities |
| `flow_env_builder/src/pattern_helper.rs` | Pattern binding helpers |
| `flow_env_builder/src/nonvoid_return.rs` | Checks for non-void returns |
