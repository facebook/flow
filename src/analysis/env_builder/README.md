# Environment Builder (`env_builder`)

This directory contains the core infrastructure for Flow's environment analysis, which
maps variable reads to writes and establishes the dependency graph needed for type
resolution.

## Overview

The environment builder operates in three main phases:

```
                 ┌─────────────────────────────────────────┐
                 │           Control Flow Graph            │
                 │          (from AST traversal)           │
                 └───────────────────┬─────────────────────┘
                                     │
                                     ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│                            name_resolver                                    │
│  Builds SSA form with refinements. Maps each read to the writes/refinements │
│  that reach it at that program point.                                       │
│                                                                             │
│  Output:                                                                    │
│  - values: read_loc → reaching writes & refinements                         │
│  - write_entries: write_loc → binding info                                  │
│  - refinement_heap: refinement_id → refinement structure                    │
└─────────────────────────────────────────────────────────────────────────────┘
                                     │
                                     ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│                                name_def                                     │
│  Analyzes each write to determine how it can be synthesized. Describes how  │
│  each write depends on reads.                                               │
│                                                                             │
│  Output:                                                                    │
│  - env_entries_map: all possible definitions                                │
│  - hint_map: type hints for synthesis                                       │
└─────────────────────────────────────────────────────────────────────────────┘
                                     │
                                     ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│                            name_def_ordering                                │
│  Builds write→write dependency graph using the read→write mapping from      │
│  name_resolver and the write→read dependencies from name_def.               │
│                                                                             │
│  Output:                                                                    │
│  - Topologically sorted resolution order                                    │
│  - SCC decomposition (detecting legal/illegal cycles)                       │
└─────────────────────────────────────────────────────────────────────────────┘
```

## Core Modules

### `name_resolver` - Read-to-Write Analysis

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

### `name_def` - Write Synthesis Analysis

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

Synthesizability is a critical concept that determines whether a definition's type can
be inferred without additional context, and more importantly, whether the definition
can participate in a legal recursive cycle.

**Why It Matters:** The dependency graph between definitions may contain cycles. A
cycle is only legal if every definition in it is "recursively resolvable" - meaning
it can be created as a placeholder type variable before being fully resolved.
Non-synthesizable definitions cannot be placeholders because their types depend on
inference from their bodies. If any definition in a cycle is non-synthesizable, the
entire cycle is illegal and Flow reports an error.

As a rule of thumb, things are only synthesizable when they are fully annotated.

### `name_def_ordering` - Write Dependency Graph

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

**Legal cycles (ResolvableSCC):**
- Mutually recursive functions
- Classes with recursive methods
- Annotated bindings

**Illegal cycles (IllegalSCC):**
- Type alias cycles without proper indirection
- Unannotated recursive values

A definition can participate in a legal cycle if it's "recursively resolvable" -
meaning it can be created as a placeholder before being fully resolved. This
includes annotated bindings, synthesizable objects, synthesizable functions,
classes, type aliases, and interfaces.

## Key Invariants

### 1. Write Entry Correspondence Between name_resolver and name_def

The `write_entries` map produced by name_resolver and the `env_entries_map` produced by
name_def must have corresponding entries for all "assigning writes". This is enforced by:

**name_resolver produces `write_entries`:** A map from `(def_loc_type, location)` to one of:
- `AssigningWrite reason` - A write that actually assigns a value
- `GlobalWrite reason` - A write to a global variable
- `NonAssigningWrite` - A syntactic write that doesn't actually assign (dead code)

**name_def only processes assigning writes:** Before adding a definition to its map,
name_def checks `has_assigning_write` which returns true only for `AssigningWrite` and
`GlobalWrite` entries. `NonAssigningWrite` entries are skipped entirely.

**Invariant:** For every `AssigningWrite` in name_resolver's `write_entries`, name_def
must produce a corresponding entry in `env_entries_map`. If name_def_ordering tries to
look up a definition that doesn't exist, it raises `NameDefGraphMismatch`.

**Why this matters:** The dependency graph is built by iterating over name_def's entries.
If there's a mismatch, either:
- A write exists in name_resolver but name_def failed to traverse that AST node
- name_def is trying to add a definition for a location that name_resolver didn't mark as a write

### 2. NonAssigningWrite and Dead Code Detection

Not every syntactic assignment produces a real write. The `NonAssigningWrite` marker is
used for code that syntactically looks like an assignment but cannot actually execute or
whose result cannot be observed. This prevents spurious type errors about assignments
that will never happen at runtime.

**Some Cases that produce NonAssigningWrite (non-exhaustive):**

1. **Const reassignment:** Assigning to a `const` variable after its initialization.
   The runtime will throw, so the type of the "assigned" value is irrelevant.

2. **Duplicate bindings:** When a name is declared multiple times in the same scope
   (except for allowed cases like multiple `var` declarations), the duplicate is
   marked as non-assigning.

3. **Reference during declaration:** Referencing a variable in its own initializer
   (outside of functions/classes) marks that binding as non-assigning because it
   would be a TDZ error.

4. **Dead writes in unreachable code:** Writes that occur after control flow has
   definitively exited (return, throw, etc.) are marked via `mark_dead_write`.

5. **Provider-tracked variables:** For variables with providers, non-provider writes
   default to non-assigning unless they match an existing entry.

**How name_def uses this:** The `add_binding` method checks `has_assigning_write` before
adding any definition. If the write is non-assigning, no definition is created, which
means no type checking happens for that assignment's value - exactly the right behavior
since the assignment can never succeed.

## Downstream Consumers

The output from env_builder is consumed by modules in `src/typing/` that perform
actual type resolution. Understanding this flow is essential for debugging type
inference issues.

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
  up all provider locations and unioning/intersecting their types

**NonAssigningWrite Handling:** When `resolve_env_entry` encounters a `NonAssigningWrite`,
it does nothing - no type is recorded because the write doesn't actually occur.

### `env_resolution` - Definition Resolution

**Purpose:** Resolves each definition in the dependency order to produce its type.

The main entry point is `resolve_component` which:

1. Receives the sorted component order from `name_def_ordering`
2. For each component (Singleton, ResolvableSCC, or IllegalSCC):
   - Creates placeholder type variables for recursive definitions
   - Resolves each definition by dispatching to the appropriate handler
   - Records the resolved type via `type_env.resolve_env_entry`

**Synthesizability at Work:** For synthesizable definitions (annotated functions,
synthesizable objects), the type can be determined from annotations alone. For
non-synthesizable definitions, the full expression/body must be type-checked.

**Illegal Cycles:** For `IllegalSCC` components, all definitions in the cycle are
resolved to `any` since their types cannot be determined.

### Data Flow Summary

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                           env_builder (analysis/)                           │
│                                                                             │
│  name_resolver → name_def → name_def_ordering                               │
│       ↓              ↓              ↓                                       │
│  env_values    env_entries_map   ordering                                   │
└─────────────────────────────────────────────────────────────────────────────┘
                                     │
                                     ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│                            typing/ modules                                  │
│                                                                             │
│  loc_env.with_info(...)          -- Initialize loc_env with env_builder    │
│       ↓                              output                                 │
│  env_resolution.resolve_component -- Resolve definitions in order          │
│       ↓                                                                     │
│  type_env.resolve_env_entry      -- Record resolved types in loc_env       │
│       ↓                                                                     │
│  type_env.get_var / ref_entry    -- Read types during expression checking  │
└─────────────────────────────────────────────────────────────────────────────┘
```

## File Reference

| File | Purpose |
|------|---------|
| `name_resolver` | Read→write analysis, SSA construction |
| `name_def` | Write synthesis and read dependencies |
| `name_def_ordering` | Write→write graph, resolution ordering |
| `name_def_types` | Core data type definitions |
| `ssa_val` | SSA value representation |
| `env_api` | Public API and type definitions |
| `selector` | Property/element selectors for destructuring |
| `find_providers` | Provider analysis for variables |
| `provider_api` | Higher-level provider API module types |
| `dependency_sigs` | Module type signatures for context dependencies |
| `invalidation_api` | Refinement invalidation tracking |
| `refinement_key` | Keys for heap-based refinements |
| `eq_test` | Equality testing utilities |
| `pattern_helper` | Pattern binding helpers |
| `nonvoid_return` | Checks for non-void returns |
