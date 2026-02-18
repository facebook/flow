---
oncalls: ['flow']
---

## Overview

Flow is a static typechecker for JavaScript, written primarily in OCaml.

## Build System

Flow uses two build systems depending on context:

### OSS Development (Make + Dune)
```bash
# Install dependencies
make deps

# Build Flow binary
eval $(opam env)
make

# This produces bin/flow
```

### Meta Internal Development (Buck)
```bash
# Build Flow binary
buck build //flow

# Get the binary path for testing
FLOW_BIN=$(buck build //flow:flow --show-full-output | awk '{print $2}')
```

## Formatting Code

```bash
arc f
```

## After Editing OCaml Code

After every edit to OCaml files (`.ml`, `.mli`), you MUST run both of these commands:
1. `arc f` — to format the code
2. `buck build //flow` — to verify the build succeeds

## Running Tests

See tests/README.md for detailed instructions.

### Checker Tests (tests/)
Run only checker-related tests (`flow full-check`) under `tests/`:
```bash
env FLOW_RUNTESTS_PARALLELISM=176 ./runtests.sh -c facebook/flowd
```

### All Tests (tests/)
Run all tests under `tests/`:
```bash
env FLOW_RUNTESTS_PARALLELISM=176 ./runtests.sh facebook/flowd
```

### Re-record Expected Outputs
To update expected test outputs:
```bash
env FLOW_RUNTESTS_PARALLELISM=176 ./runtests.sh -r -c facebook/flowd
```

### IDE / LSP Tests
Run IDE tests:
```bash
./tool test --bin facebook/flowd
```

Run a specific IDE test (e.g. `newtests/lsp/llmContext`):
```bash
./tool test --bin facebook/flowd newtests/lsp/llmContext
```

The output of a failing IDE test will show how to update expected output.

## Architecture

### Source Code Structure (`src/`)

- **`analysis/`** - Static analysis passes (bindings, hoisting, environment building)
  - **`env_builder`** - See [env_builder README](../../src/analysis/env_builder/README.md) for details on environment analysis.
- **`commands/`** - CLI command implementations
- **`common/`** - Shared utilities and data structures
- **`heap/`** - Shared memory heap for worker processes
- **`lsp/`** - Language Server Protocol implementation
- **`monitor/`** - Flow server monitoring and lifecycle management
- **`parser/`** - JavaScript parser (produces ESTree AST)
- **`parser_utils/`** - Parser utilities and helpers
- **`server/`** - Flow server core logic
- **`services/`** - Type checking services
- **`typing/`** - Type system implementation and type inference engine
- **`hack_forked/`** - Code forked from Hack (Facebook's PHP typechecker)
