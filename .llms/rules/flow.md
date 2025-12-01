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

### Meta Internal Development (Buck2)
```bash
# Build Flow binary
buck2 build //flow:flow

# Get the binary path for testing
FLOW_BIN=$(buck2 build //flow:flow --show-full-output | awk '{print $2}')
```

## Running Tests

See tests/README.md for detailed instructions.

## Architecture

### Source Code Structure (`src/`)

- **`analysis/`** - Static analysis passes (bindings, hoisting, environment building)
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
