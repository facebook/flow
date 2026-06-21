# The flow-parser package

This package contains the Flow parser in its JavaScript package form.

# What is Flow

See [flow.org](https://flow.org/). The code for the Flow parser [lives on GitHub](https://github.com/facebook/flow/tree/master/src/parser).

# What is the Flow Parser

The Flow Parser is a JavaScript parser for Flow syntax. It produces an AST that conforms to the [ESTree spec](https://github.com/estree/estree). This npm package contains the Flow Rust parser compiled to WASM with JavaScript bindings.

# Usage

You can use the Flow parser in node:

```JavaScript
const {parse} = require('flow-parser');
parse('1+1', {});
```

## Options

The second argument to `parse` is the options object. Common options:

### Basic options
* `flow` (`'detect'` or `'all'`, default `'detect'`) - parse Flow syntax only with an `@flow` pragma, or always parse Flow syntax
* `sourceType` (`'module'`, `'script'`, or `'unambiguous'`) - parse the file as a module, script, or infer it from the contents
* `tokens` (boolean, default `false`) - include a list of all parsed tokens in a top-level `tokens` property
* `babel` (boolean, default `false`) - return a Babel-shaped AST

### Language features
* `enableEnums` (boolean, default `true`) - enable parsing of [enums](https://flow.org/en/docs/enums/)
* `enableExperimentalFlowMatchSyntax` (boolean, default `true`) - enable parsing of [match expressions and match statements](https://flow.org/en/docs/match/)
* `enableExperimentalComponentSyntax` (boolean, default `true`) - enable parsing of [component syntax](https://flow.org/en/docs/react/component-syntax/)
* `assertOperator` (boolean, default `false`) - enable parsing of the assert operator
* `enableExperimentalDecorators` (boolean, default `true`) - enable parsing of decorators
