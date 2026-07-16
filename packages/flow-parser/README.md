# flow-parser

A JavaScript parser built from the Flow's parser compiled to WebAssembly. Can parse ES6, Flow, and JSX syntax.

## What is Flow

See [flow.org](https://flow.org/). The code for the Flow parser [lives on GitHub](https://github.com/facebook/flow/tree/master/src/parser).

## What is the Flow Parser

The Flow Parser is a JavaScript parser for Flow syntax. It produces an AST that conforms to the [ESTree spec](https://github.com/estree/estree).

## API
The Flow parser exposes a single `parse(code, [options])` function, where `code` is the source code to parse as a string, and `options` is an optional object that may contain the following properties:
- **babel**: `boolean`, defaults to `false`. If `true`, output an AST conforming to Babel's AST format. If `false`, output an AST conforming to the ESTree AST format.
- **allowReturnOutsideFunction**: `boolean`, defaults to `false`. If `true`, do not error on return statements found outside functions.
- **assertOperator**: `boolean`, defaults to `false`. If `true`, enable Flow's postfix non-null assertion syntax (`value!`).
- **flow**: `"all"` or `"detect"`, defaults to `"detect"`. If `"detect"`, only parse syntax as Flow syntax where it is ambiguous whether it is a Flow feature or regular JavaScript when the `@flow` pragma is present in the file. Otherwise if `"all"`, always parse ambiguous syntax as Flow syntax regardless of the presence of an `@flow` pragma. For example `foo<T>(x)` in a file without an `@flow` pragma will be parsed as two comparisons if set to `"detect"`, otherwise if set to `"all"` or the `@flow` pragma is included it will be parsed as a call expression with a type argument.
- **enableExperimentalComponentSyntax**: `boolean`, defaults to `true`. Controls parsing of Flow component syntax.
- **enableExperimentalFlowMatchSyntax**: `boolean`, defaults to `true`. Controls parsing of Flow match syntax.
- **enableExperimentalFlowRecordSyntax**: `boolean`, defaults to `true`. Controls parsing of Flow record syntax.
- **reactRuntimeTarget**: `"18"` or `"19"`, defaults to `"18"`. Controls how component `ref` parameters are lowered when `babel` is `true`. React 18 treats `ref` as a separate parameter; React 19 treats it as a regular prop.
- **sourceFilename**: `string`, defaults to `null`. The filename corresponding to the code that is to be parsed. If non-null, the filename will be added to all source locations in the output AST.
- **sourceType**: `"module"`, `"script"`, or `"unambiguous"` (default). If `"unambiguous"`, source type will be automatically detected and set to `"module"` if any ES6 imports or exports are present in the code, otherwise source type will be set to `"script"`.
- **throwOnParseErrors**: `boolean`, defaults to `true`. If `true`, throw a `SyntaxError` for the first parse error. If `false`, return the partial AST with parse errors in the root node's `errors` property.
- **tokens**: `boolean`, defaults to `false`. If `true`, add all tokens to a `tokens` property on the root node.
- **transformOptions**: `object`. Options to be supplied to relevant transforms.
  - `TransformEnumSyntax`
    - `enable`: `boolean` - Whether to enable the transform. By default, `false`.
    - `getRuntime` (optional): `() => Expression` - The expression which should be a reference to the Flow Enums runtime. By default, is `require('flow-enums-runtime')`.
