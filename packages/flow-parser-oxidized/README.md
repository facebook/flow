# flow-parser-oxidized

A drop-in replacement for `hermes-parser` that uses the Flow Rust parser
compiled to WebAssembly. Produces ESTree-compatible ASTs.

This package was forked from `xplat/hermes/tools/hermes-parser/js/hermes-parser`
to keep all Flow-specific edits inside `fbcode/flow/`.

## Layout

```
src/
  index.js                       — entrypoint; calls FlowParser and applies loc fixups
  FlowParser.js                  — JS wrapper around the Rust WASM parser
  FlowParserDeserializer.js      — decodes the Rust binary parse-result protocol
  FlowParserNodeDeserializers.js — generated table of per-node deserializers
                                   (regenerate with the codegen target — see file header)
  FlowParserWASM.js.flow         — Flow types for the WASM module exports
  HermesAST.js.flow              — generic Hermes AST types reused by the deserializer
  HermesParserDecodeUTF8String.js — UTF-8 decoder for strings shared with the heap
  ParserOptions.js               — public parser options type
scripts/
  genFlowWasmParser.js           — wraps the built WASM in the package header
  facebook/buildFlowWasmParser.sh — builds the WASM via Buck and prints its path
demo_flow_parser.js              — end-to-end smoke demo
```

## WASM build target

`flow-parser-wasm.js` is defined at
`fbsource//xplat/hermes/tools/flow-parser-wasm:flow-parser-wasm.js` and depends on
`fbcode//flow/rust_port/crates/flow_parser_wasm:flow_parser_wasm`. The wasm BUCK
rule lives in `xplat/` because the `hermes_wasm_binary` macro requires its call
site to be under the FBCODE_SELECT_RECURSIVE_ALLOWLIST (which covers `xplat/hermes`);
only the BUCK rule lives there — the JS package surface stays here.
