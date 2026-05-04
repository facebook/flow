/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow strict
 * @format
 */

'use strict';

import type {Program as ESTreeProgram} from 'flow-estree-oxidized';
import type {ParserOptions} from './ParserOptions';
import type {BabelFile} from './babel/TransformESTreeToBabel';

import * as FlowParser from './FlowParser';
import FlowVisitorKeys from './generated/ESTreeVisitorKeys';
import * as TransformComponentSyntax from './estree/TransformComponentSyntax';
import * as TransformEnumSyntax from './estree/TransformEnumSyntax';
import * as TransformMatchSyntax from './estree/TransformMatchSyntax';
import * as TransformRecordSyntax from './estree/TransformRecordSyntax';
import * as StripFlowTypesForBabel from './estree/StripFlowTypesForBabel';
import * as TransformESTreeToBabel from './babel/TransformESTreeToBabel';
import * as StripFlowTypes from './estree/StripFlowTypes';

const DEFAULTS = {
  flow: ('detect': 'detect'),
};

function getOptions(opts?: ParserOptions): ParserOptions {
  // Always build a fresh object so we never mutate the caller's input.
  // Repeated calls with the same `opts` reference must produce identical
  // results — earlier versions wrote normalized fields back onto `opts`,
  // poisoning subsequent calls.
  const options = {...DEFAULTS, ...(opts ?? {})};

  // Default to detecting whether to parse Flow syntax by the presence
  // of an @flow pragma.
  if (options.flow !== 'all' && options.flow !== 'detect') {
    throw new Error('flow option must be "all" or "detect"');
  }

  if (options.sourceType === 'unambiguous') {
    // Clear source type so that it will be detected from the contents of the file
    delete options.sourceType;
  } else if (
    options.sourceType != null &&
    options.sourceType !== 'script' &&
    options.sourceType !== 'module'
  ) {
    throw new Error(
      'sourceType option must be "script", "module", or "unambiguous" if set',
    );
  }

  if (options.enableExperimentalComponentSyntax == null) {
    options.enableExperimentalComponentSyntax = true; // Enable by default
  }

  if (options.enableExperimentalFlowMatchSyntax == null) {
    options.enableExperimentalFlowMatchSyntax = true; // Enable by default
  }

  // Record syntax: upstream defaults `enableExperimentalFlowRecordSyntax` to
  // `true`. The fork's WASM cwrap (FlowParser.js) reads `options.enableRecords`
  // — a separate, OCaml-aligned key intentionally mirroring
  // `default_parse_options`. To preserve the upstream-compatible API surface
  // on the outside while keeping the OCaml-aligned key on the WASM boundary,
  // bridge: default the upstream key, then mirror it onto the cwrap key. The
  // mirroring belongs here (the public-API layer) and not at the wasm
  // boundary, because the wasm fixture runner under
  // `__tests__/runWasmFixtures.js` calls `FlowParser.parse` directly and
  // expects OCaml's strict defaults to apply.
  if (options.enableExperimentalFlowRecordSyntax == null) {
    options.enableExperimentalFlowRecordSyntax = true; // Enable by default
  }
  options.enableRecords = options.enableExperimentalFlowRecordSyntax;

  // Enum syntax: upstream hermes-parser always parses Flow `enum` syntax. The
  // fork's WASM cwrap reads `options.enableEnums` — an OCaml-aligned key that
  // defaults to false in `default_parse_options`. Default `enableEnums` to
  // true so unflagged hermes-parser-style callers can parse `enum X { A }`
  // without opt-in. Same wasm-fixture-runner consideration as `enableRecords`.
  if (options.enableEnums == null) {
    options.enableEnums = true;
  }

  // Decorators: upstream hermes-parser supports stage-1 decorators by default.
  // The fork's WASM cwrap reads `enableExperimentalDecorators`, which
  // defaults to false in OCaml's `default_parse_options`. Default true on the
  // JS surface to match upstream. Same wasm-fixture-runner consideration as
  // `enableRecords`.
  if (options.enableExperimentalDecorators == null) {
    options.enableExperimentalDecorators = true;
  }

  options.tokens = options.tokens === true;
  options.allowReturnOutsideFunction =
    options.allowReturnOutsideFunction === true;

  return options;
}

declare function parse(
  code: string,
  opts: $ReadOnly<{...ParserOptions, babel: true}>,
): BabelFile;
// eslint-disable-next-line no-redeclare
declare function parse(
  code: string,
  opts?:
    | $ReadOnly<{...ParserOptions, babel?: false | void}>
    | $ReadOnly<{...ParserOptions, babel: false}>,
): ESTreeProgram;

// eslint-disable-next-line no-redeclare
export function parse(
  code: string,
  opts?: ParserOptions,
): BabelFile | ESTreeProgram {
  const options = getOptions(opts);

  // Resolve `flow: 'all' | 'detect'` to the Rust parser's option set.
  // `flow: 'all'` always parses Flow type syntax (`enableTypes: true`).
  // `flow: 'detect'` opts the Rust parser into docblock-pragma scanning
  // (`enableTypesPragmaDetection: true`); the Rust lexer mirrors C++
  // hermes-parser `parser::hasFlowPragma` and overrides `enableTypes`
  // based on whether the `@flow` pragma is present.
  if (options.flow === 'all') {
    options.enableTypes = true;
  } else {
    options.enableTypesPragmaDetection = true;
  }

  // Flow Rust parser outputs ESTree-compatible AST directly. The wire→ESTree
  // loc/range normalization (source-filename assignment, `node.range` synthesis
  // from `loc.rangeStart`/`rangeEnd`, then cleanup of those wire-only loc
  // fields) is done at the parser level inside `FlowParser.parse` so callers
  // that bypass this hermes-parser-compatibility wrapper still get canonical
  // ESTree loc/range. See FlowParser.js.
  const ast = FlowParser.parse(code, options);

  const estreeAST = ast;

  if (options.babel !== true) {
    return estreeAST;
  }

  // `babel: true` overload — reduce the ESTree AST through the same syntax
  // lowering pipeline upstream uses (Enum/Match/Component/Record syntax,
  // strip Flow types), then convert ESTree → Babel AST shape.
  const loweredESTreeAST = [
    options.transformOptions?.TransformEnumSyntax?.enable
      ? TransformEnumSyntax.transformProgram
      : null,
    TransformMatchSyntax.transformProgram,
    TransformComponentSyntax.transformProgram,
    TransformRecordSyntax.transformProgram,
    StripFlowTypesForBabel.transformProgram,
  ].reduce((ast, transform) => transform?.(ast, options) ?? ast, estreeAST);

  return TransformESTreeToBabel.transformProgram(loweredESTreeAST, options);
}

export type {ParserOptions} from './ParserOptions';
export * from './ParserOptions';
export * from './traverse/SimpleTraverser';
export * from './transform/SimpleTransform';
export * from './traverse/getVisitorKeys';
export {FlowVisitorKeys};
export * as astArrayMutationHelpers from './transform/astArrayMutationHelpers';
export * as astNodeMutationHelpers from './transform/astNodeMutationHelpers';
export {default as mutateESTreeASTForPrettier} from './utils/mutateESTreeASTForPrettier';

const Transforms = {
  transformEnumSyntax: TransformEnumSyntax.transformProgram,
  transformMatchSyntax: TransformMatchSyntax.transformProgram,
  transformComponentSyntax: TransformComponentSyntax.transformProgram,
  transformRecordSyntax: TransformRecordSyntax.transformProgram,
  stripFlowTypesForBabel: StripFlowTypesForBabel.transformProgram,
  stripFlowTypes: StripFlowTypes.transformProgram,
};
export {Transforms};
