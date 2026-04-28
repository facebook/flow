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
import {getModuleDocblock} from './getModuleDocblock';
import FlowVisitorKeys from './generated/ESTreeVisitorKeys';
import * as TransformComponentSyntax from './estree/TransformComponentSyntax';
import * as TransformEnumSyntax from './estree/TransformEnumSyntax';
import * as TransformMatchSyntax from './estree/TransformMatchSyntax';
import * as TransformRecordSyntax from './estree/TransformRecordSyntax';
import * as StripFlowTypesForBabel from './estree/StripFlowTypesForBabel';
import * as TransformESTreeToBabel from './babel/TransformESTreeToBabel';
import * as StripFlowTypes from './estree/StripFlowTypes';
import {ParserOptionsKeys} from './ParserOptions';

const DEFAULTS = {
  flow: ('detect': 'detect'),
};

// Scan `code`'s docblock for an `@flow` pragma. Mirrors C++ hermes-parser
// `parser::hasFlowPragma` + `parser::getCommentsInDocBlock` semantics
// (xplat/static_h/lib/Parser/FlowHelpers.cpp:17-82): walk leading whitespace,
// `//` and `/* ... */` comments, and directive-prologue string literals
// (each optionally followed by `;`) up to the first non-directive token,
// then in each collected comment search for `@flow` followed by a non-word
// boundary (end of comment, or any character outside `[A-Za-z0-9_]`). The
// Rust parser has no pragma-conditional type parsing — pragma detection is
// the JS adapter's job here so the Rust port stays OCaml-faithful (OCaml
// `default_parse_options` keeps `types: true` regardless of pragma).
function hasFlowPragma(code: string): boolean {
  const len = code.length;
  let i = 0;
  while (i < len) {
    const ch = code.charCodeAt(i);
    // Whitespace
    if (ch === 0x20 || ch === 0x09 || ch === 0x0a || ch === 0x0d) {
      i++;
      continue;
    }
    // Comments
    if (ch === 0x2f /* '/' */ && i + 1 < len) {
      const next = code.charCodeAt(i + 1);
      if (next === 0x2f /* '/' */) {
        i += 2;
        const start = i;
        while (i < len) {
          const c = code.charCodeAt(i);
          if (c === 0x0a || c === 0x0d) break;
          i++;
        }
        if (commentHasFlowPragma(code, start, i)) return true;
        continue;
      }
      if (next === 0x2a /* '*' */) {
        i += 2;
        const start = i;
        while (
          i + 1 < len &&
          !(code.charCodeAt(i) === 0x2a && code.charCodeAt(i + 1) === 0x2f)
        ) {
          i++;
        }
        const end = i;
        if (i + 1 < len) {
          i += 2; // skip the closing `*/`
        }
        if (commentHasFlowPragma(code, start, end)) return true;
        continue;
      }
    }
    // String-literal directive prologue
    if (ch === 0x22 /* '"' */ || ch === 0x27 /* "'" */) {
      const quote = ch;
      i++;
      while (i < len) {
        const c = code.charCodeAt(i);
        if (c === 0x5c /* '\\' */ && i + 1 < len) {
          // Skip the escape character; common case is `\"`/`\\`/`\n`.
          i += 2;
          continue;
        }
        if (c === quote) {
          i++;
          break;
        }
        i++;
      }
      continue;
    }
    // Optional `;` between directives
    if (ch === 0x3b /* ';' */) {
      i++;
      continue;
    }
    // First non-directive token: stop. Anything past this point is regular
    // code, not docblock.
    break;
  }
  return false;
}

function commentHasFlowPragma(
  code: string,
  start: number,
  end: number,
): boolean {
  let from = start;
  while (from < end) {
    const idx = code.indexOf('@flow', from);
    if (idx === -1 || idx >= end) return false;
    const after = idx + 5;
    if (after >= end) return true;
    const c = code.charCodeAt(after);
    const isWord =
      (c >= 0x30 && c <= 0x39) /* 0-9 */ ||
      (c >= 0x41 && c <= 0x5a) /* A-Z */ ||
      (c >= 0x61 && c <= 0x7a) /* a-z */ ||
      c === 0x5f; /* _ */
    if (!isWord) return true;
    from = idx + 1;
  }
  return false;
}

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
  // `true`. The fork's WASM cwrap (FlowParser.js:215) reads `options.enableRecords`
  // (slot 13) — a separate, OCaml-aligned key intentionally mirroring
  // `default_parse_options` (see FlowParser.js:48-50, :213-214). To preserve the
  // upstream-compatible API surface on the outside while keeping the OCaml-aligned
  // key on the WASM boundary, bridge: default the upstream key, then mirror it
  // onto the cwrap key. Naive copy of upstream default alone would be a silent
  // no-op because the cwrap reads a different field.
  if (options.enableExperimentalFlowRecordSyntax == null) {
    options.enableExperimentalFlowRecordSyntax = true; // Enable by default
  }
  options.enableRecords = options.enableExperimentalFlowRecordSyntax;

  // Enum syntax: upstream hermes-parser always parses Flow `enum` syntax. The
  // fork's WASM cwrap (FlowParser.js:212) reads `options.enableEnums` — an
  // OCaml-aligned key that defaults to false in `default_parse_options`. To
  // preserve hermes-parser parity on the JS surface, default `enableEnums` to
  // true so unflagged callers can parse `enum X { A }` without opt-in.
  if (options.enableEnums == null) {
    options.enableEnums = true;
  }

  // Decorators: upstream hermes-parser supports stage-1 decorators by default
  // (verified against 0.35.0 — `@dec class C {}` parses without explicit
  // opt-in). The fork's WASM cwrap (FlowParser.js:205) reads
  // `enableExperimentalDecorators`, which defaults to false in OCaml's
  // `default_parse_options`. Default true on the JS surface to match upstream.
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

  // Resolve `flow: 'all' | 'detect'` to a concrete `enableTypes` bool before
  // calling the Rust parser. `flow: 'all'` always parses Flow type syntax;
  // `flow: 'detect'` (the default) parses Flow type syntax only when an
  // `@flow` pragma is present in the docblock. The Rust parser does not
  // know about the pragma — it just honours `enableTypes`. Pragma detection
  // mirrors C++ hermes-parser ParseFlowSetting (no OCaml equivalent — the
  // JS adapter is the right layer for this).
  options.enableTypes =
    options.flow === 'all' ? true : hasFlowPragma(code);

  // Flow Rust parser outputs ESTree-compatible AST directly. The wire→ESTree
  // loc/range normalization (source-filename assignment, `node.range` synthesis
  // from `loc.rangeStart`/`rangeEnd`, then cleanup of those wire-only loc
  // fields) is done at the parser level inside `FlowParser.parse` so callers
  // that bypass this hermes-parser-compatibility wrapper still get canonical
  // ESTree loc/range. See FlowParser.js.
  const ast = FlowParser.parse(code, options);

  // Adapter behavior parity with hermes-parser: parse() throws on the first
  // parse error. The Rust parser surfaces errors via `program.errors` (always
  // present, possibly empty) so any caller using the raw FlowParser.parse can
  // still inspect them. Format mirrors HermesASTAdapter.formatError —
  // `${message} (${line}:${column})`. Source-line/caret rendering that Hermes
  // bakes into its error string is not available here; the loc property
  // carries the same coordinate so consumers can reconstruct it themselves.
  //
  // `allowReturnOutsideFunction` is honored at the adapter layer rather than
  // in the Rust port: OCaml flow_parser's `parse_options` has no equivalent
  // gate (Parse_error.IllegalReturn is raised unconditionally), so adding a
  // Rust option would extend the port beyond its OCaml ancestor. Instead,
  // filter the specific diagnostic out when the consumer asked for the
  // hermes-parser-style relaxation. The ReturnStatement AST node is produced
  // regardless, so dropping the error is the only behavioural difference.
  let errors = ast.errors;
  if (
    errors != null &&
    errors.length > 0 &&
    options.allowReturnOutsideFunction === true
  ) {
    errors = errors.filter(e => e.message !== 'Illegal return statement');
  }
  // hermes-parser parity (verified against 0.35.0): an invalid RegExp flag
  // (e.g. `/foo/qq`) is *not* a parse error — the regex literal is
  // syntactically well-formed and Hermes leaves the value-construction
  // failure to the JS `RegExp` constructor at adapter time. Strip the
  // OCaml flow_parser diagnostic so the Literal node is returned with
  // `value: null` (set by the regex Literal fixup) instead of throwing.
  if (errors != null && errors.length > 0) {
    errors = errors.filter(
      e => !e.message.startsWith('Invalid flags supplied to RegExp constructor'),
    );
  }
  if (errors != null && errors.length > 0) {
    const first = errors[0];
    const line = first.loc.start.line;
    const column = first.loc.start.column;
    const syntaxError = new SyntaxError(
      `${first.message} (${line}:${column})`,
    );
    // $FlowExpectedError[prop-missing]
    syntaxError.loc = {line, column};
    throw syntaxError;
  }

  // Per-node ESTree adapter fixups applied during a single tree walk. These
  // mirror upstream HermesToESTreeAdapter behavior — see ADAPTER_GAPS.md for
  // the full audit. Lives here (not in FlowParserNodeDeserializers.js, which
  // is @generated and overwritten by codegen). Track visited nodes so a
  // malformed AST with a cycle doesn't blow the stack.
  const visitedNodes = new WeakSet();
  function applyFixupsWalk(node) {
    if (node == null || typeof node !== 'object') {
      return;
    }
    if (visitedNodes.has(node)) {
      return;
    }
    visitedNodes.add(node);
    if (typeof node.type === 'string') {
      applyPerNodeFixups(node, code);
    }
    for (const key of Object.keys(node)) {
      const val = node[key];
      if (Array.isArray(val)) {
        for (const child of val) {
          applyFixupsWalk(child);
        }
      } else if (val != null && typeof val === 'object' && val.type != null) {
        applyFixupsWalk(val);
      }
    }
  }
  applyFixupsWalk(ast);

  // Adapter fixup (HermesToESTreeAdapter.mapChainExpression, lines 245-348):
  // Hermes/Flow emit OptionalMemberExpression / OptionalCallExpression nodes
  // for optional chains; ESTree wants non-optional Member/CallExpression with
  // `optional: true`, wrapped in a single ChainExpression at the chain root.
  // The algorithm is: depth-first; when leaving a Member/Call/Optional* node,
  // (a) if the child (object/callee/expression) is a ChainExpression, unwrap
  // it (lift its `expression`); (b) if the node was originally Optional*,
  // strip the `Optional` prefix and set `optional` from the original; if the
  // node was a plain MemberExpression/CallExpression (a parenthesis boundary)
  // set `optional = false`; (c) if either the original was optional or the
  // child was unwrappable, wrap the node in a ChainExpression. The
  // `isChildUnwrappable` test excludes plain Member/CallExpression children
  // because `(x?.y).z` is semantically different from `x?.y.z` and Hermes
  // marks the parenthesis boundary by emitting a non-Optional kind.
  function rewriteChain(node) {
    if (node == null || typeof node !== 'object') {
      return node;
    }
    if (Array.isArray(node)) {
      for (let i = 0; i < node.length; i++) {
        node[i] = rewriteChain(node[i]);
      }
      return node;
    }
    if (typeof node.type !== 'string') {
      return node;
    }
    // Recurse into all child node-typed properties first (depth-first).
    for (const key of Object.keys(node)) {
      const val = node[key];
      if (Array.isArray(val)) {
        for (let i = 0; i < val.length; i++) {
          val[i] = rewriteChain(val[i]);
        }
      } else if (val != null && typeof val === 'object' && val.type != null) {
        node[key] = rewriteChain(val);
      }
    }
    if (
      node.type !== 'MemberExpression' &&
      node.type !== 'OptionalMemberExpression' &&
      node.type !== 'CallExpression' &&
      node.type !== 'OptionalCallExpression'
    ) {
      return node;
    }
    const isOptional = node.optional === true;
    let child;
    let childKey;
    if (node.type.endsWith('MemberExpression')) {
      child = node.object;
      childKey = 'object';
    } else {
      child = node.callee;
      childKey = 'callee';
    }
    const isChildUnwrappable =
      child != null &&
      typeof child === 'object' &&
      child.type === 'ChainExpression' &&
      // (x?.y).z is semantically different to `x?.y.z`.
      // In the un-parenthesised case `.z` is only executed if and only if `x?.y` returns a non-nullish value.
      // In the parenthesised case, `.z` is **always** executed, regardless of the return of `x?.y`.
      // As such the AST is different between the two cases.
      //
      // In the hermes AST - any member part of a non-short-circuited optional chain is represented with `OptionalMemberExpression`
      // so if we see a `MemberExpression`, then we know we've hit a parenthesis boundary.
      node.type !== 'MemberExpression' &&
      node.type !== 'CallExpression';
    if (node.type.startsWith('Optional')) {
      node.type = node.type.replace('Optional', '');
      node.optional = isOptional;
    } else {
      node.optional = false;
    }
    if (!isChildUnwrappable && !isOptional) {
      return node;
    }
    if (isChildUnwrappable) {
      node[childKey] = child.expression;
    }
    return {
      type: 'ChainExpression',
      expression: node,
      loc: node.loc,
      range: node.range,
    };
  }
  rewriteChain(ast);

  // Adapter fixup: when `tokens: true` the Rust serializer emits each token
  // with `range` and `loc.source` (mirroring the AST node shape) and appends
  // a trailing zero-width `{type: 'Punctuator', value: ''}` end-of-input
  // sentinel. Upstream hermes-parser tokens carry only `type`, `loc`, and
  // `value` (verified against HermesParserDeserializer.deserializeTokens,
  // lines 202-218) and have no EOF sentinel. Strip the extras so the public
  // shape matches.
  if (options.tokens === true && Array.isArray(ast.tokens)) {
    if (ast.tokens.length > 0) {
      const last = ast.tokens[ast.tokens.length - 1];
      if (
        last != null &&
        last.type === 'Punctuator' &&
        last.value === ''
      ) {
        ast.tokens.pop();
      }
    }
    for (const tok of ast.tokens) {
      if (tok == null) {
        continue;
      }
      delete tok.range;
      if (tok.loc != null) {
        delete tok.loc.source;
      }
    }
  }

  // Adapter behavior parity with HermesASTAdapter (lines 113-160): the
  // returned `sourceType` defaults to `'script'` and is promoted to `'module'`
  // only when the program contains a value-kind import or export. Type-only
  // imports/exports (`import type`, `export type`) keep the program in script
  // mode. When the caller pinned `sourceType: 'script'` or `'module'` we
  // honor it verbatim; `'unambiguous'` was already deleted from `options` in
  // `getOptions` so it falls through to detection.
  if (options.sourceType === 'script' || options.sourceType === 'module') {
    ast.sourceType = options.sourceType;
  } else {
    ast.sourceType = detectSourceType(ast);
  }

  // Adapter fixup (#30): hermes-parser surfaces `Program.interpreter` as the
  // hashbang-derived `InterpreterDirective` node (or null when no hashbang
  // is present). The Rust serializer doesn't write the slot at all; default
  // to null so the public AST shape matches upstream. The full
  // hashbang-detection port is tracked separately — `null` is the correct
  // value for the overwhelmingly-common no-hashbang case.
  if (!('interpreter' in ast)) {
    ast.interpreter = null;
  }

  // Adapter fixup (HermesToESTreeAdapter.mapProgram, lines 100-107):
  // attach the module docblock derived from the leading block comment.
  ast.docblock = getModuleDocblock(ast);

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

  return TransformESTreeToBabel.transformProgram(
    loweredESTreeAST,
    options,
    code,
  );
}

// Module-vs-script detection mirroring HermesASTAdapter.setModuleSourceType
// (HermesASTAdapter.js:117-160). Walks the top-level program body for
// import/export forms; a single value-kind import or export is enough to
// classify the program as a module. Type-only imports and re-exports keep the
// program in script mode. We check only the top-level body (matching upstream)
// because nested imports are not legal at runtime.
function detectSourceType(program) {
  if (!Array.isArray(program.body)) {
    return 'script';
  }
  for (const stmt of program.body) {
    if (stmt == null) {
      continue;
    }
    switch (stmt.type) {
      case 'ImportDeclaration':
        if (stmt.importKind === 'value' || stmt.importKind == null) {
          return 'module';
        }
        break;
      case 'ExportDefaultDeclaration':
        return 'module';
      case 'ExportNamedDeclaration':
      case 'ExportAllDeclaration':
        if (stmt.exportKind === 'value' || stmt.exportKind == null) {
          return 'module';
        }
        break;
    }
  }
  return 'script';
}

// Per-node fixups that mirror HermesToESTreeAdapter's mapNode dispatch. Lives
// here (not in FlowParserNodeDeserializers.js) because that file is @generated
// from the codegen template and would be overwritten on the next regen.
// Recover the `trailingComma: boolean` field that upstream hermes-parser
// emits on `ArrayExpression` but our Rust serializer does not (because the
// underlying OCaml `flow_ast.ml` `module Array` has no such bit). Scan the
// source between the last element's end and the closing `]`, skipping
// whitespace and `//` / `/* */` comments. Returns `false` for an empty array.
function computeArrayTrailingComma(node, code) {
  if (typeof code !== 'string') {
    return false;
  }
  const elements = node.elements;
  if (!Array.isArray(elements) || elements.length === 0) {
    return false;
  }
  // The closing `]` is the last character of the ArrayExpression's range.
  // The starting position to scan from is the end of the last non-null
  // element (or the position right after the previous comma if the last
  // slot is a hole).
  if (!Array.isArray(node.range)) {
    return false;
  }
  const closeBracket = node.range[1] - 1;
  // Find the last non-hole element with a numeric `range[1]`. For trailing
  // holes (`[1,,]`) the source between the last element's end and `]`
  // contains the trailing comma we want to detect.
  let lastEnd = node.range[0] + 1; // just past the `[`
  for (let i = elements.length - 1; i >= 0; i--) {
    const el = elements[i];
    if (el != null && Array.isArray(el.range)) {
      lastEnd = el.range[1];
      break;
    }
  }
  let i = lastEnd;
  while (i < closeBracket) {
    const c = code.charCodeAt(i);
    // whitespace
    if (c === 0x20 || c === 0x09 || c === 0x0a || c === 0x0d) {
      i++;
      continue;
    }
    // `//` line comment
    if (c === 0x2f && code.charCodeAt(i + 1) === 0x2f) {
      i += 2;
      while (i < closeBracket && code.charCodeAt(i) !== 0x0a) {
        i++;
      }
      continue;
    }
    // `/* */` block comment
    if (c === 0x2f && code.charCodeAt(i + 1) === 0x2a) {
      i += 2;
      while (
        i + 1 < closeBracket &&
        !(code.charCodeAt(i) === 0x2a && code.charCodeAt(i + 1) === 0x2f)
      ) {
        i++;
      }
      i += 2;
      continue;
    }
    // any non-whitespace, non-comment character before `]` other than `,`
    // means there's no trailing comma; a `,` here means there is one.
    return c === 0x2c;
  }
  return false;
}

function applyPerNodeFixups(node, code) {
  switch (node.type) {
    case 'Literal':
      // Adapter fixup (#35 over-emit): upstream Hermes does not carry
      // `bigint` or `regex` side fields on non-bigint/non-regex literals.
      // The Rust serializer always writes both slots (with null for the
      // unused kind); strip when null so the public AST shape matches.
      if (node.bigint == null) {
        delete node.bigint;
      }
      if (node.regex == null) {
        delete node.regex;
      }
      // Adapter fixup (HermesToESTreeAdapter.mapSimpleLiteral, lines 116-138):
      // upstream's switch is on the original HermesAST node type (NullLiteral /
      // BooleanLiteral / StringLiteral / JSXStringLiteral / NumericLiteral /
      // BigIntLiteral / RegExpLiteral). Our wire never carries those distinct
      // kinds — Rust collapses everything into a single Literal record with a
      // raw side and `regex`/`bigint` side fields plus a `value` already
      // populated for boolean / number / string. We synthesize `literalType`
      // from the value/side-field shape; the mapping is semantically
      // equivalent to upstream's switch.
      if (node.literalType == null) {
        if (node.regex != null) {
          node.literalType = 'regexp';
        } else if (node.bigint != null) {
          node.literalType = 'bigint';
        } else if (node.value === null) {
          node.literalType = 'null';
        } else if (typeof node.value === 'boolean') {
          node.literalType = 'boolean';
        } else if (typeof node.value === 'number') {
          node.literalType = 'numeric';
        } else if (typeof node.value === 'string') {
          node.literalType = 'string';
        } else {
          node.literalType = null;
        }
      }
      // Adapter fixup (HermesToESTreeAdapter.mapRegExpLiteral, lines 157-176):
      // build a RegExp from pattern/flags. `new RegExp` can throw if the host
      // engine doesn't recognise the flags (the wire still carries them so
      // consumers can see the original source); swallow with null per upstream.
      // The Rust parser (expression_parser.rs::regexp) strips invalid flag
      // characters from the wire's `flags` field but preserves them verbatim
      // in `raw` (`/<pattern>/<raw_flags>`). Hermes's mapRegExpLiteral expects
      // the raw flags so the test can observe the original (invalid) source
      // and `new RegExp` throws — recover them from `raw`.
      if (node.regex != null) {
        if (typeof node.raw === 'string') {
          const lastSlash = node.raw.lastIndexOf('/');
          if (lastSlash >= 0) {
            node.regex.flags = node.raw.slice(lastSlash + 1);
          }
        }
        try {
          node.value = new RegExp(node.regex.pattern, node.regex.flags);
        } catch (e) {
          node.value = null;
        }
      }
      // Adapter fixup (HermesASTAdapter.getBigIntLiteralValue, lines 173-188):
      // coerce the pre-cleaned `bigint` string (Rust strips the trailing `n`
      // and `_` separators — see write_bigint_literal in serializer.rs) to a
      // BigInt value when the host environment supports it.
      if (node.bigint != null) {
        // coerce the string to a bigint value if supported by the environment
        node.value = typeof BigInt === 'function' ? BigInt(node.bigint) : null;
      }
      break;
    case 'ImportSpecifier':
      // Adapter fixup (HermesASTAdapter.mapImportSpecifier, lines 140-146):
      // collapse 'value' to null so unannotated specifiers carry
      // `importKind: null` while `type`/`typeof` specifiers retain their
      // explicit annotation.
      if (node.importKind === 'value') {
        node.importKind = null;
      }
      break;
    case 'ClassDeclaration':
    case 'ClassExpression':
      // Adapter fixup (#33 over-emit): `abstract` is a TypeScript-only field
      // that upstream Hermes does not emit. Rust always serializes it (false
      // by default). Strip when false to match upstream snapshots.
      if (node.abstract === false) {
        delete node.abstract;
      }
      break;
    case 'MethodDefinition':
      // Adapter fixup (#34 over-emit): `override` and `tsAccessibility` are
      // TypeScript-only fields not present in Hermes' MethodDefinition shape.
      // The Rust serializer writes both unconditionally; strip the defaults
      // (false / null) so the public AST matches upstream. MethodDefinition
      // also has no `tsModifiers` slot (verified against hermes-parser
      // 0.35.0 — the slot is PropertyDefinition-only).
      if (node.override === false) {
        delete node.override;
      }
      if (node.tsAccessibility == null) {
        delete node.tsAccessibility;
      }
      if (node.tsModifiers == null) {
        delete node.tsModifiers;
      }
      break;
    case 'PropertyDefinition':
      // Same TS-only-field strip as MethodDefinition (#34); PropertyDefinition
      // additionally carries `tsAccessibility` and `override`. PropertyDefinition
      // *does* surface `tsModifiers: null` upstream (verified against
      // hermes-parser 0.35.0); add the slot when missing so the public AST
      // matches upstream.
      if (node.override === false) {
        delete node.override;
      }
      if (node.tsAccessibility == null) {
        delete node.tsAccessibility;
      }
      if (!('tsModifiers' in node)) {
        node.tsModifiers = null;
      }
      break;
    case 'ArrayPattern':
      // Adapter fixup (#48 over-emit): `optional` on ArrayPattern is a
      // Hermes-only Flow extension that the Rust serializer always writes.
      // Strip the default (false) to match upstream snapshots.
      if (node.optional === false) {
        delete node.optional;
      }
      break;
    case 'ArrayExpression':
      // Adapter fixup (#10 under-emit): upstream hermes-parser surfaces
      // `trailingComma: boolean` on ArrayExpression (not part of the ESTree
      // spec — see hermes-estree/src/types.js: `// this is not part of the
      // ESTree spec, but hermes emits it`). The Rust serializer
      // (`fbcode/flow/rust_port/crates/flow_parser_wasm/src/serializer.rs`,
      // `serialize_array_expression` ~line 3216) writes only `elements` and
      // never emits the slot; the underlying Flow OCaml AST itself does
      // not carry a `trailing_comma` bit on `Expression.Array`
      // (`flow_ast.ml` `module Array`: only `elements` + `comments`).
      // Recover the real value by scanning the source between the last
      // element's end and the closing `]`: a `,` in that span (skipping
      // whitespace and `//`/`/* */` comments) means the array had a
      // trailing comma. The closure below captures `code` from the outer
      // `parse()` call.
      if (!('trailingComma' in node)) {
        node.trailingComma = computeArrayTrailingComma(node, code);
      }
      break;
    case 'ObjectPattern':
      // Adapter fixup (#49 over-emit): same as ArrayPattern — `optional` is a
      // Hermes-only Flow extension; strip when false to match upstream.
      if (node.optional === false) {
        delete node.optional;
      }
      break;
    case 'ExportDefaultDeclaration':
      // Adapter fixup (#49 over-emit): hermes-parser does not surface
      // `exportKind` on ExportDefaultDeclaration (verified against 0.35.0 —
      // only `ExportNamedDeclaration` carries it). The Rust serializer
      // emits the slot defaulted to 'value'; strip when 'value' so the
      // public AST matches upstream.
      if (node.exportKind === 'value') {
        delete node.exportKind;
      }
      break;
    case 'PrivateIdentifier':
      // Adapter fixup (#38 over-emit): hermes-parser's PrivateIdentifier
      // (verified against 0.35.0) carries only `type/name` plus loc/range.
      // The Rust serializer inherits the Identifier shape and emits
      // `optional` and `typeAnnotation`; strip those when null/false to match
      // upstream. Note: regular Identifier *does* carry both keys upstream so
      // is not stripped here — only PrivateIdentifier needs the cleanup.
      if (node.optional === false) {
        delete node.optional;
      }
      if (node.typeAnnotation == null) {
        delete node.typeAnnotation;
      }
      break;
    case 'ArrowFunctionExpression':
      // Adapter fixup (#51 over-emit): arrow functions cannot be generators,
      // so upstream hermes-parser does not emit the slot. Our serializer
      // inherits the FunctionExpression shape and writes `generator: false`;
      // strip when false to match upstream. The Babel snapshots in
      // ArrowFunctionExpression-test.js also rely on this.
      if (node.generator === false) {
        delete node.generator;
      }
      break;
    case 'ExportSpecifier':
      // Adapter fixup (#52 over-emit): hermes-parser does not surface a
      // per-specifier `exportKind` — the parent `ExportNamedDeclaration`'s
      // `exportKind` is the source of truth. The Rust serializer emits
      // `exportKind: 'value'` defaulted on each specifier; strip it so the
      // public AST shape matches upstream.
      if (node.exportKind === 'value') {
        delete node.exportKind;
      }
      break;
    case 'ObjectTypeProperty':
      // Adapter fixup (#44 over-emit): ObjectTypeProperty is a Flow type-grammar
      // node and never carries the class-member fields `abstract`, `computed`,
      // `init`, `override`, or `tsAccessibility` that the Rust serializer emits
      // when inheriting the class-member-like base shape. Strip them so the
      // type-grammar snapshot matches upstream.
      delete node.abstract;
      delete node.computed;
      delete node.init;
      delete node.override;
      delete node.tsAccessibility;
      break;
    case 'ObjectTypeIndexer':
      // Adapter fixup (#44 over-emit, sibling of ObjectTypeProperty):
      // upstream hermes-parser does not carry an `optional` slot on
      // ObjectTypeIndexer. Strip when false so the public AST shape matches.
      if (node.optional === false) {
        delete node.optional;
      }
      break;
    case 'DeclareVariable':
      // Adapter fixup (#47): the OCaml AST keeps DeclareVariable as a
      // VariableDeclaration-shaped node (declarations: list, kind: string).
      // Upstream hermes-parser emits a single `id` (the declarator's id) plus
      // `kind`. Lift the first declarator's id and drop `declarations`.
      if (
        Array.isArray(node.declarations) &&
        node.declarations.length === 1 &&
        node.declarations[0] != null &&
        node.declarations[0].id != null
      ) {
        node.id = node.declarations[0].id;
        delete node.declarations;
      }
      break;
    case 'DeclareNamespace':
      // Adapter fixup (#32): hermes-parser does not surface `implicitDeclare`,
      // `keyword`, or `global` on DeclareNamespace (verified against 0.35.0 —
      // only `id` and `body` are emitted). The Rust serializer carries these
      // OCaml-aligned discriminants for upstream-flow_parser fidelity; strip
      // them at the JS adapter layer for hermes-parser parity.
      delete node.implicitDeclare;
      delete node.keyword;
      delete node.global;
      break;
    case 'DeclareHook':
      // Adapter fixup (#54 over-emit): upstream hermes-parser's DeclareHook
      // carries only `type/loc/id` (verified in
      // xplat/static_h/tools/hermes-parser/js/hermes-parser/src/HermesParserNodeDeserializers.js).
      // The Rust serializer inherits extra declaration slots; strip them for
      // hermes-parser parity.
      delete node.implicitDeclare;
      delete node.typeAnnotation;
      break;
    case 'ComponentDeclaration':
      // Adapter fixup (#11 over-emit): `implicitDeclare` is a
      // DeclareNamespace-only field. Upstream hermes-parser's
      // `deserializeComponentDeclaration` emits only `type/loc/id/params/body/
      // typeParameters/rendersType/async` (verified in
      // xplat/static_h/tools/hermes-parser/js/hermes-parser/src/HermesParserNodeDeserializers.js).
      // The Rust serializer over-emits the slot; strip it unconditionally for
      // hermes-parser parity.
      delete node.implicitDeclare;
      break;
    case 'BigIntLiteralTypeAnnotation':
      // Adapter fixup (#43): upstream hermes-parser emits `bigint` (the raw
      // numeric string with the trailing `n` and any `_` separators stripped)
      // and `value` (BigInt(bigint)) on BigIntLiteralTypeAnnotation. Our Rust
      // serializer only emits `raw` (e.g. "1234n" / "12_34n") with `value: null`.
      // Synthesize `bigint` from `raw` and coerce `value` to BigInt when the
      // host environment supports it.
      if (node.raw != null && node.bigint == null) {
        node.bigint = node.raw.replace(/n$/, '').replace(/_/g, '');
      }
      if (node.bigint != null) {
        node.value = typeof BigInt === 'function' ? BigInt(node.bigint) : null;
      }
      break;
    case 'EnumDeclaration':
    case 'DeclareEnum':
      // Adapter fixup: upstream hermes-parser's `EnumDeclaration` and
      // `DeclareEnum` carry only `id` and `body` (verified against
      // hermes-parser 0.35.0). The Rust serializer emits an additional
      // `const: bool` discriminant for the OCaml-aligned binary protocol;
      // strip it for hermes-parser parity.
      delete node.const;
      break;
    case 'EnumBody':
      // Adapter fixup: the Rust serializer emits a generic `EnumBody` kind
      // (NodeKind 223) for all enum bodies, while upstream hermes-parser
      // exposes typed body kinds (`EnumStringBody`, `EnumNumberBody`,
      // `EnumBigIntBody`, `EnumBooleanBody`, `EnumSymbolBody`). Specialize
      // based on member type — Symbol bodies use `EnumDefaultedMember`s with
      // `explicitType: 'symbol'`; the other typed bodies are inferred from
      // their first typed member; pure-defaulted bodies fall through to
      // `EnumStringBody` (the upstream default for string-defaulted enums).
      // `explicitType` on the wire is a string sentinel ("symbol" or null);
      // upstream emits a boolean indicating whether the type was explicitly
      // annotated in source — coerce to the boolean shape.
      {
        const explicitTypeStr = node.explicitType;
        let bodyType = 'EnumStringBody';
        if (explicitTypeStr === 'symbol') {
          bodyType = 'EnumSymbolBody';
        } else if (Array.isArray(node.members) && node.members.length > 0) {
          const firstMember = node.members[0];
          if (firstMember != null) {
            switch (firstMember.type) {
              case 'EnumNumberMember':
                bodyType = 'EnumNumberBody';
                break;
              case 'EnumBigIntMember':
                bodyType = 'EnumBigIntBody';
                break;
              case 'EnumBooleanMember':
                bodyType = 'EnumBooleanBody';
                break;
              case 'EnumStringMember':
                bodyType = 'EnumStringBody';
                break;
              case 'EnumDefaultedMember':
                if (explicitTypeStr === 'string') {
                  bodyType = 'EnumStringBody';
                } else if (explicitTypeStr === 'number') {
                  bodyType = 'EnumNumberBody';
                } else if (explicitTypeStr === 'bigint') {
                  bodyType = 'EnumBigIntBody';
                } else if (explicitTypeStr === 'boolean') {
                  bodyType = 'EnumBooleanBody';
                }
                // else falls through to EnumStringBody default
                break;
            }
          }
        } else if (typeof explicitTypeStr === 'string') {
          // Empty body with explicit type annotation
          if (explicitTypeStr === 'string') {
            bodyType = 'EnumStringBody';
          } else if (explicitTypeStr === 'number') {
            bodyType = 'EnumNumberBody';
          } else if (explicitTypeStr === 'bigint') {
            bodyType = 'EnumBigIntBody';
          } else if (explicitTypeStr === 'boolean') {
            bodyType = 'EnumBooleanBody';
          }
        }
        node.type = bodyType;
        // explicitType on EnumSymbolBody is omitted entirely upstream;
        // on the typed bodies it's a boolean indicating whether the type
        // was explicit (e.g. `enum E of string`). The wire string is the
        // explicit type name when explicit, null otherwise.
        if (bodyType === 'EnumSymbolBody') {
          delete node.explicitType;
        } else {
          node.explicitType = typeof explicitTypeStr === 'string';
        }
      }
      break;
    case 'GenericTypeAnnotation':
      // Adapter fixup (HermesToESTreeAdapter.mapGenericTypeAnnotation, lines
      // 198-213): a Generic with no type arguments and an unqualified `this`
      // identifier collapses to the ESTree `ThisTypeAnnotation` leaf node.
      // The Rust serializer emits the underlying GenericTypeAnnotation
      // (matching OCaml flow-parser baseline; see node_kinds.rs commentary
      // on `ThisTypeAnnotation`), so the conversion lives here in the JS
      // adapter layer.
      if (
        node.typeParameters == null &&
        node.id != null &&
        node.id.type === 'Identifier' &&
        node.id.name === 'this'
      ) {
        node.type = 'ThisTypeAnnotation';
        delete node.id;
        delete node.typeParameters;
      }
      break;
    case 'ObjectTypeMappedTypeProperty':
      // Adapter fixup (#55 over-emit): upstream hermes-parser does not emit
      // `nameType` or `varianceOp` on ObjectTypeMappedTypeProperty. The Rust
      // serializer carries those newer wire slots, but when they are absent in
      // source they should not surface as explicit `null`s in the public AST.
      if (node.nameType == null) {
        delete node.nameType;
      }
      if (node.varianceOp == null) {
        delete node.varianceOp;
      }
      break;
    case 'TypeParameter':
      // Adapter fixup (#56): upstream hermes-parser stores `bound` directly as
      // the annotation node (for example NumberTypeAnnotation), while the Rust
      // serializer currently wraps some bounds in a TypeAnnotation node.
      // Unwrap the extra shell for parity.
      if (node.bound?.type === 'TypeAnnotation') {
        node.bound = node.bound.typeAnnotation;
      }
      break;
  }
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

// Re-export ParserOptionsKeys explicitly. `export * from './ParserOptions'`
// above already covers it, but a named re-export is more discoverable for
// downstream consumers and matches upstream's pattern of making the public API
// surface explicit at the bottom of the file.
export {ParserOptionsKeys};
