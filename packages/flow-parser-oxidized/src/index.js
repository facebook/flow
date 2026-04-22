/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 */

'use strict';

const FlowParser = require('./FlowParser');
const ParserOptions = require('./ParserOptions');

const DEFAULTS = {
  flow: 'detect',
};

function getOptions(opts) {
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

  options.tokens = options.tokens === true;
  options.allowReturnOutsideFunction =
    options.allowReturnOutsideFunction === true;

  return options;
}

function parse(code, opts) {
  const options = getOptions(opts);

  // Flow Rust parser outputs ESTree-compatible AST directly.
  const ast = FlowParser.parse(code, options);
  // Apply source location fixups (source filename, range array). Track
  // visited nodes so a malformed AST with a cycle doesn't blow the stack.
  // Track visited locs separately because the binary protocol carries
  // `rangeStart`/`rangeEnd` on the loc itself; we must read them before
  // deleting so callers see the standard ESTree `range` on the node and a
  // clean loc shape ({ start, end, source }) — even if a future change
  // ever made two nodes share a loc reference.
  const sourceFilename = options.sourceFilename ?? null;
  const visitedNodes = new WeakSet();
  const locRanges = new WeakMap();
  function fixLocs(node) {
    if (node == null || typeof node !== 'object') {
      return;
    }
    if (visitedNodes.has(node)) {
      return;
    }
    visitedNodes.add(node);
    if (node.loc != null) {
      let range = locRanges.get(node.loc);
      if (range == null) {
        node.loc.source = sourceFilename;
        range = [node.loc.rangeStart, node.loc.rangeEnd];
        locRanges.set(node.loc, range);
        delete node.loc.rangeStart;
        delete node.loc.rangeEnd;
      }
      node.range = range;
    }
    for (const key of Object.keys(node)) {
      const val = node[key];
      if (Array.isArray(val)) {
        for (const child of val) {
          fixLocs(child);
        }
      } else if (val != null && typeof val === 'object' && val.type != null) {
        fixLocs(val);
      }
    }
  }
  fixLocs(ast);
  ast.sourceType = options.sourceType ?? 'module';
  return ast;
}

module.exports = {
  parse,
  ParserOptionsKeys: ParserOptions.ParserOptionsKeys,
};
