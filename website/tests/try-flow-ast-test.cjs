/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @noflow
 * @format
 */

const assert = require('node:assert/strict');
const {test} = require('node:test');
const Module = require('node:module');
const path = require('node:path');
const {transformFileSync} = require('@babel/core');

const filename = path.resolve(
  __dirname,
  '../src/try-flow/formatAstForDisplay.js',
);
const compiled = new Module(filename, module);
compiled._compile(
  transformFileSync(filename, {
    configFile: false,
    babelrc: false,
    presets: ['@babel/preset-flow'],
    plugins: ['@babel/plugin-transform-modules-commonjs'],
  }).code,
  filename,
);
const formatAstForDisplay = compiled.exports.default;

// Filename metadata is omitted throughout the tree without changing the input.
test('omits location sources recursively and leaves the original AST intact', () => {
  const ast = {
    type: 'Program',
    loc: {source: null, start: {line: 1, column: 0}, end: {line: 2, column: 0}},
    body: [{type: 'Identifier', loc: {source: null}, name: 'example'}],
    comments: [{type: 'Line', loc: {source: null}, value: 'comment'}],
  };
  const original = JSON.stringify(ast);
  const displayed = formatAstForDisplay(ast);
  assert.equal(Object.hasOwn(displayed.loc, 'source'), false); // OK
  assert.equal(Object.hasOwn(displayed.body[0].loc, 'source'), false); // OK
  assert.equal(Object.hasOwn(displayed.comments[0].loc, 'source'), false); // OK
  assert.deepEqual(displayed.loc.start, ast.loc.start); // OK
  assert.equal(JSON.stringify(ast), original); // OK
});

// Module sources are AST nodes, not filename metadata.
test('preserves import and export sources', () => {
  const source = {type: 'Literal', value: 'example', loc: {source: null}};
  const ast = {
    body: [
      {type: 'ImportDeclaration', source},
      {type: 'ExportNamedDeclaration', source},
      {type: 'ExportNamedDeclaration', source: null},
    ],
  };
  const displayed = formatAstForDisplay(ast);
  assert.equal(displayed.body[0].source.value, 'example'); // OK
  assert.equal(displayed.body[1].source.value, 'example'); // OK
  assert.equal(displayed.body[2].source, null); // OK
  assert.equal(Object.hasOwn(displayed.body[0].source.loc, 'source'), false); // OK
});

// Locations can be hidden and restored without modifying the parser's AST.
test('hides loc and range recursively only when requested', () => {
  const loc = {
    source: null,
    start: {line: 1, column: 0},
    end: {line: 1, column: 1},
  };
  const ast = {
    type: 'Program',
    loc,
    range: [0, 1],
    body: [{type: 'Identifier', name: 'x', loc, range: [0, 1]}],
    comments: [{type: 'Line', value: 'comment', loc, range: [0, 1]}],
    errors: [{message: 'parse error', loc}],
  };
  const original = JSON.stringify(ast);
  const hidden = formatAstForDisplay(ast, true);
  for (const node of [
    hidden,
    hidden.body[0],
    hidden.comments[0],
    hidden.errors[0],
  ]) {
    assert.equal(Object.hasOwn(node, 'loc'), false); // OK
    assert.equal(Object.hasOwn(node, 'range'), false); // OK
  }
  assert.equal(hidden.body[0].name, 'x'); // OK
  const shown = formatAstForDisplay(ast, false);
  assert.deepEqual(shown.body[0].range, [0, 1]); // OK
  assert.deepEqual(shown.body[0].loc.start, loc.start); // OK
  assert.equal(Object.hasOwn(shown.body[0].loc, 'source'), false); // OK
  assert.equal(JSON.stringify(ast), original); // OK
});

// Primitive values, empty results, and unsupported-version messages are unchanged.
test('preserves non-object values and empty trees', () => {
  for (const value of [null, false, 0, '', 'AST output is not supported.']) {
    assert.equal(formatAstForDisplay(value), value); // OK
  }
  assert.deepEqual(formatAstForDisplay({}), {}); // OK
  assert.deepEqual(formatAstForDisplay([]), []); // OK
});
