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

// Node kinds lead each object, while other fields and array elements keep their order.
test('puts type first recursively without sorting other fields', () => {
  const ast = {
    body: [
      {name: 'first', optional: false, type: 'Identifier'},
      {value: 2, raw: '2', type: 'Literal'},
    ],
    sourceType: 'module',
    type: 'Program',
    metadata: {z: 1, a: 2},
  };
  const original = JSON.stringify(ast);
  for (const hideLocations of [false, true]) {
    const displayed = formatAstForDisplay(ast, hideLocations);
    assert.deepEqual(Object.keys(displayed), [
      'type',
      'body',
      'sourceType',
      'metadata',
    ]); // OK
    assert.deepEqual(Object.keys(displayed.body[0]), [
      'type',
      'name',
      'optional',
    ]); // OK
    assert.deepEqual(Object.keys(displayed.body[1]), ['type', 'value', 'raw']); // OK
    assert.deepEqual(Object.keys(displayed.metadata), ['z', 'a']); // OK
    assert.equal(displayed.body[0].name, 'first'); // OK
    assert.equal(displayed.body[1].value, 2); // OK
  }
  assert.equal(JSON.stringify(ast), original); // OK
});

// Only null/undefined object fields are omitted; other empty/falsy values remain.
test('hides null and undefined fields recursively without mutating input', () => {
  const ast = {
    missing: null,
    absent: undefined,
    body: [
      {name: 'x', optional: null, annotation: undefined, type: 'Identifier'},
    ],
    falseValue: false,
    zeroValue: 0,
    emptyString: '',
    emptyArray: [],
    emptyObject: {},
    type: 'Program',
  };
  const hidden = formatAstForDisplay(ast, false, true);
  assert.equal(Object.hasOwn(hidden, 'missing'), false); // OK
  assert.equal(Object.hasOwn(hidden, 'absent'), false); // OK
  assert.deepEqual(Object.keys(hidden.body[0]), ['type', 'name']); // OK
  assert.equal(hidden.falseValue, false); // OK
  assert.equal(hidden.zeroValue, 0); // OK
  assert.equal(hidden.emptyString, ''); // OK
  assert.deepEqual(hidden.emptyArray, []); // OK
  assert.deepEqual(hidden.emptyObject, {}); // OK
  assert.equal(Object.keys(hidden)[0], 'type'); // OK
  assert.equal(ast.missing, null); // OK
  assert.equal(Object.hasOwn(ast, 'absent'), true); // OK
  assert.equal(ast.body[0].optional, null); // OK
  assert.equal(Object.hasOwn(ast.body[0], 'annotation'), true); // OK
  const shown = formatAstForDisplay(ast, false, false);
  assert.equal(Object.hasOwn(shown, 'missing'), true); // OK
  assert.equal(Object.hasOwn(shown, 'absent'), true); // OK
  assert.equal(Object.hasOwn(shown.body[0], 'optional'), true); // OK
  assert.equal(Object.hasOwn(shown.body[0], 'annotation'), true); // OK
});

// Location filtering is independent, and null array slots must not shift indices.
test('combines empty-field and location filters without removing array slots', () => {
  const ast = {
    elements: [
      null,
      undefined,
      {name: 'x', optional: null, type: 'Identifier'},
    ],
    loc: {source: null, start: {line: 1, column: 0}, end: {line: 1, column: 1}},
    range: [0, 1],
    missing: null,
    type: 'ArrayExpression',
  };
  for (const hideLocations of [false, true]) {
    const displayed = formatAstForDisplay(ast, hideLocations, true);
    assert.equal(displayed.elements.length, 3); // OK
    assert.equal(displayed.elements[0], null); // OK
    assert.equal(displayed.elements[1], undefined); // OK
    assert.equal(displayed.elements[2].name, 'x'); // OK
    assert.equal(Object.hasOwn(displayed.elements[2], 'optional'), false); // OK
    assert.equal(Object.hasOwn(displayed, 'missing'), false); // OK
    assert.equal(Object.hasOwn(displayed, 'loc'), !hideLocations); // OK
    assert.equal(Object.hasOwn(displayed, 'range'), !hideLocations); // OK
  }
  const locationsHidden = formatAstForDisplay(ast, true, false);
  assert.equal(locationsHidden.missing, null); // OK
  assert.equal(Object.hasOwn(locationsHidden, 'loc'), false); // OK
});

// Primitive values, empty results, and unsupported-version messages are unchanged.
test('preserves non-object values and empty trees', () => {
  for (const value of [null, false, 0, '', 'AST output is not supported.']) {
    assert.equal(formatAstForDisplay(value), value); // OK
  }
  assert.deepEqual(formatAstForDisplay({}), {}); // OK
  assert.deepEqual(formatAstForDisplay([]), []); // OK
});
