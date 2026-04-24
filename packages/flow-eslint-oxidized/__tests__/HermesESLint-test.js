/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow strict-local
 * @format
 */

'use strict';

const {parseForESLint} = require('../src');

test('Parser produces ESTree AST', () => {
  expect(parseForESLint('const x = 1').ast).toMatchObject({
    type: 'Program',
    body: [
      {
        type: 'VariableDeclaration',
        kind: 'const',
        declarations: [
          {
            type: 'VariableDeclarator',
            id: {
              type: 'Identifier',
              name: 'x',
            },
            init: {
              type: 'Literal',
              value: 1,
            },
          },
        ],
      },
    ],
    tokens: [
      {
        type: 'Keyword',
        value: 'const',
      },
      {
        type: 'Identifier',
        value: 'x',
      },
      {
        type: 'Punctuator',
        value: '=',
      },
      {
        type: 'Numeric',
        value: '1',
      },
    ],
  });
});

test('Parses ambiguous Flow syntax', () => {
  expect(parseForESLint(`foo<T>(x)`).ast).toMatchObject({
    type: 'Program',
    body: [
      {
        type: 'ExpressionStatement',
        expression: {
          type: 'CallExpression',
        },
      },
    ],
  });
});

test('Parser allows return outside function', () => {
  expect(parseForESLint('return 1').ast).toMatchObject({
    type: 'Program',
    body: [
      {
        type: 'ReturnStatement',
        argument: {
          type: 'Literal',
          value: 1,
        },
      },
    ],
  });
});

test('Visitor key order for control flow nodes', () => {
  // Visitor keys for control flow nodes must have a particular order for
  // ESLint's code path analysis to work correctly.
  const {visitorKeys} = parseForESLint('null');
  expect(visitorKeys).toMatchObject({
    IfStatement: ['test', 'consequent', 'alternate'],
    ConditionalExpression: ['test', 'consequent', 'alternate'],
    WhileStatement: ['test', 'body'],
    DoWhileStatement: ['body', 'test'],
    ForStatement: ['init', 'test', 'update', 'body'],
    ForInStatement: ['left', 'right', 'body'],
    ForOfStatement: ['left', 'right', 'body'],
    SwitchStatement: ['discriminant', 'cases'],
    TryStatement: ['block', 'handler', 'finalizer'],
    CatchClause: ['param', 'body'],
  });
});

test('Parse error messages formatted for ESLint', () => {
  expect(() => parseForESLint('const = 1')).toThrow(
    expect.objectContaining({
      lineNumber: 1,
      column: 6,
    }),
  );
});
