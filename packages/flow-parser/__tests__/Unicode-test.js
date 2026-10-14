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

import {parse} from '../__test_utils__/parse';

test('Locations and ranges use UTF-16 columns after surrogate pairs', () => {
  const ast = parse('const 𩸽 = "😀"; const value = 1;');
  expect(ast.body[0].declarations[0].id).toMatchObject({
    range: [6, 8],
    loc: {start: {line: 1, column: 6}, end: {line: 1, column: 8}},
  });
  expect(ast.body[1].declarations[0].id).toMatchObject({
    range: [23, 28],
    loc: {start: {line: 1, column: 23}, end: {line: 1, column: 28}},
  });
});

test('Unicode strings and identifiers', () => {
  const source = `
    // Null byte in middle of string
    'foo \0 bar';
    // Unicode directly in text
    const 𩸽 = '𩸽';
    // Unicode literals
    const a\u033f = '\u033f';
    // Surrogate pairs (invalid individual UTF-8 code points)
    '\ud83d\udea6';
  `;

  expect(parse(source)).toMatchObject({
    type: 'Program',
    body: [
      {
        type: 'ExpressionStatement',
        expression: {
          type: 'Literal',
          value: 'foo \0 bar',
        },
      },
      {
        type: 'VariableDeclaration',
        declarations: [
          {
            type: 'VariableDeclarator',
            id: {
              type: 'Identifier',
              name: '𩸽',
            },
            init: {
              type: 'Literal',
              value: '𩸽',
            },
          },
        ],
      },
      {
        type: 'VariableDeclaration',
        declarations: [
          {
            type: 'VariableDeclarator',
            id: {
              type: 'Identifier',
              name: 'a\u033f',
            },
            init: {
              type: 'Literal',
              value: '\u033f',
            },
          },
        ],
      },
      {
        type: 'ExpressionStatement',
        expression: {
          type: 'Literal',
          value: '\ud83d\udea6',
        },
      },
    ],
  });
});
