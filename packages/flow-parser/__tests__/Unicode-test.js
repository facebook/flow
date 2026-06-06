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
