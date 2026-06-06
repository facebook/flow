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

import {parseForSnapshot} from '../__test_utils__/parse';

describe('Babel-Specific Tests', () => {
  test('Babel root File node', () => {
    expect(parseForSnapshot('test', {babel: true})).toMatchInlineSnapshot(`
      {
        "body": [
          {
            "expression": {
              "name": "test",
              "type": "Identifier",
            },
            "type": "ExpressionStatement",
          },
        ],
        "type": "Program",
      }
    `);
  });

  test('Babel identifierName', () => {
    expect(parseForSnapshot('test', {babel: true})).toMatchInlineSnapshot(`
      {
        "body": [
          {
            "expression": {
              "name": "test",
              "type": "Identifier",
            },
            "type": "ExpressionStatement",
          },
        ],
        "type": "Program",
      }
    `);
  });
});
