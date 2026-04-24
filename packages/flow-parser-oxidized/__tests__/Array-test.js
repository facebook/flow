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

import type {AlignmentCase} from '../__test_utils__/alignment-utils';

import {
  expectBabelAlignment,
  expectEspreeAlignment,
} from '../__test_utils__/alignment-utils';
import {parseForSnapshot} from '../__test_utils__/parse';

const testCase: AlignmentCase = {
  code: 'const [a,,b] = [1,,2];',
  espree: {expectToFail: false},
  babel: {expectToFail: false},
};

describe('Array', () => {
  test('ESTree', () => {
    expect(parseForSnapshot(testCase.code)).toMatchInlineSnapshot(`
      {
        "body": [
          {
            "declarations": [
              {
                "id": {
                  "elements": [
                    {
                      "name": "a",
                      "optional": false,
                      "type": "Identifier",
                      "typeAnnotation": null,
                    },
                    null,
                    {
                      "name": "b",
                      "optional": false,
                      "type": "Identifier",
                      "typeAnnotation": null,
                    },
                  ],
                  "type": "ArrayPattern",
                  "typeAnnotation": null,
                },
                "init": {
                  "elements": [
                    {
                      "literalType": "numeric",
                      "raw": "1",
                      "type": "Literal",
                      "value": 1,
                    },
                    null,
                    {
                      "literalType": "numeric",
                      "raw": "2",
                      "type": "Literal",
                      "value": 2,
                    },
                  ],
                  "trailingComma": false,
                  "type": "ArrayExpression",
                },
                "type": "VariableDeclarator",
              },
            ],
            "kind": "const",
            "type": "VariableDeclaration",
          },
        ],
        "type": "Program",
      }
    `);
    expectEspreeAlignment(testCase);
  });

  test('Babel', () => {
    // Babel AST array nodes
    expect(parseForSnapshot(testCase.code, {babel: true}))
      .toMatchInlineSnapshot(`
      {
        "body": [
          {
            "declarations": [
              {
                "id": {
                  "elements": [
                    {
                      "name": "a",
                      "type": "Identifier",
                    },
                    null,
                    {
                      "name": "b",
                      "type": "Identifier",
                    },
                  ],
                  "type": "ArrayPattern",
                },
                "init": {
                  "elements": [
                    {
                      "extra": {
                        "raw": "1",
                        "rawValue": 1,
                      },
                      "type": "NumericLiteral",
                      "value": 1,
                    },
                    null,
                    {
                      "extra": {
                        "raw": "2",
                        "rawValue": 2,
                      },
                      "type": "NumericLiteral",
                      "value": 2,
                    },
                  ],
                  "type": "ArrayExpression",
                },
                "type": "VariableDeclarator",
              },
            ],
            "kind": "const",
            "type": "VariableDeclaration",
          },
        ],
        "type": "Program",
      }
    `);
    expectBabelAlignment(testCase);
  });
});
