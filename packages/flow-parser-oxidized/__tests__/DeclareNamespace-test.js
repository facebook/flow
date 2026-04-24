/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow strict-local
 * @format
 */

import type {AlignmentCase} from '../__test_utils__/alignment-utils';

import {
  expectBabelAlignment,
  expectEspreeAlignment,
} from '../__test_utils__/alignment-utils';
import {parseForSnapshot} from '../__test_utils__/parse';

describe('DeclareNamespace', () => {
  const testCase: AlignmentCase = {
    code: `
      declare namespace NS { declare const foo: string };
    `,
    espree: {
      expectToFail: 'espree-exception',
      expectedExceptionMessage: 'Unexpected token namespace',
    },
    babel: {
      expectToFail: 'babel-exception',
      expectedExceptionMessage: 'Unexpected token',
    },
  };

  test('ESTree', () => {
    expect(parseForSnapshot(testCase.code)).toMatchInlineSnapshot(`
      {
        "body": [
          {
            "body": {
              "body": [
                {
                  "id": {
                    "name": "foo",
                    "optional": false,
                    "type": "Identifier",
                    "typeAnnotation": {
                      "type": "TypeAnnotation",
                      "typeAnnotation": {
                        "type": "StringTypeAnnotation",
                      },
                    },
                  },
                  "kind": "const",
                  "type": "DeclareVariable",
                },
              ],
              "type": "BlockStatement",
            },
            "id": {
              "name": "NS",
              "optional": false,
              "type": "Identifier",
              "typeAnnotation": null,
            },
            "type": "DeclareNamespace",
          },
          {
            "type": "EmptyStatement",
          },
        ],
        "type": "Program",
      }
    `);
    expectEspreeAlignment(testCase);
  });

  test('Babel', () => {
    expect(parseForSnapshot(testCase.code, {babel: true}))
      .toMatchInlineSnapshot(`
      {
        "body": [
          {
            "id": {
              "name": "NS",
              "type": "Identifier",
              "typeAnnotation": {
                "type": "TypeAnnotation",
                "typeAnnotation": {
                  "type": "AnyTypeAnnotation",
                },
              },
            },
            "type": "DeclareVariable",
          },
          {
            "type": "EmptyStatement",
          },
        ],
        "type": "Program",
      }
    `);
    expectBabelAlignment(testCase);
  });
});
