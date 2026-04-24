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

describe('DeclareEnum', () => {
  const testCase: AlignmentCase = {
    code: `
      declare enum T1 { A, B };
    `,
    espree: {
      expectToFail: 'espree-exception',
      expectedExceptionMessage: 'Unexpected token enum',
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
              "explicitType": false,
              "hasUnknownMembers": false,
              "members": [
                {
                  "id": {
                    "name": "A",
                    "optional": false,
                    "type": "Identifier",
                    "typeAnnotation": null,
                  },
                  "type": "EnumDefaultedMember",
                },
                {
                  "id": {
                    "name": "B",
                    "optional": false,
                    "type": "Identifier",
                    "typeAnnotation": null,
                  },
                  "type": "EnumDefaultedMember",
                },
              ],
              "type": "EnumStringBody",
            },
            "id": {
              "name": "T1",
              "optional": false,
              "type": "Identifier",
              "typeAnnotation": null,
            },
            "type": "DeclareEnum",
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
              "name": "T1",
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
