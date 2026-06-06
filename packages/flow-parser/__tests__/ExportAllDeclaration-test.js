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

describe('ExportAllDeclaration', () => {
  describe('Unnamed', () => {
    const testCase: AlignmentCase = {
      code: `
        export * from 'z'
      `,
      espree: {expectToFail: false},
      babel: {expectToFail: false},
    };

    test('ESTree', () => {
      expect(parseForSnapshot(testCase.code)).toMatchInlineSnapshot(`
        {
          "body": [
            {
              "exportKind": "value",
              "exported": null,
              "source": {
                "literalType": "string",
                "raw": "'z'",
                "type": "Literal",
                "value": "z",
              },
              "type": "ExportAllDeclaration",
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
              "exportKind": "value",
              "source": {
                "extra": {
                  "raw": "'z'",
                  "rawValue": "z",
                },
                "type": "StringLiteral",
                "value": "z",
              },
              "type": "ExportAllDeclaration",
            },
          ],
          "type": "Program",
        }
      `);
      expectBabelAlignment(testCase);
    });
  });

  describe('Renamed', () => {
    const testCase: AlignmentCase = {
      code: `
        export * as y from 'z'
      `,
      espree: {expectToFail: false},
      babel: {expectToFail: false},
    };

    test('ESTree', () => {
      expect(parseForSnapshot(testCase.code)).toMatchInlineSnapshot(`
        {
          "body": [
            {
              "exportKind": "value",
              "exported": {
                "name": "y",
                "optional": false,
                "type": "Identifier",
                "typeAnnotation": null,
              },
              "source": {
                "literalType": "string",
                "raw": "'z'",
                "type": "Literal",
                "value": "z",
              },
              "type": "ExportAllDeclaration",
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
              "exportKind": "value",
              "source": {
                "extra": {
                  "raw": "'z'",
                  "rawValue": "z",
                },
                "type": "StringLiteral",
                "value": "z",
              },
              "specifiers": [
                {
                  "exported": {
                    "name": "y",
                    "type": "Identifier",
                  },
                  "type": "ExportNamespaceSpecifier",
                },
              ],
              "type": "ExportNamedDeclaration",
            },
          ],
          "type": "Program",
        }
      `);
      expectBabelAlignment(testCase);
    });
  });
});
