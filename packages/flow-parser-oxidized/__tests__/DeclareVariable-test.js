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

describe('DeclareVariable', () => {
  describe('var', () => {
    const testCase: AlignmentCase = {
      code: `
        declare var T1: number;
      `,
      espree: {
        expectToFail: 'espree-exception',
        expectedExceptionMessage: 'Unexpected token var',
      },
      babel: {
        expectToFail: false,
      },
    };

    test('ESTree', () => {
      expect(parseForSnapshot(testCase.code)).toMatchInlineSnapshot(`
        {
          "body": [
            {
              "id": {
                "name": "T1",
                "optional": false,
                "type": "Identifier",
                "typeAnnotation": {
                  "type": "TypeAnnotation",
                  "typeAnnotation": {
                    "type": "NumberTypeAnnotation",
                  },
                },
              },
              "kind": "var",
              "type": "DeclareVariable",
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
                    "type": "NumberTypeAnnotation",
                  },
                },
              },
              "type": "DeclareVariable",
            },
          ],
          "type": "Program",
        }
      `);
      expectBabelAlignment(testCase);
    });
  });

  describe('let', () => {
    const testCase: AlignmentCase = {
      code: `
        declare let T1: number;
      `,
      espree: {
        expectToFail: 'espree-exception',
        expectedExceptionMessage: 'Unexpected token let',
      },
      babel: {
        expectToFail: 'espree-exception',
        expectedExceptionMessage: 'Unexpected token',
      },
    };

    test('ESTree', () => {
      expect(parseForSnapshot(testCase.code)).toMatchInlineSnapshot(`
        {
          "body": [
            {
              "id": {
                "name": "T1",
                "optional": false,
                "type": "Identifier",
                "typeAnnotation": {
                  "type": "TypeAnnotation",
                  "typeAnnotation": {
                    "type": "NumberTypeAnnotation",
                  },
                },
              },
              "kind": "let",
              "type": "DeclareVariable",
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
                    "type": "NumberTypeAnnotation",
                  },
                },
              },
              "type": "DeclareVariable",
            },
          ],
          "type": "Program",
        }
      `);
      expectBabelAlignment(testCase);
    });
  });

  describe('const', () => {
    const testCase: AlignmentCase = {
      code: `
        declare const T1: number;
      `,
      espree: {
        expectToFail: 'espree-exception',
        expectedExceptionMessage: 'Unexpected token const',
      },
      babel: {
        expectToFail: 'espree-exception',
        expectedExceptionMessage: 'Unexpected token',
      },
    };

    test('ESTree', () => {
      expect(parseForSnapshot(testCase.code)).toMatchInlineSnapshot(`
        {
          "body": [
            {
              "id": {
                "name": "T1",
                "optional": false,
                "type": "Identifier",
                "typeAnnotation": {
                  "type": "TypeAnnotation",
                  "typeAnnotation": {
                    "type": "NumberTypeAnnotation",
                  },
                },
              },
              "kind": "const",
              "type": "DeclareVariable",
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
                    "type": "NumberTypeAnnotation",
                  },
                },
              },
              "type": "DeclareVariable",
            },
          ],
          "type": "Program",
        }
      `);
      expectBabelAlignment(testCase);
    });
  });
});
