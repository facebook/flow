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

describe('JSXElement', () => {
  describe('With type', () => {
    const testCase: AlignmentCase = {
      code: '<Component<string> />',
      espree: {
        expectToFail: 'espree-exception',
        expectedExceptionMessage: 'Unexpected token <',
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
                    "directive": null,
                    "expression": {
                      "children": [],
                      "closingElement": null,
                      "openingElement": {
                        "attributes": [],
                        "name": {
                          "name": "Component",
                          "type": "JSXIdentifier",
                        },
                        "selfClosing": true,
                        "type": "JSXOpeningElement",
                        "typeArguments": {
                          "params": [
                            {
                              "type": "StringTypeAnnotation",
                            },
                          ],
                          "type": "TypeParameterInstantiation",
                        },
                      },
                      "type": "JSXElement",
                    },
                    "type": "ExpressionStatement",
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
                    "expression": {
                      "children": [],
                      "closingElement": null,
                      "openingElement": {
                        "attributes": [],
                        "name": {
                          "name": "Component",
                          "type": "JSXIdentifier",
                        },
                        "selfClosing": true,
                        "type": "JSXOpeningElement",
                      },
                      "type": "JSXElement",
                    },
                    "type": "ExpressionStatement",
                  },
                ],
                "type": "Program",
              }
          `);
      expectBabelAlignment(testCase);
    });
  });
  describe('With text', () => {
    const testCase: AlignmentCase = {
      code: '<Foo>text</Foo>',
      espree: {
        expectToFail: false,
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
              "directive": null,
              "expression": {
                "children": [
                  {
                    "raw": "text",
                    "type": "JSXText",
                    "value": "text",
                  },
                ],
                "closingElement": {
                  "name": {
                    "name": "Foo",
                    "type": "JSXIdentifier",
                  },
                  "type": "JSXClosingElement",
                },
                "openingElement": {
                  "attributes": [],
                  "name": {
                    "name": "Foo",
                    "type": "JSXIdentifier",
                  },
                  "selfClosing": false,
                  "type": "JSXOpeningElement",
                  "typeArguments": null,
                },
                "type": "JSXElement",
              },
              "type": "ExpressionStatement",
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
              "expression": {
                "children": [
                  {
                    "extra": {
                      "raw": "text",
                      "rawValue": "text",
                    },
                    "type": "JSXText",
                    "value": "text",
                  },
                ],
                "closingElement": {
                  "name": {
                    "name": "Foo",
                    "type": "JSXIdentifier",
                  },
                  "type": "JSXClosingElement",
                },
                "openingElement": {
                  "attributes": [],
                  "name": {
                    "name": "Foo",
                    "type": "JSXIdentifier",
                  },
                  "selfClosing": false,
                  "type": "JSXOpeningElement",
                },
                "type": "JSXElement",
              },
              "type": "ExpressionStatement",
            },
          ],
          "type": "Program",
        }
      `);
      expectBabelAlignment(testCase);
    });
  });
});
