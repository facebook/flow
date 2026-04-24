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

describe('RestElement', () => {
  const testCase: AlignmentCase = {
    code: `
      function test1(...rest) {}
      function test2([...rest]) {}
    `,
    espree: {expectToFail: false},
    babel: {expectToFail: false},
  };

  test('ESTree', () => {
    expect(parseForSnapshot(testCase.code)).toMatchInlineSnapshot(`
      {
        "body": [
          {
            "async": false,
            "body": {
              "body": [],
              "type": "BlockStatement",
            },
            "expression": false,
            "generator": false,
            "id": {
              "name": "test1",
              "optional": false,
              "type": "Identifier",
              "typeAnnotation": null,
            },
            "params": [
              {
                "argument": {
                  "name": "rest",
                  "optional": false,
                  "type": "Identifier",
                  "typeAnnotation": null,
                },
                "type": "RestElement",
              },
            ],
            "predicate": null,
            "returnType": null,
            "type": "FunctionDeclaration",
            "typeParameters": null,
          },
          {
            "async": false,
            "body": {
              "body": [],
              "type": "BlockStatement",
            },
            "expression": false,
            "generator": false,
            "id": {
              "name": "test2",
              "optional": false,
              "type": "Identifier",
              "typeAnnotation": null,
            },
            "params": [
              {
                "elements": [
                  {
                    "argument": {
                      "name": "rest",
                      "optional": false,
                      "type": "Identifier",
                      "typeAnnotation": null,
                    },
                    "type": "RestElement",
                  },
                ],
                "type": "ArrayPattern",
                "typeAnnotation": null,
              },
            ],
            "predicate": null,
            "returnType": null,
            "type": "FunctionDeclaration",
            "typeParameters": null,
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
            "async": false,
            "body": {
              "body": [],
              "directives": [],
              "type": "BlockStatement",
            },
            "generator": false,
            "id": {
              "name": "test1",
              "type": "Identifier",
            },
            "params": [
              {
                "argument": {
                  "name": "rest",
                  "type": "Identifier",
                },
                "type": "RestElement",
              },
            ],
            "type": "FunctionDeclaration",
          },
          {
            "async": false,
            "body": {
              "body": [],
              "directives": [],
              "type": "BlockStatement",
            },
            "generator": false,
            "id": {
              "name": "test2",
              "type": "Identifier",
            },
            "params": [
              {
                "elements": [
                  {
                    "argument": {
                      "name": "rest",
                      "type": "Identifier",
                    },
                    "type": "RestElement",
                  },
                ],
                "type": "ArrayPattern",
              },
            ],
            "type": "FunctionDeclaration",
          },
        ],
        "type": "Program",
      }
    `);
    expectBabelAlignment(testCase);
  });

  describe('with type', () => {
    const testCase: AlignmentCase = {
      code: `
        function test1(...rest: string) {}
        function test2([...rest: string]) {}
      `,
      espree: {
        expectToFail: 'espree-exception',
        expectedExceptionMessage: 'Unexpected token :',
      },
      babel: {expectToFail: false},
    };

    test('ESTree', () => {
      expect(parseForSnapshot(testCase.code)).toMatchInlineSnapshot(`
        {
          "body": [
            {
              "async": false,
              "body": {
                "body": [],
                "type": "BlockStatement",
              },
              "expression": false,
              "generator": false,
              "id": {
                "name": "test1",
                "optional": false,
                "type": "Identifier",
                "typeAnnotation": null,
              },
              "params": [
                {
                  "argument": {
                    "name": "rest",
                    "optional": false,
                    "type": "Identifier",
                    "typeAnnotation": {
                      "type": "TypeAnnotation",
                      "typeAnnotation": {
                        "type": "StringTypeAnnotation",
                      },
                    },
                  },
                  "type": "RestElement",
                },
              ],
              "predicate": null,
              "returnType": null,
              "type": "FunctionDeclaration",
              "typeParameters": null,
            },
            {
              "async": false,
              "body": {
                "body": [],
                "type": "BlockStatement",
              },
              "expression": false,
              "generator": false,
              "id": {
                "name": "test2",
                "optional": false,
                "type": "Identifier",
                "typeAnnotation": null,
              },
              "params": [
                {
                  "elements": [
                    {
                      "argument": {
                        "name": "rest",
                        "optional": false,
                        "type": "Identifier",
                        "typeAnnotation": {
                          "type": "TypeAnnotation",
                          "typeAnnotation": {
                            "type": "StringTypeAnnotation",
                          },
                        },
                      },
                      "type": "RestElement",
                    },
                  ],
                  "type": "ArrayPattern",
                  "typeAnnotation": null,
                },
              ],
              "predicate": null,
              "returnType": null,
              "type": "FunctionDeclaration",
              "typeParameters": null,
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
              "async": false,
              "body": {
                "body": [],
                "directives": [],
                "type": "BlockStatement",
              },
              "generator": false,
              "id": {
                "name": "test1",
                "type": "Identifier",
              },
              "params": [
                {
                  "argument": {
                    "name": "rest",
                    "type": "Identifier",
                  },
                  "type": "RestElement",
                  "typeAnnotation": {
                    "type": "TypeAnnotation",
                    "typeAnnotation": {
                      "type": "StringTypeAnnotation",
                    },
                  },
                },
              ],
              "type": "FunctionDeclaration",
            },
            {
              "async": false,
              "body": {
                "body": [],
                "directives": [],
                "type": "BlockStatement",
              },
              "generator": false,
              "id": {
                "name": "test2",
                "type": "Identifier",
              },
              "params": [
                {
                  "elements": [
                    {
                      "argument": {
                        "name": "rest",
                        "type": "Identifier",
                      },
                      "type": "RestElement",
                      "typeAnnotation": {
                        "type": "TypeAnnotation",
                        "typeAnnotation": {
                          "type": "StringTypeAnnotation",
                        },
                      },
                    },
                  ],
                  "type": "ArrayPattern",
                },
              ],
              "type": "FunctionDeclaration",
            },
          ],
          "type": "Program",
        }
      `);
      expectBabelAlignment(testCase);
    });
  });

  describe('with type but not an identifier', () => {
    const testCase: AlignmentCase = {
      code: `
        function test1(...[]: string) {}
        function test2([...[]: string]) {}
      `,
      espree: {
        expectToFail: 'espree-exception',
        expectedExceptionMessage: 'Unexpected token :',
      },
      babel: {
        // there's no way for us to get the end location right in this case :(
        expectToFail: 'ast-diff',
      },
    };

    test('ESTree', () => {
      expect(parseForSnapshot(testCase.code)).toMatchInlineSnapshot(`
        {
          "body": [
            {
              "async": false,
              "body": {
                "body": [],
                "type": "BlockStatement",
              },
              "expression": false,
              "generator": false,
              "id": {
                "name": "test1",
                "optional": false,
                "type": "Identifier",
                "typeAnnotation": null,
              },
              "params": [
                {
                  "argument": {
                    "elements": [],
                    "type": "ArrayPattern",
                    "typeAnnotation": {
                      "type": "TypeAnnotation",
                      "typeAnnotation": {
                        "type": "StringTypeAnnotation",
                      },
                    },
                  },
                  "type": "RestElement",
                },
              ],
              "predicate": null,
              "returnType": null,
              "type": "FunctionDeclaration",
              "typeParameters": null,
            },
            {
              "async": false,
              "body": {
                "body": [],
                "type": "BlockStatement",
              },
              "expression": false,
              "generator": false,
              "id": {
                "name": "test2",
                "optional": false,
                "type": "Identifier",
                "typeAnnotation": null,
              },
              "params": [
                {
                  "elements": [
                    {
                      "argument": {
                        "elements": [],
                        "type": "ArrayPattern",
                        "typeAnnotation": {
                          "type": "TypeAnnotation",
                          "typeAnnotation": {
                            "type": "StringTypeAnnotation",
                          },
                        },
                      },
                      "type": "RestElement",
                    },
                  ],
                  "type": "ArrayPattern",
                  "typeAnnotation": null,
                },
              ],
              "predicate": null,
              "returnType": null,
              "type": "FunctionDeclaration",
              "typeParameters": null,
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
              "async": false,
              "body": {
                "body": [],
                "directives": [],
                "type": "BlockStatement",
              },
              "generator": false,
              "id": {
                "name": "test1",
                "type": "Identifier",
              },
              "params": [
                {
                  "argument": {
                    "elements": [],
                    "type": "ArrayPattern",
                  },
                  "type": "RestElement",
                  "typeAnnotation": {
                    "type": "TypeAnnotation",
                    "typeAnnotation": {
                      "type": "StringTypeAnnotation",
                    },
                  },
                },
              ],
              "type": "FunctionDeclaration",
            },
            {
              "async": false,
              "body": {
                "body": [],
                "directives": [],
                "type": "BlockStatement",
              },
              "generator": false,
              "id": {
                "name": "test2",
                "type": "Identifier",
              },
              "params": [
                {
                  "elements": [
                    {
                      "argument": {
                        "elements": [],
                        "type": "ArrayPattern",
                      },
                      "type": "RestElement",
                      "typeAnnotation": {
                        "type": "TypeAnnotation",
                        "typeAnnotation": {
                          "type": "StringTypeAnnotation",
                        },
                      },
                    },
                  ],
                  "type": "ArrayPattern",
                },
              ],
              "type": "FunctionDeclaration",
            },
          ],
          "type": "Program",
        }
      `);
      expectBabelAlignment(testCase);
    });
  });
});
