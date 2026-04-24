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

describe('Object properties', () => {
  const testCase: AlignmentCase = {
    code: `
      ({
        prop1: 1,
        prop2: function() {},
        prop3() {},
        async prop4() {},
        get prop5() {},
        set prop6(x) {},
      })
    `,
    espree: {expectToFail: false},
    babel: {expectToFail: false},
  };

  test('ESTree', () => {
    expect(parseForSnapshot(testCase.code, {preserveRange: true}))
      .toMatchInlineSnapshot(`
      {
        "body": [
          {
            "directive": null,
            "expression": {
              "properties": [
                {
                  "computed": false,
                  "key": {
                    "name": "prop1",
                    "optional": false,
                    "range": [
                      18,
                      23,
                    ],
                    "type": "Identifier",
                    "typeAnnotation": null,
                  },
                  "kind": "init",
                  "method": false,
                  "range": [
                    18,
                    26,
                  ],
                  "shorthand": false,
                  "type": "Property",
                  "value": {
                    "literalType": "numeric",
                    "range": [
                      25,
                      26,
                    ],
                    "raw": "1",
                    "type": "Literal",
                    "value": 1,
                  },
                },
                {
                  "computed": false,
                  "key": {
                    "name": "prop2",
                    "optional": false,
                    "range": [
                      36,
                      41,
                    ],
                    "type": "Identifier",
                    "typeAnnotation": null,
                  },
                  "kind": "init",
                  "method": false,
                  "range": [
                    36,
                    56,
                  ],
                  "shorthand": false,
                  "type": "Property",
                  "value": {
                    "async": false,
                    "body": {
                      "body": [],
                      "range": [
                        54,
                        56,
                      ],
                      "type": "BlockStatement",
                    },
                    "expression": false,
                    "generator": false,
                    "id": null,
                    "params": [],
                    "predicate": null,
                    "range": [
                      43,
                      56,
                    ],
                    "returnType": null,
                    "type": "FunctionExpression",
                    "typeParameters": null,
                  },
                },
                {
                  "computed": false,
                  "key": {
                    "name": "prop3",
                    "optional": false,
                    "range": [
                      66,
                      71,
                    ],
                    "type": "Identifier",
                    "typeAnnotation": null,
                  },
                  "kind": "init",
                  "method": true,
                  "range": [
                    66,
                    76,
                  ],
                  "shorthand": false,
                  "type": "Property",
                  "value": {
                    "async": false,
                    "body": {
                      "body": [],
                      "range": [
                        74,
                        76,
                      ],
                      "type": "BlockStatement",
                    },
                    "expression": false,
                    "generator": false,
                    "id": null,
                    "params": [],
                    "predicate": null,
                    "range": [
                      71,
                      76,
                    ],
                    "returnType": null,
                    "type": "FunctionExpression",
                    "typeParameters": null,
                  },
                },
                {
                  "computed": false,
                  "key": {
                    "name": "prop4",
                    "optional": false,
                    "range": [
                      92,
                      97,
                    ],
                    "type": "Identifier",
                    "typeAnnotation": null,
                  },
                  "kind": "init",
                  "method": true,
                  "range": [
                    86,
                    102,
                  ],
                  "shorthand": false,
                  "type": "Property",
                  "value": {
                    "async": true,
                    "body": {
                      "body": [],
                      "range": [
                        100,
                        102,
                      ],
                      "type": "BlockStatement",
                    },
                    "expression": false,
                    "generator": false,
                    "id": null,
                    "params": [],
                    "predicate": null,
                    "range": [
                      97,
                      102,
                    ],
                    "returnType": null,
                    "type": "FunctionExpression",
                    "typeParameters": null,
                  },
                },
                {
                  "computed": false,
                  "key": {
                    "name": "prop5",
                    "optional": false,
                    "range": [
                      116,
                      121,
                    ],
                    "type": "Identifier",
                    "typeAnnotation": null,
                  },
                  "kind": "get",
                  "method": false,
                  "range": [
                    112,
                    126,
                  ],
                  "shorthand": false,
                  "type": "Property",
                  "value": {
                    "async": false,
                    "body": {
                      "body": [],
                      "range": [
                        124,
                        126,
                      ],
                      "type": "BlockStatement",
                    },
                    "expression": false,
                    "generator": false,
                    "id": null,
                    "params": [],
                    "predicate": null,
                    "range": [
                      121,
                      126,
                    ],
                    "returnType": null,
                    "type": "FunctionExpression",
                    "typeParameters": null,
                  },
                },
                {
                  "computed": false,
                  "key": {
                    "name": "prop6",
                    "optional": false,
                    "range": [
                      140,
                      145,
                    ],
                    "type": "Identifier",
                    "typeAnnotation": null,
                  },
                  "kind": "set",
                  "method": false,
                  "range": [
                    136,
                    151,
                  ],
                  "shorthand": false,
                  "type": "Property",
                  "value": {
                    "async": false,
                    "body": {
                      "body": [],
                      "range": [
                        149,
                        151,
                      ],
                      "type": "BlockStatement",
                    },
                    "expression": false,
                    "generator": false,
                    "id": null,
                    "params": [
                      {
                        "name": "x",
                        "optional": false,
                        "range": [
                          146,
                          147,
                        ],
                        "type": "Identifier",
                        "typeAnnotation": null,
                      },
                    ],
                    "predicate": null,
                    "range": [
                      145,
                      151,
                    ],
                    "returnType": null,
                    "type": "FunctionExpression",
                    "typeParameters": null,
                  },
                },
              ],
              "range": [
                8,
                160,
              ],
              "type": "ObjectExpression",
            },
            "range": [
              7,
              161,
            ],
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
              "properties": [
                {
                  "computed": false,
                  "key": {
                    "name": "prop1",
                    "type": "Identifier",
                  },
                  "method": false,
                  "shorthand": false,
                  "type": "ObjectProperty",
                  "value": {
                    "extra": {
                      "raw": "1",
                      "rawValue": 1,
                    },
                    "type": "NumericLiteral",
                    "value": 1,
                  },
                },
                {
                  "computed": false,
                  "key": {
                    "name": "prop2",
                    "type": "Identifier",
                  },
                  "method": false,
                  "shorthand": false,
                  "type": "ObjectProperty",
                  "value": {
                    "async": false,
                    "body": {
                      "body": [],
                      "directives": [],
                      "type": "BlockStatement",
                    },
                    "generator": false,
                    "id": null,
                    "params": [],
                    "type": "FunctionExpression",
                  },
                },
                {
                  "async": false,
                  "body": {
                    "body": [],
                    "directives": [],
                    "type": "BlockStatement",
                  },
                  "computed": false,
                  "generator": false,
                  "id": null,
                  "key": {
                    "name": "prop3",
                    "type": "Identifier",
                  },
                  "kind": "method",
                  "method": true,
                  "params": [],
                  "type": "ObjectMethod",
                },
                {
                  "async": true,
                  "body": {
                    "body": [],
                    "directives": [],
                    "type": "BlockStatement",
                  },
                  "computed": false,
                  "generator": false,
                  "id": null,
                  "key": {
                    "name": "prop4",
                    "type": "Identifier",
                  },
                  "kind": "method",
                  "method": true,
                  "params": [],
                  "type": "ObjectMethod",
                },
                {
                  "async": false,
                  "body": {
                    "body": [],
                    "directives": [],
                    "type": "BlockStatement",
                  },
                  "computed": false,
                  "generator": false,
                  "id": null,
                  "key": {
                    "name": "prop5",
                    "type": "Identifier",
                  },
                  "kind": "get",
                  "method": false,
                  "params": [],
                  "type": "ObjectMethod",
                  "variance": null,
                },
                {
                  "async": false,
                  "body": {
                    "body": [],
                    "directives": [],
                    "type": "BlockStatement",
                  },
                  "computed": false,
                  "generator": false,
                  "id": null,
                  "key": {
                    "name": "prop6",
                    "type": "Identifier",
                  },
                  "kind": "set",
                  "method": false,
                  "params": [
                    {
                      "name": "x",
                      "type": "Identifier",
                    },
                  ],
                  "type": "ObjectMethod",
                  "variance": null,
                },
              ],
              "type": "ObjectExpression",
            },
            "type": "ExpressionStatement",
          },
        ],
        "type": "Program",
      }
    `);
    expectBabelAlignment(testCase);
  });

  test('Location of object literal method does not contain key', () => {
    // We know the structure
    const ast: $FlowFixMe = parseForSnapshot(`({key/*comment*/(){}})`, {
      preserveRange: true,
    });
    expect(ast.body[0].expression.properties[0]).toMatchInlineSnapshot(`
      {
        "computed": false,
        "key": {
          "name": "key",
          "optional": false,
          "range": [
            2,
            5,
          ],
          "type": "Identifier",
          "typeAnnotation": null,
        },
        "kind": "init",
        "method": true,
        "range": [
          2,
          20,
        ],
        "shorthand": false,
        "type": "Property",
        "value": {
          "async": false,
          "body": {
            "body": [],
            "range": [
              18,
              20,
            ],
            "type": "BlockStatement",
          },
          "expression": false,
          "generator": false,
          "id": null,
          "params": [],
          "predicate": null,
          "range": [
            16,
            20,
          ],
          "returnType": null,
          "type": "FunctionExpression",
          "typeParameters": null,
        },
      }
    `);
  });
});
