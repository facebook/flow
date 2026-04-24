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

describe('MemberExpression', () => {
  describe('Non-computed', () => {
    const testCase: AlignmentCase = {
      code: `
        x.y;
      `,
      espree: {expectToFail: false},
      babel: {expectToFail: false},
    };

    test('ESTree', () => {
      expect(parseForSnapshot(testCase.code)).toMatchInlineSnapshot(`
        {
          "body": [
            {
              "directive": null,
              "expression": {
                "computed": false,
                "object": {
                  "name": "x",
                  "optional": false,
                  "type": "Identifier",
                  "typeAnnotation": null,
                },
                "optional": false,
                "property": {
                  "name": "y",
                  "optional": false,
                  "type": "Identifier",
                  "typeAnnotation": null,
                },
                "type": "MemberExpression",
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
      expectBabelAlignment(testCase);
    });
  });

  describe('Computed', () => {
    const testCase: AlignmentCase = {
      code: `
        x['y'];
      `,
      espree: {expectToFail: false},
      babel: {expectToFail: false},
    };

    test('ESTree', () => {
      expect(parseForSnapshot(testCase.code)).toMatchInlineSnapshot(`
        {
          "body": [
            {
              "directive": null,
              "expression": {
                "computed": true,
                "object": {
                  "name": "x",
                  "optional": false,
                  "type": "Identifier",
                  "typeAnnotation": null,
                },
                "optional": false,
                "property": {
                  "literalType": "string",
                  "raw": "'y'",
                  "type": "Literal",
                  "value": "y",
                },
                "type": "MemberExpression",
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
      expectBabelAlignment(testCase);
    });
  });
});

describe('OptionalMemberExpression', () => {
  describe('Non-computed', () => {
    const testCase: AlignmentCase = {
      code: `
        one?.two;
        one?.two.three;
        one.two?.three;
        one.two?.three.four;
        one.two?.three?.four;
      `,
      espree: {expectToFail: false},
      babel: {expectToFail: false},
    };

    test('ESTree', () => {
      expect(parseForSnapshot(testCase.code)).toMatchInlineSnapshot(`
        {
          "body": [
            {
              "directive": null,
              "expression": {
                "expression": {
                  "computed": false,
                  "object": {
                    "name": "one",
                    "optional": false,
                    "type": "Identifier",
                    "typeAnnotation": null,
                  },
                  "optional": true,
                  "property": {
                    "name": "two",
                    "optional": false,
                    "type": "Identifier",
                    "typeAnnotation": null,
                  },
                  "type": "MemberExpression",
                },
                "type": "ChainExpression",
              },
              "type": "ExpressionStatement",
            },
            {
              "directive": null,
              "expression": {
                "expression": {
                  "computed": false,
                  "object": {
                    "computed": false,
                    "object": {
                      "name": "one",
                      "optional": false,
                      "type": "Identifier",
                      "typeAnnotation": null,
                    },
                    "optional": true,
                    "property": {
                      "name": "two",
                      "optional": false,
                      "type": "Identifier",
                      "typeAnnotation": null,
                    },
                    "type": "MemberExpression",
                  },
                  "optional": false,
                  "property": {
                    "name": "three",
                    "optional": false,
                    "type": "Identifier",
                    "typeAnnotation": null,
                  },
                  "type": "MemberExpression",
                },
                "type": "ChainExpression",
              },
              "type": "ExpressionStatement",
            },
            {
              "directive": null,
              "expression": {
                "expression": {
                  "computed": false,
                  "object": {
                    "computed": false,
                    "object": {
                      "name": "one",
                      "optional": false,
                      "type": "Identifier",
                      "typeAnnotation": null,
                    },
                    "optional": false,
                    "property": {
                      "name": "two",
                      "optional": false,
                      "type": "Identifier",
                      "typeAnnotation": null,
                    },
                    "type": "MemberExpression",
                  },
                  "optional": true,
                  "property": {
                    "name": "three",
                    "optional": false,
                    "type": "Identifier",
                    "typeAnnotation": null,
                  },
                  "type": "MemberExpression",
                },
                "type": "ChainExpression",
              },
              "type": "ExpressionStatement",
            },
            {
              "directive": null,
              "expression": {
                "expression": {
                  "computed": false,
                  "object": {
                    "computed": false,
                    "object": {
                      "computed": false,
                      "object": {
                        "name": "one",
                        "optional": false,
                        "type": "Identifier",
                        "typeAnnotation": null,
                      },
                      "optional": false,
                      "property": {
                        "name": "two",
                        "optional": false,
                        "type": "Identifier",
                        "typeAnnotation": null,
                      },
                      "type": "MemberExpression",
                    },
                    "optional": true,
                    "property": {
                      "name": "three",
                      "optional": false,
                      "type": "Identifier",
                      "typeAnnotation": null,
                    },
                    "type": "MemberExpression",
                  },
                  "optional": false,
                  "property": {
                    "name": "four",
                    "optional": false,
                    "type": "Identifier",
                    "typeAnnotation": null,
                  },
                  "type": "MemberExpression",
                },
                "type": "ChainExpression",
              },
              "type": "ExpressionStatement",
            },
            {
              "directive": null,
              "expression": {
                "expression": {
                  "computed": false,
                  "object": {
                    "computed": false,
                    "object": {
                      "computed": false,
                      "object": {
                        "name": "one",
                        "optional": false,
                        "type": "Identifier",
                        "typeAnnotation": null,
                      },
                      "optional": false,
                      "property": {
                        "name": "two",
                        "optional": false,
                        "type": "Identifier",
                        "typeAnnotation": null,
                      },
                      "type": "MemberExpression",
                    },
                    "optional": true,
                    "property": {
                      "name": "three",
                      "optional": false,
                      "type": "Identifier",
                      "typeAnnotation": null,
                    },
                    "type": "MemberExpression",
                  },
                  "optional": true,
                  "property": {
                    "name": "four",
                    "optional": false,
                    "type": "Identifier",
                    "typeAnnotation": null,
                  },
                  "type": "MemberExpression",
                },
                "type": "ChainExpression",
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
      expectBabelAlignment(testCase);
    });
  });

  describe('Computed', () => {
    const testCase: AlignmentCase = {
      code: `
        one?.[2];
        one?.[2][3];
        one[2]?.[3];
        one[2]?.[3];
        one[2]?.[3][4];
        one[2]?.[3]?.[4];
      `,
      espree: {expectToFail: false},
      babel: {expectToFail: false},
    };

    test('ESTree', () => {
      expect(parseForSnapshot(testCase.code)).toMatchInlineSnapshot(`
        {
          "body": [
            {
              "directive": null,
              "expression": {
                "expression": {
                  "computed": true,
                  "object": {
                    "name": "one",
                    "optional": false,
                    "type": "Identifier",
                    "typeAnnotation": null,
                  },
                  "optional": true,
                  "property": {
                    "literalType": "numeric",
                    "raw": "2",
                    "type": "Literal",
                    "value": 2,
                  },
                  "type": "MemberExpression",
                },
                "type": "ChainExpression",
              },
              "type": "ExpressionStatement",
            },
            {
              "directive": null,
              "expression": {
                "expression": {
                  "computed": true,
                  "object": {
                    "computed": true,
                    "object": {
                      "name": "one",
                      "optional": false,
                      "type": "Identifier",
                      "typeAnnotation": null,
                    },
                    "optional": true,
                    "property": {
                      "literalType": "numeric",
                      "raw": "2",
                      "type": "Literal",
                      "value": 2,
                    },
                    "type": "MemberExpression",
                  },
                  "optional": false,
                  "property": {
                    "literalType": "numeric",
                    "raw": "3",
                    "type": "Literal",
                    "value": 3,
                  },
                  "type": "MemberExpression",
                },
                "type": "ChainExpression",
              },
              "type": "ExpressionStatement",
            },
            {
              "directive": null,
              "expression": {
                "expression": {
                  "computed": true,
                  "object": {
                    "computed": true,
                    "object": {
                      "name": "one",
                      "optional": false,
                      "type": "Identifier",
                      "typeAnnotation": null,
                    },
                    "optional": false,
                    "property": {
                      "literalType": "numeric",
                      "raw": "2",
                      "type": "Literal",
                      "value": 2,
                    },
                    "type": "MemberExpression",
                  },
                  "optional": true,
                  "property": {
                    "literalType": "numeric",
                    "raw": "3",
                    "type": "Literal",
                    "value": 3,
                  },
                  "type": "MemberExpression",
                },
                "type": "ChainExpression",
              },
              "type": "ExpressionStatement",
            },
            {
              "directive": null,
              "expression": {
                "expression": {
                  "computed": true,
                  "object": {
                    "computed": true,
                    "object": {
                      "name": "one",
                      "optional": false,
                      "type": "Identifier",
                      "typeAnnotation": null,
                    },
                    "optional": false,
                    "property": {
                      "literalType": "numeric",
                      "raw": "2",
                      "type": "Literal",
                      "value": 2,
                    },
                    "type": "MemberExpression",
                  },
                  "optional": true,
                  "property": {
                    "literalType": "numeric",
                    "raw": "3",
                    "type": "Literal",
                    "value": 3,
                  },
                  "type": "MemberExpression",
                },
                "type": "ChainExpression",
              },
              "type": "ExpressionStatement",
            },
            {
              "directive": null,
              "expression": {
                "expression": {
                  "computed": true,
                  "object": {
                    "computed": true,
                    "object": {
                      "computed": true,
                      "object": {
                        "name": "one",
                        "optional": false,
                        "type": "Identifier",
                        "typeAnnotation": null,
                      },
                      "optional": false,
                      "property": {
                        "literalType": "numeric",
                        "raw": "2",
                        "type": "Literal",
                        "value": 2,
                      },
                      "type": "MemberExpression",
                    },
                    "optional": true,
                    "property": {
                      "literalType": "numeric",
                      "raw": "3",
                      "type": "Literal",
                      "value": 3,
                    },
                    "type": "MemberExpression",
                  },
                  "optional": false,
                  "property": {
                    "literalType": "numeric",
                    "raw": "4",
                    "type": "Literal",
                    "value": 4,
                  },
                  "type": "MemberExpression",
                },
                "type": "ChainExpression",
              },
              "type": "ExpressionStatement",
            },
            {
              "directive": null,
              "expression": {
                "expression": {
                  "computed": true,
                  "object": {
                    "computed": true,
                    "object": {
                      "computed": true,
                      "object": {
                        "name": "one",
                        "optional": false,
                        "type": "Identifier",
                        "typeAnnotation": null,
                      },
                      "optional": false,
                      "property": {
                        "literalType": "numeric",
                        "raw": "2",
                        "type": "Literal",
                        "value": 2,
                      },
                      "type": "MemberExpression",
                    },
                    "optional": true,
                    "property": {
                      "literalType": "numeric",
                      "raw": "3",
                      "type": "Literal",
                      "value": 3,
                    },
                    "type": "MemberExpression",
                  },
                  "optional": true,
                  "property": {
                    "literalType": "numeric",
                    "raw": "4",
                    "type": "Literal",
                    "value": 4,
                  },
                  "type": "MemberExpression",
                },
                "type": "ChainExpression",
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
      expectBabelAlignment(testCase);
    });
  });

  // This is a special case to test because of the way parentheses
  // short-circuit the optional chain
  describe('With parentheses', () => {
    describe('Non-computed', () => {
      const testCase: AlignmentCase = {
        code: `
          (one?.two);
          (one?.two).three;
          (one.two?.three);
          (one.two?.three).four;
          (one.two?.three?.four);
          (one.two?.three?.four).five;
        `,
        espree: {expectToFail: false},
        babel: {expectToFail: false},
      };

      test('ESTree', () => {
        expect(parseForSnapshot(testCase.code)).toMatchInlineSnapshot(`
          {
            "body": [
              {
                "directive": null,
                "expression": {
                  "expression": {
                    "computed": false,
                    "object": {
                      "name": "one",
                      "optional": false,
                      "type": "Identifier",
                      "typeAnnotation": null,
                    },
                    "optional": true,
                    "property": {
                      "name": "two",
                      "optional": false,
                      "type": "Identifier",
                      "typeAnnotation": null,
                    },
                    "type": "MemberExpression",
                  },
                  "type": "ChainExpression",
                },
                "type": "ExpressionStatement",
              },
              {
                "directive": null,
                "expression": {
                  "computed": false,
                  "object": {
                    "expression": {
                      "computed": false,
                      "object": {
                        "name": "one",
                        "optional": false,
                        "type": "Identifier",
                        "typeAnnotation": null,
                      },
                      "optional": true,
                      "property": {
                        "name": "two",
                        "optional": false,
                        "type": "Identifier",
                        "typeAnnotation": null,
                      },
                      "type": "MemberExpression",
                    },
                    "type": "ChainExpression",
                  },
                  "optional": false,
                  "property": {
                    "name": "three",
                    "optional": false,
                    "type": "Identifier",
                    "typeAnnotation": null,
                  },
                  "type": "MemberExpression",
                },
                "type": "ExpressionStatement",
              },
              {
                "directive": null,
                "expression": {
                  "expression": {
                    "computed": false,
                    "object": {
                      "computed": false,
                      "object": {
                        "name": "one",
                        "optional": false,
                        "type": "Identifier",
                        "typeAnnotation": null,
                      },
                      "optional": false,
                      "property": {
                        "name": "two",
                        "optional": false,
                        "type": "Identifier",
                        "typeAnnotation": null,
                      },
                      "type": "MemberExpression",
                    },
                    "optional": true,
                    "property": {
                      "name": "three",
                      "optional": false,
                      "type": "Identifier",
                      "typeAnnotation": null,
                    },
                    "type": "MemberExpression",
                  },
                  "type": "ChainExpression",
                },
                "type": "ExpressionStatement",
              },
              {
                "directive": null,
                "expression": {
                  "computed": false,
                  "object": {
                    "expression": {
                      "computed": false,
                      "object": {
                        "computed": false,
                        "object": {
                          "name": "one",
                          "optional": false,
                          "type": "Identifier",
                          "typeAnnotation": null,
                        },
                        "optional": false,
                        "property": {
                          "name": "two",
                          "optional": false,
                          "type": "Identifier",
                          "typeAnnotation": null,
                        },
                        "type": "MemberExpression",
                      },
                      "optional": true,
                      "property": {
                        "name": "three",
                        "optional": false,
                        "type": "Identifier",
                        "typeAnnotation": null,
                      },
                      "type": "MemberExpression",
                    },
                    "type": "ChainExpression",
                  },
                  "optional": false,
                  "property": {
                    "name": "four",
                    "optional": false,
                    "type": "Identifier",
                    "typeAnnotation": null,
                  },
                  "type": "MemberExpression",
                },
                "type": "ExpressionStatement",
              },
              {
                "directive": null,
                "expression": {
                  "expression": {
                    "computed": false,
                    "object": {
                      "computed": false,
                      "object": {
                        "computed": false,
                        "object": {
                          "name": "one",
                          "optional": false,
                          "type": "Identifier",
                          "typeAnnotation": null,
                        },
                        "optional": false,
                        "property": {
                          "name": "two",
                          "optional": false,
                          "type": "Identifier",
                          "typeAnnotation": null,
                        },
                        "type": "MemberExpression",
                      },
                      "optional": true,
                      "property": {
                        "name": "three",
                        "optional": false,
                        "type": "Identifier",
                        "typeAnnotation": null,
                      },
                      "type": "MemberExpression",
                    },
                    "optional": true,
                    "property": {
                      "name": "four",
                      "optional": false,
                      "type": "Identifier",
                      "typeAnnotation": null,
                    },
                    "type": "MemberExpression",
                  },
                  "type": "ChainExpression",
                },
                "type": "ExpressionStatement",
              },
              {
                "directive": null,
                "expression": {
                  "computed": false,
                  "object": {
                    "expression": {
                      "computed": false,
                      "object": {
                        "computed": false,
                        "object": {
                          "computed": false,
                          "object": {
                            "name": "one",
                            "optional": false,
                            "type": "Identifier",
                            "typeAnnotation": null,
                          },
                          "optional": false,
                          "property": {
                            "name": "two",
                            "optional": false,
                            "type": "Identifier",
                            "typeAnnotation": null,
                          },
                          "type": "MemberExpression",
                        },
                        "optional": true,
                        "property": {
                          "name": "three",
                          "optional": false,
                          "type": "Identifier",
                          "typeAnnotation": null,
                        },
                        "type": "MemberExpression",
                      },
                      "optional": true,
                      "property": {
                        "name": "four",
                        "optional": false,
                        "type": "Identifier",
                        "typeAnnotation": null,
                      },
                      "type": "MemberExpression",
                    },
                    "type": "ChainExpression",
                  },
                  "optional": false,
                  "property": {
                    "name": "five",
                    "optional": false,
                    "type": "Identifier",
                    "typeAnnotation": null,
                  },
                  "type": "MemberExpression",
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
        expectBabelAlignment(testCase);
      });
    });

    describe('Tricky short-circuiting case', () => {
      const testCase: AlignmentCase = {
        code: `
          (one?.two)?.three?.four
          one?.two?.three?.four
        `,
        espree: {
          // unfortunately the hermes AST doesn't let us differentiate between
          // these two code cases, so we can't create the proper ESTree AST here :(
          expectToFail: 'ast-diff',
        },
        babel: {expectToFail: false},
      };

      test('ESTree', () => {
        expect(parseForSnapshot(testCase.code)).toMatchInlineSnapshot(`
          {
            "body": [
              {
                "directive": null,
                "expression": {
                  "expression": {
                    "computed": false,
                    "object": {
                      "computed": false,
                      "object": {
                        "computed": false,
                        "object": {
                          "name": "one",
                          "optional": false,
                          "type": "Identifier",
                          "typeAnnotation": null,
                        },
                        "optional": true,
                        "property": {
                          "name": "two",
                          "optional": false,
                          "type": "Identifier",
                          "typeAnnotation": null,
                        },
                        "type": "MemberExpression",
                      },
                      "optional": true,
                      "property": {
                        "name": "three",
                        "optional": false,
                        "type": "Identifier",
                        "typeAnnotation": null,
                      },
                      "type": "MemberExpression",
                    },
                    "optional": true,
                    "property": {
                      "name": "four",
                      "optional": false,
                      "type": "Identifier",
                      "typeAnnotation": null,
                    },
                    "type": "MemberExpression",
                  },
                  "type": "ChainExpression",
                },
                "type": "ExpressionStatement",
              },
              {
                "directive": null,
                "expression": {
                  "expression": {
                    "computed": false,
                    "object": {
                      "computed": false,
                      "object": {
                        "computed": false,
                        "object": {
                          "name": "one",
                          "optional": false,
                          "type": "Identifier",
                          "typeAnnotation": null,
                        },
                        "optional": true,
                        "property": {
                          "name": "two",
                          "optional": false,
                          "type": "Identifier",
                          "typeAnnotation": null,
                        },
                        "type": "MemberExpression",
                      },
                      "optional": true,
                      "property": {
                        "name": "three",
                        "optional": false,
                        "type": "Identifier",
                        "typeAnnotation": null,
                      },
                      "type": "MemberExpression",
                    },
                    "optional": true,
                    "property": {
                      "name": "four",
                      "optional": false,
                      "type": "Identifier",
                      "typeAnnotation": null,
                    },
                    "type": "MemberExpression",
                  },
                  "type": "ChainExpression",
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
        expectBabelAlignment(testCase);
      });
    });

    describe('Computed', () => {
      const testCase: AlignmentCase = {
        code: `
          (one?.[2]);
          (one?.[2])[3];
          (one[2]?.[3]);
          (one[2]?.[3])[4];
          (one[2]?.[3]?.[4]);
          (one[2]?.[3]?.[4])[5];
        `,
        espree: {expectToFail: false},
        babel: {expectToFail: false},
      };

      test('ESTree', () => {
        expect(parseForSnapshot(testCase.code)).toMatchInlineSnapshot(`
          {
            "body": [
              {
                "directive": null,
                "expression": {
                  "expression": {
                    "computed": true,
                    "object": {
                      "name": "one",
                      "optional": false,
                      "type": "Identifier",
                      "typeAnnotation": null,
                    },
                    "optional": true,
                    "property": {
                      "literalType": "numeric",
                      "raw": "2",
                      "type": "Literal",
                      "value": 2,
                    },
                    "type": "MemberExpression",
                  },
                  "type": "ChainExpression",
                },
                "type": "ExpressionStatement",
              },
              {
                "directive": null,
                "expression": {
                  "computed": true,
                  "object": {
                    "expression": {
                      "computed": true,
                      "object": {
                        "name": "one",
                        "optional": false,
                        "type": "Identifier",
                        "typeAnnotation": null,
                      },
                      "optional": true,
                      "property": {
                        "literalType": "numeric",
                        "raw": "2",
                        "type": "Literal",
                        "value": 2,
                      },
                      "type": "MemberExpression",
                    },
                    "type": "ChainExpression",
                  },
                  "optional": false,
                  "property": {
                    "literalType": "numeric",
                    "raw": "3",
                    "type": "Literal",
                    "value": 3,
                  },
                  "type": "MemberExpression",
                },
                "type": "ExpressionStatement",
              },
              {
                "directive": null,
                "expression": {
                  "expression": {
                    "computed": true,
                    "object": {
                      "computed": true,
                      "object": {
                        "name": "one",
                        "optional": false,
                        "type": "Identifier",
                        "typeAnnotation": null,
                      },
                      "optional": false,
                      "property": {
                        "literalType": "numeric",
                        "raw": "2",
                        "type": "Literal",
                        "value": 2,
                      },
                      "type": "MemberExpression",
                    },
                    "optional": true,
                    "property": {
                      "literalType": "numeric",
                      "raw": "3",
                      "type": "Literal",
                      "value": 3,
                    },
                    "type": "MemberExpression",
                  },
                  "type": "ChainExpression",
                },
                "type": "ExpressionStatement",
              },
              {
                "directive": null,
                "expression": {
                  "computed": true,
                  "object": {
                    "expression": {
                      "computed": true,
                      "object": {
                        "computed": true,
                        "object": {
                          "name": "one",
                          "optional": false,
                          "type": "Identifier",
                          "typeAnnotation": null,
                        },
                        "optional": false,
                        "property": {
                          "literalType": "numeric",
                          "raw": "2",
                          "type": "Literal",
                          "value": 2,
                        },
                        "type": "MemberExpression",
                      },
                      "optional": true,
                      "property": {
                        "literalType": "numeric",
                        "raw": "3",
                        "type": "Literal",
                        "value": 3,
                      },
                      "type": "MemberExpression",
                    },
                    "type": "ChainExpression",
                  },
                  "optional": false,
                  "property": {
                    "literalType": "numeric",
                    "raw": "4",
                    "type": "Literal",
                    "value": 4,
                  },
                  "type": "MemberExpression",
                },
                "type": "ExpressionStatement",
              },
              {
                "directive": null,
                "expression": {
                  "expression": {
                    "computed": true,
                    "object": {
                      "computed": true,
                      "object": {
                        "computed": true,
                        "object": {
                          "name": "one",
                          "optional": false,
                          "type": "Identifier",
                          "typeAnnotation": null,
                        },
                        "optional": false,
                        "property": {
                          "literalType": "numeric",
                          "raw": "2",
                          "type": "Literal",
                          "value": 2,
                        },
                        "type": "MemberExpression",
                      },
                      "optional": true,
                      "property": {
                        "literalType": "numeric",
                        "raw": "3",
                        "type": "Literal",
                        "value": 3,
                      },
                      "type": "MemberExpression",
                    },
                    "optional": true,
                    "property": {
                      "literalType": "numeric",
                      "raw": "4",
                      "type": "Literal",
                      "value": 4,
                    },
                    "type": "MemberExpression",
                  },
                  "type": "ChainExpression",
                },
                "type": "ExpressionStatement",
              },
              {
                "directive": null,
                "expression": {
                  "computed": true,
                  "object": {
                    "expression": {
                      "computed": true,
                      "object": {
                        "computed": true,
                        "object": {
                          "computed": true,
                          "object": {
                            "name": "one",
                            "optional": false,
                            "type": "Identifier",
                            "typeAnnotation": null,
                          },
                          "optional": false,
                          "property": {
                            "literalType": "numeric",
                            "raw": "2",
                            "type": "Literal",
                            "value": 2,
                          },
                          "type": "MemberExpression",
                        },
                        "optional": true,
                        "property": {
                          "literalType": "numeric",
                          "raw": "3",
                          "type": "Literal",
                          "value": 3,
                        },
                        "type": "MemberExpression",
                      },
                      "optional": true,
                      "property": {
                        "literalType": "numeric",
                        "raw": "4",
                        "type": "Literal",
                        "value": 4,
                      },
                      "type": "MemberExpression",
                    },
                    "type": "ChainExpression",
                  },
                  "optional": false,
                  "property": {
                    "literalType": "numeric",
                    "raw": "5",
                    "type": "Literal",
                    "value": 5,
                  },
                  "type": "MemberExpression",
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
        expectBabelAlignment(testCase);
      });
    });
  });
});
