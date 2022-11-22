/*
 * @flow
 */

import type {Suite} from "flow-dev-tools/src/test/Suite";
const { suite, test } = require("flow-dev-tools/src/test/Tester");

module.exports = (suite(({ addFile, addFiles, addCode }) => [
  test("BigInt invalid decimal type literal", [
    addCode(`
      type InvalidDecimal = 1.0n;
    `).newErrors(
        `
          test.js:4
            4:       type InvalidDecimal = 1.0n;
                                           ^^^^ A bigint literal must be an integer
        `,
      )
  ]),

  test("BigInt invalid negative decimal type literal", [
    addCode(`
      type InvalidNegDecimal = -1.0n;
    `).newErrors(
        `
          test.js:4
            4:       type InvalidNegDecimal = -1.0n;
                                              ^^^^^ A bigint literal must be an integer
        `,
      )
  ]),

  test("BigInt invalid decimal literal", [
    addCode(`
      const invalid_decimal = 1.0n;
    `).newErrors(
        `
          test.js:4
            4:       const invalid_decimal = 1.0n;
                                             ^^^^ A bigint literal must be an integer
        `,
      )
  ]),

  test("BigInt invalid negative decimal literal", [
    addCode(`
      const invalid_neg_decimal = -1.0n;
    `).newErrors(
        `
          test.js:4
            4:       const invalid_neg_decimal = -1.0n;
                                                  ^^^^ A bigint literal must be an integer
        `,
      )
  ]),

  test("BigInt invalid scientific type literal", [
    addCode(`
      type InvalidE = 2e9n;
    `).newErrors(
        `
          test.js:4
            4:       type InvalidE = 2e9n;
                                     ^^^^ A bigint literal cannot use exponential notation
        `,
      )
  ]),

  test("BigInt invalid negative scientific type literal", [
    addCode(`
      type InvalidNegE = -2e9n;
    `).newErrors(
        `
          test.js:4
            4:       type InvalidNegE = -2e9n;
                                        ^^^^^ A bigint literal cannot use exponential notation
        `,
      )
  ]),

  test("BigInt invalid scientific decimal type literal", [
    addCode(`
      type InvalidNegDecimalE = 2.0e9n;
    `).newErrors(
        `
          test.js:4
            4:       type InvalidNegDecimalE = 2.0e9n;
                                               ^^^^^^ A bigint literal cannot use exponential notation
        `,
      )
  ]),

  test("BigInt invalid negative scientific decimal type literal", [
    addCode(`
      type InvalidNegDecimalE = -2.0e9n;
    `).newErrors(
        `
          test.js:4
            4:       type InvalidNegDecimalE = -2.0e9n;
                                               ^^^^^^^ A bigint literal cannot use exponential notation
        `,
      )
  ]),

  test("BigInt invalid scientific literal", [
    addCode(`
      const invalid_e = 2e9n;
    `).newErrors(
        `
          test.js:4
            4:       const invalid_e = 2e9n;
                                       ^^^^ A bigint literal cannot use exponential notation
        `,
      )
  ]),

  test("BigInt invalid negative scientific literal", [
    addCode(`
      const invalid_neg_e = -2e9n;
    `).newErrors(
        `
          test.js:4
            4:       const invalid_neg_e = -2e9n;
                                            ^^^^ A bigint literal cannot use exponential notation
        `,
      )
  ]),

  test("BigInt invalid octal legacy type literal", [
    addCode(`
      type InvalidOctalLegacy = 016432n;
    `).newErrors(
        `
          test.js:4
            4:       type InvalidOctalLegacy = 016432n;
                                               ^^^^^^ Octal literals are not allowed in strict mode.
        `,
      )
  ]),

  test("BigInt invalid negative octal legacy type literal", [
    addCode(`
      type InvalidNegOctalLegacy = -016432n;
    `).newErrors(
        `
          test.js:4
            4:       type InvalidNegOctalLegacy = -016432n;
                                                  ^^^^^^^ Octal literals are not allowed in strict mode.
        `,
      )
  ]),

  test("BigInt invalid octal legacy literal", [
    addCode(`
      const invalid_octal_legacy = 016432n;
    `).newErrors(
        `
          test.js:4
            4:       const invalid_octal_legacy = 016432n;
                                                  ^^^^^^^ Unexpected token ILLEGAL
        `,
      )
  ]),

  test("BigInt invalid negative octal legacy literal", [
    addCode(`
      const invalid_neg_octal_legacy = -016432n;
    `).newErrors(
        `
          test.js:4
            4:       const invalid_neg_octal_legacy = -016432n;
                                                       ^^^^^^^ Unexpected token ILLEGAL
        `,
      )
  ]),

  test("BigInt can be suppressed", [
    addCode(`
      //$FlowFixMe
      type S = bigint;
      //$FlowFixMe
      type A = 1n;
      //$FlowFixMe
      const valid_binary = 0b101011101n;
      //$FlowFixMe
      const valid_hex = 0xfff123n;
      //$FlowFixMe
      const valid_large = 9223372036854775807n;
      //$FlowFixMe
      const valid_octal_new = 0o16432n;
      //$FlowFixMe
      const valid_small = 100n;
    `).noNewErrors()
  ])
]): Suite);
