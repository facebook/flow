/*
 * @flow
 */

import { suite, test } from "flow-dev-tools/src/test/Tester";

export default suite(({ addFile, addFiles, addCode }) => [
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
                                               ^^^^^^^ Unexpected token ILLEGAL
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
                                                  ^^^^^^^^ Unexpected token ILLEGAL
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

  test("BigInt is not supported yet", [
    addCode(`
      type S = bigint;

      const valid_binary = 0b101011101n;
      const valid_neg_binary = -0b101011101n;
      type ValidBinary = 0b101011101n;
      type ValidNegBinary = -0b101011101n;

      const valid_hex = 0xfff123n;
      const valid_neg_hex = -0xfff123n;
      type ValidHex = 0xfff123n;
      type ValidNegHex = -0xfff123n;

      const valid_large = 9223372036854775807n;
      const valid_neg_large = -9223372036854775807n;
      type ValidLarge = 9223372036854775807n;
      type ValidNegLarge = -9223372036854775807n;

      const valid_octal_new = 0o16432n;
      const valid_neg_octal_new = -0o16432n;
      type ValidOctalNew = 0o16432n;
      type ValidNegOctalNew = -0o16432n;

      const valid_small = 100n;
      const valid_neg_small = -100n;
      type ValidSmall = 100n;
      type ValidNegSmall = -1n;
    `).newErrors(
        `
          test.js:4
            4:       type S = bigint;
                              ^^^^^^ BigInt bigint [1] is not yet supported. [bigint-unsupported]
            References:
              4:       type S = bigint;
                                ^^^^^^ [1]

          test.js:6
            6:       const valid_binary = 0b101011101n;
                                          ^^^^^^^^^^^^ BigInt bigint literal \`0b101011101n\` [1] is not yet supported. [bigint-unsupported]
            References:
              6:       const valid_binary = 0b101011101n;
                                            ^^^^^^^^^^^^ [1]

          test.js:7
            7:       const valid_neg_binary = -0b101011101n;
                                               ^^^^^^^^^^^^ BigInt bigint literal \`0b101011101n\` [1] is not yet supported. [bigint-unsupported]
            References:
              7:       const valid_neg_binary = -0b101011101n;
                                                 ^^^^^^^^^^^^ [1]

          test.js:8
            8:       type ValidBinary = 0b101011101n;
                                        ^^^^^^^^^^^^ BigInt bigint literal \`0b101011101n\` [1] is not yet supported. [bigint-unsupported]
            References:
              8:       type ValidBinary = 0b101011101n;
                                          ^^^^^^^^^^^^ [1]

          test.js:9
            9:       type ValidNegBinary = -0b101011101n;
                                           ^^^^^^^^^^^^^ BigInt bigint literal \`-0b101011101n\` [1] is not yet supported. [bigint-unsupported]
            References:
              9:       type ValidNegBinary = -0b101011101n;
                                             ^^^^^^^^^^^^^ [1]

          test.js:11
           11:       const valid_hex = 0xfff123n;
                                       ^^^^^^^^^ BigInt bigint literal \`0xfff123n\` [1] is not yet supported. [bigint-unsupported]
            References:
             11:       const valid_hex = 0xfff123n;
                                         ^^^^^^^^^ [1]

          test.js:12
           12:       const valid_neg_hex = -0xfff123n;
                                            ^^^^^^^^^ BigInt bigint literal \`0xfff123n\` [1] is not yet supported. [bigint-unsupported]
            References:
             12:       const valid_neg_hex = -0xfff123n;
                                              ^^^^^^^^^ [1]

          test.js:13
           13:       type ValidHex = 0xfff123n;
                                     ^^^^^^^^^ BigInt bigint literal \`0xfff123n\` [1] is not yet supported. [bigint-unsupported]
            References:
             13:       type ValidHex = 0xfff123n;
                                       ^^^^^^^^^ [1]

          test.js:14
           14:       type ValidNegHex = -0xfff123n;
                                        ^^^^^^^^^^ BigInt bigint literal \`-0xfff123n\` [1] is not yet supported. [bigint-unsupported]
            References:
             14:       type ValidNegHex = -0xfff123n;
                                          ^^^^^^^^^^ [1]

          test.js:16
           16:       const valid_large = 9223372036854775807n;
                                         ^^^^^^^^^^^^^^^^^^^^ BigInt bigint literal \`9223372036854775807n\` [1] is not yet supported. [bigint-unsupported]
            References:
             16:       const valid_large = 9223372036854775807n;
                                           ^^^^^^^^^^^^^^^^^^^^ [1]

          test.js:17
           17:       const valid_neg_large = -9223372036854775807n;
                                              ^^^^^^^^^^^^^^^^^^^^ BigInt bigint literal \`9223372036854775807n\` [1] is not yet supported. [bigint-unsupported]
            References:
             17:       const valid_neg_large = -9223372036854775807n;
                                                ^^^^^^^^^^^^^^^^^^^^ [1]

          test.js:18
           18:       type ValidLarge = 9223372036854775807n;
                                       ^^^^^^^^^^^^^^^^^^^^ BigInt bigint literal \`9223372036854775807n\` [1] is not yet supported. [bigint-unsupported]
            References:
             18:       type ValidLarge = 9223372036854775807n;
                                         ^^^^^^^^^^^^^^^^^^^^ [1]

          test.js:19
           19:       type ValidNegLarge = -9223372036854775807n;
                                          ^^^^^^^^^^^^^^^^^^^^^ BigInt bigint literal \`-9223372036854775807n\` [1] is not yet supported. [bigint-unsupported]
            References:
             19:       type ValidNegLarge = -9223372036854775807n;
                                            ^^^^^^^^^^^^^^^^^^^^^ [1]

          test.js:21
           21:       const valid_octal_new = 0o16432n;
                                             ^^^^^^^^ BigInt bigint literal \`0o16432n\` [1] is not yet supported. [bigint-unsupported]
            References:
             21:       const valid_octal_new = 0o16432n;
                                               ^^^^^^^^ [1]

          test.js:22
           22:       const valid_neg_octal_new = -0o16432n;
                                                  ^^^^^^^^ BigInt bigint literal \`0o16432n\` [1] is not yet supported. [bigint-unsupported]
            References:
             22:       const valid_neg_octal_new = -0o16432n;
                                                    ^^^^^^^^ [1]

          test.js:23
           23:       type ValidOctalNew = 0o16432n;
                                          ^^^^^^^^ BigInt bigint literal \`0o16432n\` [1] is not yet supported. [bigint-unsupported]
            References:
             23:       type ValidOctalNew = 0o16432n;
                                            ^^^^^^^^ [1]

          test.js:24
           24:       type ValidNegOctalNew = -0o16432n;
                                             ^^^^^^^^^ BigInt bigint literal \`-0o16432n\` [1] is not yet supported. [bigint-unsupported]
            References:
             24:       type ValidNegOctalNew = -0o16432n;
                                               ^^^^^^^^^ [1]

          test.js:26
           26:       const valid_small = 100n;
                                         ^^^^ BigInt bigint literal \`100n\` [1] is not yet supported. [bigint-unsupported]
            References:
             26:       const valid_small = 100n;
                                           ^^^^ [1]

          test.js:27
           27:       const valid_neg_small = -100n;
                                              ^^^^ BigInt bigint literal \`100n\` [1] is not yet supported. [bigint-unsupported]
            References:
             27:       const valid_neg_small = -100n;
                                                ^^^^ [1]

          test.js:28
           28:       type ValidSmall = 100n;
                                       ^^^^ BigInt bigint literal \`100n\` [1] is not yet supported. [bigint-unsupported]
            References:
             28:       type ValidSmall = 100n;
                                         ^^^^ [1]

          test.js:29
           29:       type ValidNegSmall = -1n;
                                          ^^^ BigInt bigint literal \`-1n\` [1] is not yet supported. [bigint-unsupported]
            References:
             29:       type ValidNegSmall = -1n;
                                            ^^^ [1]
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
]);
