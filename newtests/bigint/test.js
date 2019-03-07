/*
 * @flow
 * @lint-ignore-every LINEWRAP1
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
                                           ^^^^ Invalid bigint literal
        `
    )
  ]),

  test("BigInt invalid decimal literal", [
    addCode(`
      const invalid_decimal = 1.0n;
    `).newErrors(
      `
          test.js:4
            4:       const invalid_decimal = 1.0n;
                                             ^^^^ Invalid bigint literal
        `
    )
  ]),

  test("BigInt invalid scientific type literal", [
    addCode(`
      type InvalidE = 2e9n;
    `).newErrors(
        `
          test.js:4
            4:       type InvalidE = 2e9n;
                                     ^^^^ Invalid bigint literal
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
                                       ^^^^ Invalid bigint literal
        `
    )
  ]),

  test("BigInt invalid octal legacy type literal", [
    addCode(`
      type InvalidOctalLegacy = 016432n;
    `).newErrors(
        `
          test.js:4
            4:       type InvalidOctalLegacy = 016432n;
                                                      ^ Octal literals are not allowed in strict mode.
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
                                                  ^^^^^^^ Octal literals are not allowed in strict mode.
        `
    )
  ]),

  test("BigInt is not supported yet", [
    addCode(`
      type S = bigint;
      type A = 1n;
      const valid_binary = 0b101011101n;
      const valid_hex = 0xfff123n;
      const valid_large = 9223372036854775807n;
      const valid_octal_new = 0o16432n;
      const valid_small = 100n;
    `).newErrors(
      `
          test.js:4
            4:       type S = bigint;
                              ^^^^^^ BigInt bigint [1] is not yet supported.
            References:
              4:       type S = bigint;
                                ^^^^^^ [1]

          test.js:5
            5:       type A = 1n;
                              ^^ BigInt bigint literal \`1n\` [1] is not yet supported.
            References:
              5:       type A = 1n;
                                ^^ [1]

          test.js:6
            6:       const valid_binary = 0b101011101n;
                                          ^^^^^^^^^^^^ BigInt bigint literal \`0b101011101n\` [1] is not yet supported.
            References:
              6:       const valid_binary = 0b101011101n;
                                            ^^^^^^^^^^^^ [1]

          test.js:7
            7:       const valid_hex = 0xfff123n;
                                       ^^^^^^^^^ BigInt bigint literal \`0xfff123n\` [1] is not yet supported.
            References:
              7:       const valid_hex = 0xfff123n;
                                         ^^^^^^^^^ [1]

          test.js:8
            8:       const valid_large = 9223372036854775807n;
                                         ^^^^^^^^^^^^^^^^^^^^ BigInt bigint literal \`9223372036854775807n\` [1] is not yet supported.
            References:
              8:       const valid_large = 9223372036854775807n;
                                           ^^^^^^^^^^^^^^^^^^^^ [1]

          test.js:9
            9:       const valid_octal_new = 0o16432n;
                                             ^^^^^^^^ BigInt bigint literal \`0o16432n\` [1] is not yet supported.
            References:
              9:       const valid_octal_new = 0o16432n;
                                               ^^^^^^^^ [1]

          test.js:10
           10:       const valid_small = 100n;
                                         ^^^^ BigInt bigint literal \`100n\` [1] is not yet supported.
            References:
             10:       const valid_small = 100n;
                                           ^^^^ [1]
        `
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
