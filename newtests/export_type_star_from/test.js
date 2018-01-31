/*
 * @flow
 * @lint-ignore-every LINEWRAP1
 */


import {suite, test} from '../../packages/flow-dev-tools/src/test/Tester';

export default suite(({addFile, addFiles, addCode}) => [
  test('forwards type but not value exports', [
    addFile('./origin.js').noNewErrors(),
    addFile('./forward_only.js').noNewErrors(),

    addCode(`
      import type {aType} from "./forward_only";
      ("asdf": aType);
    `).noNewErrors(),
    addCode('(42: aType)').newErrors(
                            `
                              test.js:8
                                8: (42: aType)
                                    ^^ Cannot cast number to \`aType\` because number [1] is incompatible with string [2].
                                References:
                                  8: (42: aType)
                                      ^^ [1]: number
                                  8: (42: aType)
                                          ^^^^^ [2]: string
                            `,
                          ),














    addCode('import type {nope} from "./forward_only";').newErrors(`
      test.js:10
        10: import type {nope} from "./forward_only";
                         ^^^^ Named import from module ${'`'}./forward_only${'`'}. This module has no named export called ${'`'}nope${'`'}.
    `),
    addCode('import {aNum} from "./forward_only";').newErrors(`
      test.js:12
        12: import {aNum} from "./forward_only";
                        ^^^^ Named import from module ${'`'}./forward_only${'`'}. This module has no named export called ${'`'}aNum${'`'}.
    `),
  ]),

  test('forwards types alongside existing type exports', [
    addFile('./origin.js').noNewErrors(),
    addFile('./forward_with_exports.js').noNewErrors(),

    addCode(`
      import {
        type aType,
        type middleType,
        middleString,
      } from "./forward_with_exports";
    `).noNewErrors(),
    addCode('import {aNum} from "./forward_with_exports";').newErrors(`
      test.js:11
        11: import {aNum} from "./forward_with_exports";
                    ^^^^ Named import from module ${'`'}./forward_with_exports${'`'}. This module has no named export called ${'`'}aNum${'`'}.
    `),
  ]),

  test('local exports override remote exports', [
    addFile('./origin.js').noNewErrors(),
    addFile('./local_override1.js').noNewErrors(),

    addCode(`
      import {type aType} from "./local_override1";
      (42: aType);
    `).noNewErrors(),

    addCode('("asdf": aType);').newErrors(
                                 `
                                   test.js:8
                                     8: ("asdf": aType);
                                         ^^^^^^ Cannot cast string to \`aType\` because string [1] is incompatible with number [2].
                                     References:
                                       8: ("asdf": aType);
                                           ^^^^^^ [1]: string
                                       8: ("asdf": aType);
                                                   ^^^^^ [2]: number
                                 `,
                               ),
  ]),

  test('local exports override remote exports regardless of export order', [
    addFile('./origin.js').noNewErrors(),
    addFile('./local_override2.js').noNewErrors(),

    addCode(`
      import {type aType} from "./local_override2";
      (42: aType);
    `).noNewErrors(),

    addCode('("asdf": aType);').newErrors(
                                 `
                                   test.js:8
                                     8: ("asdf": aType);
                                         ^^^^^^ Cannot cast string to \`aType\` because string [1] is incompatible with number [2].
                                     References:
                                       8: ("asdf": aType);
                                           ^^^^^^ [1]: string
                                       8: ("asdf": aType);
                                                   ^^^^^ [2]: number
                                 `,
                               ),
  ]),
]);
