/*
 * @flow
 * @lint-ignore-every LINE_WRAP1
 */


import {suite, test} from '../../tsrc/test/Tester';

export default suite(({addFile, addFiles, addCode}) => [
  test('forwards type but not value exports', [
    addFile('./origin.js').noNewErrors(),
    addFile('./forward_only.js').noNewErrors(),

    addCode(`
      import type {aType} from "./forward_only";
      ("asdf": aType);
    `).noNewErrors(''),
    addCode('(42: aType)').newErrors(`
      test.js:8
        8: (42: aType)
            ^^ number. This type is incompatible with
        8: (42: aType)
                ^^^^^ string
    `),
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

    addCode('("asdf": aType);').newErrors(`
      test.js:8
        8: ("asdf": aType);
            ^^^^^^ string. This type is incompatible with
        8: ("asdf": aType);
                    ^^^^^ number
    `),
  ]),

  test('local exports override remote exports regardless of export order', [
    addFile('./origin.js').noNewErrors(),
    addFile('./local_override2.js').noNewErrors(),

    addCode(`
      import {type aType} from "./local_override2";
      (42: aType);
    `).noNewErrors(),

    addCode('("asdf": aType);').newErrors(`
      test.js:8
        8: ("asdf": aType);
            ^^^^^^ string. This type is incompatible with
        8: ("asdf": aType);
                    ^^^^^ number
    `),
  ]),
]);
