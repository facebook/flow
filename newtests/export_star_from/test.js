/*
 * @flow
 */


import {suite, test} from 'flow-dev-tools/src/test/Tester';

export default suite(({addFile, addFiles, addCode}) => [
  test('local exports override remote exports', [
    addFile('./origin.js').noNewErrors(),
    addFile('./local_override1.js').noNewErrors(),

    addCode(`
      import {C} from "./local_override1";
      (C: string);
    `).noNewErrors(),

    addCode('(C: number);').newErrors(
                             `
                               test.js:8
                                 8: (C: number);
                                     ^ Cannot cast \`C\` to number because string [1] is incompatible with number [2]. [incompatible-cast]
                                 References:
                                   3: export const C = "asdf";
                                                       ^^^^^^ [1]. See: local_override1.js:3
                                   8: (C: number);
                                          ^^^^^^ [2]
                             `,
                           ),
  ]),

  test('local exports override remote exports regardless of export order', [
    addFile('./origin.js').noNewErrors(),
    addFile('./local_override2.js').noNewErrors(),

    addCode(`
      import {C} from "./local_override2";
      (C: string);
    `).noNewErrors(),

    addCode('(C: number);').newErrors(
                             `
                               test.js:8
                                 8: (C: number);
                                     ^ Cannot cast \`C\` to number because string [1] is incompatible with number [2]. [incompatible-cast]
                                 References:
                                   4: export const C = "asdf";
                                                       ^^^^^^ [1]. See: local_override2.js:4
                                   8: (C: number);
                                          ^^^^^^ [2]
                             `,
                           ),
  ]),
]);
