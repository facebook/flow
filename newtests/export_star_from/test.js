/*
 * @flow
 * @lint-ignore-every LINE_WRAP1
 */


import {suite, test} from '../../tsrc/test/Tester';

export default suite(({addFile, addFiles, addCode}) => [
  test('local exports override remote exports', [
    addFile('./origin.js').noNewErrors(),
    addFile('./local_override1.js').noNewErrors(),

    addCode(`
      import {C} from "./local_override1";
      (C: string);
    `).noNewErrors(),

    addCode('(C: number);').newErrors(`
      test.js:8
        8: (C: number);
            ^ string. This type is incompatible with
        8: (C: number);
               ^^^^^^ number
    `),
  ]),

  test('local exports override remote exports regardless of export order', [
    addFile('./origin.js').noNewErrors(),
    addFile('./local_override2.js').noNewErrors(),

    addCode(`
      import {C} from "./local_override2";
      (C: string);
    `).noNewErrors(),

    addCode('(C: number);').newErrors(`
      test.js:8
        8: (C: number);
            ^ string. This type is incompatible with
        8: (C: number);
               ^^^^^^ number
    `),
  ]),
]);
