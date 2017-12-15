/*
 * @flow
 * @lint-ignore-every LINEWRAP1
 */


import {suite, test} from '../../tsrc/test/Tester';

export default suite(({addFile, addFiles, addCode}) => [
  test('Unaliased type import', [
    addFile('esmodule.js')
      .addCode('import {type T, C} from "./esmodule";')
      .addCode('new C();')
      .addCode('(42: T);')
        .noNewErrors(),

    addCode('("str": T);')
      .newErrors(
        `
          test.js:9
            9: ("str": T);
                ^^^^^ string. This type is incompatible with
            9: ("str": T);
                       ^ T
        `,
      ),
  ]),

  test('Aliased type import', [
    addFile('esmodule.js')
      .addCode('import {type T as U, C} from "./esmodule";')
      .addCode('new C();')
      .addCode('(42: U);')
        .noNewErrors(),

    addCode('("str": U);')
      .newErrors(
        `
          test.js:9
            9: ("str": U);
                ^^^^^ string. This type is incompatible with
            9: ("str": U);
                       ^ T
        `,
      ),
  ]),

  test('Unaliased typeof import', [
    addFile('esmodule.js')
      .addCode('import {typeof C, C as CImpl} from "./esmodule";')
      .addCode('new CImpl();')
      .addCode('(CImpl: C);')
        .noNewErrors(),

    addCode('("str": C);')
      .newErrors(
        `
          test.js:9
            9: ("str": C);
                ^^^^^ string. This type is incompatible with
            9: ("str": C);
                       ^ statics of \`C\`
        `,
      ),
  ]),

  test('Aliased type import', [
    addFile('esmodule.js')
      .addCode('import {typeof C as CPrime, C as CImpl} from "./esmodule";')
      .addCode('new CImpl();')
      .addCode('(CImpl: CPrime);')
        .noNewErrors(),

    addCode('("str": CPrime);')
      .newErrors(
        `
          test.js:9
            9: ("str": CPrime);
                ^^^^^ string. This type is incompatible with
            9: ("str": CPrime);
                       ^^^^^^ statics of \`C\`
        `,
      ),
  ]),
]);
