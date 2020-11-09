/*
 * @flow
 */


import type Suite from "flow-dev-tools/src/test/Suite.js";
import {suite, test} from 'flow-dev-tools/src/test/Tester';

export default (suite(({addFile, addFiles, addCode}) => [
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
                ^^^^^ Cannot cast \`"str"\` to \`T\` because string [1] is incompatible with number [2]. [incompatible-cast]
            References:
              9: ("str": T);
                  ^^^^^ [1]
              9: ("str": T);
                         ^ [2]
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
                ^^^^^ Cannot cast \`"str"\` to \`U\` because string [1] is incompatible with number [2]. [incompatible-cast]
            References:
              9: ("str": U);
                  ^^^^^ [1]
              9: ("str": U);
                         ^ [2]
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
                ^^^^^ Cannot cast \`"str"\` to \`C\` because string [1] is incompatible with class \`C\` [2]. [incompatible-cast]
            References:
              9: ("str": C);
                  ^^^^^ [1]
              9: ("str": C);
                         ^ [2]
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
                ^^^^^ Cannot cast \`"str"\` to \`CPrime\` because string [1] is incompatible with class \`C\` [2]. [incompatible-cast]
            References:
              9: ("str": CPrime);
                  ^^^^^ [1]
              9: ("str": CPrime);
                         ^^^^^^ [2]
        `,
      ),
  ]),
]): Suite);
