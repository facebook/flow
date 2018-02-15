/*
 * @flow
 * @lint-ignore-every LINEWRAP1
 */


import {suite, test} from '../../packages/flow-dev-tools/src/test/Tester';

export default suite(({addFile, addFiles, addCode}) => [
  test('import named', [
    addFile('flow-typed/lib.js'),
    addCode('import type {BT} from "B";').noNewErrors(),
    addCode('(42: BT);').noNewErrors(),
    addCode('("str": BT);').newErrors(
                             `
                               test.js:7
                                 7: ("str": BT);
                                     ^^^^^ Cannot cast \`"str"\` to \`BT\` because string [1] is incompatible with number [2].
                                 References:
                                   7: ("str": BT);
                                       ^^^^^ [1]
                                   7: ("str": BT);
                                              ^^ [2]
                             `,
                           ),
  ]),
  test('import default', [
    addFile('flow-typed/lib.js'),
    addCode('import type BDefault from "B";').noNewErrors(),
    addCode('import BDefaultValue from "B";').noNewErrors(),
    addCode('(new BDefaultValue(): BDefault);').noNewErrors(),
    addCode('(42: BDefault);').newErrors(
                                `
                                  test.js:9
                                    9: (42: BDefault);
                                        ^^ Cannot cast \`42\` to \`BDefault\` because number [1] is incompatible with \`Def\` [2].
                                    References:
                                      9: (42: BDefault);
                                          ^^ [1]
                                      9: (42: BDefault);
                                              ^^^^^^^^ [2]
                                `,
                              ),
  ]),
  test('import between libdef files', [
    addFile('flow-typed/C.js'),
    addFile('flow-typed/D.js'),
    addCode('import type {CT} from "C";').noNewErrors(),
    addCode('import type {DT} from "D";').noNewErrors(),
    addCode(`
      const cVal = {};
      const dVal = {};
      cVal.D = dVal;
      dVal.C = cVal;
    `).noNewErrors(),

    addCode('(cVal: CT);').noNewErrors(),
    addCode('(cVal.D: DT);').noNewErrors(),
    addCode('(cVal: DT);').newErrors(
                            `
                              test.js:18
                               18: (cVal: DT);
                                    ^^^^ Cannot cast \`cVal\` to \`DT\` because property \`C\` is missing in object literal [1] but exists in \`DT\` [2].
                                References:
                                  8:       const cVal = {};
                                                        ^^ [1]
                                 18: (cVal: DT);
                                            ^^ [2]
                            `,
                          ),
    addCode('(cVal.D: CT);').newErrors(
                              `
                                test.js:20
                                 20: (cVal.D: CT);
                                      ^^^^^^ Cannot cast \`cVal.D\` to \`CT\` because property \`D\` is missing in object literal [1] but exists in \`CT\` [2].
                                  References:
                                    9:       const dVal = {};
                                                          ^^ [1]
                                   20: (cVal.D: CT);
                                                ^^ [2]
                              `,
                            ),

    addCode('(dVal: DT);').noNewErrors(),
    addCode('(dVal.C: CT);').noNewErrors(),
    addCode('(dVal: CT);').newErrors(
                            `
                              test.js:26
                               26: (dVal: CT);
                                    ^^^^ Cannot cast \`dVal\` to \`CT\` because property \`D\` is missing in object literal [1] but exists in \`CT\` [2].
                                References:
                                  9:       const dVal = {};
                                                        ^^ [1]
                                 26: (dVal: CT);
                                            ^^ [2]
                            `,
                          ),
    addCode('(dVal.C: DT);').newErrors(
                              `
                                test.js:28
                                 28: (dVal.C: DT);
                                      ^^^^^^ Cannot cast \`dVal.C\` to \`DT\` because property \`C\` is missing in object literal [1] but exists in \`DT\` [2].
                                  References:
                                    8:       const cVal = {};
                                                          ^^ [1]
                                   28: (dVal.C: DT);
                                                ^^ [2]
                              `,
                            ),
  ]),

  /**
   * TODO: At the moment it isn't possible to import a non-libdef module from a
   *       libdef. There's no good reason to ban this, it's just a limitation of
   *       the way Flow handles libdefs at the moment. We should fix this test
   *       to pass at some point.
   */
  /*
  test('import from non-libdef within a libdef file', [
    addFile('node_modules/RealModule/index.js').noNewErrors(),
    addFile('flow-typed/dependsOnRealModule.js').noNewErrors(),
    addCode('import {T} from "DependsOnRealModule";').noNewErrors(),
    addCode('(42: T);').noNewErrors(),
    addCode('("str": T);').newErrors(
      `...Fill this in when this test runs as expected...`
    ),
  ]),
  */
]);
