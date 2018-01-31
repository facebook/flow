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
                                     ^^^^^ Cannot cast string to \`BT\` because string [1] is incompatible with number [2].
                                 References:
                                   7: ("str": BT);
                                       ^^^^^ [1]: string
                                   7: ("str": BT);
                                              ^^ [2]: number
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
                                        ^^ Cannot cast number to \`BDefault\` because number [1] is incompatible with \`Def\` [2].
                                    References:
                                      9: (42: BDefault);
                                          ^^ [1]: number
                                      9: (42: BDefault);
                                              ^^^^^^^^ [2]: \`Def\`
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
                                    ^^^^ object literal. This type is incompatible with
                               18: (cVal: DT);
                                          ^^ DT
                                Property \`C\` is incompatible:
                                   18: (cVal: DT);
                                              ^^ property \`C\`. Property not found in
                                   18: (cVal: DT);
                                        ^^^^ object literal
                            `,
                          ),
    addCode('(cVal.D: CT);').newErrors(
                              `
                                test.js:20
                                 20: (cVal.D: CT);
                                      ^^^^^^ object literal. This type is incompatible with
                                 20: (cVal.D: CT);
                                              ^^ CT
                                  Property \`D\` is incompatible:
                                     20: (cVal.D: CT);
                                                  ^^ property \`D\`. Property not found in
                                     20: (cVal.D: CT);
                                          ^^^^^^ object literal
                              `,
                            ),

    addCode('(dVal: DT);').noNewErrors(),
    addCode('(dVal.C: CT);').noNewErrors(),
    addCode('(dVal: CT);').newErrors(
                            `
                              test.js:26
                               26: (dVal: CT);
                                    ^^^^ object literal. This type is incompatible with
                               26: (dVal: CT);
                                          ^^ CT
                                Property \`D\` is incompatible:
                                   26: (dVal: CT);
                                              ^^ property \`D\`. Property not found in
                                   26: (dVal: CT);
                                        ^^^^ object literal
                            `,
                          ),
    addCode('(dVal.C: DT);').newErrors(
                              `
                                test.js:28
                                 28: (dVal.C: DT);
                                      ^^^^^^ object literal. This type is incompatible with
                                 28: (dVal.C: DT);
                                              ^^ DT
                                  Property \`C\` is incompatible:
                                     28: (dVal.C: DT);
                                                  ^^ property \`C\`. Property not found in
                                     28: (dVal.C: DT);
                                          ^^^^^^ object literal
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
