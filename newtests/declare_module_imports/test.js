/*
 * @flow
 */


import type {Suite} from "flow-dev-tools/src/test/Suite";
const {suite, test} = require('flow-dev-tools/src/test/Tester');

module.exports = (suite(({addFile, addFiles, addCode}) => [
  test('import named', [
    addFile('flow-typed/lib.js'),
    addCode('import type {BT} from "B";').noNewErrors(),
    addCode('(42: BT);').noNewErrors(),
    addCode('("str": BT);').newErrors(
                             `
                               test.js:7
                                 7: ("str": BT);
                                     ^^^^^ Cannot cast \`"str"\` to \`BT\` because string [1] is incompatible with number [2]. [incompatible-cast]
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
                                        ^^ Cannot cast \`42\` to \`BDefault\` because number [1] is incompatible with \`Def\` [2]. [incompatible-cast]
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
    addCode('(cVal: DT);').noNewErrors(),
    addCode('(cVal.D: CT);').noNewErrors(),

    addCode('(dVal: DT);').noNewErrors(),
    addCode('(dVal.C: CT);').noNewErrors(),
    addCode('(dVal: CT);').noNewErrors(),
    addCode('(dVal.C: DT);').noNewErrors(),
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
]): Suite);
