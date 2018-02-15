/*
 * @flow
 * @lint-ignore-every LINEWRAP1
 */


import {suite, test} from '../../packages/flow-dev-tools/src/test/Tester';

export default suite(({addFile, addFiles, addCode}) => [
  test('const named imports', [
    addFile('dep.js'),
    addCode('import {named} from "./dep.js"; named = 43;').newErrors(
                                                            `
                                                              test.js:3
                                                                3: import {named} from "./dep.js"; named = 43;
                                                                                                   ^^^^^ Cannot reassign import \`named\` [1].
                                                                References:
                                                                  3: import {named} from "./dep.js"; named = 43;
                                                                             ^^^^^ [1]: \`named\`
                                                            `,
                                                          ),
  ]),

  test('const default imports', [
    addFile('dep.js'),
    addCode('import def from "./dep.js"; def = "nope";').newErrors(
                                                          `
                                                            test.js:3
                                                              3: import def from "./dep.js"; def = "nope";
                                                                                             ^^^ Cannot reassign import \`def\` [1].
                                                              References:
                                                                3: import def from "./dep.js"; def = "nope";
                                                                          ^^^ [1]: \`def\`
                                                          `,
                                                        )
  ]),

  test('const namespace imports', [
    addFile('dep.js'),
    addCode('import * as ns from "./dep.js"; ns = {};').newErrors(
                                                         `
                                                           test.js:3
                                                             3: import * as ns from "./dep.js"; ns = {};
                                                                                                ^^ Cannot reassign import \`ns\` [1].
                                                             References:
                                                               3: import * as ns from "./dep.js"; ns = {};
                                                                              ^^ [1]: \`ns\`
                                                         `,
                                                       )
  ]),
]);
