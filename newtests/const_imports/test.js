/* @flow */


import {suite, test} from '../../tsrc/test/Tester';

export default suite(({addFile, addFiles, addCode}) => [
  test('const named imports', [
    addFile('dep.js'),
    addCode('import {named} from "./dep.js"; named = 43;').newErrors(`
      test.js:3
        3: import {named} from "./dep.js"; named = 43;
                                           ^^^^^ named. import cannot be reassigned
        3: import {named} from "./dep.js"; named = 43;
                   ^^^^^ import named
    `),
  ]),

  test('const default imports', [
    addFile('dep.js'),
    addCode('import def from "./dep.js"; def = "nope";').newErrors(`
      test.js:3
        3: import def from "./dep.js"; def = "nope";
                                       ^^^ def. import cannot be reassigned
        3: import def from "./dep.js"; def = "nope";
                  ^^^ import def
    `)
  ]),

  test('const namespace imports', [
    addFile('dep.js'),
    addCode('import * as ns from "./dep.js"; ns = {};').newErrors(`
      test.js:3
        3: import * as ns from "./dep.js"; ns = {};
                                           ^^ ns. import cannot be reassigned
        3: import * as ns from "./dep.js"; ns = {};
                       ^^ import ns
    `)
  ]),
]);
