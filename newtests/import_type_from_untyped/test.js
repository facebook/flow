/* @flow */


import {suite, test} from '../../tsrc/test/Tester';

export default suite(({addFile, addFiles, addCode}) => [
  test('importing a type from an untyped module errors', [
    addFile('untyped.js'),
    addCode('import type {TNamed} from "./untyped";').newErrors(`
      test.js:3
        3: import type {TNamed} from "./untyped";
                        ^^^^^^ Named import from module ${'`'}./untyped${'`'}. Importing a type from an untyped module is not safe! Did you mean to add ${'`'}// @flow${'`'} to the top of the module that exports ${'`'}TNamed${'`'}?
    `),
    addCode('import type TDefault from "./untyped";').newErrors(`
      test.js:5
        5: import type TDefault from "./untyped";
                       ^^^^^^^^ Default import from ${'`'}./untyped${'`'}. Importing a type from an untyped module is not safe! Did you mean to add ${'`'}// @flow${'`'} to the top of ${'`'}./untyped${'`'}?
    `)
  ]),
]);
