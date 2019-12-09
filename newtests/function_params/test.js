/*
 * @flow
 */


import {suite, test} from '../../packages/flow-dev-tools/src/test/Tester';

export default suite(({addFile, addFiles, addCode}) => [
  test('Non explicit optional param are required', [
    addCode(`
      function fn(a: ?string) {
        return a;
      }

      fn();
    `).newErrors(
        `
          test.js:8
            8:       fn();
                     ^^ Cannot call \`fn\` because function [1] requires another argument.
            References:
              4:       function fn(a: ?string) {
                       ^^^^^^^^^^^^^^^^^^^^^^^ [1]
        `,
      )
  ]),

  test('Non explicit optional generic param are required', [
    addCode(`
      function fn<T>(a: T) {
        return a;
      }

      fn();
    `).newErrors(
        `
          test.js:8
            8:       fn();
                     ^^ Cannot call \`fn\` because function [1] requires another argument.
            References:
              4:       function fn<T>(a: T) {
                       ^^^^^^^^^^^^^^^^^^^^ [1]
        `,
      )
  ]),
]);
