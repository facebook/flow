/* @flow */


import {suite, test} from '../../tsrc/test/Tester';

export default suite(({addFile, addFiles, addCode}) => [
  test('while(true) { return; } forwards the return abnormal', [
    addCode(`
      function a(): string {
        while (true) {
          return '';
        }
      }
    `).noNewErrors(),

    addCode(`
      function b(): string {
        while (true) {
          return '';
        }
        return '';
      }
    `).newErrors(`
      test.js:16
       16:         return '';
                   ^^^^^^^^^^ unreachable code
    `),
  ]),
]);
