/*
 * @flow
 * @lint-ignore-every LINE_WRAP1
 */


import {suite, test} from '../../tsrc/test/Tester';

export default suite(({addFile, addFiles, addCode}) => [
  test('Functions with unions: https://github.com/facebook/flow/issues/1948', [
    addFile('issue-1948.js').noNewErrors(),
  ]),
]);
